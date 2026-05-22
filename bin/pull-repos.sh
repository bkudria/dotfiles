#!/usr/bin/env bash
#
# pull-repos.sh — keep direct-subdirectory git repos in sync with origin
#
# For each subdirectory of $PWD that is a git repo:
#   1. Fetch (--all --prune).
#   2. Fast-forward the default branch (read from origin/HEAD), even when
#      it's not currently checked out.
#   3. If HEAD is not the default branch, also fast-forward HEAD against
#      its upstream (if one exists).
#
# Reports only what's unusual; on-main + up-to-date is the quiet path.
# Skipped (and reported): dirty working tree, missing origin/HEAD,
# detached HEAD, fetch failure, HEAD branch with no upstream, non-FF.
#
# Per-repo output streams live as each worker finishes; a categorized
# summary prints once all repos are done.
#
# Requires: git.
#
# Usage: pull-repos.sh [--dry-run] [--jobs N]
#
#   --dry-run    Print what would be done; make no ref-modifying changes.
#                (Still runs `git fetch` to get accurate info.)
#   --jobs N     Repos to process in parallel (default: number of CPU cores).
#   -h, --help   Show this help.

set -euo pipefail

# ───────────────────────────── Flags ──────────────────────────────────
DRY_RUN=0
MAX_JOBS=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)

usage() {
    sed -n '3,/^set -euo/p' "$0" | sed -e 's/^# \{0,1\}//' -e '$d'
}

while [[ $# -gt 0 ]]; do
    case "$1" in
    --dry-run)
        DRY_RUN=1
        shift
        ;;
    --jobs)
        [[ $# -ge 2 ]] || {
            echo "pull-repos: --jobs needs a value" >&2
            exit 2
        }
        MAX_JOBS="$2"
        shift 2
        ;;
    --jobs=*)
        MAX_JOBS="${1#--jobs=}"
        shift
        ;;
    -h | --help)
        usage
        exit 0
        ;;
    *)
        echo "pull-repos: unknown arg: $1" >&2
        exit 2
        ;;
    esac
done

[[ "$MAX_JOBS" =~ ^[0-9]+$ ]] && ((MAX_JOBS >= 1)) ||
    {
        echo "pull-repos: --jobs must be a positive integer" >&2
        exit 2
    }

# ──────────────────────────── Preflight ───────────────────────────────
command -v git >/dev/null 2>&1 ||
    {
        echo "pull-repos: required tool not found: git" >&2
        exit 1
    }

# ───────────────────────────── State ──────────────────────────────────
TMPDIR="$(mktemp -d -t pull-repos.XXXXXX)"
trap 'rm -rf "$TMPDIR"' EXIT

# ────────────────────────── Inner git worker ──────────────────────────
# Emits structured key=value lines to its stdout (caller redirects to logfile).
_process_repo_inner() {
    local dir="$1"

    if ! cd "$dir" 2>/dev/null; then
        echo "STATUS=enter-failed"
        return 0
    fi

    if ! git rev-parse --git-dir >/dev/null 2>&1; then
        echo "STATUS=not-a-repo"
        return 0
    fi

    local porcelain
    porcelain=$(git status --porcelain)
    local untracked_n=0 modified_n=0
    if [[ -n "$porcelain" ]]; then
        untracked_n=$(printf '%s\n' "$porcelain" | grep -c '^??' || true)
        modified_n=$(printf '%s\n' "$porcelain" | grep -cv '^??' || true)
    fi

    if ((modified_n > 0)); then
        echo "STATUS=dirty"
        echo "DIRTY_COUNT=$modified_n"
        if ((untracked_n > 0)); then echo "UNTRACKED_COUNT=$untracked_n"; fi
        return 0
    fi
    if ((untracked_n > 0)); then echo "UNTRACKED_COUNT=$untracked_n"; fi

    local default
    if ! default=$(git symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null); then
        echo "STATUS=missing-origin-head"
        return 0
    fi
    default="${default#origin/}"
    echo "DEFAULT=$default"

    local current
    if ! current=$(git symbolic-ref --short HEAD 2>/dev/null); then
        echo "STATUS=detached"
        return 0
    fi
    echo "CURRENT=$current"

    local on_main=0
    [[ "$current" == "$default" ]] && on_main=1
    # Reset $? — bash `set -e` is twitchy after a `&&` whose lhs was false.
    :

    # Fetch
    local fetch_err
    if ! fetch_err=$(git fetch --all --prune --quiet 2>&1 >/dev/null); then
        echo "STATUS=fetch-failed"
        local reason
        reason=$(printf '%s\n' "$fetch_err" |
            grep -E '^(fatal:|error:|Permission denied|.*Permission denied)' |
            head -1)
        [[ -z "$reason" ]] && reason=$(printf '%s\n' "$fetch_err" | head -1)
        echo "FETCH_ERR=$reason"
        return 0
    fi

    if ! git rev-parse --verify --quiet "origin/$default" >/dev/null; then
        echo "STATUS=remote-default-gone"
        return 0
    fi

    # HEAD off-main with no resolvable upstream:
    #   - No upstream config ever → STATUS=no-upstream
    #   - Upstream configured but pruned on origin → run a cherry-pick gate
    #     against origin/<default>. Empty result (every branch commit has a
    #     patch-id equivalent in origin/<default>) means it's safe to switch.
    #     Otherwise → STATUS=upstream-gone with the unique-commit count.
    local switched_from=""
    if ((!on_main)); then
        if ! git rev-parse --abbrev-ref --symbolic-full-name "$current@{upstream}" >/dev/null 2>&1; then
            if ! git config --get "branch.${current}.remote" >/dev/null 2>&1; then
                echo "STATUS=no-upstream"
                return 0
            fi

            local gate_out
            if ! gate_out=$(git log --cherry-pick --right-only --format=%H \
                "origin/${default}...${current}" 2>/dev/null); then
                echo "STATUS=upstream-gone"
                echo "GATE_ERROR=1"
                return 0
            fi

            local unique_n=0
            if [[ -n "$gate_out" ]]; then
                unique_n=$(grep -c '.' <<<"$gate_out" || true)
            fi

            # Cherry-pick alone misses squash merges (N commits → 1 combined
            # commit on main; no individual patch-id match). Second-pass via
            # in-memory merge: if merging this branch into origin/<default>
            # produces a tree identical to origin/<default>'s tree, the
            # branch's *content* is already in main (squash or otherwise).
            local content_subsumed=0
            if ((unique_n > 0)); then
                local merged_tree main_tree
                if merged_tree=$(git merge-tree --write-tree \
                    "origin/${default}" "$current" 2>/dev/null); then
                    main_tree=$(git rev-parse "origin/${default}^{tree}")
                    if [[ "$merged_tree" == "$main_tree" ]]; then
                        content_subsumed=1
                    fi
                fi
            fi

            if ((unique_n > 0)) && ((!content_subsumed)); then
                echo "STATUS=upstream-gone"
                echo "UNIQUE_COMMITS=$unique_n"
                return 0
            fi

            # Gate passed (either no unique commits, or branch content is
            # subsumed by origin/<default>). Safe to switch.
            if ((DRY_RUN)); then
                switched_from="$current"
                current="$default"
                on_main=1
            elif git checkout --quiet "$default" 2>/dev/null; then
                switched_from="$current"
                current="$default"
                on_main=1
            else
                echo "STATUS=upstream-gone"
                echo "SWITCH_FAILED=1"
                return 0
            fi
        fi
    fi

    echo "STATUS=ok"
    if [[ -n "$switched_from" ]]; then echo "SWITCHED_FROM=$switched_from"; fi

    # FF the default branch (works whether or not it's checked out)
    local default_status="up-to-date"
    local default_sha origin_default_sha
    default_sha=$(git rev-parse --verify --quiet "$default" 2>/dev/null || echo "")
    origin_default_sha=$(git rev-parse "origin/$default")

    if [[ -z "$default_sha" ]]; then
        default_status="missing-local"
    elif [[ "$default_sha" != "$origin_default_sha" ]]; then
        if git merge-base --is-ancestor "$default" "origin/$default"; then
            if ((DRY_RUN)); then
                default_status="would-update"
            elif ((on_main)); then
                if git merge --ff-only --quiet "origin/$default" 2>/dev/null; then
                    default_status="updated"
                else
                    default_status="non-ff"
                fi
            else
                if git update-ref "refs/heads/$default" "origin/$default" 2>/dev/null; then
                    default_status="updated"
                else
                    default_status="non-ff"
                fi
            fi
        else
            default_status="non-ff"
        fi
    fi
    echo "DEFAULT_STATUS=$default_status"

    # FF the current branch (only if non-main; main is already handled)
    if ((!on_main)); then
        local upstream b_sha u_sha branch_status="up-to-date"
        upstream=$(git rev-parse --abbrev-ref --symbolic-full-name "$current@{upstream}")
        b_sha=$(git rev-parse "$current")
        u_sha=$(git rev-parse --verify --quiet "$upstream" 2>/dev/null || echo "")

        if [[ -z "$u_sha" ]]; then
            branch_status="upstream-gone"
        elif [[ "$b_sha" != "$u_sha" ]]; then
            if git merge-base --is-ancestor "$current" "$upstream"; then
                if ((DRY_RUN)); then
                    branch_status="would-update"
                elif git merge --ff-only --quiet "$upstream" 2>/dev/null; then
                    branch_status="updated"
                else
                    branch_status="non-ff"
                fi
            else
                branch_status="non-ff"
            fi
        fi
        echo "CURRENT_STATUS=$branch_status"
    fi

    return 0
}

# ──────────────────────────── Helpers ─────────────────────────────────
field() { awk -F= -v k="$1" '$1==k{sub(/^[^=]+=/,""); print; exit}' "$2"; }

# Format one repo's display line. Pure: reads logfile, prints one line.
render_line() {
    local rn="$1" logfile="$2" maxw="$3" dry_run="$4"
    local fmt="%-${maxw}s  %-13s  %s\n"

    if [[ ! -f "$logfile" ]]; then
        printf "$fmt" "$rn" "ERR" "no log written"
        return
    fi

    local status
    status=$(field STATUS "$logfile")
    case "$status" in
    ok)
        local current default dstat cstat
        current=$(field CURRENT "$logfile")
        default=$(field DEFAULT "$logfile")
        dstat=$(field DEFAULT_STATUS "$logfile")
        cstat=$(field CURRENT_STATUS "$logfile")

        local on_main=0
        [[ "$current" == "$default" ]] && on_main=1
        :

        local display_status="ok"
        local -a bits=()

        local switched_from
        switched_from=$(field SWITCHED_FROM "$logfile")
        if [[ -n "$switched_from" ]]; then
            if ((dry_run)); then
                bits+=("would switch from $switched_from to $default")
            else
                bits+=("switched from $switched_from to $default")
            fi
        fi

        if ((on_main)); then
            case "$dstat" in
            up-to-date)
                # Suppress "up-to-date" when the switch message already implies it.
                if [[ -z "$switched_from" ]]; then bits+=("up-to-date"); fi
                ;;
            updated) bits+=("FF'd $default") ;;
            would-update) bits+=("would FF $default") ;;
            non-ff)
                bits+=("$default NON-FF")
                display_status="non-FF"
                ;;
            missing-local)
                bits+=("$default missing locally")
                display_status="action"
                ;;
            esac
        else
            case "$cstat" in
            up-to-date) bits+=("$current up-to-date") ;;
            updated) bits+=("FF'd $current") ;;
            would-update) bits+=("would FF $current") ;;
            non-ff)
                bits+=("$current NON-FF")
                display_status="non-FF"
                ;;
            upstream-gone)
                bits+=("$current: upstream gone")
                display_status="action"
                ;;
            esac
            case "$dstat" in
            updated) bits+=("$default FF'd") ;;
            would-update) bits+=("would FF $default") ;;
            non-ff)
                bits+=("$default NON-FF")
                display_status="non-FF"
                ;;
            missing-local)
                bits+=("$default missing locally")
                display_status="action"
                ;;
                # up-to-date: omit — not interesting when we're on a feature branch
            esac
        fi

        local untracked
        untracked=$(field UNTRACKED_COUNT "$logfile")
        if [[ -n "$untracked" ]] && ((untracked > 0)); then
            bits+=("$untracked untracked")
        fi

        local summary=""
        local bit
        for bit in "${bits[@]}"; do
            if [[ -z "$summary" ]]; then summary="$bit"; else summary="$summary, $bit"; fi
        done
        [[ -z "$summary" ]] && summary="up-to-date"

        printf "$fmt" "$rn" "$display_status" "$summary"
        ;;
    no-upstream)
        local current u
        current=$(field CURRENT "$logfile")
        u=$(field UNTRACKED_COUNT "$logfile")
        if [[ -n "$u" ]] && ((u > 0)); then
            printf "$fmt" "$rn" "no-upstream" "$current (local only, $u untracked)"
        else
            printf "$fmt" "$rn" "no-upstream" "$current (local only)"
        fi
        ;;
    upstream-gone)
        local current u unique sw_failed gate_err msg s
        current=$(field CURRENT "$logfile")
        u=$(field UNTRACKED_COUNT "$logfile")
        unique=$(field UNIQUE_COMMITS "$logfile")
        sw_failed=$(field SWITCH_FAILED "$logfile")
        gate_err=$(field GATE_ERROR "$logfile")

        if [[ -n "$unique" ]] && ((unique > 0)); then
            s=""
            if ((unique != 1)); then s="s"; fi
            msg="$current (origin ref pruned; $unique unique commit$s not in origin/$(field DEFAULT "$logfile"))"
        elif [[ "$sw_failed" == "1" ]]; then
            msg="$current (origin ref pruned; gate passed but checkout failed)"
        elif [[ "$gate_err" == "1" ]]; then
            msg="$current (origin ref pruned; cherry-pick gate errored)"
        else
            msg="$current (origin ref pruned; likely merged & deleted)"
        fi

        if [[ -n "$u" ]] && ((u > 0)); then
            msg="$msg, $u untracked"
        fi
        printf "$fmt" "$rn" "upstream-gone" "$msg"
        ;;
    dirty)
        local n u
        n=$(field DIRTY_COUNT "$logfile")
        u=$(field UNTRACKED_COUNT "$logfile")
        if [[ -n "$u" ]] && ((u > 0)); then
            printf "$fmt" "$rn" "dirty" "skipped ($n modified, $u untracked)"
        else
            printf "$fmt" "$rn" "dirty" "skipped ($n modified)"
        fi
        ;;
    missing-origin-head)
        printf "$fmt" "$rn" "no-head" "origin/HEAD unset (run: git remote set-head origin -a)"
        ;;
    fetch-failed)
        local reason
        reason=$(field FETCH_ERR "$logfile")
        printf "$fmt" "$rn" "fetch-err" "${reason:-git fetch failed}"
        ;;
    detached)
        printf "$fmt" "$rn" "detached" "HEAD is detached"
        ;;
    remote-default-gone)
        printf "$fmt" "$rn" "no-remote" "origin/$(field DEFAULT "$logfile") not present after fetch"
        ;;
    not-a-repo | enter-failed | *)
        printf "$fmt" "$rn" "skipped" "${status:-unknown}"
        ;;
    esac
}

# ────────────────────────── Per-repo wrapper ──────────────────────────
# Runs git work (output → logfile), then atomically deposits the rendered
# display line at $linefile so the live-output poll loop can pick it up.
process_repo() {
    local dir="$1" logfile="$2" linefile="$3" maxw="$4" dry_run="$5"
    _process_repo_inner "$dir" >"$logfile" 2>&1
    render_line "${dir%/}" "$logfile" "$maxw" "$dry_run" >"$linefile.partial"
    mv "$linefile.partial" "$linefile"
}

# ────────────────────────── Discover repos ────────────────────────────
repos=()
shopt -s nullglob
for d in */; do
    if (cd "$d" 2>/dev/null && git rev-parse --git-dir >/dev/null 2>&1); then
        repos+=("$d")
    fi
done
shopt -u nullglob

if ((${#repos[@]} == 0)); then
    echo "pull-repos: no git repos in $(pwd)" >&2
    exit 0
fi

maxw=0
for r in "${repos[@]}"; do
    rn="${r%/}"
    if ((${#rn} > maxw)); then maxw=${#rn}; fi
done

((DRY_RUN)) && echo "[dry-run] no ref-modifying changes will be made"
echo "scanning ${#repos[@]} repos (jobs=$MAX_JOBS)..."
echo

# ───────────── Dispatch (backgrounded) + live print loop ──────────────
{
    for repo in "${repos[@]}"; do
        while (($(jobs -rp | wc -l) >= MAX_JOBS)); do
            sleep 0.05
        done
        rn="${repo%/}"
        process_repo "$repo" \
            "$TMPDIR/${rn}.log" \
            "$TMPDIR/${rn}.line" \
            "$maxw" "$DRY_RUN" &
    done
    wait
} &
dispatcher_pid=$!

remaining=${#repos[@]}
while ((remaining > 0)); do
    any=0
    for repo in "${repos[@]}"; do
        rn="${repo%/}"
        f="$TMPDIR/${rn}.line"
        if [[ -f "$f" ]]; then
            cat "$f"
            rm "$f"
            remaining=$((remaining - 1))
            any=1
        fi
    done
    ((remaining > 0 && any == 0)) && sleep 0.05
done

wait "$dispatcher_pid" 2>/dev/null || true

# ───────────────────────── Summary (counts) ───────────────────────────
n_ok=0 n_action=0 n_dirty=0 n_missing=0 n_fetch=0
n_detached=0 n_no_upstream=0 n_upstream_gone=0 n_remote_gone=0 n_other=0
fetch_err_log="$TMPDIR/_fetch_errs"
: >"$fetch_err_log"

for r in "${repos[@]}"; do
    rn="${r%/}"
    logfile="$TMPDIR/${rn}.log"
    [[ -f "$logfile" ]] || {
        n_other=$((n_other + 1))
        continue
    }
    status=$(field STATUS "$logfile")
    case "$status" in
    ok)
        action=0
        dstat=$(field DEFAULT_STATUS "$logfile")
        cstat=$(field CURRENT_STATUS "$logfile")
        case "$dstat" in non-ff | missing-local) action=1 ;; esac
        case "$cstat" in non-ff | upstream-gone) action=1 ;; esac
        if ((action)); then
            n_action=$((n_action + 1))
        else
            n_ok=$((n_ok + 1))
        fi
        ;;
    no-upstream) n_no_upstream=$((n_no_upstream + 1)) ;;
    upstream-gone) n_upstream_gone=$((n_upstream_gone + 1)) ;;
    dirty) n_dirty=$((n_dirty + 1)) ;;
    missing-origin-head) n_missing=$((n_missing + 1)) ;;
    fetch-failed)
        reason=$(field FETCH_ERR "$logfile")
        printf '%s\t%s\n' "${reason:-git fetch failed}" "$rn" >>"$fetch_err_log"
        n_fetch=$((n_fetch + 1))
        ;;
    detached) n_detached=$((n_detached + 1)) ;;
    remote-default-gone) n_remote_gone=$((n_remote_gone + 1)) ;;
    *) n_other=$((n_other + 1)) ;;
    esac
done

echo
divlen=$((maxw + 30))
printf '%*s\n' "$divlen" '' | tr ' ' '─'
printf "%d repos scanned" "${#repos[@]}"
((DRY_RUN)) && printf "  [dry-run]"
echo
printf "  %d ok\n" "$n_ok"
((n_action)) && printf "  %d action needed (non-FF / upstream gone)\n" "$n_action"
((n_no_upstream)) && printf "  %d on local-only branch (no upstream)\n" "$n_no_upstream"
((n_upstream_gone)) && printf "  %d on branch whose origin ref was pruned (likely merged)\n" "$n_upstream_gone"
((n_dirty)) && printf "  %d dirty (skipped)\n" "$n_dirty"
((n_missing)) && printf "  %d missing origin/HEAD\n" "$n_missing"
((n_fetch)) && printf "  %d fetch failed\n" "$n_fetch"
((n_detached)) && printf "  %d detached HEAD\n" "$n_detached"
((n_remote_gone)) && printf "  %d remote default branch gone\n" "$n_remote_gone"
((n_other)) && printf "  %d other\n" "$n_other"

if [[ -s "$fetch_err_log" ]]; then
    echo
    echo "Fetch errors grouped by message:"
    awk -F'\t' '{counts[$1]++; repos[$1]=repos[$1]" "$2} END {
    for (msg in counts) printf "%d\t%s\t%s\n", counts[msg], msg, repos[msg]
  }' "$fetch_err_log" | sort -rn | while IFS=$'\t' read -r cnt msg rlist; do
        printf "  %d×  %s\n" "$cnt" "$msg"
        if ((cnt <= 5)); then
            printf "       repos:%s\n" "$rlist"
        fi
    done
    if grep -q "Permission denied (publickey)" "$fetch_err_log" 2>/dev/null; then
        echo "  → hint: ssh agent likely missing keys (try: ssh-add ~/.ssh/id_ed25519)"
    fi
fi

exit 0
