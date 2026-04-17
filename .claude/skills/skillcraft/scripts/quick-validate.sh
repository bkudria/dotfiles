#!/usr/bin/env bash
# quick-validate.sh — Fast structural validation for Claude Code skills
#
# Usage:
#   quick-validate.sh <skill-directory>
#   quick-validate.sh --all [<base-path>]
#
# Runs automated structural checks (subset of the full 44-item checklist).
# Use this for fast pre-flight; use the full checklist for deep audits.
#
# Checks performed:
#   S1: SKILL.md exists
#   S2: name matches directory
#   S3: Valid YAML frontmatter (basic check)
#   S4: Only valid frontmatter fields
#   S5: Referenced files exist
#   S6: Scripts executable
#   S7: No orphan files
#   M1: Name format (hyphen-case, ≤64 chars)
#   M2: Description present and within length bounds
#   M9: CSO red-flag detection (workflow summary warning)
#   M5: argument-hint present if $ARGUMENTS used
#   P5: Scripts have usage headers
#   C1: Second-person voice detection
#   C2: Wall of text (body >500 lines, no references/)
#
# Provenance checks (only when provenance.yml exists):
#   PV1: Files in curation_decisions exist
#   PV2: Provenance staleness (warn if >30 days since last check)

set -euo pipefail
shopt -s nullglob

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

PASS=0
FAIL=0
WARN=0

pass() { echo -e "  ${GREEN}PASS${NC} $1: $2"; PASS=$((PASS + 1)); }
fail() { echo -e "  ${RED}FAIL${NC} $1: $2"; FAIL=$((FAIL + 1)); }
warn() { echo -e "  ${YELLOW}WARN${NC} $1: $2"; WARN=$((WARN + 1)); }

VALID_FIELDS="name description disable-model-invocation user-invocable allowed-tools model context agent argument-hint hooks"

validate_skill() {
    local skill_dir="$1"
    local dir_name
    dir_name=$(basename "$skill_dir")

    echo ""
    echo "Validating: $dir_name"
    echo "  Path: $skill_dir"
    echo ""

    # S1: SKILL.md exists
    if [[ -f "$skill_dir/SKILL.md" ]]; then
        pass "S1" "SKILL.md exists"
    else
        fail "S1" "SKILL.md not found"
        echo ""
        echo "  Score: $PASS passed, $FAIL failed, $WARN warnings"
        return
    fi

    local skill_md="$skill_dir/SKILL.md"

    # Extract frontmatter
    local frontmatter=""
    if head -1 "$skill_md" | grep -q '^---'; then
        frontmatter=$(awk 'NR==1{next} /^---$/{exit} {print}' "$skill_md")
    fi

    # S3: Valid YAML frontmatter
    if [[ -z "$frontmatter" ]]; then
        fail "S3" "No frontmatter found (missing --- delimiters)"
    else
        # Check for name field
        local name_value
        name_value=$(echo "$frontmatter" | grep -E '^name:' | sed 's/^name:[[:space:]]*//' | tr -d '"' | tr -d "'" || true)
        if [[ -z "$name_value" ]]; then
            fail "S3" "Frontmatter missing 'name' field"
        else
            pass "S3" "Valid frontmatter with name field"

            # S2: name matches directory
            if [[ "$name_value" == "$dir_name" ]]; then
                pass "S2" "Name '$name_value' matches directory"
            else
                # Check for colon-separated sub-skill names (e.g., "parent:child")
                local base_name
                base_name=$(echo "$name_value" | cut -d: -f1)
                if [[ "$base_name" == "$dir_name" ]]; then
                    pass "S2" "Name prefix '$base_name' matches directory"
                else
                    fail "S2" "Name '$name_value' does not match directory '$dir_name'"
                fi
            fi

            # M1: Name format
            if echo "$name_value" | grep -qE '^[a-z0-9]([a-z0-9:-]*[a-z0-9])?$' && [[ ${#name_value} -le 64 ]]; then
                pass "M1" "Name format valid"
            else
                fail "M1" "Name must be hyphen-case, ≤64 chars (got '$name_value')"
            fi
        fi

        # S4: Only valid frontmatter fields
        local unknown_fields=""
        while IFS= read -r line; do
            local field
            field=$(echo "$line" | grep -oE '^[a-z][a-z-]*' || true)
            if [[ -n "$field" ]]; then
                local valid=false
                for vf in $VALID_FIELDS; do
                    if [[ "$field" == "$vf" ]]; then
                        valid=true
                        break
                    fi
                done
                if ! $valid; then
                    unknown_fields="$unknown_fields $field"
                fi
            fi
        done <<< "$frontmatter"

        if [[ -z "$unknown_fields" ]]; then
            pass "S4" "All frontmatter fields valid"
        else
            fail "S4" "Unknown frontmatter fields:$unknown_fields"
        fi

        # M2: Description present
        local desc
        desc=$(echo "$frontmatter" | grep -E '^description:' | sed 's/^description:[[:space:]]*//' || true)
        if [[ -z "$desc" ]]; then
            fail "M2" "Description missing"
        else
            local desc_len=${#desc}
            if [[ $desc_len -ge 10 && $desc_len -le 1024 ]]; then
                pass "M2" "Description present ($desc_len chars)"
            else
                fail "M2" "Description length $desc_len (must be 10-1024)"
            fi
        fi

        # M9: CSO red-flag detection — warn if description looks like a workflow summary
        # Note: "before"/"after"/"between" omitted — too common in legitimate trigger phrases
        if [[ -n "$desc" ]]; then
            local cso_flags=""
            # Process-sequence words that strongly suggest step ordering
            if echo "$desc" | grep -iE '(^| )(then |first |next |finally )' >/dev/null 2>&1; then
                cso_flags="process-sequence words"
            fi
            # Orchestration verbs
            if echo "$desc" | grep -iE '(dispatches|orchestrates|coordinates|delegates|routes to)' >/dev/null 2>&1; then
                cso_flags="${cso_flags:+$cso_flags, }orchestration verbs"
            fi
            # Step indicators
            if echo "$desc" | grep -iE '(step [0-9]|phase [0-9]|stage [0-9])' >/dev/null 2>&1; then
                cso_flags="${cso_flags:+$cso_flags, }step indicators"
            fi
            if [[ -n "$cso_flags" ]]; then
                warn "M9" "Description may contain workflow summary ($cso_flags) — review for CSO pitfall"
            else
                pass "M9" "No workflow-summary red flags in description"
            fi
        fi

        # M5: argument-hint if $ARGUMENTS used
        local uses_args=false
        if grep -q '\$ARGUMENTS\|{{ARGUMENTS}}' "$skill_md" 2>/dev/null; then
            uses_args=true
        fi
        local has_hint=false
        if echo "$frontmatter" | grep -q '^argument-hint:'; then
            has_hint=true
        fi
        if $uses_args && ! $has_hint; then
            fail "M5" "Uses \$ARGUMENTS but no argument-hint in frontmatter"
        elif $uses_args && $has_hint; then
            pass "M5" "argument-hint present for \$ARGUMENTS"
        else
            pass "M5" "No \$ARGUMENTS usage (check N/A)"
        fi
    fi

    # S5: Referenced files exist (only this skill's own files, not cross-skill refs)
    # Extract backtick-quoted relative paths like `references/foo.md` or `scripts/bar.sh`
    # but exclude paths that are part of another skill's full path
    local referenced_files
    referenced_files=$(grep -oE '`(references|scripts|assets)/[a-zA-Z0-9._-]+`' "$skill_md" 2>/dev/null \
        | tr -d '`' | sort -u || true)
    local missing_refs=0
    local checked_refs=0
    while IFS= read -r ref; do
        [[ -z "$ref" ]] && continue
        checked_refs=$((checked_refs + 1))
        if [[ ! -f "$skill_dir/$ref" ]]; then
            fail "S5" "Referenced file not found: $ref"
            missing_refs=$((missing_refs + 1))
        fi
    done <<< "$referenced_files"
    if [[ $checked_refs -gt 0 && $missing_refs -eq 0 ]]; then
        pass "S5" "All $checked_refs referenced files exist"
    elif [[ $checked_refs -eq 0 ]]; then
        pass "S5" "No file references to check"
    fi

    # S6: Scripts executable
    if [[ -d "$skill_dir/scripts" ]]; then
        local non_exec=0
        for script in "$skill_dir"/scripts/*.sh; do
            [[ ! -f "$script" ]] && continue
            if [[ ! -x "$script" ]]; then
                fail "S6" "Script not executable: $(basename "$script")"
                non_exec=$((non_exec + 1))
            fi
        done
        if [[ $non_exec -eq 0 ]]; then
            pass "S6" "All scripts executable"
        fi
    else
        pass "S6" "No scripts/ directory (check N/A)"
    fi

    # S7: Orphan files
    local orphans=""
    for dir in references scripts; do
        if [[ -d "$skill_dir/$dir" ]]; then
            for file in "$skill_dir/$dir"/*; do
                [[ ! -f "$file" ]] && continue
                local basename_file
                basename_file=$(basename "$file")
                if ! grep -q "$basename_file" "$skill_md" 2>/dev/null; then
                    orphans="$orphans $dir/$basename_file"
                fi
            done
        fi
    done
    if [[ -z "$orphans" ]]; then
        pass "S7" "No orphan files"
    else
        warn "S7" "Unreferenced files:$orphans"
    fi

    # P5: Scripts have usage headers
    if [[ -d "$skill_dir/scripts" ]]; then
        local scripts_without_usage=0
        for script in "$skill_dir"/scripts/*; do
            [[ ! -f "$script" ]] && continue
            if ! head -10 "$script" | grep -qi 'usage\|Usage' 2>/dev/null; then
                warn "P5" "No usage header: scripts/$(basename "$script")"
                scripts_without_usage=$((scripts_without_usage + 1))
            fi
        done
        if [[ $scripts_without_usage -eq 0 ]]; then
            pass "P5" "All scripts have usage headers"
        fi
    else
        pass "P5" "No scripts/ directory (check N/A)"
    fi

    # C2: Wall of text — body >500 lines with no references/ directory
    local body_text
    body_text=$(awk '/^---$/{if(++c==2){found=1;next}} found{print}' "$skill_md")
    local body_lines body_words approx_tokens
    body_lines=$(echo "$body_text" | wc -l | tr -d ' ')
    body_words=$(echo "$body_text" | wc -w | tr -d ' ')
    approx_tokens=$(( body_words * 13 / 10 ))  # words × 1.3
    if [[ $body_lines -gt 500 ]] && [[ ! -d "$skill_dir/references" ]]; then
        warn "C2" "Body is $body_lines lines, ~${approx_tokens} tokens with no references/ directory (wall of text)"
    elif [[ $body_lines -gt 500 ]]; then
        pass "C2" "Body is $body_lines lines, ~${approx_tokens} tokens (references/ exists)"
    else
        pass "C2" "Body length OK ($body_lines lines, ~${approx_tokens} tokens)"
    fi

    # C1: Second-person voice — "you should/can/will/need" in body
    # Exclude lines inside code blocks or that document the pattern itself
    local voice_count
    voice_count=$(awk '/^---$/{if(++c==2){found=1;next}} found{print}' "$skill_md" \
        | awk '/^```/{code=!code} !code{print}' \
        | grep -ciE '(you should|you can|you will|you need|you must)' || true)
    if [[ "$voice_count" -gt 0 ]]; then
        warn "C1" "Second-person voice found ($voice_count occurrences of 'you should/can/will/need/must')"
    else
        pass "C1" "No second-person voice detected"
    fi

    # --- Provenance checks (only when provenance.yml exists) ---

    local prov="$skill_dir/provenance.yml"
    if [[ -f "$prov" ]]; then
        if ! command -v yq &>/dev/null; then
            fail "PV0" "provenance.yml found but yq not installed (brew install yq)"
        else
            # PV1: Every file in curation_decisions exists
            local pv1_missing=0
            local pv1_checked=0
            while IFS= read -r file; do
                [[ -z "$file" ]] && continue
                pv1_checked=$((pv1_checked + 1))
                if [[ ! -f "$skill_dir/$file" ]]; then
                    fail "PV1" "File in curation_decisions not found: $file"
                    pv1_missing=$((pv1_missing + 1))
                fi
            done < <(yq '.curation_decisions | keys | .[]' "$prov")
            if [[ $pv1_checked -gt 0 && $pv1_missing -eq 0 ]]; then
                pass "PV1" "All $pv1_checked files in curation_decisions exist"
            fi

            # PV2: Staleness check
            local last_update
            last_update=$(yq '.last_full_update' "$prov" | tr -d '"')
            local last_epoch now_epoch days_ago
            last_epoch=$(date -j -f "%Y-%m-%d" "$last_update" +%s 2>/dev/null || date -d "$last_update" +%s 2>/dev/null || echo 0)
            now_epoch=$(date +%s)
            if [[ "$last_epoch" -gt 0 ]]; then
                days_ago=$(( (now_epoch - last_epoch) / 86400 ))
                if [[ $days_ago -gt 30 ]]; then
                    warn "PV2" "Provenance last checked $days_ago days ago ($last_update). Run check-upstream.sh"
                else
                    pass "PV2" "Provenance checked $days_ago days ago ($last_update)"
                fi
            else
                warn "PV2" "Could not parse last_full_update date: $last_update"
            fi

            # PV3: Source reference integrity — every source: ref in curation_decisions exists in sources
            local pv3_bad=0
            local pv3_checked=0
            local source_keys
            source_keys=$(yq '.sources | keys | .[]' "$prov" 2>/dev/null)
            while IFS= read -r src_ref; do
                [[ -z "$src_ref" ]] && continue
                pv3_checked=$((pv3_checked + 1))
                if ! echo "$source_keys" | grep -qxF "$src_ref"; then
                    fail "PV3" "Source ref '$src_ref' in curation_decisions not found in sources"
                    pv3_bad=$((pv3_bad + 1))
                fi
            done < <(yq '.curation_decisions[][] | select(.source) | .source' "$prov" 2>/dev/null | sort -u)
            if [[ $pv3_checked -gt 0 && $pv3_bad -eq 0 ]]; then
                pass "PV3" "All $pv3_checked source refs resolve to entries in sources"
            elif [[ $pv3_checked -eq 0 ]]; then
                pass "PV3" "No source refs to check (all entries are original)"
            fi

            # PV4: No orphan sources — every source in sources is referenced by at least one curation_decisions entry
            local pv4_orphan=0
            local pv4_checked=0
            local used_sources
            used_sources=$(yq '.curation_decisions[][] | select(.source) | .source' "$prov" 2>/dev/null | sort -u)
            while IFS= read -r src_name; do
                [[ -z "$src_name" ]] && continue
                pv4_checked=$((pv4_checked + 1))
                if ! echo "$used_sources" | grep -qxF "$src_name"; then
                    fail "PV4" "Source '$src_name' defined but never referenced in curation_decisions"
                    pv4_orphan=$((pv4_orphan + 1))
                fi
            done < <(yq '.sources | keys | .[]' "$prov" 2>/dev/null)
            if [[ $pv4_checked -gt 0 && $pv4_orphan -eq 0 ]]; then
                pass "PV4" "All $pv4_checked sources are referenced in curation_decisions"
            fi

            # PV5: Decision taxonomy validation — every decision value is from the allowed set
            local pv5_bad=0
            local pv5_checked=0
            local valid_decisions="kept simplified elided altered synthesized original"
            while IFS= read -r decision; do
                [[ -z "$decision" ]] && continue
                pv5_checked=$((pv5_checked + 1))
                if ! echo "$valid_decisions" | grep -qwF "$decision"; then
                    fail "PV5" "Invalid decision value: '$decision' (expected: $valid_decisions)"
                    pv5_bad=$((pv5_bad + 1))
                fi
            done < <(yq '.curation_decisions[][].decision' "$prov" 2>/dev/null | sort -u)
            if [[ $pv5_checked -gt 0 && $pv5_bad -eq 0 ]]; then
                pass "PV5" "All $pv5_checked decision values are valid"
            fi

            # PV6: No orphan files — every skill file should have a curation_decisions entry (warn only)
            local pv6_missing=0
            local pv6_checked=0
            local curated_files
            curated_files=$(yq '.curation_decisions | keys | .[]' "$prov" 2>/dev/null)
            for f in "$skill_dir"/SKILL.md "$skill_dir"/references/*.md "$skill_dir"/scripts/*; do
                [[ -f "$f" ]] || continue
                local rel_path="${f#$skill_dir/}"
                [[ "$rel_path" == "provenance.yml" ]] && continue
                pv6_checked=$((pv6_checked + 1))
                if ! echo "$curated_files" | grep -qxF "$rel_path"; then
                    warn "PV6" "File '$rel_path' has no curation_decisions entry"
                    pv6_missing=$((pv6_missing + 1))
                fi
            done
            if [[ $pv6_checked -gt 0 && $pv6_missing -eq 0 ]]; then
                pass "PV6" "All $pv6_checked skill files have curation_decisions entries"
            fi

        fi
    fi

    echo ""
    echo "  Score: $PASS passed, $FAIL failed, $WARN warnings"
}

# --- Main ---

if [[ $# -lt 1 ]]; then
    echo "Usage: quick-validate.sh <skill-directory>"
    echo "       quick-validate.sh --all [<base-path>]"
    exit 1
fi

TOTAL_PASS=0
TOTAL_FAIL=0
TOTAL_WARN=0

if [[ "$1" == "--all" ]]; then
    base="${2:-$HOME/.claude/skills}"
    base="${base/#\~/$HOME}"
    echo "Scanning all skills in: $base"
    for skill_dir in "$base"/*/; do
        [[ ! -f "$skill_dir/SKILL.md" ]] && continue
        PASS=0; FAIL=0; WARN=0
        validate_skill "${skill_dir%/}"
        TOTAL_PASS=$((TOTAL_PASS + PASS))
        TOTAL_FAIL=$((TOTAL_FAIL + FAIL))
        TOTAL_WARN=$((TOTAL_WARN + WARN))
    done
    echo ""
    echo "================================"
    echo "Total: $TOTAL_PASS passed, $TOTAL_FAIL failed, $TOTAL_WARN warnings"
else
    skill_path="${1/#\~/$HOME}"
    if [[ ! -d "$skill_path" ]]; then
        echo "Error: Directory not found: $skill_path"
        exit 1
    fi
    validate_skill "$skill_path"
    TOTAL_FAIL=$FAIL
fi

[[ $TOTAL_FAIL -gt 0 ]] && exit 1
exit 0
