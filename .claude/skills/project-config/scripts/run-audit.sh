#!/usr/bin/env bash
# run-audit.sh — Two-pass audit runner for the project-config skill.
#
# Every verb operates on a state-dir produced by --init. The state-dir holds
# canonical files: collect-required.json (round-1 output),
# collect-suggested.json (round-2 output, only after the gate passes),
# responses/<id>.txt (sub-agent verdicts), merged.json (union of present
# collect-*.json with sub-agent responses folded in).
#
# Suggested standards are gated behind required-pass: round 2 runs only if
# every required standard from round 1 has status PASS. This saves haiku
# dispatches on failing audits and keeps the output focused on FAILs.
#
# Usage:
#   run-audit.sh --init
#       Emits a fresh state-dir path on stdout.
#
#   run-audit.sh --collect <project-root> <state-dir> --scope <required|suggested>
#       Reads <project-root>/project.yaml, walks selected profile directories,
#       and writes <state-dir>/collect-<scope>.json with shape
#       {"resolved": [...], "pending": [...], ...}.
#       --scope required walks only effective-required standards (intrinsic
#       `required: true` OR id in project.yaml's `required:` overrides) and
#       additionally stashes `suggested_total` (count of would-have-been-
#       suggested standards, after the disabled filter).
#       --scope suggested walks only effective-suggested standards.
#       Each deterministic check (check.script) runs immediately and lands in
#       `resolved` with status PASS/FAIL/SUGG. Each prompt-based check
#       (check.prompt) goes to `pending` with its rendered prompt for
#       sub-agent verification, plus the response_path the agent should
#       Write its verdict to.
#
#   run-audit.sh --merge <state-dir>
#       Folds sub-agent responses into the collect outputs. Reads every
#       <state-dir>/collect-*.json present, looks up each pending entry's
#       response at <state-dir>/responses/<id>.txt, extracts the JSON
#       ({"met": bool, "detail": string}), and writes <state-dir>/merged.json
#       with every entry resolved to PASS/FAIL/SUGG. Missing files, parse
#       failures, or non-bool `met` resolve to FAIL. The output records
#       `scopes_collected` (which collect files were present) so render can
#       surface the skipped count when round 2 was gated out.
#
#   run-audit.sh --gate <state-dir>
#       Reads <state-dir>/merged.json and exits 0 if every effective-required
#       entry has status PASS, 1 if any has status FAIL. Operational errors
#       (missing file, malformed JSON) exit ≥2. Use this between round 1's
#       --merge and round 2's --collect --scope suggested to decide whether
#       to spend cost on suggested checks.
#
#   run-audit.sh --render <state-dir>
#       Reads <state-dir>/merged.json and emits the markdown audit table,
#       per-status counts, optional disabled-count line, and (when round 2
#       was skipped due to required failures) a count of suggested standards
#       skipped. Always exits 0 on successful render. For CI pass/fail
#       signal, use --check.
#
#   run-audit.sh --check <state-dir>
#       Reads <state-dir>/merged.json and exits 1 if any resolved entry has
#       status FAIL, 0 otherwise. Operational errors (missing file,
#       malformed JSON, unresolved pending entries) exit ≥2 to distinguish
#       from "audit had FAILs."
set -euo pipefail

SKILL_DIR="${CLAUDE_SKILL_DIR:-${HOME}/.claude/skills/project-config}"

usage() {
  echo "Usage:" >&2
  echo "  run-audit.sh --init" >&2
  echo "  run-audit.sh --collect <project-root> <state-dir> --scope <required|suggested>" >&2
  echo "  run-audit.sh --merge   <state-dir>" >&2
  echo "  run-audit.sh --gate    <state-dir>" >&2
  echo "  run-audit.sh --render  <state-dir>" >&2
  echo "  run-audit.sh --check   <state-dir>" >&2
  echo "" >&2
  echo "  Each verb except --init reads from / writes to canonical files inside" >&2
  echo "  <state-dir>: collect-required.json, collect-suggested.json (optional)," >&2
  echo "  responses/<id>.txt, merged.json. Use --init to obtain a fresh state-dir." >&2
  exit 1
}

[[ $# -ge 1 ]] || usage
MODE="$1"
shift

require_cmd() {
  for cmd in "$@"; do
    command -v "$cmd" >/dev/null 2>&1 || {
      echo "Error: $cmd is required" >&2
      exit 1
    }
  done
}
require_cmd yq jq

# ───── --init ───────────────────────────────────────────────────────────────

init() {
  local dir
  dir=$(mktemp -d "${TMPDIR:-/tmp}/project-config-audit.XXXXXX")
  echo "$dir"
}

# ───── project-context detection ────────────────────────────────────────────
# Detect language/runtime + (when applicable) package manager from manifest
# files in the project root. Emits a multi-line "Detected project context"
# block that the runner bakes into every prompt-based standard's rendered
# prompt file (state-dir/prompts/<id>.txt), so sub-agents skip the redundant
# discovery preamble. Emits empty string when no manifest matches; the runner
# then skips the header (graceful fallback).

detect_project_context() {
  local root="${1:-}"
  [[ -d "$root" ]] || { printf ''; return; }

  local language="" manifest="" pm=""

  if [[ -f "$root/package.json" ]]; then
    language="JavaScript/TypeScript (Node.js)"
    manifest="package.json"
    if [[ -f "$root/bun.lock" || -f "$root/bun.lockb" ]]; then
      pm="bun"
    elif [[ -f "$root/pnpm-lock.yaml" ]]; then
      pm="pnpm"
    elif [[ -f "$root/yarn.lock" ]]; then
      pm="yarn"
    elif [[ -f "$root/package-lock.json" ]]; then
      pm="npm"
    fi
  elif [[ -f "$root/Gemfile" ]]; then
    language="Ruby"
    manifest="Gemfile"
  elif [[ -f "$root/pyproject.toml" ]]; then
    language="Python"
    manifest="pyproject.toml"
  elif [[ -f "$root/requirements.txt" ]]; then
    language="Python"
    manifest="requirements.txt"
  elif [[ -f "$root/setup.py" ]]; then
    language="Python"
    manifest="setup.py"
  elif [[ -f "$root/Cargo.toml" ]]; then
    language="Rust"
    manifest="Cargo.toml"
  elif [[ -f "$root/go.mod" ]]; then
    language="Go"
    manifest="go.mod"
  elif [[ -f "$root/deno.json" ]]; then
    language="Deno"
    manifest="deno.json"
  elif [[ -f "$root/deno.jsonc" ]]; then
    language="Deno"
    manifest="deno.jsonc"
  elif [[ -f "$root/pubspec.yaml" ]]; then
    language="Dart"
    manifest="pubspec.yaml"
  elif [[ -f "$root/Package.swift" ]]; then
    language="Swift"
    manifest="Package.swift"
  fi

  if [[ -z "$language" ]]; then
    printf ''
    return
  fi

  printf 'Detected project context (auto-detected from manifest files; verify before relying on it):\n'
  printf -- '- Language/runtime: %s\n' "$language"
  if [[ -n "$pm" ]]; then
    printf -- '- Package manager: %s\n' "$pm"
  fi
  printf -- '- Primary manifest: %s\n' "$manifest"
  if git -C "$root" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    local listing
    listing=$(git -C "$root" ls-files 2>/dev/null | head -200)
    if [[ -n "$listing" ]]; then
      printf -- '- Project file listing (git ls-files, max 200 entries):\n'
      printf '%s\n' "$listing"
    fi
  fi
}

# ───── --collect ────────────────────────────────────────────────────────────

collect() {
  local project_root="" state_dir="" scope=""
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --scope)
        scope="${2:-}"
        if [[ -z "$scope" ]]; then
          echo "Error: --scope requires a value (required|suggested)" >&2
          exit 1
        fi
        shift 2
        ;;
      --scope=*)
        scope="${1#--scope=}"
        shift
        ;;
      --*)
        echo "Error: unknown flag: $1" >&2
        exit 1
        ;;
      *)
        if [[ -z "$project_root" ]]; then
          project_root="$1"
        elif [[ -z "$state_dir" ]]; then
          state_dir="$1"
        else
          echo "Error: unexpected argument: $1" >&2
          exit 1
        fi
        shift
        ;;
    esac
  done

  [[ -n "$project_root" && -n "$state_dir" ]] || usage
  if [[ -z "$scope" ]]; then
    echo "Error: --collect requires --scope <required|suggested>" >&2
    exit 1
  fi
  if [[ "$scope" != "required" && "$scope" != "suggested" ]]; then
    echo "Error: invalid --scope value: $scope (must be 'required' or 'suggested')" >&2
    exit 1
  fi

  project_root="${project_root%/}"
  [[ -d "$state_dir" ]] || {
    echo "Error: state-dir not found: $state_dir" >&2
    exit 1
  }

  [[ -f "$project_root/project.yaml" ]] || {
    echo "Error: $project_root/project.yaml not found" >&2
    exit 1
  }

  local runner_dir lint_script
  runner_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  lint_script="$runner_dir/lint-project-yaml.sh"
  if [[ -x "$lint_script" ]]; then
    "$lint_script" "$project_root/project.yaml" >/dev/null || exit 1
  fi

  local pyaml="$project_root/project.yaml"
  local profiles
  profiles=$(yq -r '.profiles[]?' "$pyaml" 2>/dev/null || true)

  local disabled_keys
  disabled_keys=$(yq -r '.disabled // {} | keys | .[]?' "$pyaml" 2>/dev/null || true)

  declare -A DISABLED
  while IFS= read -r k; do
    [[ -n "$k" ]] && DISABLED["$k"]=1
  done <<<"$disabled_keys"

  local required_overrides
  required_overrides=$(yq -r '.required[]?' "$pyaml" 2>/dev/null || true)

  declare -A REQUIRED_OVERRIDE
  while IFS= read -r k; do
    [[ -n "$k" ]] && REQUIRED_OVERRIDE["$k"]=1
  done <<<"$required_overrides"

  local required_overrides_json
  required_overrides_json=$(yq -o=json -I=0 '.required // []' "$pyaml" 2>/dev/null || true)
  if [[ -z "$required_overrides_json" || "$required_overrides_json" == "null" ]]; then
    required_overrides_json='[]'
  fi

  local resolved_json="[]"
  local pending_json="[]"
  local disabled_count=0
  local suggested_total=0

  local project_context
  project_context=$(detect_project_context "$project_root")

  while IFS= read -r profile; do
    [[ -n "$profile" ]] || continue
    local pdir="$SKILL_DIR/profiles/$profile"
    [[ -d "$pdir" ]] || {
      echo "Error: profile not found: $profile" >&2
      exit 1
    }

    while IFS= read -r std_yaml; do
      [[ -n "$std_yaml" ]] || continue
      local basename id
      basename="$(basename "$std_yaml" .yaml)"
      id="$profile/$basename"

      if [[ -n "${DISABLED[$id]:-}" ]]; then
        disabled_count=$((disabled_count + 1))
        continue
      fi

      local required description has_script has_prompt
      required=$(yq -r '.required' "$std_yaml")
      description=$(yq -r '.description // ""' "$std_yaml")
      has_script=$(yq -r '.check | has("script")' "$std_yaml")
      has_prompt=$(yq -r '.check | has("prompt")' "$std_yaml")

      if [[ "$has_script" == "true" && "$has_prompt" == "true" ]]; then
        echo "Error: malformed standard $id (has both check.script and check.prompt)" >&2
        exit 1
      fi
      if [[ "$has_script" != "true" && "$has_prompt" != "true" ]]; then
        echo "Error: malformed standard $id (has neither check.script nor check.prompt)" >&2
        exit 1
      fi

      local effective_required="$required"
      if [[ -n "${REQUIRED_OVERRIDE[$id]:-}" ]]; then
        effective_required="true"
      fi

      # Scope filter: only collect standards matching the requested scope.
      # During --scope required, count effective-suggested standards into
      # suggested_total so render can surface the skipped count even when
      # the suggested round never runs.
      if [[ "$scope" == "required" && "$effective_required" != "true" ]]; then
        suggested_total=$((suggested_total + 1))
        continue
      fi
      if [[ "$scope" == "suggested" && "$effective_required" == "true" ]]; then
        continue
      fi

      local intrinsic_bool
      if [[ "$required" == "true" ]]; then intrinsic_bool=true; else intrinsic_bool=false; fi

      if [[ "$has_script" == "true" ]]; then
        local script_body status detail exit_code stdout_capture
        script_body=$(yq -r '.check.script' "$std_yaml")
        set +e
        stdout_capture=$(PROJECT_ROOT="$project_root" bash -c "set -euo pipefail
$script_body" 2>&1)
        exit_code=$?
        set -e
        detail=$(printf '%s\n' "$stdout_capture" | awk 'NF{last=$0} END{print last}')

        if [[ "$exit_code" -eq 0 ]]; then
          status="PASS"
        elif [[ "$effective_required" == "true" ]]; then
          status="FAIL"
        else
          status="SUGG"
        fi

        resolved_json=$(jq -c --arg id "$id" --arg s "$status" --arg d "$detail" --arg desc "$description" --argjson ir "$intrinsic_bool" \
          '. + [{id:$id, status:$s, detail:$d, description:$desc, intrinsic_required:$ir}]' <<<"$resolved_json")
      else
        local prompt_body rendered req_bool
        prompt_body=$(yq -r '.check.prompt' "$std_yaml")
        rendered="${prompt_body//\$PROJECT_ROOT/$project_root}"
        if [[ -n "$project_context" ]]; then
          rendered="${project_context}
${rendered}"
        fi
        if [[ "$effective_required" == "true" ]]; then req_bool=true; else req_bool=false; fi
        local response_path prompt_path directive
        response_path="$state_dir/responses/$id.txt"
        prompt_path="$state_dir/prompts/$id.txt"
        directive="Verify the standard below. After verification, use the Write tool to save your verdict to this absolute path:

  $response_path

The file's contents must be exactly one JSON object: {\"met\": true|false, \"detail\": \"<one-line summary>\"} — nothing else, no fenced code block, no surrounding prose. The runner reads only that file; your conversational reply is ignored.

"
        rendered="${directive}${rendered}"
        mkdir -p "$(dirname "$prompt_path")"
        printf '%s' "$rendered" > "$prompt_path"
        pending_json=$(jq -c --arg id "$id" --argjson req "$req_bool" --argjson ir "$intrinsic_bool" --arg desc "$description" --arg pp "$prompt_path" --arg rp "$response_path" \
          '. + [{id:$id, required:$req, intrinsic_required:$ir, description:$desc, response_path:$rp, prompt_path:$pp}]' <<<"$pending_json")
      fi
    done < <(find "$pdir" -maxdepth 1 -type f -name '*.yaml' | sort)
  done <<<"$profiles"

  local out_path="$state_dir/collect-$scope.json"
  if [[ "$scope" == "required" ]]; then
    jq -n --ascii-output --argjson resolved "$resolved_json" --argjson pending "$pending_json" \
          --argjson dc "$disabled_count" --argjson ro "$required_overrides_json" \
          --arg pc "$project_context" --argjson st "$suggested_total" \
      '{resolved:$resolved, pending:$pending, required_overrides:$ro, disabled_count:$dc, project_context:$pc, suggested_total:$st}' \
      > "$out_path"
  else
    jq -n --ascii-output --argjson resolved "$resolved_json" --argjson pending "$pending_json" \
          --argjson dc "$disabled_count" --argjson ro "$required_overrides_json" \
          --arg pc "$project_context" \
      '{resolved:$resolved, pending:$pending, required_overrides:$ro, disabled_count:$dc, project_context:$pc}' \
      > "$out_path"
  fi
}

# ───── --merge ──────────────────────────────────────────────────────────────

extract_last_json_block() {
  local input="$1"
  printf '%s\n' "$input" | awk '
    /^```json[[:space:]]*$/ { in_block=1; buf=""; next }
    /^```[[:space:]]*$/     { if (in_block) { last_block=buf; in_block=0 }; next }
    in_block                { buf = buf $0 "\n" }
    END                     { printf "%s", last_block }
  '
}

# Extract a JSON payload from a sub-agent response. Per the dispatch
# directive, agents Write a single raw JSON object to their response_path.
# Try that contract first; fall back to extracting the last fenced
# ```json ... ``` block for backward compatibility with older responses.
extract_json_payload() {
  local input="$1"
  if echo "$input" | jq -e '.' >/dev/null 2>&1; then
    printf '%s' "$input"
    return
  fi
  local block
  block=$(extract_last_json_block "$input")
  if [[ -n "$block" ]]; then
    printf '%s' "$block"
  fi
}

merge() {
  local state_dir="${1:-}"
  [[ -n "$state_dir" && -d "$state_dir" ]] || usage

  # Backward compatibility: support legacy collect.json (used by older tests
  # that hand-craft a fixture). Otherwise, prefer collect-required.json (and
  # optionally collect-suggested.json) per the two-pass flow.
  local sources=()
  local scopes_collected_json='[]'
  local has_collect_required=false has_collect_suggested=false has_legacy=false
  if [[ -f "$state_dir/collect-required.json" ]]; then
    sources+=("$state_dir/collect-required.json")
    has_collect_required=true
  fi
  if [[ -f "$state_dir/collect-suggested.json" ]]; then
    sources+=("$state_dir/collect-suggested.json")
    has_collect_suggested=true
  fi
  if [[ ${#sources[@]} -eq 0 && -f "$state_dir/collect.json" ]]; then
    sources+=("$state_dir/collect.json")
    has_legacy=true
  fi
  if [[ ${#sources[@]} -eq 0 ]]; then
    echo "Error: no collect file found in state-dir: $state_dir (expected collect-required.json)" >&2
    exit 1
  fi

  if [[ "$has_collect_required" == "true" ]]; then
    scopes_collected_json='["required"]'
    if [[ "$has_collect_suggested" == "true" ]]; then
      scopes_collected_json='["required","suggested"]'
    fi
  fi

  local responses_dir="$state_dir/responses"
  [[ -d "$responses_dir" ]] || mkdir -p "$responses_dir"

  # Combine resolved + pending across all sources. Top-level scalars
  # (disabled_count, required_overrides, suggested_total, project_context)
  # are taken from the first source (collect-required.json under the new
  # flow; collect.json under legacy).
  local resolved='[]' pending='[]'
  local disabled_count required_overrides suggested_total
  disabled_count=$(jq -r '.disabled_count // 0' "${sources[0]}")
  required_overrides=$(jq -c '.required_overrides // []' "${sources[0]}")
  suggested_total=$(jq -r '.suggested_total // 0' "${sources[0]}")

  local src
  for src in "${sources[@]}"; do
    local src_resolved src_pending
    src_resolved=$(jq -c '.resolved // []' "$src")
    src_pending=$(jq -c '.pending // []' "$src")
    resolved=$(jq -c --argjson a "$resolved" --argjson b "$src_resolved" -n '$a + $b')
    pending=$(jq -c --argjson a "$pending" --argjson b "$src_pending" -n '$a + $b')
  done

  local pending_count
  pending_count=$(jq 'length' <<<"$pending")

  local i id required description intrinsic_required response_path response status detail json_block met
  local parse_rc met_rc
  for ((i = 0; i < pending_count; i++)); do
    id=$(jq -r ".[$i].id" <<<"$pending")
    required=$(jq -r ".[$i].required" <<<"$pending")
    description=$(jq -r ".[$i].description // \"\"" <<<"$pending")
    intrinsic_required=$(jq -r ".[$i].intrinsic_required // false" <<<"$pending")
    response_path="$responses_dir/$id.txt"

    if [[ ! -f "$response_path" ]]; then
      status="FAIL"
      detail="no response file at $response_path"
    else
      response=$(cat "$response_path")
      json_block=$(extract_json_payload "$response")

      if [[ -z "$json_block" ]]; then
        status="FAIL"
        detail="no JSON payload in response"
      else
        set +e
        echo "$json_block" | jq '.' >/dev/null 2>&1
        parse_rc=$?
        set -e
        if [[ "$parse_rc" -ne 0 ]]; then
          status="FAIL"
          detail="malformed JSON block: parse error"
        else
          set +e
          echo "$json_block" | jq -e '.met == true or .met == false' >/dev/null 2>&1
          met_rc=$?
          set -e
          if [[ "$met_rc" -ne 0 ]]; then
            status="FAIL"
            detail="malformed JSON block: missing or non-bool .met"
          else
            met=$(echo "$json_block" | jq -r '.met')
            detail=$(echo "$json_block" | jq -r '.detail // ""')
            if [[ "$met" == "true" ]]; then
              status="PASS"
            elif [[ "$required" == "true" ]]; then
              status="FAIL"
            else
              status="SUGG"
            fi
          fi
        fi
      fi
    fi

    resolved=$(jq -c --arg id "$id" --arg s "$status" --arg d "$detail" --arg desc "$description" --argjson ir "$intrinsic_required" \
      '. + [{id:$id, status:$s, detail:$d, description:$desc, intrinsic_required:$ir}]' <<<"$resolved")
  done

  jq -n --ascii-output --argjson resolved "$resolved" --argjson dc "$disabled_count" \
        --argjson ro "$required_overrides" --argjson sc "$scopes_collected_json" \
        --argjson st "$suggested_total" \
    '{resolved:$resolved, pending:[], required_overrides:$ro, disabled_count:$dc, scopes_collected:$sc, suggested_total:$st}' \
    > "$state_dir/merged.json"
}

# ───── --render ─────────────────────────────────────────────────────────────

render() {
  local state_dir="${1:-}"
  [[ -n "$state_dir" && -d "$state_dir" ]] || usage
  [[ -f "$state_dir/merged.json" ]] || {
    echo "Error: merged.json not found in state-dir: $state_dir/merged.json" >&2
    exit 1
  }
  local results
  results=$(cat "$state_dir/merged.json")

  local pending_count
  pending_count=$(jq '.pending // [] | length' <<<"$results")
  if [[ "$pending_count" -gt 0 ]]; then
    local pending_ids
    pending_ids=$(jq -r '.pending // [] | map(.id) | join(", ")' <<<"$results")
    echo "Error: results JSON has $pending_count unresolved pending entry/entries; resolve them before rendering." >&2
    echo "Pending: $pending_ids" >&2
    exit 1
  fi

  local sorted
  sorted=$(jq '
    .resolved
    | sort_by(.id)
    | sort_by(if .status=="FAIL" then 0
              elif .status=="SUGG" then 1
              elif .status=="PASS" then 2
              else 3 end)
  ' <<<"$results")

  local pass_count fail_count sugg_count disabled_count
  pass_count=$(jq '[.[] | select(.status=="PASS")] | length' <<<"$sorted")
  fail_count=$(jq '[.[] | select(.status=="FAIL")] | length' <<<"$sorted")
  sugg_count=$(jq '[.[] | select(.status=="SUGG")] | length' <<<"$sorted")
  disabled_count=$(jq -r '.disabled_count // 0' <<<"$results")

  # scopes_collected defaults to ["required","suggested"] when absent so that
  # legacy merged.json fixtures (without the field) skip the new "suggested
  # skipped" line — preserves prior render behaviour.
  local has_suggested_scope suggested_total
  has_suggested_scope=$(jq -r '
    (.scopes_collected // ["required","suggested"]) | index("suggested") != null
  ' <<<"$results")
  suggested_total=$(jq -r '.suggested_total // 0' <<<"$results")

  echo "| Standard | Status | Detail |"
  echo "| --- | --- | --- |"
  jq -r '.[] | select(.status != "PASS") | "| \(.id) | \(.status) | \(.detail) |"' <<<"$sorted"
  echo
  echo "${pass_count} PASS, ${fail_count} FAIL, ${sugg_count} SUGG"
  if [[ "$disabled_count" -gt 0 ]]; then
    echo "${disabled_count} standards disabled in project.yaml"
  fi
  if [[ "$has_suggested_scope" == "false" && "$suggested_total" -gt 0 ]]; then
    echo "${suggested_total} suggested standards skipped (required failures present)"
  fi

  # Lock-in suggestion fires only when the suggested round actually ran:
  # without that data we cannot tell which SUGG-style standards would have
  # PASSed, so the suggestion would be unfounded.
  if [[ "$fail_count" -eq 0 && "$sugg_count" -eq 0 && "$has_suggested_scope" == "true" ]]; then
    local overrides_json eligible_ids
    overrides_json=$(jq -c '.required_overrides // []' <<<"$results")
    eligible_ids=$(jq -r --argjson overrides "$overrides_json" '
      [.resolved[]
       | select(.status == "PASS" and .intrinsic_required == false)
       | select((.id as $id | $overrides | index($id)) | not)
       | .id]
      | sort
      | .[]?
    ' <<<"$results")
    if [[ -n "$eligible_ids" ]]; then
      echo
      echo "All standards pass — to enforce them going forward, add to project.yaml:"
      echo
      echo "required:"
      while IFS= read -r eid; do
        [[ -n "$eid" ]] && echo "  - $eid"
      done <<<"$eligible_ids"
    fi
  fi
}

# ───── --check ──────────────────────────────────────────────────────────────
# CI pass/fail signal. Reads the same JSON shape as --render and exits:
#   0  — no FAIL rows (audit passed)
#   1  — at least one FAIL row (audit failed)
#   2  — operational error (missing file, malformed JSON, unresolved pending)

check() {
  local state_dir="${1:-}"
  [[ -n "$state_dir" && -d "$state_dir" ]] || usage
  [[ -f "$state_dir/merged.json" ]] || {
    echo "Error: merged.json not found in state-dir: $state_dir/merged.json" >&2
    exit 2
  }
  local results
  results=$(cat "$state_dir/merged.json")

  jq -e . <<<"$results" >/dev/null 2>&1 || {
    echo "Error: results JSON is malformed" >&2
    exit 2
  }

  local pending_count
  pending_count=$(jq '.pending // [] | length' <<<"$results")
  if [[ "$pending_count" -gt 0 ]]; then
    local pending_ids
    pending_ids=$(jq -r '.pending // [] | map(.id) | join(", ")' <<<"$results")
    echo "Error: results JSON has $pending_count unresolved pending entry/entries; resolve them before checking." >&2
    echo "Pending: $pending_ids" >&2
    exit 2
  fi

  local fail_count
  fail_count=$(jq '[.resolved[]? | select(.status=="FAIL")] | length' <<<"$results")
  if [[ "$fail_count" -gt 0 ]]; then
    exit 1
  fi
}

# ───── --gate ───────────────────────────────────────────────────────────────
# Mid-flow gate between round 1 (required) and round 2 (suggested). Reads
# merged.json and exits:
#   0  — every effective-required entry has status PASS (run round 2)
#   1  — at least one effective-required entry has status FAIL (skip round 2)
#   2  — operational error (missing file, malformed JSON)
#
# Effective-required: an entry whose intrinsic_required=true OR whose id
# appears in required_overrides. Suggested entries (intrinsic_required=false
# and not overridden) never affect the gate; their presence with FAIL/SUGG
# status is a no-op as far as round 2 eligibility is concerned.

gate() {
  local state_dir="${1:-}"
  [[ -n "$state_dir" && -d "$state_dir" ]] || usage
  [[ -f "$state_dir/merged.json" ]] || {
    echo "Error: merged.json not found in state-dir: $state_dir/merged.json" >&2
    exit 2
  }
  local results
  results=$(cat "$state_dir/merged.json")

  jq -e . <<<"$results" >/dev/null 2>&1 || {
    echo "Error: results JSON is malformed" >&2
    exit 2
  }

  local required_fail_count
  required_fail_count=$(jq '
    (.required_overrides // []) as $overrides
    | [.resolved[]?
       | select(
           .intrinsic_required == true
           or (.id as $id | $overrides | index($id)) != null
         )
       | select(.status == "FAIL")]
    | length
  ' <<<"$results")

  if [[ "$required_fail_count" -gt 0 ]]; then
    exit 1
  fi
}

# ───── dispatch ─────────────────────────────────────────────────────────────

case "$MODE" in
  --init)    init    "$@" ;;
  --collect) collect "$@" ;;
  --merge)   merge   "$@" ;;
  --gate)    gate    "$@" ;;
  --render)  render  "$@" ;;
  --check)   check   "$@" ;;
  *) usage ;;
esac
