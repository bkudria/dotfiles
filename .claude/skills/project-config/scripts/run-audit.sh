#!/usr/bin/env bash
# run-audit.sh — Three-phase audit runner for the project-config skill.
#
# Usage:
#   run-audit.sh --collect <project-root>
#       Reads <project-root>/project.yaml, walks selected profile directories,
#       and emits {"resolved": [...], "pending": [...], "disabled_count": N}.
#       Each deterministic check (check.script) runs immediately and lands in
#       `resolved` with status PASS/FAIL/SUGG. Each prompt-based check
#       (check.prompt) goes to `pending` with its rendered prompt for
#       sub-agent verification.
#
#   run-audit.sh --merge <collect-file|-> <responses-dir>
#       Folds sub-agent responses into the collect output. Reads the JSON
#       produced by --collect from a file path or `-` (stdin), looks up each
#       pending entry's response at <responses-dir>/<id>.txt, extracts the
#       last fenced JSON block ({"met": bool, "detail": string}), and emits
#       a merged results JSON with every entry resolved to PASS/FAIL/SUGG.
#       Missing files, parse failures, or non-bool `met` resolve to FAIL.
#
#   run-audit.sh --render <results-json|->
#       Reads a results JSON ({"resolved": [...], "disabled_count": N})
#       from a file path or `-` (stdin), and emits the markdown audit table,
#       per-status counts, optional disabled-count line, and remediation list.
set -euo pipefail

SKILL_DIR="${CLAUDE_SKILL_DIR:-${HOME}/.claude/skills/project-config}"

usage() {
  echo "Usage:" >&2
  echo "  run-audit.sh --collect <project-root>" >&2
  echo "  run-audit.sh --merge   <collect-file|-> <responses-dir>" >&2
  echo "  run-audit.sh --render  <results-json|->" >&2
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

# ───── --collect ────────────────────────────────────────────────────────────

collect() {
  local project_root="${1:-}"
  [[ -n "$project_root" ]] || usage
  project_root="${project_root%/}"

  [[ -f "$project_root/project.yaml" ]] || {
    echo "Error: $project_root/project.yaml not found" >&2
    exit 1
  }

  local pyaml="$project_root/project.yaml"
  local profiles
  profiles=$(yq -r '.profiles[]?' "$pyaml" 2>/dev/null || true)

  local disabled_keys
  disabled_keys=$(yq -r '.disabled // {} | keys | .[]?' "$pyaml" 2>/dev/null || true)

  declare -A DISABLED
  while IFS= read -r k; do
    [[ -n "$k" ]] && DISABLED["$k"]=1
  done <<<"$disabled_keys"

  local resolved_json="[]"
  local pending_json="[]"
  local disabled_count=0

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
        elif [[ "$required" == "true" ]]; then
          status="FAIL"
        else
          status="SUGG"
        fi

        resolved_json=$(jq -c --arg id "$id" --arg s "$status" --arg d "$detail" --arg desc "$description" \
          '. + [{id:$id, status:$s, detail:$d, description:$desc}]' <<<"$resolved_json")
      else
        local prompt_body rendered req_bool
        prompt_body=$(yq -r '.check.prompt' "$std_yaml")
        rendered="${prompt_body//\$PROJECT_ROOT/$project_root}"
        if [[ "$required" == "true" ]]; then req_bool=true; else req_bool=false; fi
        pending_json=$(jq -c --arg id "$id" --argjson req "$req_bool" --arg desc "$description" --arg p "$rendered" \
          '. + [{id:$id, required:$req, description:$desc, rendered_prompt:$p}]' <<<"$pending_json")
      fi
    done < <(find "$pdir" -maxdepth 1 -type f -name '*.yaml' | sort)
  done <<<"$profiles"

  jq -n --ascii-output --argjson resolved "$resolved_json" --argjson pending "$pending_json" --argjson dc "$disabled_count" \
    '{resolved:$resolved, pending:$pending, disabled_count:$dc}'
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

merge() {
  local collect_source="${1:-}"
  local responses_dir="${2:-}"
  [[ -n "$collect_source" && -n "$responses_dir" ]] || usage

  [[ -d "$responses_dir" ]] || {
    echo "Error: responses dir not found: $responses_dir" >&2
    exit 1
  }

  local collect_json
  if [[ "$collect_source" == "-" ]]; then
    collect_json=$(cat)
  else
    [[ -f "$collect_source" ]] || {
      echo "Error: collect file not found: $collect_source" >&2
      exit 1
    }
    collect_json=$(cat "$collect_source")
  fi

  local resolved pending_count disabled_count
  resolved=$(jq -c '.resolved // []' <<<"$collect_json")
  pending_count=$(jq '.pending // [] | length' <<<"$collect_json")
  disabled_count=$(jq -r '.disabled_count // 0' <<<"$collect_json")

  local i id required description response_path response status detail json_block met
  local parse_rc met_rc
  for ((i = 0; i < pending_count; i++)); do
    id=$(jq -r ".pending[$i].id" <<<"$collect_json")
    required=$(jq -r ".pending[$i].required" <<<"$collect_json")
    description=$(jq -r ".pending[$i].description // \"\"" <<<"$collect_json")
    response_path="$responses_dir/$id.txt"

    if [[ ! -f "$response_path" ]]; then
      status="FAIL"
      detail="no response file at $response_path"
    else
      response=$(cat "$response_path")
      json_block=$(extract_last_json_block "$response")

      if [[ -z "$json_block" ]]; then
        status="FAIL"
        detail="no fenced JSON block in response"
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

    resolved=$(jq -c --arg id "$id" --arg s "$status" --arg d "$detail" --arg desc "$description" \
      '. + [{id:$id, status:$s, detail:$d, description:$desc}]' <<<"$resolved")
  done

  jq -n --ascii-output --argjson resolved "$resolved" --argjson dc "$disabled_count" \
    '{resolved:$resolved, pending:[], disabled_count:$dc}'
}

# ───── --render ─────────────────────────────────────────────────────────────

render() {
  local source="${1:-}"
  [[ -n "$source" ]] || usage

  local results
  if [[ "$source" == "-" ]]; then
    results=$(cat)
  else
    [[ -f "$source" ]] || {
      echo "Error: results file not found: $source" >&2
      exit 1
    }
    results=$(cat "$source")
  fi

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

  echo "| Standard | Status | Detail |"
  echo "| --- | --- | --- |"
  jq -r '.[] | select(.status != "PASS") | "| \(.id) | \(.status) | \(.detail) |"' <<<"$sorted"
  echo
  echo "${pass_count} PASS, ${fail_count} FAIL, ${sugg_count} SUGG"
  if [[ "$disabled_count" -gt 0 ]]; then
    echo "${disabled_count} standards disabled in project.yaml"
  fi
  echo
  echo "## Remediation"
  jq -r '
    [.[] | select(.status=="FAIL" or .status=="SUGG")]
    | .[]
    | "- **[\(.status)] \(.id)** — \(.description)\n  - detail: \(.detail)"
  ' <<<"$sorted"

  if [[ "$fail_count" -gt 0 ]]; then
    exit 1
  fi
}

# ───── dispatch ─────────────────────────────────────────────────────────────

case "$MODE" in
  --collect) collect "$@" ;;
  --merge)   merge   "$@" ;;
  --render)  render  "$@" ;;
  *) usage ;;
esac
