#!/usr/bin/env bash
# run-audit.sh — Three-phase audit runner for the project-config skill.
#
# Every verb operates on a state-dir produced by --init. The state-dir holds
# canonical files: collect.json (collect output), responses/<id>.txt
# (sub-agent verdicts), merged.json (merge output).
#
# Usage:
#   run-audit.sh --init
#       Emits a fresh state-dir path on stdout.
#
#   run-audit.sh --collect <project-root> <state-dir>
#       Reads <project-root>/project.yaml, walks selected profile directories,
#       and writes <state-dir>/collect.json with shape
#       {"resolved": [...], "pending": [...], "disabled_count": N,
#        "project_context": "..."}.
#       Each deterministic check (check.script) runs immediately and lands in
#       `resolved` with status PASS/FAIL/SUGG. Each prompt-based check
#       (check.prompt) goes to `pending` with its rendered prompt for
#       sub-agent verification, plus the response_path the agent should
#       Write its verdict to.
#
#   run-audit.sh --merge <state-dir>
#       Folds sub-agent responses into the collect output. Reads
#       <state-dir>/collect.json, looks up each pending entry's response at
#       <state-dir>/responses/<id>.txt, extracts the JSON ({"met": bool,
#       "detail": string}), and writes <state-dir>/merged.json with every
#       entry resolved to PASS/FAIL/SUGG. Missing files, parse failures, or
#       non-bool `met` resolve to FAIL.
#
#   run-audit.sh --render <state-dir>
#       Reads <state-dir>/merged.json and emits the markdown audit table,
#       per-status counts, and optional disabled-count line. Always exits 0
#       on successful render. For CI pass/fail signal, use --check.
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
  echo "  run-audit.sh --collect <project-root> <state-dir>" >&2
  echo "  run-audit.sh --merge   <state-dir>" >&2
  echo "  run-audit.sh --render  <state-dir>" >&2
  echo "  run-audit.sh --check   <state-dir>" >&2
  echo "" >&2
  echo "  Each verb except --init reads from / writes to canonical files inside" >&2
  echo "  <state-dir>: collect.json, responses/<id>.txt, merged.json. Use --init" >&2
  echo "  to obtain a fresh state-dir." >&2
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
# block that the runner prepends to every prompt-based rendered_prompt, so
# sub-agents skip the redundant discovery preamble. Emits empty string when
# no manifest matches; the runner then skips the header (graceful fallback).

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
}

# ───── --collect ────────────────────────────────────────────────────────────

collect() {
  local project_root="${1:-}"
  local state_dir="${2:-}"
  [[ -n "$project_root" && -n "$state_dir" ]] || usage
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

        resolved_json=$(jq -c --arg id "$id" --arg s "$status" --arg d "$detail" --arg desc "$description" \
          '. + [{id:$id, status:$s, detail:$d, description:$desc}]' <<<"$resolved_json")
      else
        local prompt_body rendered req_bool
        prompt_body=$(yq -r '.check.prompt' "$std_yaml")
        rendered="${prompt_body//\$PROJECT_ROOT/$project_root}"
        if [[ -n "$project_context" ]]; then
          rendered="${project_context}
${rendered}"
        fi
        if [[ "$effective_required" == "true" ]]; then req_bool=true; else req_bool=false; fi
        local response_path directive
        response_path="$state_dir/responses/$id.txt"
        directive="Verify the standard below. After verification, use the Write tool to save your verdict to this absolute path:

  $response_path

The file's contents must be exactly one JSON object: {\"met\": true|false, \"detail\": \"<one-line summary>\"} — nothing else, no fenced code block, no surrounding prose. The runner reads only that file; your conversational reply is ignored.

"
        rendered="${directive}${rendered}"
        pending_json=$(jq -c --arg id "$id" --argjson req "$req_bool" --arg desc "$description" --arg p "$rendered" --arg rp "$response_path" \
          '. + [{id:$id, required:$req, description:$desc, response_path:$rp, rendered_prompt:$p}]' <<<"$pending_json")
      fi
    done < <(find "$pdir" -maxdepth 1 -type f -name '*.yaml' | sort)
  done <<<"$profiles"

  jq -n --ascii-output --argjson resolved "$resolved_json" --argjson pending "$pending_json" --argjson dc "$disabled_count" --arg pc "$project_context" \
    '{resolved:$resolved, pending:$pending, disabled_count:$dc, project_context:$pc}' \
    > "$state_dir/collect.json"
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
  [[ -f "$state_dir/collect.json" ]] || {
    echo "Error: collect.json not found in state-dir: $state_dir/collect.json" >&2
    exit 1
  }
  local responses_dir="$state_dir/responses"
  [[ -d "$responses_dir" ]] || mkdir -p "$responses_dir"

  local collect_json
  collect_json=$(cat "$state_dir/collect.json")

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

    resolved=$(jq -c --arg id "$id" --arg s "$status" --arg d "$detail" --arg desc "$description" \
      '. + [{id:$id, status:$s, detail:$d, description:$desc}]' <<<"$resolved")
  done

  jq -n --ascii-output --argjson resolved "$resolved" --argjson dc "$disabled_count" \
    '{resolved:$resolved, pending:[], disabled_count:$dc}' \
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

  echo "| Standard | Status | Detail |"
  echo "| --- | --- | --- |"
  jq -r '.[] | select(.status != "PASS") | "| \(.id) | \(.status) | \(.detail) |"' <<<"$sorted"
  echo
  echo "${pass_count} PASS, ${fail_count} FAIL, ${sugg_count} SUGG"
  if [[ "$disabled_count" -gt 0 ]]; then
    echo "${disabled_count} standards disabled in project.yaml"
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

# ───── dispatch ─────────────────────────────────────────────────────────────

case "$MODE" in
  --init)    init    "$@" ;;
  --collect) collect "$@" ;;
  --merge)   merge   "$@" ;;
  --render)  render  "$@" ;;
  --check)   check   "$@" ;;
  *) usage ;;
esac
