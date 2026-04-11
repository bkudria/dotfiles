#!/usr/bin/env bash
# find-skill-usage.sh — Find sessions that loaded a specific skill.
#
# Usage:
#   find-skill-usage.sh <skill-name>                   # Search across all projects
#   find-skill-usage.sh <skill-name> --project <path>  # Search within one project
#
# Output: matching session paths with project name, preview, and skill invocation details.
# Uses a two-stage search: fast grep pre-filter, then jq structural confirmation.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECTS_DIR="${HOME}/.claude/projects"
source "$SCRIPT_DIR/lib.sh"

if [[ $# -lt 1 ]]; then
  echo "Usage: find-skill-usage.sh <skill-name> [--project <path>]" >&2
  echo "" >&2
  echo "Options:" >&2
  echo "  --project <path>   Search only within this project" >&2
  exit 1
fi

SKILL_NAME="$1"; shift
PROJECT_FILTER=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --project) PROJECT_FILTER="${2:?--project requires a path}"; shift 2 ;;
    *)         echo "Unknown option: $1" >&2; exit 1 ;;
  esac
done

echo "Searching for sessions that loaded skill '$SKILL_NAME'..." >&2

found=0

# Determine which project directories to search
if [[ -n "$PROJECT_FILTER" ]]; then
  encoded="$(encode_path "$PROJECT_FILTER")"
  if [[ ! -d "$PROJECTS_DIR/$encoded" ]]; then
    echo "Project directory not found: $PROJECTS_DIR/$encoded" >&2
    exit 1
  fi
  project_dirs=("$PROJECTS_DIR/$encoded/")
else
  project_dirs=("$PROJECTS_DIR"/*/)
fi

for project_dir in "${project_dirs[@]}"; do
  [[ -d "$project_dir" ]] || continue
  project_name="$(basename "$project_dir")"
  decoded="$(decode_path "$project_name")"

  while IFS= read -r file; do
    # Fast pre-filter: grep for the skill name in Skill tool invocation context.
    # Check both compact and spaced JSON formats.
    if grep -qF "\"skill\":\"${SKILL_NAME}\"" "$file" 2>/dev/null || \
       grep -qF "\"skill\": \"${SKILL_NAME}\"" "$file" 2>/dev/null; then
      # Structural confirmation: use jq to verify it's a real Skill tool_use
      skill_info=$(jq -L "$SCRIPT_DIR" -r '
        import "lib" as lib;
        select(.type == "assistant")
        | (.timestamp | lib::format_timestamp) as $ts
        | lib::tool_use_blocks[]
        | select(.name == "Skill" and .input.skill == "'"$SKILL_NAME"'")
        | "\($ts)  \(.input.skill)\(if .input.args then "  args: \(.input.args | lib::truncate(80))" else "" end)"
      ' "$file" 2>/dev/null)

      if [[ -n "$skill_info" ]]; then
        preview="$(first_user_message "$file")"
        echo "  ${file}"
        echo "    Project: ${decoded}"
        echo "    Preview: ${preview}"
        echo "    Skill invocations:"
        while IFS= read -r line; do
          echo "      ${line}"
        done <<< "$skill_info"
        echo ""
        found=$((found + 1))
      fi
    fi
  done < <(find "$project_dir" -maxdepth 1 -name '*.jsonl' -type f 2>/dev/null)
done

if [[ $found -eq 0 ]]; then
  echo "No sessions found using skill '$SKILL_NAME'" >&2
  exit 1
fi
echo "Found $found session(s)." >&2
