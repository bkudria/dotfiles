#!/usr/bin/env bash
# find-session.sh — Find a session transcript file by UUID or content search.
#
# Usage:
#   find-session.sh <uuid>           # Find by session UUID (partial match OK)
#   find-session.sh -s <search-term> # Search content across all sessions
#
# Output: matching file path(s) with project name and first user message preview.

set -euo pipefail

PROJECTS_DIR="${HOME}/.claude/projects"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

if [[ $# -lt 1 ]]; then
  echo "Usage: find-session.sh <uuid>           — find by session UUID" >&2
  echo "       find-session.sh -s <search-term> — search content across sessions" >&2
  exit 1
fi

# Decode project path from directory name (best-effort — encoding is lossy).
decode_path() {
  local name="$1"
  echo "$name" | sed 's/^-/\//' | sed 's/--/\/./g' | sed 's/-/\//g'
}

# Extract first user message from a session file (truncated)
first_user_message() {
  local file="$1"
  jq -r 'select(.type == "user") | .message.content | if type == "string" then . elif type == "array" then [.[] | select(.type == "text") | .text] | join(" ") else "" end' "$file" \
    | head -1 \
    | cut -c1-100
}

if [[ "$1" == "-s" ]]; then
  # Content search mode
  shift
  search_term="${1:?Search term required}"
  echo "Searching for '$search_term' across all sessions..." >&2

  found=0
  for project_dir in "$PROJECTS_DIR"/*/; do
    [[ -d "$project_dir" ]] || continue
    project_name="$(basename "$project_dir")"
    decoded="$(decode_path "$project_name")"

    while IFS= read -r file; do
      if grep -q "$search_term" "$file" 2>/dev/null; then
        session_id="$(basename "$file" .jsonl)"
        preview="$(first_user_message "$file")"
        echo "  ${file}"
        echo "    Project: ${decoded}"
        echo "    Preview: ${preview}"
        echo ""
        found=$((found + 1))
      fi
    done < <(find "$project_dir" -maxdepth 1 -name '*.jsonl' -type f 2>/dev/null)
  done

  if [[ $found -eq 0 ]]; then
    echo "No sessions found matching '$search_term'" >&2
    exit 1
  fi
  echo "Found $found session(s)." >&2

else
  # UUID search mode
  uuid_pattern="$1"
  echo "Searching for session matching '$uuid_pattern'..." >&2

  found=0
  for project_dir in "$PROJECTS_DIR"/*/; do
    [[ -d "$project_dir" ]] || continue
    project_name="$(basename "$project_dir")"
    decoded="$(decode_path "$project_name")"

    while IFS= read -r file; do
      session_id="$(basename "$file" .jsonl)"
      if [[ "$session_id" == *"$uuid_pattern"* ]] || [[ "$(basename "$file")" == *"$uuid_pattern"* ]]; then
        preview="$(first_user_message "$file")"
        echo "  ${file}"
        echo "    Project: ${decoded}"
        echo "    Preview: ${preview}"
        echo ""
        found=$((found + 1))
      fi
    done < <(find "$project_dir" -maxdepth 1 -name '*.jsonl' -type f 2>/dev/null)
  done

  if [[ $found -eq 0 ]]; then
    echo "No sessions found matching '$uuid_pattern'" >&2
    exit 1
  fi
  echo "Found $found session(s)." >&2
fi
