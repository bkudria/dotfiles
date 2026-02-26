#!/usr/bin/env bash
# list-sessions.sh — List sessions for a project with dates and first message.
#
# Usage:
#   list-sessions.sh                        # List for current working directory
#   list-sessions.sh /Users/foo/project     # List for specific project path
#   list-sessions.sh -Users-foo-project     # List using encoded project name
#
# Output: table of session ID, date, file size, first message preview.
# For large projects (100+ sessions), skips first-message extraction for speed.

set -euo pipefail

PROJECTS_DIR="${HOME}/.claude/projects"

# Encode a filesystem path to the directory name format
encode_path() {
  local path="$1"
  echo "$path" | sed 's/^\///' | sed 's/\//-/g' | sed 's/^/-/'
}

# Determine the project directory
if [[ $# -ge 1 ]]; then
  arg="$1"
  if [[ "$arg" == -* && ! "$arg" =~ ^/ ]]; then
    # Already encoded
    project_encoded="$arg"
  else
    # Filesystem path — encode it
    project_encoded="$(encode_path "$arg")"
  fi
else
  # Use current working directory
  project_encoded="$(encode_path "$(pwd)")"
fi

project_dir="${PROJECTS_DIR}/${project_encoded}"

if [[ ! -d "$project_dir" ]]; then
  echo "No project directory found: $project_dir" >&2
  echo "Encoded name: $project_encoded" >&2
  echo "" >&2
  echo "Available projects:" >&2
  ls -1 "$PROJECTS_DIR" 2>/dev/null | head -20 >&2
  exit 1
fi

# Extract first user message from a session file (fast: reads only first 50 lines)
first_user_message() {
  local file="$1"
  head -50 "$file" 2>/dev/null \
    | jq -r 'select(.type == "user") | .message.content | if type == "string" then . elif type == "array" then [.[] | select(.type == "text") | .text] | join(" ") else "" end' 2>/dev/null \
    | head -1 \
    | cut -c1-80
}

# Human-readable file size
human_size() {
  local bytes="$1"
  if [[ "$bytes" -ge 1048576 ]]; then
    echo "$((bytes / 1048576))MB"
  elif [[ "$bytes" -ge 1024 ]]; then
    echo "$((bytes / 1024))KB"
  else
    echo "${bytes}B"
  fi
}

# Count files first to decide whether to include previews
file_count=$(find "$project_dir" -maxdepth 1 -name '*.jsonl' -type f 2>/dev/null | wc -l | tr -d ' ')
include_preview=true
if [[ "$file_count" -gt 50 ]]; then
  include_preview=false
  echo "($file_count sessions — skipping message previews for speed)" >&2
fi

printf "%-8s  %-18s %8s  %s\n" "SESSION" "DATE" "SIZE" "FIRST MESSAGE"
printf "%-8s  %-18s %8s  %s\n" "--------" "$(printf '%0.s-' {1..18})" "--------" "$(printf '%0.s-' {1..40})"

# Build output lines with timestamp prefix for sorting
{
  while IFS= read -r file; do
    session_id="$(basename "$file" .jsonl)"

    # Skip non-session files
    [[ "$session_id" == "history" ]] && continue

    # Get file modification time and size
    if [[ "$(uname)" == "Darwin" ]]; then
      file_ts=$(stat -f '%m' "$file" 2>/dev/null || echo 0)
      file_size=$(stat -f '%z' "$file" 2>/dev/null || echo 0)
      date_str=$(date -r "$file_ts" '+%Y-%m-%d %H:%M' 2>/dev/null || echo "unknown")
    else
      file_ts=$(stat -c '%Y' "$file" 2>/dev/null || echo 0)
      file_size=$(stat -c '%s' "$file" 2>/dev/null || echo 0)
      date_str=$(date -d "@$file_ts" '+%Y-%m-%d %H:%M' 2>/dev/null || echo "unknown")
    fi

    size_str="$(human_size "$file_size")"

    if [[ "$include_preview" == true ]]; then
      preview="$(first_user_message "$file")"
    else
      preview=""
    fi

    printf '%s\t%-8s  %-18s %8s  %s\n' "$file_ts" "${session_id:0:8}" "$date_str" "$size_str" "$preview"
  done < <(find "$project_dir" -maxdepth 1 -name '*.jsonl' -type f 2>/dev/null)
} | sort -t$'\t' -k1 -rn | cut -f2-
