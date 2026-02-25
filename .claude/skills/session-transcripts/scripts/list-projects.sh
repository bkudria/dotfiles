#!/usr/bin/env bash
# list-projects.sh — List all projects with decoded paths and session counts.
#
# Usage: list-projects.sh
#
# Output: table of project path, session count, latest session date.

set -euo pipefail

PROJECTS_DIR="${HOME}/.claude/projects"

if [[ ! -d "$PROJECTS_DIR" ]]; then
  echo "No projects directory found at $PROJECTS_DIR" >&2
  exit 1
fi

# Decode project path from directory name (best-effort — encoding is lossy).
# Handles: leading - → /, -- → /. (hidden dirs), single - → /
decode_path() {
  local name="$1"
  echo "$name" | sed 's/^-/\//' | sed 's/--/\/./g' | sed 's/-/\//g'
}

printf "%-60s %8s %s\n" "PROJECT PATH" "SESSIONS" "LATEST SESSION"
printf "%-60s %8s %s\n" "$(printf '%0.s-' {1..60})" "--------" "--------------"

for project_dir in "$PROJECTS_DIR"/*/; do
  [[ -d "$project_dir" ]] || continue

  project_name="$(basename "$project_dir")"
  decoded="$(decode_path "$project_name")"

  # Count .jsonl session files (exclude history.jsonl pattern if it exists)
  session_count=0
  latest_date="none"
  latest_ts=0

  while IFS= read -r file; do
    session_count=$((session_count + 1))
    # Get file modification time as epoch for sorting
    if [[ "$(uname)" == "Darwin" ]]; then
      file_ts=$(stat -f '%m' "$file" 2>/dev/null || echo 0)
    else
      file_ts=$(stat -c '%Y' "$file" 2>/dev/null || echo 0)
    fi
    if [[ "$file_ts" -gt "$latest_ts" ]]; then
      latest_ts="$file_ts"
      if [[ "$(uname)" == "Darwin" ]]; then
        latest_date=$(date -r "$file_ts" '+%Y-%m-%d %H:%M' 2>/dev/null || echo "unknown")
      else
        latest_date=$(date -d "@$file_ts" '+%Y-%m-%d %H:%M' 2>/dev/null || echo "unknown")
      fi
    fi
  done < <(find "$project_dir" -maxdepth 1 -name '*.jsonl' -type f 2>/dev/null)

  [[ $session_count -eq 0 ]] && continue

  printf "%-60s %8d %s\n" "$decoded" "$session_count" "$latest_date"
done | sort -t' ' -k3 -r
