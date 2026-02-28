#!/usr/bin/env bash
# find-subagent-files.sh — Find all subagent transcript files for a given session.
#
# Usage:
#   find-subagent-files.sh <session.jsonl>
#
# Discovers subagent files in both storage formats:
#   New: <session-uuid>/subagents/agent-<id>.jsonl  (17-char hex IDs)
#   Old: agent-<id>.jsonl in the same project directory  (8-char hex IDs)
#
# Output: list of subagent file paths with sizes.

set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: find-subagent-files.sh <session.jsonl>" >&2
  echo "" >&2
  echo "Finds all subagent transcript files associated with a session." >&2
  exit 1
fi

SESSION_FILE="$1"

if [[ ! -f "$SESSION_FILE" ]]; then
  echo "File not found: $SESSION_FILE" >&2
  exit 1
fi

# Extract session UUID and project directory from the file path
SESSION_BASENAME="$(basename "$SESSION_FILE" .jsonl)"
PROJECT_DIR="$(cd "$(dirname "$SESSION_FILE")" && pwd)"

echo "Session: $SESSION_BASENAME"

# Collect found files (deduplicated by realpath)
declare -A found_files=()


# --- New format: <project-dir>/<session-uuid>/subagents/agent-*.jsonl ---
SUBAGENTS_DIR="$PROJECT_DIR/$SESSION_BASENAME/subagents"
if [[ -d "$SUBAGENTS_DIR" ]]; then
  while IFS= read -r f; do
    real="$(realpath "$f" 2>/dev/null || echo "$f")"
    found_files["$real"]="$f"
  done < <(find "$SUBAGENTS_DIR" -name "agent-*.jsonl" -type f 2>/dev/null)
fi

# --- Old format: extract agentIds from progress entries, check flat files ---
while IFS= read -r agent_id; do
  [[ -z "$agent_id" ]] && continue
  candidate="$PROJECT_DIR/agent-${agent_id}.jsonl"
  if [[ -f "$candidate" ]]; then
    real="$(realpath "$candidate" 2>/dev/null || echo "$candidate")"
    found_files["$real"]="$candidate"
  fi
done < <(jq -r 'select(.type == "progress") | select(.data.type? == "agent_progress") | .data.agentId // empty' "$SESSION_FILE" 2>/dev/null | sort -u)

# Report results
count=${#found_files[@]}
if [[ $count -eq 0 ]]; then
  echo "No subagent files found."
  exit 0
fi

echo "Subagent files: $count"
echo ""

# Sort by filename and print with human-readable size
for f in "${found_files[@]}"; do
  size="$(du -h "$f" 2>/dev/null | cut -f1 | tr -d ' ')"
  name="$(basename "$f")"
  printf "  %-45s %6s\n" "$name" "$size"
done | sort
