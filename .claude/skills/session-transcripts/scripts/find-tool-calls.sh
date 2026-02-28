#!/usr/bin/env bash
# find-tool-calls.sh — Find tool calls filtered by file path and/or tool name.
#
# Usage:
#   find-tool-calls.sh <session.jsonl> [--path <pattern>] [--tools <names>] [--commands-only]
#
# Options:
#   --path <pattern>     Filter by regex match on file_path/command/url inputs
#   --tools <names>      Filter by tool name(s), comma-separated (e.g., "Write,Edit")
#   --commands-only      Output only the primary field (command/path/pattern), one per line
#
# Output: timestamp, tool name, file path, and content preview for each match.
# With --commands-only, outputs just the command text (Bash), file path (Read/Write/Edit),
# pattern (Grep/Glob), or URL (WebFetch) — useful for piping into grep/sort/uniq.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

if [[ $# -lt 1 ]]; then
  echo "Usage: find-tool-calls.sh <session.jsonl> [--path <pattern>] [--tools <names>] [--commands-only]" >&2
  echo "" >&2
  echo "Options:" >&2
  echo "  --path <pattern>     Filter by regex match on file_path/command/url inputs" >&2
  echo "  --tools <names>      Comma-separated tool names (e.g., Write,Edit,Bash)" >&2
  echo "  --commands-only      Output only primary field (command/path), one per line" >&2
  exit 1
fi

SESSION_FILE="$1"; shift
PATH_PATTERN=""
TOOL_FILTER=""
COMMANDS_ONLY=false

while [[ $# -gt 0 ]]; do
  case "$1" in
    --path)           PATH_PATTERN="${2:?--path requires a pattern}"; shift 2 ;;
    --tools)          TOOL_FILTER="${2:?--tools requires tool names}"; shift 2 ;;
    --commands-only)  COMMANDS_ONLY=true; shift ;;
    *)                echo "Unknown option: $1" >&2; exit 1 ;;
  esac
done

if [[ ! -f "$SESSION_FILE" ]]; then
  echo "File not found: $SESSION_FILE" >&2
  exit 1
fi

# Build jq filter conditions
JQ_TOOL_FILTER="true"
if [[ -n "$TOOL_FILTER" ]]; then
  # Convert "Write,Edit" to jq regex "^(Write|Edit)$"
  JQ_TOOL_FILTER=".name | test(\"^(${TOOL_FILTER//,/|})$\")"
fi

JQ_PATH_FILTER="true"
if [[ -n "$PATH_PATTERN" ]]; then
  # Escape backslashes and quotes for safe jq string embedding
  ESCAPED=$(printf '%s' "$PATH_PATTERN" | sed 's/\\/\\\\/g; s/"/\\"/g')
  JQ_PATH_FILTER="(lib::tool_file_path // \"\") | test(\"${ESCAPED}\")"
fi

# Choose output format
if [[ "$COMMANDS_ONLY" == true ]]; then
  JQ_FORMAT='lib::tool_file_path // empty'
else
  JQ_FORMAT='"\($time)  \(.name | lib::truncate(12))  \(lib::tool_file_path // "-" | lib::truncate(60))\n          \(lib::tool_content_preview(200))"'
fi

jq -L "$SCRIPT_DIR" -r '
  import "lib" as lib;
  select(.type == "assistant")
  | (.timestamp | lib::format_time_only) as $time
  | lib::tool_use_blocks[]
  | select('"$JQ_TOOL_FILTER"')
  | select('"$JQ_PATH_FILTER"')
  | '"$JQ_FORMAT"'
' "$SESSION_FILE"
