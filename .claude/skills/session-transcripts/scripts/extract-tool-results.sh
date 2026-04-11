#!/usr/bin/env bash
# extract-tool-results.sh — Extract tool calls paired with their results.
#
# Usage:
#   extract-tool-results.sh <session.jsonl> [--full] [--tools <names>] [--errors-only]
#
# Options:
#   --full          Show complete result content (no truncation)
#   --tools <names> Filter by tool name(s), comma-separated (e.g., "Bash,Read")
#   --errors-only   Show only tool calls that returned errors
#
# Pairs each tool_use block with its corresponding tool_result via tool_use_id.
# Uses slurp mode for correlation. Default truncates results to 200 chars.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

if [[ $# -lt 1 ]]; then
  echo "Usage: extract-tool-results.sh <session.jsonl> [--full] [--tools <names>] [--errors-only]" >&2
  echo "" >&2
  echo "Options:" >&2
  echo "  --full          Show complete result content (no truncation)" >&2
  echo "  --tools <names> Comma-separated tool names (e.g., Bash,Read)" >&2
  echo "  --errors-only   Show only tool calls that returned errors" >&2
  exit 1
fi

TRUNCATE_LEN=200
TOOL_FILTER=""
ERRORS_ONLY=false
POSITIONALS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --full)         TRUNCATE_LEN=0; shift ;;
    --tools)        TOOL_FILTER="${2:?--tools requires tool names}"; shift 2 ;;
    --errors-only)  ERRORS_ONLY=true; shift ;;
    -*)             echo "Unknown option: $1" >&2; exit 1 ;;
    *)              POSITIONALS+=("$1"); shift ;;
  esac
done

SESSION_FILE="${POSITIONALS[0]:?Session file required}"

if [[ ! -f "$SESSION_FILE" ]]; then
  echo "File not found: $SESSION_FILE" >&2
  exit 1
fi

jq -L "$SCRIPT_DIR" -sr \
  --arg tool_filter "$TOOL_FILTER" \
  --argjson errors_only "$( [[ "$ERRORS_ONLY" == true ]] && echo true || echo false )" \
  --argjson truncate_len "$TRUNCATE_LEN" \
'
import "lib" as lib;

# Build lookup: tool_use_id -> {content, is_error}
. as $all
| (
    [.[] | select(.type == "user") | .message.content
     | select(type == "array") | .[] | select(.type == "tool_result")
     | { key: .tool_use_id, value: { content: lib::tool_result_text, is_error: (.is_error // false) } }
    ] | from_entries
  ) as $results

# Tool name regex (convert comma-separated to alternation)
| ($tool_filter | if . == "" then null else "^(" + gsub(","; "|") + ")$" end) as $tool_re

# Iterate assistant entries, pair tool_use blocks with results
| $all[] | select(.type == "assistant")
| (.timestamp | lib::format_time_only) as $time
| [.message.content // [] | .[] | select(.type == "tool_use")][]
| select(if $tool_re then .name | test($tool_re) else true end)
| . as $call | $results[.id] as $result
| select($result != null)
| select(if $errors_only then $result.is_error else true end)
| ($result.content | length) as $len
| ($result.is_error // false) as $err
| (if $err then "ERROR" else "ok" end) as $status
| (if $truncate_len > 0 then ($result.content | gsub("\n"; " ") | lib::truncate($truncate_len)) else $result.content end) as $preview
| "\($time)  \(.name | lib::truncate(12))  \(.id | lib::truncate(20))  \(lib::tool_file_path // "-" | lib::truncate(70))"
+ "\n          \u2192 \($status) (\($len | lib::format_tokens) chars)"
+ if ($preview | length) > 0 then "\n          \($preview)" else "" end
+ "\n"
' "$SESSION_FILE"
