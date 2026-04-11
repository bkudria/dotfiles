#!/usr/bin/env bash
# search-session.sh — Search within a session for a keyword with context.
#
# Usage:
#   search-session.sh <session.jsonl> <keyword> [--context <n>]
#
# Options:
#   --context <n>   Show n lines of context around matches (default: 2)
#
# Searches user and assistant text content. Shows role, timestamp, and
# matching lines with keyword highlighted (case-insensitive).

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

if [[ $# -lt 1 ]]; then
  echo "Usage: search-session.sh <session.jsonl> <keyword> [--context <n>]" >&2
  exit 1
fi

CONTEXT=2
POSITIONALS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --context) CONTEXT="${2:?--context requires a number}"; shift 2 ;;
    -*)        echo "Unknown option: $1" >&2; exit 1 ;;
    *)         POSITIONALS+=("$1"); shift ;;
  esac
done

SESSION_FILE="${POSITIONALS[0]:?Session file required}"
KEYWORD="${POSITIONALS[1]:?Keyword required}"

if [[ ! -f "$SESSION_FILE" ]]; then
  echo "File not found: $SESSION_FILE" >&2
  exit 1
fi

echo "Searching for '$KEYWORD' in $(basename "$SESSION_FILE")..." >&2

# Stage 1: jq extracts text with role/timestamp prefix
# Stage 2: grep filters for keyword with context and highlighting
jq -L "$SCRIPT_DIR" -r '
  import "lib" as lib;
  select(.type == "user" or .type == "assistant")
  | (.timestamp | lib::format_time_only) as $time
  | (
      if .type == "user" then
        (lib::user_text | select(length > 0) | split("\n")[]
         | "user \($time) | \(.)"),
        (.message.content | select(type == "array") | .[]
         | select(.type == "tool_result") | lib::tool_result_text
         | select(length > 0) | split("\n")[]
         | "result \($time) | \(.)")
      elif .type == "assistant" then
        (lib::assistant_text | select(length > 0) | split("\n")[]
         | "assistant \($time) | \(.)"),
        (lib::tool_use_blocks[] | "\(.name) \(lib::tool_content_preview(500))"
         | "tool_input \($time) | \(.)")
      else empty
      end
    )
' "$SESSION_FILE" \
  | grep -i --color=auto -C "$CONTEXT" -- "$KEYWORD" \
  || echo "(no matches found)" >&2
