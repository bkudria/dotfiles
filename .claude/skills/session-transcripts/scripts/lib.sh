# lib.sh — Shared bash functions for session transcript scripts.
# Usage: source "$SCRIPT_DIR/lib.sh"

# Decode encoded project path (best-effort — encoding is lossy).
# Claude Code encodes paths by replacing / with - and stripping all dots.
# This reverses -- → /. (hidden dirs) and - → /, but cannot recover
# dashes or dots within directory names.
decode_path() {
  local name="$1"
  echo "$name" | sed 's/^-/\//' | sed 's/--/\/./g' | sed 's/-/\//g'
}

# Encode filesystem path to project directory name (matches Claude Code).
# Algorithm: replace both / and . with -.
encode_path() {
  local path="${1%/}"
  echo "$path" | sed 's/\//-/g' | sed 's/\./-/g'
}

# Extract first user message from a session file (truncated, SIGPIPE-safe).
# Uses head -50 + jq -s to avoid SIGPIPE under set -euo pipefail.
# Args: $1 = file path, $2 = max chars (default 100)
first_user_message() {
  local file="$1" max_len="${2:-100}"
  head -50 "$file" | jq -rs --argjson n "$max_len" '
    [.[] | select(.type == "user") | .message.content
     | if type == "string" then .
       elif type == "array" then [.[] | select(.type == "text") | .text] | join(" ")
       else "" end
     | select(length > 0)
    ] | if length > 0 then .[0] else "" end
    | gsub("\n"; " ") | .[:$n]
  ' 2>/dev/null || true
}
