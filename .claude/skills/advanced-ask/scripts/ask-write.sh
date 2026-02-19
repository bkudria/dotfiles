#!/bin/bash
# ask-write.sh - Get multi-line text input from user
#
# Usage: ask-write.sh [--header "text"] [--placeholder "hint"] [--value "default"]
# Returns: user text to stdout (Ctrl+D or Esc to submit)
# Exit: 0 on input, 1 on cancel/error

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUN_INTERACTIVE="$HOME/.claude/skills/interactive-tmux/scripts/run-interactive.sh"

# Parse arguments
header=""
placeholder=""
value=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --header)
            header="$2"
            shift 2
            ;;
        --placeholder)
            placeholder="$2"
            shift 2
            ;;
        --value)
            value="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
    esac
done

# Build gum command
cmd=(gum write)
[[ -n "$header" ]] && cmd+=(--header "$header")
[[ -n "$placeholder" ]] && cmd+=(--placeholder "$placeholder")
[[ -n "$value" ]] && cmd+=(--value "$value")

# Run interactively
"$RUN_INTERACTIVE" "${cmd[@]}"
