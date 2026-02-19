#!/bin/bash
# start-interaction.sh - Start a persistent interactive pane for multiple TUI commands
#
# Usage: start-interaction.sh
# Returns: interaction ID to stdout (use with run-interaction.sh and end-interaction.sh)
# Exit: 0 on success, 1 on error
#
# Example:
#   id=$(start-interaction.sh)
#   result1=$(run-interaction.sh "$id" gum choose "a" "b" "c")
#   result2=$(run-interaction.sh "$id" gum input --placeholder "Name")
#   end-interaction.sh "$id"

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/session-lib.sh"

id=$(create_session)

# Set active interaction in tmux environment so run-interactive.sh auto-detects it
tmux set-environment -g ACTIVE_INTERACTION_ID "$id"

echo "$id"
