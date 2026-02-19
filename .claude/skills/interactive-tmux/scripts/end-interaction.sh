#!/bin/bash
# end-interaction.sh - Close an interaction pane and clean up
#
# Usage: end-interaction.sh <interaction-id>
# Exit: 0 on success
#
# Example:
#   end-interaction.sh "$id"

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/session-lib.sh"

if [[ $# -lt 1 ]]; then
    echo "Usage: end-interaction.sh <interaction-id>" >&2
    exit 1
fi

interaction_id="$1"

# Only unset ACTIVE_INTERACTION_ID if it matches this session's ID
active=$(tmux show-environment -g ACTIVE_INTERACTION_ID 2>/dev/null | cut -d= -f2- || echo "")
if [[ "$active" == "$interaction_id" ]]; then
    tmux set-environment -g -u ACTIVE_INTERACTION_ID 2>/dev/null || true
fi

destroy_session "$interaction_id"
