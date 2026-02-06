#!/bin/bash
# end-interaction.sh - Close an interaction pane and clean up
#
# Usage: end-interaction.sh <interaction-id>
# Exit: 0 on success
#
# Example:
#   end-interaction.sh "$id"

set -euo pipefail

if [[ $# -lt 1 ]]; then
    echo "Usage: end-interaction.sh <interaction-id>" >&2
    exit 1
fi

interaction_id="$1"

# Get interaction directory
if [[ ! -f "/tmp/$interaction_id.dir" ]]; then
    # Already cleaned up, that's fine
    exit 0
fi

interaction_dir=$(cat "/tmp/$interaction_id.dir")

# Send exit signal to the runner
if [[ -p "$interaction_dir/cmd_fifo" ]]; then
    echo "__EXIT__" > "$interaction_dir/cmd_fifo" 2>/dev/null || true
fi

# Kill the pane if it still exists
if [[ -f "/tmp/$interaction_id.pane" ]]; then
    pane_id=$(cat "/tmp/$interaction_id.pane")
    tmux kill-pane -t "$pane_id" 2>/dev/null || true
    rm -f "/tmp/$interaction_id.pane"
fi

# Clean up temp files
rm -f "/tmp/$interaction_id.dir"
rm -rf "$interaction_dir" 2>/dev/null || true

# Unset active interaction in tmux environment
tmux set-environment -g -u ACTIVE_INTERACTION_ID 2>/dev/null || true

exit 0
