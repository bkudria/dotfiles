#!/bin/bash
# run-interactive.sh - Run an interactive TUI command in a tmux pane and capture output
#
# Usage: run-interactive.sh <command> [args...]
#
# If an interaction is active (started via start-interaction.sh), the command
# runs in that existing pane. Otherwise, creates a one-off pane.
#
# Automatically detects terminal aspect ratio and splits accordingly:
#   - Landscape (wide): horizontal split (side by side), 50% width
#   - Portrait (tall): vertical split (stacked), dynamic height
#
# For gum choose/filter commands, automatically sizes the pane and sets
# --height to match the number of options (clamped to 20%-80% of window).
#
# Outputs the command's stdout and exits with the command's exit code.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/gum-sizing.sh"
source "$SCRIPT_DIR/session-lib.sh"

if [ $# -eq 0 ]; then
    echo "Usage: run-interactive.sh <command> [args...]" >&2
    exit 1
fi

# Path 1: Nesting prevention
# If we're already inside an interactive-tmux pane, resize + run directly
# (prevents double-pane nesting when scripts like ask-choose.sh call us internally)
if [[ "${INTERACTIVE_TMUX_PANE:-}" == "1" ]]; then
    if calculate_gum_sizing "$@"; then
        # Resize the current pane to fit
        tmux resize-pane -y "$pane_lines" 2>/dev/null || true
        # Inject --height for choose/filter commands
        if [[ -n "${gum_height:-}" ]]; then
            inject_gum_height "$gum_height" "$@"
            exec "${injected_args[@]}"
        fi
    fi
    exec "$@"
fi

# Path 2: Active interaction delegation
active_interaction=$(tmux show-environment -g ACTIVE_INTERACTION_ID 2>/dev/null | cut -d= -f2- || echo "")

if [[ -n "$active_interaction" && -f "/tmp/$active_interaction.dir" ]]; then
    exec "$SCRIPT_DIR/run-interaction.sh" "$active_interaction" "$@"
fi

# Path 3: One-off command via unified session lifecycle
size_flag=""
if calculate_gum_sizing "$@"; then
    # Check split direction to decide if size_flag applies
    local_width=$(tmux display-message -p '#{pane_width}')
    local_height=$(tmux display-message -p '#{pane_height}')
    if [ "$local_width" -le $((local_height * 2)) ]; then
        # Portrait mode: set pane height directly
        size_flag="-l $pane_lines"
    fi
fi

id=$(create_session "$size_flag")
trap 'destroy_session "$id"' EXIT

set +e
"$SCRIPT_DIR/run-interaction.sh" "$id" "$@"
run_exit=$?
set -e

destroy_session "$id"
trap - EXIT
exit "$run_exit"
