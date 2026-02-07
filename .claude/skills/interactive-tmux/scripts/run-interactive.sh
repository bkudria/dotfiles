#!/bin/bash
# run-interactive.sh - Run an interactive TUI command in a tmux pane and capture output
#
# Usage: run-interactive.sh <command> [args...]
#
# If an interaction is active (started via start-interaction.sh), the command
# runs in that existing pane. Otherwise, creates a one-off pane.
#
# Automatically detects terminal aspect ratio and splits accordingly:
#   - Landscape (wide): horizontal split (side by side)
#   - Portrait (tall): vertical split (stacked)
#
# Outputs the command's stdout and exits with the command's exit code.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ $# -eq 0 ]; then
    echo "Usage: run-interactive.sh <command> [args...]" >&2
    exit 1
fi

# If we're already inside an interactive-tmux pane, just run the command directly
# (prevents double-pane nesting when scripts like ask-choose.sh call us internally)
if [[ "${INTERACTIVE_TMUX_PANE:-}" == "1" ]]; then
    exec "$@"
fi

# Check if there's an active interaction we should use
active_interaction=$(tmux show-environment -g ACTIVE_INTERACTION_ID 2>/dev/null | cut -d= -f2- || echo "")

if [[ -n "$active_interaction" && -f "/tmp/$active_interaction.dir" ]]; then
    # Use the existing interaction pane
    exec "$SCRIPT_DIR/run-interaction.sh" "$active_interaction" "$@"
fi

# No active interaction - create a one-off pane (original behavior)

# Get current pane dimensions
width=$(tmux display-message -p '#{pane_width}')
height=$(tmux display-message -p '#{pane_height}')

# Character aspect ratio adjustment (chars are ~2x taller than wide)
char_ratio=2

# Determine split direction based on physical aspect ratio
# If width > height * char_ratio, terminal is landscape → horizontal split
if [ "$width" -gt $((height * char_ratio)) ]; then
    split_flag="-h"
else
    split_flag="-v"
fi

# Generate unique channel name for this invocation
channel="interactive-$$-$RANDOM"

# Create a temporary script that will run in the pane
# This keeps the pane clean - user only sees the TUI, not our bookkeeping
wrapper_script=$(mktemp)
cat > "$wrapper_script" << 'WRAPPER_EOF'
#!/bin/bash
export INTERACTIVE_TMUX_PANE=1
channel="$1"
shift
clear
__result=$("$@")
__exit_code=$?
tmux set-environment -g INTERACTIVE_RESULT "$__result"
tmux set-environment -g INTERACTIVE_EXIT_CODE "$__exit_code"
tmux wait-for -S "$channel"
WRAPPER_EOF
chmod +x "$wrapper_script"

# Create the pane running our wrapper script
# The pane will close automatically when the wrapper exits
pane=$(tmux split-window $split_flag -P -F '#{pane_id}' "$wrapper_script" "$channel" "$@")

# Wait for the command to complete (blocks until user interaction finishes)
tmux wait-for "$channel"

# Retrieve results from tmux environment
result=$(tmux show-environment -g INTERACTIVE_RESULT 2>/dev/null | cut -d= -f2- || echo "")
exit_code=$(tmux show-environment -g INTERACTIVE_EXIT_CODE 2>/dev/null | cut -d= -f2- || echo "0")

# Clean up: kill the pane (should already be closed, but just in case)
tmux kill-pane -t "$pane" 2>/dev/null || true

# Clean up: remove environment variables and temp script
tmux set-environment -g -u INTERACTIVE_RESULT 2>/dev/null || true
tmux set-environment -g -u INTERACTIVE_EXIT_CODE 2>/dev/null || true
rm -f "$wrapper_script"

# Output the result
echo "$result"

# Exit with the command's exit code
exit "${exit_code:-0}"
