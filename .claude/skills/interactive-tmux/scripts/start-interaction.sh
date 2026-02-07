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

# Generate unique interaction ID
interaction_id="interaction-$$-$RANDOM"

# Get current pane dimensions for split direction
width=$(tmux display-message -p '#{pane_width}')
height=$(tmux display-message -p '#{pane_height}')
char_ratio=2

if [ "$width" -gt $((height * char_ratio)) ]; then
    split_flag="-h"
else
    split_flag="-v"
fi

# Create temp directory for this interaction
interaction_dir=$(mktemp -d)
echo "$interaction_dir" > "/tmp/$interaction_id.dir"

# Create the command FIFO
mkfifo "$interaction_dir/cmd_fifo"

# Create the runner script that will live in the pane
runner_script="$interaction_dir/runner.sh"
cat > "$runner_script" << 'RUNNER_EOF'
#!/bin/bash
export INTERACTIVE_TMUX_PANE=1
interaction_dir="$1"
interaction_id="$2"

# Signal that we're ready
tmux wait-for -S "${interaction_id}-ready"

# Main loop - read and execute commands
while true; do
    # Read command from FIFO (blocks until command arrives)
    if ! read -r cmd < "$interaction_dir/cmd_fifo"; then
        break
    fi

    # Check for exit signal
    if [[ "$cmd" == "__EXIT__" ]]; then
        break
    fi

    # Clear screen for clean UX
    clear

    # Execute the command using command substitution
    # This allows gum to access /dev/tty for UI while capturing stdout for result
    set +e
    __result=$(eval "$cmd")
    exit_code=$?
    set -e

    # Write result to file
    printf '%s' "$__result" > "$interaction_dir/result"
    echo "$exit_code" > "$interaction_dir/exit_code"

    # Signal completion
    tmux wait-for -S "${interaction_id}-done"
done
RUNNER_EOF
chmod +x "$runner_script"

# Create the pane running our runner script
pane_id=$(tmux split-window $split_flag -P -F '#{pane_id}' "$runner_script" "$interaction_dir" "$interaction_id")

# Store pane ID
echo "$pane_id" > "/tmp/$interaction_id.pane"

# Wait for the pane to be ready
tmux wait-for "${interaction_id}-ready"

# Set active interaction in tmux environment so run-interactive.sh auto-detects it
tmux set-environment -g ACTIVE_INTERACTION_ID "$interaction_id"

# Output the interaction ID
echo "$interaction_id"
