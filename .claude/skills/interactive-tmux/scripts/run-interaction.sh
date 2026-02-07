#!/bin/bash
# run-interaction.sh - Run a command in an existing interaction pane
#
# Usage: run-interaction.sh <interaction-id> <command> [args...]
# Returns: command output to stdout
# Exit: same exit code as the command
#
# For gum choose/filter commands, automatically resizes the interaction pane
# and injects --height to match the number of options.
#
# Example:
#   result=$(run-interaction.sh "$id" gum choose "a" "b" "c")

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/gum-sizing.sh"

if [[ $# -lt 2 ]]; then
    echo "Usage: run-interaction.sh <interaction-id> <command> [args...]" >&2
    exit 1
fi

interaction_id="$1"
shift

# Get interaction directory
if [[ ! -f "/tmp/$interaction_id.dir" ]]; then
    echo "Error: Interaction '$interaction_id' not found" >&2
    exit 1
fi

interaction_dir=$(cat "/tmp/$interaction_id.dir")

if [[ ! -d "$interaction_dir" ]]; then
    echo "Error: Interaction directory not found" >&2
    exit 1
fi

# Check pane liveness before sending commands
if [[ -f "$interaction_dir/pane_id" ]]; then
    pane_id=$(cat "$interaction_dir/pane_id")
    if ! tmux list-panes -t "$pane_id" &>/dev/null; then
        echo "Error: Interaction pane '$pane_id' is no longer alive" >&2
        exit 1
    fi
fi

# Dynamic sizing for gum commands: resize pane + inject --height
cmd_args=("$@")
if calculate_gum_sizing "$@"; then
    # Resize the interaction pane
    if [[ -f "$interaction_dir/pane_id" ]]; then
        tmux resize-pane -t "$pane_id" -y "$pane_lines" 2>/dev/null || true
    fi
    # Inject --height for choose/filter commands
    if [[ -n "${gum_height:-}" ]]; then
        inject_gum_height "$gum_height" "$@"
        cmd_args=("${injected_args[@]}")
    fi
fi

# Build the command string with proper quoting
cmd=""
for arg in "${cmd_args[@]}"; do
    cmd+="$(printf '%q ' "$arg")"
done

# Send command to the FIFO
echo "$cmd" > "$interaction_dir/cmd_fifo"

# Wait for completion
tmux wait-for "${interaction_id}-done"

# Read and output result
if [[ -f "$interaction_dir/result" ]]; then
    cat "$interaction_dir/result"
fi

# Exit with the command's exit code
exit_code=0
if [[ -f "$interaction_dir/exit_code" ]]; then
    exit_code=$(cat "$interaction_dir/exit_code")
fi

exit "$exit_code"
