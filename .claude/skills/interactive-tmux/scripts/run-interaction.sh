#!/bin/bash
# run-interaction.sh - Run a command in an existing interaction pane
#
# Usage: run-interaction.sh <interaction-id> <command> [args...]
# Returns: command output to stdout
# Exit: same exit code as the command
#
# Example:
#   result=$(run-interaction.sh "$id" gum choose "a" "b" "c")

set -euo pipefail

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

# Build the command string with proper quoting
cmd=""
for arg in "$@"; do
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
