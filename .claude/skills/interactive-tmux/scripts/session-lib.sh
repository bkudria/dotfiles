#!/bin/bash
# session-lib.sh - Shared session lifecycle (create/destroy pane + runner)
#
# Source this file to get: create_session, destroy_session
#
# Usage:
#   source "$(dirname "${BASH_SOURCE[0]}")/session-lib.sh"
#   id=$(create_session "-l 15")   # optional size_flag
#   destroy_session "$id"

# Create a new interactive session: tmux pane + runner loop + FIFO.
# Args: [size_flag] - optional tmux split-window size flag (e.g. "-l 15")
# Stdout: session ID
# Returns 0 on success, 1 on error.
create_session() {
    local size_flag="${1:-}"

    # Generate unique ID ($BASHPID is the actual subshell PID, unlike $$ which is inherited)
    local id="interaction-${BASHPID:-$$}-$RANDOM"

    # Create temp dir and pointer
    local dir
    dir=$(mktemp -d)
    echo "$dir" > "/tmp/$id.dir"

    # Create command FIFO
    mkfifo "$dir/cmd_fifo"

    # Determine split direction from terminal aspect ratio
    local width height char_ratio=2 split_flag
    width=$(tmux display-message -p '#{pane_width}')
    height=$(tmux display-message -p '#{pane_height}')
    if [ "$width" -gt $((height * char_ratio)) ]; then
        split_flag="-h"
    else
        split_flag="-v"
    fi

    # Write runner script
    cat > "$dir/runner.sh" << 'RUNNER_EOF'
#!/bin/bash
export INTERACTIVE_TMUX_PANE=1
interaction_dir="$1"
interaction_id="$2"

# If the pane dies unexpectedly, signal done so callers don't hang
trap 'tmux wait-for -S "${interaction_id}-done" 2>/dev/null' EXIT

# Signal that we're ready
tmux wait-for -S "${interaction_id}-ready"

# Main loop - read and execute commands from FIFO
while true; do
    if ! read -r cmd < "$interaction_dir/cmd_fifo"; then
        break  # FIFO EOF
    fi

    if [[ "$cmd" == "__EXIT__" ]]; then
        break
    fi

    clear

    set +e
    __result=$(eval "$cmd")
    exit_code=$?
    set -e

    printf '%s' "$__result" > "$interaction_dir/result"
    echo "$exit_code" > "$interaction_dir/exit_code"

    # Signal completion (clear EXIT trap signal so we don't double-signal)
    tmux wait-for -S "${interaction_id}-done"
done

# Disable the EXIT trap since we're exiting cleanly
trap - EXIT
RUNNER_EOF
    chmod +x "$dir/runner.sh"

    # Create the pane
    # shellcheck disable=SC2086
    local pane_id
    pane_id=$(tmux split-window $split_flag $size_flag -P -F '#{pane_id}' "$dir/runner.sh" "$dir" "$id")

    # Store pane ID inside the session dir
    echo "$pane_id" > "$dir/pane_id"

    # Wait for the runner to be ready
    tmux wait-for "${id}-ready"

    echo "$id"
}

# Destroy a session: signal runner to exit, kill pane, clean up files.
# Args: <id>
# Returns 0 always (idempotent).
destroy_session() {
    local id="$1"

    # Look up dir
    if [[ ! -f "/tmp/$id.dir" ]]; then
        return 0  # already cleaned up
    fi

    local dir
    dir=$(cat "/tmp/$id.dir")

    # Signal runner to exit
    if [[ -p "$dir/cmd_fifo" ]]; then
        echo "__EXIT__" > "$dir/cmd_fifo" 2>/dev/null || true
    fi

    # Kill the pane
    if [[ -f "$dir/pane_id" ]]; then
        local pane_id
        pane_id=$(cat "$dir/pane_id")
        tmux kill-pane -t "$pane_id" 2>/dev/null || true
    fi

    # Clean up
    rm -f "/tmp/$id.dir"
    rm -rf "$dir" 2>/dev/null || true

    return 0
}
