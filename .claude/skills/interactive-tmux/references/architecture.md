# Architecture

## Three-Path Design in run-interactive.sh

`run-interactive.sh` handles three mutually exclusive execution paths, checked in order:

### Path 1: Nesting Prevention (INTERACTIVE_TMUX_PANE)

When the `INTERACTIVE_TMUX_PANE` environment variable is set to `1`, the script is already running inside an interactive-tmux pane. This happens when an outer script (like `~/.claude/skills/advanced-ask/scripts/ask-choose.sh`) calls `run-interactive.sh` internally, and the outer script was itself wrapped in `run-interactive.sh`.

Instead of creating a nested pane, this path:
1. Calls `calculate_gum_sizing` to determine the correct pane size
2. Resizes the current pane via `tmux resize-pane -y`
3. Injects `--height` for `gum choose`/`gum filter` commands
4. Runs the command directly via `exec` (replaces the current process)

The `INTERACTIVE_TMUX_PANE=1` variable is set by `runner.sh` (written by `session-lib.sh`) when the pane is created.

### Path 2: Active Interaction Delegation (ACTIVE_INTERACTION_ID)

When a persistent interaction is active (started via `start-interaction.sh`), the tmux global environment variable `ACTIVE_INTERACTION_ID` holds the interaction ID.

This path checks for that variable and, if the corresponding session directory exists, delegates to `run-interaction.sh` with the active interaction ID. This allows any script that calls `run-interactive.sh` to transparently reuse the existing pane.

### Path 3: One-Off Session Creation

When neither of the above conditions is met, a fresh tmux pane is created for this single command:
1. Calls `calculate_gum_sizing` to determine sizing (if applicable)
2. Detects terminal orientation (landscape vs portrait) for split direction
3. Calls `create_session` from `session-lib.sh` to create the pane
4. Delegates to `run-interaction.sh` for command execution
5. Calls `destroy_session` to clean up (also registered as a trap)

## Gum Sizing (gum-sizing.sh)

### Pane Height Calculation

For `gum choose` and `gum filter` commands, the sizing logic dynamically calculates pane height based on the number and visual size of items:

1. **Count items**: Parse arguments after `gum choose/filter`, skipping flags and their values, to count the number of selectable items.
2. **Count visual lines**: For each item, count embedded newlines (multi-line items rendered with `--label-delimiter` occupy more than one terminal line). Use `printf '%s' "$item" | wc -l` for correct newline counting.
3. **Calculate desired height**: `visual_lines + padding` (padding = 6 lines for header, help line, prompt, and margins).
4. **Clamp**: Enforce a minimum of 5 lines and a maximum of 80% of `#{window_height}`.

For other gum subcommands (`confirm`, `input`, `write`, `file`, `table`, `spin`), fixed or percentage-based pane heights are used.

### gum --height Conversion

The `gum --height` flag specifies the number of visible **items**, not terminal lines. When items span multiple lines (e.g., items with descriptions via `--label-delimiter`):

```
gum_height = available_lines * item_count / visual_lines
```

Where `available_lines = pane_lines - padding`. For single-line items, `gum_height = available_lines` (1:1 mapping).

### inject_gum_height

After calculating `gum_height`, `inject_gum_height` inserts `--height <N>` into the argument list at position 2 (after `gum choose`/`gum filter`). If `--height` is already present in the arguments, the existing value is preserved.

## Session Lifecycle (session-lib.sh)

### create_session

1. Generate a unique session ID: `interaction-${BASHPID}-$RANDOM`
2. Create a temp directory and register it at `/tmp/<id>.dir`
3. Create a FIFO at `<dir>/cmd_fifo` for command dispatch
4. Detect terminal orientation: landscape (width > height * 2) splits horizontally (`-h`), portrait splits vertically (`-v`)
5. Write `runner.sh` into the temp directory -- this script:
   - Sets `INTERACTIVE_TMUX_PANE=1` (enables Path 1 nesting prevention)
   - Signals readiness via `tmux wait-for -S <id>-ready`
   - Loops reading commands from the FIFO
   - Executes each command via `eval` (necessary because the command string arrives serialized through the FIFO with `printf '%q'` quoting applied by `run-interaction.sh`)
   - Writes stdout to `<dir>/result` and exit code to `<dir>/exit_code`
   - Signals completion via `tmux wait-for -S <id>-done`
   - Has an EXIT trap to signal `-done` if the pane dies unexpectedly
6. Open the tmux pane via `tmux split-window`, passing the runner script
7. Store the pane ID at `<dir>/pane_id`
8. Wait for the runner's ready signal

### destroy_session

1. Send `__EXIT__` to the FIFO to signal the runner to exit its loop
2. Kill the tmux pane via `tmux kill-pane -t <pane_id>`
3. Remove `/tmp/<id>.dir` and the temp directory

Idempotent: returns 0 if the session is already cleaned up.

### eval Usage

The runner script uses `eval "$cmd"` to execute commands received via the FIFO. This is required because `run-interaction.sh` serializes command arguments using `printf '%q'` quoting (shell-escaped), which must be re-interpreted by the shell to reconstruct the original argument list. The FIFO transports a single string, not an argument array, so `eval` is the correct mechanism to restore the original quoting.

## Interaction Lifecycle

Persistent interactions allow multiple commands to share a single pane:

1. **Start**: `start-interaction.sh` calls `create_session` and sets `ACTIVE_INTERACTION_ID` in the tmux global environment
2. **Run commands**: Each call to `run-interactive.sh` detects `ACTIVE_INTERACTION_ID` (Path 2) and delegates to `run-interaction.sh`, which:
   - Resizes the pane if gum sizing applies
   - Serializes and sends the command through the FIFO
   - Waits for the `-done` signal
   - Reads result and exit code from the temp directory
3. **End**: `end-interaction.sh` unsets `ACTIVE_INTERACTION_ID` (only if it matches) and calls `destroy_session`
