---
name: interactive-tmux
description: Run interactive TUI commands (like gum, fzf, etc.) in a tmux pane and capture their output. Use when running gum, fzf, or other commands that require a TTY for user input. Supports persistent interactions for multi-command sequences.
---

# Interactive Tmux

Run interactive TUI commands that require user input and capture the result. Essential for commands like `gum choose`, `gum input`, `fzf`, or any other interactive terminal application.

## Dependencies

| Dependency | Required | Notes |
|------------|----------|-------|
| `tmux` | Yes | Must be running inside a tmux session |
| `gum`, `fzf`, etc. | No | The TUI commands themselves — install whichever ones are needed |

## When to Use

- Running `gum` commands that need user interaction (choose, input, confirm, filter, etc.)
- Running `fzf` or similar fuzzy finders
- Running any command that requires a TTY for user input
- Capturing the output of an interactive command
- Running multiple interactive commands in the same pane (use interactions)

## Scripts

| Script | Purpose |
|--------|---------|
| `run-interactive.sh` | Run a single interactive command (auto-detects interactions) |
| `start-interaction.sh` | Start a persistent pane for multiple commands |
| `run-interaction.sh` | Run a command in an existing interaction |
| `end-interaction.sh` | Close an interaction pane |
| `session-lib.sh` | Shared session lifecycle (create/destroy pane + runner) |
| `gum-sizing.sh` | Shared library for dynamic pane/gum height calculation |

## Basic Usage (One-off Commands)

```bash
~/.claude/skills/interactive-tmux/scripts/run-interactive.sh <command> [args...]
```

### Examples

```bash
SCRIPTS=~/.claude/skills/interactive-tmux/scripts

# Get user choice from a list
framework=$("$SCRIPTS/run-interactive.sh" gum choose "React" "Vue" "Svelte" "Angular")

# Get text input from user
project_name=$("$SCRIPTS/run-interactive.sh" gum input --placeholder "Enter project name")

# Get confirmation (check exit code)
if "$SCRIPTS/run-interactive.sh" gum confirm "Delete all test fixtures?"; then
    echo "User confirmed"
else
    echo "User declined"
fi

# Use fzf to select a file
file=$("$SCRIPTS/run-interactive.sh" fzf --preview 'cat {}')
```

## Interactions (Multiple Commands, Same Pane)

For a smoother UX when asking multiple questions, use interactions. This keeps a single pane open for all commands instead of opening/closing for each one.

```bash
SCRIPTS=~/.claude/skills/interactive-tmux/scripts

# Start an interaction (opens pane, returns ID)
id=$("$SCRIPTS/start-interaction.sh")

# Run multiple commands - pane stays open!
db_type=$("$SCRIPTS/run-interactive.sh" gum choose "PostgreSQL" "MySQL" "SQLite")
db_name=$("$SCRIPTS/run-interactive.sh" gum input --placeholder "Database name")
"$SCRIPTS/run-interactive.sh" gum confirm "Create $db_type database '$db_name'?"

# End interaction (closes pane)
"$SCRIPTS/end-interaction.sh" "$id"
```

### Auto-Detection

When an interaction is active, `run-interactive.sh` **automatically detects and reuses it**. This means scripts that use `run-interactive.sh` (like the `advanced-ask` scripts) work seamlessly with interactions - no code changes needed.

```bash
# These ask-* scripts automatically use the interaction!
id=$("$SCRIPTS/start-interaction.sh")
color=$("$ASK/ask-choose.sh" --header "Color" "Red" "Green" "Blue")
name=$("$ASK/ask-input.sh" --header "Name")
"$SCRIPTS/end-interaction.sh" "$id"
```

## Behavior

- **Dynamic pane sizing**: For `gum choose`/`gum filter`, automatically sizes the pane height and gum `--height` to match the number of options (capped at 80% of window height). Works for both one-off panes and interactions (pane resizes between questions).
- **Auto-detects terminal orientation**: Splits horizontally (side-by-side) in landscape mode, vertically (stacked) in portrait mode
- **Nesting prevention**: Scripts that call `run-interactive.sh` internally (like `advanced-ask` scripts) can themselves be wrapped in `run-interactive.sh` without creating double panes
- **Clean UX**: The user only sees the TUI, not any setup commands
- **Auto-closes**: One-off panes close automatically; interactions close when ended
- **Captures output**: Returns stdout from the command
- **Preserves exit code**: Exits with the same exit code as the command
- **Auto-detects interactions**: `run-interactive.sh` reuses active interactions
