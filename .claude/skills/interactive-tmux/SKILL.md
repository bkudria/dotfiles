---
name: interactive-tmux
description: Run interactive TUI commands (like gum, fzf, etc.) in a tmux pane and capture their output. Use when you need user input from an interactive terminal UI. Supports persistent interactions for multi-command sequences.
---

# Interactive Tmux

Use this skill when you need to run interactive TUI commands that require user input and capture the result. This is essential for commands like `gum choose`, `gum input`, `fzf`, or any other interactive terminal application.

## Dependencies

| Dependency | Required | Notes |
|------------|----------|-------|
| `tmux` | Yes | Must be running inside a tmux session |
| `gum`, `fzf`, etc. | No | The TUI commands themselves — install whichever ones are needed |

## When to Use

- When running `gum` commands that need user interaction (choose, input, confirm, filter, etc.)
- When running `fzf` or similar fuzzy finders
- When running any command that requires a TTY for user input
- When you need to capture the output of an interactive command
- When you need multiple interactive commands in the same pane (use interactions)

## Scripts

| Script | Purpose |
|--------|---------|
| `run-interactive.sh` | Run a single interactive command (auto-detects interactions) |
| `start-interaction.sh` | Start a persistent pane for multiple commands |
| `run-interaction.sh` | Run a command in an existing interaction |
| `end-interaction.sh` | Close an interaction pane |

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

- **Auto-detects terminal orientation**: Splits horizontally (side-by-side) in landscape mode, vertically (stacked) in portrait mode
- **Clean UX**: The user only sees the TUI, not any setup commands
- **Auto-closes**: One-off panes close automatically; interactions close when ended
- **Captures output**: Returns stdout from the command
- **Preserves exit code**: Exits with the same exit code as the command
- **Auto-detects interactions**: `run-interactive.sh` reuses active interactions
