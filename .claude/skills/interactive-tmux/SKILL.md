---
name: interactive-tmux
description: Run interactive TUI commands (like gum, fzf, etc.) in a tmux pane and capture their output. Use when you need user input from an interactive terminal UI. Supports persistent interactions for multi-command sequences.
allowed-tools: Bash
---

# Interactive Tmux

Use this skill when you need to run interactive TUI commands that require user input and capture the result. This is essential for commands like `gum choose`, `gum input`, `fzf`, or any other interactive terminal application.

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
# Get user choice from a list
~/.claude/skills/interactive-tmux/scripts/run-interactive.sh gum choose "option1" "option2" "option3"

# Get text input from user
~/.claude/skills/interactive-tmux/scripts/run-interactive.sh gum input --placeholder "Enter your name"

# Get confirmation
~/.claude/skills/interactive-tmux/scripts/run-interactive.sh gum confirm "Are you sure?"

# Use fzf to select a file
~/.claude/skills/interactive-tmux/scripts/run-interactive.sh fzf
```

## Interactions (Multiple Commands, Same Pane)

For a smoother UX when asking multiple questions, use interactions. This keeps a single pane open for all commands instead of opening/closing for each one.

```bash
SCRIPTS=~/.claude/skills/interactive-tmux/scripts

# Start an interaction (opens pane, returns ID)
id=$("$SCRIPTS/start-interaction.sh")

# Run multiple commands - pane stays open!
result1=$("$SCRIPTS/run-interactive.sh" gum choose "a" "b" "c")
result2=$("$SCRIPTS/run-interactive.sh" gum input --placeholder "Name")
result3=$("$SCRIPTS/run-interactive.sh" gum confirm "Proceed?")

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

## Output

The script outputs the command's stdout and exits with the command's exit code:

```bash
result=$(~/.claude/skills/interactive-tmux/scripts/run-interactive.sh gum choose "a" "b" "c")
echo "User chose: $result"
```

Check exit code for commands like `gum confirm`:

```bash
if ~/.claude/skills/interactive-tmux/scripts/run-interactive.sh gum confirm "Proceed?"; then
    echo "User confirmed"
else
    echo "User declined"
fi
```
