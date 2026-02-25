---
name: gum
description: Reference for using the `gum` CLI tool from Charmbracelet to create glamorous shell scripts with interactive TUI components. Use when building interactive prompts, styling terminal output, displaying spinners, or creating user-facing CLI experiences. CRITICAL - many gum commands are interactive TUIs that block waiting for user input.
---

# Gum CLI Reference

Gum is a tool for glamorous shell scripts, providing interactive TUI components and text styling.

## Critical: Interactive vs Non-Interactive Commands

**INTERACTIVE (block for user input - cannot be used directly by Claude):**
- `choose` - select from list
- `confirm` - yes/no prompt
- `file` - file picker
- `filter` - fuzzy filter list
- `input` - single-line text input
- `pager` - scroll through content
- `table` - interactive table selection
- `write` - multi-line text input

**NON-INTERACTIVE (safe for Claude to use directly):**
- `style` - apply colors/borders to text
- `format` - format markdown/code/emoji
- `join` - combine text blocks
- `log` - structured logging output
- `spin` - show spinner while running a command (runs the command, not interactive)

## Command Summary

| Command | Purpose | Key Flag |
|---------|---------|----------|
| `choose` | Select from options | `--header`, `--limit`, `--height` |
| `confirm` | Yes/no prompt | `--affirmative`, `--negative` |
| `input` | Single-line text input | `--placeholder`, `--password` |
| `write` | Multi-line text input | `--header`, `--show-line-numbers` |
| `filter` | Fuzzy filter list | `--strict`, `--no-fuzzy` |
| `file` | File picker (buggy — prefer `fzf`) | `--all`, `--directory` |
| `table` | Interactive table | `--print` (non-interactive mode) |
| `spin` | Spinner while running command | `--show-output`, `--spinner` |
| `style` | Apply colors/borders to text | `--border`, `--foreground` |
| `format` | Format markdown/code/emoji | `--type`, `--language` |
| `join` | Combine text blocks | `--horizontal`, `--vertical` |
| `log` | Structured logging output | `--level`, `--structured` |
| `pager` | Scroll through content | `--show-line-numbers` |

Consult `references/commands.md` for full flag reference and examples for each command.

## Styling Colors

Colors can be specified as:

| Format | Example |
|--------|---------|
| ANSI codes | `212`, `0`, `255` |
| Hex | `"#FF0000"`, `"#0FF"` |
| Named | `"red"`, `"blue"` (some terminals) |

Common ANSI codes:

| Code | Color |
|------|-------|
| `212` | pink/magenta |
| `99` | purple |
| `10` | green |
| `9` | red |
| `11` | yellow |
| `240` | gray |

## Environment Variables

All flags can be set via environment variables:
- Pattern: `GUM_<COMMAND>_<FLAG>`
- Examples: `GUM_INPUT_PLACEHOLDER="Enter value"`, `GUM_CHOOSE_HEIGHT=15`, `GUM_SPIN_SPINNER=dot`

## Exit Codes

| Command | Code | Meaning |
|---------|------|---------|
| `gum confirm` | 0 | Yes |
| `gum confirm` | 1 | No |
| `gum filter --strict` | 1 | No match found |
| Other commands | 0 | Success |
| Other commands | non-zero | Error or cancel |

## Dependencies

- **gum** — `brew install gum` ([charmbracelet/gum](https://github.com/charmbracelet/gum))

## Reference Files

| File | Contents |
|------|----------|
| `references/commands.md` | Complete command reference with all flags and examples |
| `references/patterns.md` | Common multi-command patterns (commit messages, wizards, dashboards) |
