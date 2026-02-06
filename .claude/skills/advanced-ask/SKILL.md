---
name: advanced-ask
description: This skill should be used when the built-in AskUserQuestion tool is insufficient - specifically when needing to ask more than 4 questions, present more than 4 options, get direct text input without "Other" workaround, pick files/directories, or fuzzy-filter through long lists. Complements gum and interactive-tmux skills.
---

# Advanced Ask

Extended user questioning capabilities that complement the built-in `AskUserQuestion` tool using `gum` via `interactive-tmux`.

## When to Use This Skill

Use `advanced-ask` when `AskUserQuestion` cannot handle the scenario:

| Scenario | AskUserQuestion | advanced-ask |
|----------|-----------------|--------------|
| >4 options | No (max 4) | Yes (unlimited) |
| >4 questions | No (max 4) | Yes (unlimited via form) |
| Direct text input | No (only via "Other") | Yes (`ask-input`) |
| Multi-line text | No | Yes (`ask-write`) |
| File/dir picker | No | Yes (`ask-file`) |
| Fuzzy filter list | No | Yes (`ask-filter`) |
| Custom confirm labels | No | Yes (`ask-confirm`) |

**Prefer AskUserQuestion** for simple cases (≤4 questions, ≤4 options) as it has better integration with Claude Code's UI.

## Quick Reference

All scripts are in `~/.claude/skills/advanced-ask/scripts/`.

### Single Selection (Many Options)
```bash
~/.claude/skills/advanced-ask/scripts/ask-choose.sh --header "Pick a framework" \
    "React" "Vue" "Angular" "Svelte" "Solid" "Preact" "Qwik"

# With descriptions (use "label|description" format)
~/.claude/skills/advanced-ask/scripts/ask-choose.sh --header "Pick a language" --descriptions \
    "Python|Great for scripting" "Go|Fast and compiled" "Rust|Memory safe"

# With Other/Skip/Chat options (like AskUserQuestion)
~/.claude/skills/advanced-ask/scripts/ask-choose.sh --header "Pick a language" \
    --other --skippable --chattable \
    "Python" "Go" "Rust"
```

### Multi-Selection
```bash
~/.claude/skills/advanced-ask/scripts/ask-multi.sh --header "Select features" \
    "Auth" "Database" "API" "Cache" "Queue" "Search"

# With descriptions
~/.claude/skills/advanced-ask/scripts/ask-multi.sh --header "Select features" --descriptions \
    "Auth|User authentication" "Database|PostgreSQL setup" "Cache|Redis caching"

# With Other option (allows custom additions)
~/.claude/skills/advanced-ask/scripts/ask-multi.sh --header "Select features" --other \
    "Auth" "Database" "API"
```

### Text Input
```bash
~/.claude/skills/advanced-ask/scripts/ask-input.sh \
    --header "Configuration" \
    --placeholder "Enter API key"
```

### Multi-line Text
```bash
~/.claude/skills/advanced-ask/scripts/ask-write.sh \
    --header "Description" \
    --placeholder "Enter detailed description..."
```

### File Picker
```bash
~/.claude/skills/advanced-ask/scripts/ask-file.sh ~/projects
~/.claude/skills/advanced-ask/scripts/ask-file.sh --directory              # directories only
~/.claude/skills/advanced-ask/scripts/ask-file.sh --preview                # with file preview
~/.claude/skills/advanced-ask/scripts/ask-file.sh --all                    # include hidden files
~/.claude/skills/advanced-ask/scripts/ask-file.sh --glob "SKILL.md" ~/.claude/skills  # only SKILL.md files
~/.claude/skills/advanced-ask/scripts/ask-file.sh --ext "ts" ~/project     # only .ts files
~/.claude/skills/advanced-ask/scripts/ask-file.sh --name "*.test.js"       # pattern match
```

Note: Uses `fzf` instead of `gum file` due to [gum display bugs](https://github.com/charmbracelet/gum/issues/977).

### Fuzzy Filter
```bash
# From arguments
~/.claude/skills/advanced-ask/scripts/ask-filter.sh --header "Search" \
    "item1" "item2" "item3" ...

# From stdin
git branch | ~/.claude/skills/advanced-ask/scripts/ask-filter.sh --header "Select branch"
```

### Confirmation
```bash
~/.claude/skills/advanced-ask/scripts/ask-confirm.sh \
    --yes "Deploy" --no "Cancel" \
    "Deploy to production?"
# Exit code: 0=yes, 1=no
```

### Multi-Question Form
```bash
# From JSON file
~/.claude/skills/advanced-ask/scripts/ask-form.sh form.json

# Inline JSON
~/.claude/skills/advanced-ask/scripts/ask-form.sh --inline '{
  "questions": [
    {"question": "Project name?", "type": "input", "key": "name"},
    {"question": "Language?", "type": "choose", "options": ["TypeScript", "Python", "Go", "Rust"], "key": "lang"},
    {"question": "Features?", "type": "multi", "options": ["Tests", "CI", "Docker", "Docs"], "key": "features"},
    {"question": "Description?", "type": "write", "key": "desc"}
  ]
}'
# Returns JSON: {"name": "...", "lang": "...", "features": [...], "desc": "..."}
```

## Script Reference

| Script | Purpose | Output |
|--------|---------|--------|
| `ask-choose.sh` | Single selection from many options | Selected option |
| `ask-multi.sh` | Multiple selections | Newline-separated selections |
| `ask-input.sh` | Single-line text input | User input |
| `ask-write.sh` | Multi-line text input | User text |
| `ask-file.sh` | File/directory picker with glob/ext filters | Selected path |
| `ask-filter.sh` | Fuzzy search through list | Selected item(s) |
| `ask-confirm.sh` | Yes/no with custom labels | Exit code (0=yes, 1=no) |
| `ask-form.sh` | Multi-question form | JSON object with answers |

### Common Flags for ask-choose.sh and ask-multi.sh

| Flag | Description |
|------|-------------|
| `--header "text"` | Header text shown above choices |
| `--descriptions` | Enable descriptions using "label\|description" format |
| `--other` | Add "Other..." option for custom input |
| `--skippable` | Add "Skip" option (returns empty) |
| `--chattable` | Add "Chat about this" option (exits with code 2) |
| `--limit N` | (multi only) Maximum selections |

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Normal selection (including Skip, Other, empty) |
| 1 | Cancelled or error |
| 2 | "Chat about this" selected (header text in stdout) |

## Form JSON Schema

```json
{
  "questions": [
    {
      "question": "Display text for the question",
      "type": "input|choose|multi|write|file|filter|confirm",
      "key": "result_key_name",
      "options": ["for choose/multi/filter types"],
      "placeholder": "optional hint text",
      "default": "optional default value",
      "yes": "optional yes label (confirm only)",
      "no": "optional no label (confirm only)",
      "descriptions": true,
      "other": true,
      "skippable": true,
      "chattable": true,
      "limit": 3
    }
  ]
}
```

### Form Options (choose/multi types)

| Field | Type | Description |
|-------|------|-------------|
| `descriptions` | boolean | Parse options as "label\|description" |
| `other` | boolean | Add "Other..." for custom input |
| `skippable` | boolean | Add "Skip" option |
| `chattable` | boolean | Add "Chat about this" (exits form with code 2) |
| `limit` | number | (multi only) Max selections |

Example with descriptions:
```json
{
  "questions": [
    {
      "question": "Favorite language?",
      "type": "choose",
      "key": "lang",
      "descriptions": true,
      "other": true,
      "options": ["Python|Great for scripting", "Go|Fast compiled", "Rust|Memory safe"]
    }
  ]
}
```

## Dependencies

This skill depends on:
- **gum** - Charmbracelet's CLI tool (`brew install gum`)
- **fzf** - Fuzzy finder, used for file picking (`brew install fzf`)
- **interactive-tmux** skill - For running interactive TUIs
- **jq** - For JSON processing in `ask-form.sh` (`brew install jq`)
- **fd** (optional) - Faster file finding for `ask-file.sh` (`brew install fd`)

## Additional Resources

For advanced patterns and examples, see [references/patterns.md](references/patterns.md).
