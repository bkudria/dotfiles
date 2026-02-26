# Skill Interactive TUI Guide

Guide for using interactive TUI tools (gum, fzf, advanced-ask) within Claude Code skills.

---

## When to Use Interactive TUIs

| Scenario | Use TUI? | Reason |
|----------|----------|--------|
| User selects from >4 options | Yes | AskUserQuestion caps at 4 |
| User provides direct text input | Yes | AskUserQuestion requires "Other" workaround |
| User picks files or directories | Yes | No built-in equivalent |
| Multi-question form (>4 questions) | Yes | AskUserQuestion caps at 4 |
| Simple yes/no or <=4 choices | No | AskUserQuestion has better UI integration |
| Non-interactive or auto-triggered skill | No | TUI blocks waiting for input with no user present |

---

## Choosing the Right Tool

| Need | Tool | Script |
|------|------|--------|
| Simple choice (<=4 options) | AskUserQuestion | (built-in) |
| Many options (5+) | advanced-ask | `ask-choose.sh` |
| Multiple selections | advanced-ask | `ask-multi.sh` |
| Short text input | advanced-ask | `ask-input.sh` |
| Long text input | advanced-ask | `ask-write.sh` |
| File/directory picking | advanced-ask | `ask-file.sh` |
| Fuzzy search through list | advanced-ask | `ask-filter.sh` |
| Yes/no with custom labels | advanced-ask | `ask-confirm.sh` |
| Multi-question form | advanced-ask | `ask-form.sh` |

**Trade-off**: AskUserQuestion renders inline in the Claude Code UI and requires no dependencies. Advanced-ask opens a tmux pane and requires gum/fzf, but handles complex input scenarios that AskUserQuestion cannot.

---

## Integration Patterns

### Capture Output in SKILL.md Instructions

Reference advanced-ask scripts from SKILL.md and tell Claude what to do with the result:

```markdown
Run `~/.claude/skills/advanced-ask/scripts/ask-choose.sh --header "Framework" "React" "Vue" "Svelte"`
and use the selected framework to scaffold the project.
```

### Chain Prompts

Feed one script's output into the next:

```bash
SCRIPTS="$HOME/.claude/skills/advanced-ask/scripts"
lang=$("$SCRIPTS/ask-choose.sh" --header "Language" "TypeScript" "Python" "Go")
features=$("$SCRIPTS/ask-multi.sh" --header "Features for $lang" "Tests" "CI" "Docker")
```

### Handle Exit Codes

| Exit Code | Meaning | Action |
|-----------|---------|--------|
| 0 | Normal selection (including Skip, Other, empty) | Process the output |
| 1 | Cancelled or error | Abort gracefully or re-prompt |
| 2 | "Chat about this" selected | Return to conversation with the header text |

```bash
result=$("$SCRIPTS/ask-choose.sh" --header "Deploy target" --chattable "Staging" "Production")
code=$?
case $code in
    0) echo "Selected: $result" ;;
    1) echo "Cancelled" ;;
    2) echo "User wants to discuss: $result" ;;
esac
```

### Use Interactions for Multiple Prompts

Wrap multiple prompts in an interaction to keep a single tmux pane open:

```bash
TMUX_SCRIPTS=~/.claude/skills/interactive-tmux/scripts
ASK_SCRIPTS=~/.claude/skills/advanced-ask/scripts

id=$("$TMUX_SCRIPTS/start-interaction.sh")
name=$("$ASK_SCRIPTS/ask-input.sh" --header "Project name")
lang=$("$ASK_SCRIPTS/ask-choose.sh" --header "Language" "TypeScript" "Python" "Go")
"$TMUX_SCRIPTS/end-interaction.sh" "$id"
```

---

## Form Design

- Keep forms to 3-5 questions. Beyond 5, split into sections with progressive disclosure (confirm before showing the next section).
- Use appropriate question types: `input` for short text, `write` for paragraphs, `choose` for selection, `multi` for checkboxes.
- Provide helpful `placeholder` text that shows expected format.
- Group related questions in one `ask-form.sh` call rather than separate prompts.

```bash
"$SCRIPTS/ask-form.sh" --inline '{
  "questions": [
    {"question": "Project name?", "type": "input", "key": "name", "placeholder": "my-app"},
    {"question": "Language?", "type": "choose", "key": "lang", "options": ["TypeScript", "Python", "Go"]},
    {"question": "Features?", "type": "multi", "key": "features", "options": ["Tests", "CI", "Docker"]}
  ]
}'
```

---

## Common Mistakes

| Mistake | Problem | Fix |
|---------|---------|-----|
| Running `gum` directly via Bash | Blocks without a TTY; no output captured | Use advanced-ask scripts or `run-interactive.sh` |
| Ignoring exit code 1 (cancel) | Skill proceeds with empty/stale data | Check `$?` and abort or re-prompt |
| Ignoring empty output | Downstream commands break on empty strings | Validate output before using it |
| Forms with >7 questions | User fatigue; high abandonment | Split into sections with confirm gates |
| TUI prompts in auto-triggered skills | Skill blocks waiting for input with no user present | Restrict TUI usage to user-invoked skills only |
| Not using interactions for multi-prompt flows | Tmux pane opens and closes for each question | Wrap in `start-interaction.sh` / `end-interaction.sh` |
| Hardcoding `gum` flags instead of using ask scripts | Duplicates logic; misses exit code handling | Use `ask-*.sh` wrappers which handle edge cases |
