# Skill Dynamic Context

Guide to string substitutions, backtick-bang commands, and argument handling in SKILL.md files.

---

## String Substitutions

Placeholders in the SKILL.md **body text** that are replaced at skill load time.

| Placeholder | Replaced With | Notes |
|-------------|--------------|-------|
| `$ARGUMENTS` | Full text after `/skill-name ` | Empty string if no arguments given |
| `$ARGUMENTS[N]` | Specific argument by 0-based index | Undefined if not enough args provided |
| `$1`, `$2`, ... `$N` | Shorthand for `$ARGUMENTS[N]` | Same as above |
| `${CLAUDE_SESSION_ID}` | Current session UUID | Stable for the session lifetime |

### Example Usage in Body

```markdown
---
name: lookup
argument-hint: "[topic] [depth]"
---

# Lookup: $1

Research the topic "$1" at depth level $2.

Full input was: $ARGUMENTS
Temp file: /tmp/lookup-${CLAUDE_SESSION_ID}.md
```

**Where substitutions work**: Body text only. They do **not** expand in frontmatter fields.

---

## Backtick-Bang Dynamic Context

Run shell commands at skill load time and inject their output into the skill content. The command output replaces the placeholder so Claude receives actual data, not the command itself.

### Syntax

Use `` !`command` `` directly in the **body text** of SKILL.md:

```markdown
---
name: pr-summary
description: Summarize changes in a pull request
context: fork
agent: Explore
---

## Pull request context
- PR diff: !`gh pr diff`
- PR comments: !`gh pr view --comments`
- Changed files: !`gh pr diff --name-only`

## Your task
Summarize this pull request...
```

### How It Works

1. At skill load time, each `` !`command` `` in the body is evaluated
2. The command runs in the current working directory
3. The command output replaces the `` !`command` `` placeholder
4. Claude receives the fully-rendered prompt with actual data

This is **preprocessing** — Claude only sees the final result, not the commands.

### More Examples

```markdown
# Current project state
- Branch: !`git branch --show-current`
- Status: !`git status --short`
- Recent commits: !`git log --oneline -5`

# Package info
- Node version: !`node --version`
- Dependencies: !`cat package.json | jq '.dependencies | keys[]'`
```

---

## Use Cases and Patterns

| Pattern | Syntax | When to Use |
|---------|--------|-------------|
| Load config file | `` !`cat .config` `` in body | Skill needs project configuration |
| Get git state | `` !`git status` `` in body | Skill operates on current repo state |
| List files | `` !`ls src/` `` in body | Skill needs directory listing |
| Env detection | `` !`node --version` `` in body | Skill adapts to runtime environment |
| Pass user input | `$ARGUMENTS` in body | Skill processes user-provided text |
| Route by position | `$1`, `$2` in body | Skill has sub-commands or structured args |
| Unique temp files | `${CLAUDE_SESSION_ID}` in body | Skill needs session-scoped scratch space |

---

## Best Practices

- **Keep commands fast** — backtick-bang commands block skill loading. Target < 1 second.
- **Limit output size** — large command outputs pollute context. Pipe through `head` or `tail` if needed.
- **Handle empty `$ARGUMENTS`** — always include fallback instructions when no arguments are provided:
  ```markdown
  If no arguments were provided ($ARGUMENTS is empty), ask the user what to look up.
  ```
- **Test both paths** — invoke the skill with and without arguments to verify behavior.
- **Quote arguments in bash** — if passing `$ARGUMENTS` to a bash command in instructions, remind Claude to quote it.

**Tip**: Include "ultrathink" anywhere in skill content to enable extended thinking.

---

## Common Mistakes

| Mistake | Problem | Fix |
|---------|---------|-----|
| `$ARGUMENTS` in frontmatter `description` | Not reliably substituted | Use `$ARGUMENTS` in body text only |
| Slow backtick-bang command | Blocks skill loading for seconds | Use fast commands or pipe through `head` |
| Assuming `$1` exists | Breaks when invoked without args | Check for empty and prompt user |
| Unquoted `$ARGUMENTS` in bash | Word splitting on spaces | Always wrap in double quotes |
| Huge command output | Wastes context window | Pipe through `head -20` or similar |
