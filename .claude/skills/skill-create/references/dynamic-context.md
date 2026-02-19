# Dynamic Context in Skills

Guide to string substitutions, backtick-bang commands, and argument handling in SKILL.md files.

---

## String Substitutions

Placeholders in the SKILL.md **body text** that are replaced at skill load time.

| Placeholder | Replaced With | Notes |
|-------------|--------------|-------|
| `$ARGUMENTS` | Full text after `/skill-name ` | Empty string if no arguments given |
| `$1`, `$2`, ... `$N` | Positional args (space-separated) | Undefined if not enough args provided |
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

**Where substitutions work**: Body text only. They do **not** expand in frontmatter fields (except `description` has limited support for `$ARGUMENTS`).

---

## Backtick-Bang Dynamic Context

Run shell commands at skill load time and inject their output as context. Declared in the `context` frontmatter field.

### Syntax

```yaml
---
name: my-skill
context:
  - "`!git log --oneline -5`"
  - "`!cat package.json`"
  - "static-file.md"
---
```

### How It Works

1. At skill load, each `context` entry is evaluated
2. Entries matching `` `!command` `` run the command in the working directory
3. Plain strings are treated as file paths (relative to skill directory)
4. All outputs become part of the skill's loaded context

### Examples

```yaml
# Load project config
context: ["`!cat package.json`"]

# Get current git state
context: ["`!git status --short`"]

# Combine static and dynamic
context:
  - "references/api-spec.md"
  - "`!git branch --show-current`"
  - "`!ls src/`"
```

---

## Use Cases and Patterns

| Pattern | Syntax | When to Use |
|---------|--------|-------------|
| Load config file | `context: ["\`!cat .config\`"]` | Skill needs project configuration |
| Get git state | `context: ["\`!git status\`"]` | Skill operates on current repo state |
| List files | `context: ["\`!ls src/\`"]` | Skill needs directory listing |
| Env detection | `context: ["\`!node --version\`"]` | Skill adapts to runtime environment |
| Pass user input | `$ARGUMENTS` in body | Skill processes user-provided text |
| Route by position | `$1`, `$2` in body | Skill has sub-commands or structured args |
| Unique temp files | `${CLAUDE_SESSION_ID}` in body | Skill needs session-scoped scratch space |

---

## Best Practices

- **Keep commands fast** — backtick-bang commands block skill loading. Target < 1 second.
- **Use static context for stable content** — if a file rarely changes, list it as a plain path instead of `` `!cat file` ``.
- **Limit output size** — large command outputs pollute context. Pipe through `head` or `tail` if needed.
- **Handle empty `$ARGUMENTS`** — always include fallback instructions when no arguments are provided:
  ```markdown
  If no arguments were provided ($ARGUMENTS is empty), ask the user what to look up.
  ```
- **Test both paths** — invoke the skill with and without arguments to verify behavior.
- **Combine static and dynamic** — use static files for reference material, dynamic commands for current state.
- **Quote arguments in bash** — if passing `$ARGUMENTS` to a bash command in instructions, remind Claude to quote it.

---

## Common Mistakes

| Mistake | Problem | Fix |
|---------|---------|-----|
| `$ARGUMENTS` in frontmatter `description` | Not reliably substituted | Use `$ARGUMENTS` in body text only |
| Slow command in `context` | Blocks skill loading for seconds | Use fast commands or move to body instructions |
| Assuming `$1` exists | Breaks when invoked without args | Check for empty and prompt user |
| Unquoted `$ARGUMENTS` in bash | Word splitting on spaces | Always wrap in double quotes |
| Huge command output in context | Wastes context window | Pipe through `head -20` or similar |
| Using backtick-bang in body | Wrong location — only works in `context` frontmatter | Move to `context:` field in frontmatter |
