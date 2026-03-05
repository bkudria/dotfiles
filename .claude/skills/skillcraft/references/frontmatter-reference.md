# Skill Frontmatter Reference

Complete catalog of all valid YAML frontmatter fields for Claude Code SKILL.md files.

Frontmatter is a YAML block at the top of SKILL.md, delimited by `---`:

```yaml
---
name: my-skill
description: What this skill does
---
```

---

## Fields

### name

- **Type**: string
- **Constraint**: hyphen-case, lowercase, max 64 characters
- **Default**: Parent directory name
- **Description**: Unique identifier for the skill. Used in `/name` invocation and skill system references.
- **When to use**: Always include. Must match the directory name.
- **Example**: `name: docker-helper`
- **Trade-offs**: Short names are easier to type but less descriptive. Prefer clarity over brevity.

### description

- **Type**: string
- **Constraint**: 10-1024 characters
- **Default**: None (strongly recommended)
- **Description**: Tells Claude when to auto-load this skill. The most important field for discoverability. Include specific trigger phrases — verb+noun combinations that match what users say or do.
- **When to use**: Always include. Write as if answering "When should Claude load this skill?"
- **Example**: `description: Reference for Docker commands and Dockerfile best practices. Use when writing Dockerfiles, debugging container issues, optimizing Docker builds, or configuring multi-stage builds.`
- **Trade-offs**: Longer descriptions with more trigger phrases improve auto-triggering but consume more metadata space. Aim for 3-5 specific trigger phrases.

### disable-model-invocation

- **Type**: boolean
- **Default**: `false` (auto-triggering enabled)
- **Description**: When `true`, Claude will never auto-load this skill. The skill can only be invoked manually via `/skill-name`.
- **When to use**: Set `true` for heavy workflows (wizards, multi-step processes) that shouldn't fire automatically. Keep `false` for knowledge and reference skills.
- **Example**: `disable-model-invocation: true`
- **Trade-offs**: Disabling auto-invocation means the skill is only useful if the user knows it exists. Good for heavy workflows, bad for reference material.

### user-invocable

- **Type**: boolean
- **Default**: `true`
- **Description**: When `true`, the skill appears as a `/skill-name` command. When `false`, the skill is hidden from the `/` menu but Claude can still auto-trigger it. Note: `user-invocable` only controls menu visibility, not Skill tool access. Use `disable-model-invocation: true` to block programmatic invocation.
- **When to use**: Set `false` for internal/helper skills that other skills depend on but users shouldn't invoke directly.
- **Example**: `user-invocable: false`
- **Trade-offs**: Non-user-invocable skills are invisible to the user. Combined with `disable-model-invocation: true`, the skill becomes completely invisible (anti-pattern #10).

### allowed-tools

- **Type**: string (comma-separated tool names)
- **Default**: All tools available
- **Description**: Restricts which tools Claude can use while this skill is active. Tools: `Bash`, `Read`, `Write`, `Edit`, `Glob`, `Grep`, `WebFetch`, `WebSearch`, `Task`, `AskUserQuestion`, `Skill`, `NotebookEdit`.
- **When to use**: Only when the skill should be restricted — e.g., a read-only reference skill that shouldn't edit files.
- **Example**: `allowed-tools: Bash, Read, Glob, Grep`
- **Trade-offs**: Too restrictive breaks functionality (anti-pattern #9). Too permissive provides no safety boundary. When in doubt, omit this field.

### model

- **Type**: string (valid model ID)
- **Default**: Inherited from session
- **Description**: Override the model used when this skill is active. Valid values: `sonnet`, `opus`, `haiku`, or full model IDs like `claude-sonnet-4-5-20250929`.
- **When to use**: Rarely. Use `haiku` for simple, fast tasks. Use `opus` for complex reasoning. Document the rationale.
- **Example**: `model: haiku`
- **Trade-offs**: Model overrides affect cost and capability. `haiku` is cheaper but less capable. Always justify in the skill body.

### context

- **Type**: string
- **Default**: Shared (no value set)
- **Description**: Set to `fork` to run the skill in a forked context. The skill sees the current conversation but its actions don't pollute the main context.
- **When to use**: For reference-heavy skills that load large amounts of content, or for skills that make many exploratory reads you don't want cluttering context.
- **Example**: `context: fork`
- **Trade-offs**: Forked context prevents the skill's reads/searches from consuming main context, but the skill can't carry state back to the main conversation except through its final output.

### agent

- **Type**: string
- **Default**: None (runs in main agent)
- **Description**: Run the skill as a specific agent type. Valid values: `Explore`, `Plan`, `general-purpose`, `Bash`, or a custom agent name from `.claude/agents/`.
- **When to use**: When the skill's task matches an agent specialty. `Explore` for codebase research. `Plan` for architecture. `Bash` for shell operations.
- **Example**: `agent: Explore`
- **Trade-offs**: Agent types have different tool access. `Explore` and `Plan` cannot edit files. `Bash` can only run commands. Choose based on what tools the skill needs.

### argument-hint

- **Type**: string
- **Default**: None
- **Description**: Hint text shown after the `/skill-name` command to indicate expected arguments. Displayed in the skill list and autocomplete.
- **When to use**: When the skill accepts arguments via `$ARGUMENTS`. Describe what the argument should be.
- **Example**: `argument-hint: "[path to file]"`
- **Trade-offs**: None — always include if the skill uses `$ARGUMENTS`.

### hooks

- **Type**: object
- **Default**: None
- **Description**: Define pre/post hooks that run shell commands around tool calls. Follows the Claude Code hooks schema.
- **When to use**: When the skill needs to run setup/teardown around specific tool calls, or enforce constraints.
- **Example**:
  ```yaml
  hooks:
    PreToolUse:
      - matcher: Bash
        hooks:
          - type: command
            command: echo "About to run bash"
  ```
- **Trade-offs**: Hooks add complexity. Use sparingly and test thoroughly.

---

## String Substitutions

Use these placeholders in SKILL.md body text — they're replaced at load time.

| Placeholder | Replaced With | Example Use |
|-------------|--------------|-------------|
| `$ARGUMENTS` or `{{ARGUMENTS}}` | Text after `/skill-name ` | Pre-fill inputs from user's command |
| `$1`, `$2`, etc. | Positional arguments (space-separated) | Parse structured arguments |
| `${CLAUDE_SESSION_ID}` | Current session UUID | Generate unique temp files |
| `${CLAUDE_SKILL_DIR}` | Directory containing the skill's SKILL.md | Reference bundled scripts/files regardless of CWD |

### Usage Pattern

```markdown
## Arguments: $ARGUMENTS

If arguments were provided, parse them as: ...
```

---

## Dynamic Context

Embed command output in the SKILL.md **body text** using backtick-bang syntax (`` !`command` ``). Commands run at load time as preprocessing — their output replaces the placeholder before Claude sees the content.

```markdown
Current git branch: !`git branch --show-current`
Recent commits: !`git log --oneline -5`
```

**Guidelines**:
- Commands must be fast (< 1 second)
- Limit output size — pipe through `head` or `tail` if needed
- Use for: current state (branch, directory), available tools, environment detection

---

## Progressive Disclosure Strategy

Structure skill files for optimal context usage:

| File | Content | Target Length |
|------|---------|--------------|
| SKILL.md | Workflow, quick reference, links | 100-300 lines |
| references/*.md | Detailed specifications, catalogs | 100-400 lines each |
| scripts/*.sh | Executable automation | As needed |

**Principle**: SKILL.md is always loaded. References are loaded on demand. Keep SKILL.md lean so the common case is fast; put depth in references/ for when it's needed.

---

## Common Field Combinations

| Skill Type | Typical Frontmatter |
|------------|-------------------|
| Knowledge/Reference | `name`, `description` (rich triggers) |
| Heavy Workflow | `name`, `description`, `disable-model-invocation: true`, `argument-hint` |
| Read-only Reference | `name`, `description`, `allowed-tools: Read, Glob, Grep` |
| Forked Research | `name`, `description`, `context: fork`, `agent: Explore` |
| Internal Helper | `name`, `description`, `user-invocable: false` |
