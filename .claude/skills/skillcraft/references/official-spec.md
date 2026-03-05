# Official Anthropic Skill Specification

## Overview

Skills extend what Claude can do. Create a `SKILL.md` file with instructions, and Claude adds it to its toolkit. Claude uses skills when relevant, or you can invoke one directly with `/skill-name`.

Custom slash commands have been merged into skills. A file at `.claude/commands/review.md` and a skill at `.claude/skills/review/SKILL.md` both create `/review` and work the same way. Skills add optional features: a directory for supporting files, frontmatter to control whether you or Claude invokes them, and the ability for Claude to load them automatically when relevant.

Claude Code skills follow the Agent Skills open standard, which works across multiple AI tools. Claude Code extends the standard with additional features like invocation control, subagent execution, and dynamic context injection.

## Bundled Skills

Bundled skills ship with Claude Code and are available in every session. Unlike built-in commands, bundled skills are prompt-based: they give Claude a detailed playbook and let it orchestrate the work using its tools.

- **`/simplify`**: Reviews recently changed files for code reuse, quality, and efficiency, then fixes them
- **`/batch <instruction>`**: Orchestrates large-scale changes across a codebase in parallel using git worktrees
- **`/debug [description]`**: Troubleshoots your current session by reading the debug log
- **`/claude-api`**: Loads Claude API reference material for your project's language and Agent SDK reference

## Skill Directory Structure

Each skill is a directory with `SKILL.md` as the entrypoint:

```
my-skill/
├── SKILL.md           # Main instructions (required)
├── template.md        # Template for Claude to fill in
├── examples/
│   └── sample.md      # Example output showing expected format
└── scripts/
    └── validate.sh    # Script Claude can execute
```

Reference supporting files from `SKILL.md` so Claude knows what they contain and when to load them.

## Where Skills Live

| Location   | Path                                                     | Applies to                     |
| :--------- | :------------------------------------------------------- | :----------------------------- |
| Enterprise | See managed settings                                     | All users in your organization |
| Personal   | `~/.claude/skills/<skill-name>/SKILL.md`                 | All your projects              |
| Project    | `.claude/skills/<skill-name>/SKILL.md`                   | This project only              |
| Plugin     | `<plugin>/skills/<skill-name>/SKILL.md`                  | Where plugin is enabled        |

When skills share the same name across levels, higher-priority locations win: enterprise > personal > project. Plugin skills use a `plugin-name:skill-name` namespace, so they cannot conflict.

Automatic discovery from nested directories: when working with files in subdirectories, Claude Code also looks for skills in nested `.claude/skills/` directories (e.g., `packages/frontend/.claude/skills/`). This supports monorepo setups.

Skills from additional directories: skills defined in `.claude/skills/` within directories added via `--add-dir` are loaded automatically and support live change detection — you can edit them during a session without restarting.

## Frontmatter Reference

YAML frontmatter fields between `---` markers at the top of `SKILL.md`:

| Field                      | Required    | Description                                                                                                                                           |
| :------------------------- | :---------- | :---------------------------------------------------------------------------------------------------------------------------------------------------- |
| `name`                     | No          | Display name. If omitted, uses directory name. Lowercase letters, numbers, hyphens only (max 64 chars).                    |
| `description`              | Recommended | What the skill does and when to use it. Claude uses this for auto-triggering. If omitted, uses first paragraph of content. |
| `argument-hint`            | No          | Hint shown during autocomplete. Example: `[issue-number]` or `[filename] [format]`.                                    |
| `disable-model-invocation` | No          | Set `true` to prevent Claude auto-loading. Manual `/name` only. Default: `false`. |
| `user-invocable`           | No          | Set `false` to hide from `/` menu. Background knowledge only. Default: `true`.                              |
| `allowed-tools`            | No          | Tools Claude can use without permission when skill is active.                                                             |
| `model`                    | No          | Model to use when skill is active.                                                                                               |
| `context`                  | No          | Set to `fork` to run in a forked subagent context.                                                                               |
| `agent`                    | No          | Which subagent type when `context: fork`. Options: `Explore`, `Plan`, `general-purpose`, or custom from `.claude/agents/`.                                                                               |
| `hooks`                    | No          | Hooks scoped to this skill's lifecycle.                                                     |

## String Substitutions

| Variable               | Description                                                                                                                                  |
| :--------------------- | :------------------------------------------------------------------------------------------------------------------------------------------- |
| `$ARGUMENTS`           | All arguments passed when invoking. If not present in content, arguments appended as `ARGUMENTS: <value>`. |
| `$ARGUMENTS[N]`        | Specific argument by 0-based index (e.g., `$ARGUMENTS[0]`).                                                 |
| `$N`                   | Shorthand for `$ARGUMENTS[N]` (e.g., `$0`, `$1`).                                                   |
| `${CLAUDE_SESSION_ID}` | Current session ID.                      |
| `${CLAUDE_SKILL_DIR}`  | Directory containing the skill's SKILL.md. Use in `!`command`` to reference bundled scripts/files regardless of CWD. |

## Types of Skill Content

**Reference content** adds knowledge Claude applies to current work (conventions, patterns, style guides). Runs inline with conversation context.

**Task content** gives step-by-step instructions for specific actions (deployments, commits, generation). Often paired with `disable-model-invocation: true`.

## Supporting Files

Keep `SKILL.md` under 500 lines. Move detailed reference material to separate files:

```
my-skill/
├── SKILL.md (required - overview and navigation)
├── reference.md (detailed API docs - loaded when needed)
├── examples.md (usage examples - loaded when needed)
└── scripts/
    └── helper.py (utility script - executed, not loaded)
```

Reference from SKILL.md:

```markdown
## Additional resources
- For complete API details, see [reference.md](reference.md)
- For usage examples, see [examples.md](examples.md)
```

## Invocation Control

| Frontmatter                      | User can invoke | Claude can invoke | Context behavior                                     |
| :------------------------------- | :------------- | :---------------- | :----------------------------------------------------------- |
| (default)                        | Yes            | Yes               | Description always in context, full skill loads when invoked |
| `disable-model-invocation: true` | Yes            | No                | Description not in context, full skill loads when user invokes |
| `user-invocable: false`          | No             | Yes               | Description always in context, full skill loads when invoked |

## Arguments

Use `$ARGUMENTS` placeholder. If skill doesn't include `$ARGUMENTS`, Claude Code appends `ARGUMENTS: <your input>` to the end.

Positional access: `$ARGUMENTS[N]` or shorthand `$N` (0-based).

Example: `/migrate-component SearchBar React Vue` → `$0`=SearchBar, `$1`=React, `$2`=Vue.

## Dynamic Context Injection

The `` !`command` `` syntax runs shell commands before skill content is sent to Claude. Output replaces the placeholder.

```yaml
## Pull request context
- PR diff: !`gh pr diff`
- PR comments: !`gh pr view --comments`
```

Commands execute immediately (preprocessing). Claude only sees the final rendered result.

Tip: Include "ultrathink" anywhere in skill content to enable extended thinking.

## Subagent Execution

Add `context: fork` to run in isolation. The skill content becomes the subagent's prompt. No access to conversation history.

Warning: `context: fork` only makes sense for skills with explicit task instructions. Guidelines-only skills return without meaningful output.

The `agent` field specifies the execution environment. Options: built-in (`Explore`, `Plan`, `general-purpose`) or custom from `.claude/agents/`. Default: `general-purpose`.

## Restricting Skill Access

Three ways to control which skills Claude can invoke:

1. **Disable all skills**: Deny the `Skill` tool in `/permissions`
2. **Allow/deny specific skills**: Use permission rules — `Skill(commit)` for exact match, `Skill(review-pr *)` for prefix match with any arguments
3. **Hide individual skills**: Add `disable-model-invocation: true` to frontmatter

Note: `user-invocable` only controls menu visibility, not Skill tool access. Use `disable-model-invocation: true` to block programmatic invocation.

## Sharing & Distribution

- **Project skills**: Commit `.claude/skills/` to version control
- **Plugins**: Create a `skills/` directory in your plugin
- **Managed**: Deploy organization-wide through managed settings

## Visual Output Pattern

Skills can bundle and run scripts in any language. A powerful pattern is generating interactive HTML files — dependency graphs, test coverage reports, API documentation, schema visualizations. The bundled script does the heavy lifting while Claude handles orchestration.

## Skill Description Budget

Skill descriptions are loaded into context so Claude knows what's available. The budget scales dynamically at 2% of the context window, with a fallback of 16,000 characters. Many skills may exceed this budget. Check with `/context` for warnings about excluded skills. Override with `SLASH_COMMAND_TOOL_CHAR_BUDGET` environment variable.
