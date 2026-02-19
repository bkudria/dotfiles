# Phase 2: Design

Select skill features and type using interactive prompts.

## Skill Type

```bash
~/.claude/skills/advanced-ask/scripts/ask-choose.sh \
    --header "What kind of skill is this?" --descriptions \
    "Knowledge|Pure reference material, auto-triggers when topic comes up" \
    "Workflow|Multi-step procedure with phases, usually manual invoke" \
    "Tool Integration|Wraps an external CLI tool with command reference" \
    "Hybrid|Combines knowledge, workflow, and/or scripts"
```

## Resource Types

```bash
~/.claude/skills/advanced-ask/scripts/ask-multi.sh \
    --header "What supporting resources does this skill need?" --descriptions \
    "References|Detailed reference docs in references/ directory" \
    "Scripts|Shell scripts in scripts/ directory" \
    "Assets|Static files (templates, configs) in assets/ directory"
```

See `references/resource-usage.md` for guidance on when to use each directory.

## Frontmatter Features

```bash
~/.claude/skills/advanced-ask/scripts/ask-multi.sh \
    --header "Select frontmatter features to configure" --descriptions \
    "allowed-tools|Restrict which tools the skill can use" \
    "model|Override the model (e.g., haiku for fast tasks)" \
    "context:fork|Fork context to avoid polluting main conversation" \
    "hooks|Add pre/post hooks for tool calls" \
    "agent|Run as a specific agent type (Explore, Plan, Bash, etc.)" \
    "disable-model-invocation|Manual-only, no auto-trigger"
```

See `references/frontmatter-reference.md` for complete field details and `references/dynamic-context.md` for context/substitution features.

## Invocation Decision

Based on skill type, recommend invocation settings:

| Skill Type | Recommended Setting | Rationale |
|------------|-------------------|-----------|
| Knowledge | Auto-trigger (default) | Should load when topic is relevant |
| Workflow | `disable-model-invocation: true` | Heavy wizard shouldn't auto-fire |
| Tool Integration | Auto-trigger (default) | Should load when tool is mentioned |
| Hybrid | Depends on weight | Light → auto, heavy → manual |

**Next**: Proceed to Phase 3 (read `workflows/phase3-scaffold.md`)
