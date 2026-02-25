> Sources: original (not derived from upstream)
> Created: 2026-02-06

# Skill Dependencies Guide

How to declare, check, and manage dependencies in Claude Code skills.

## Types of Dependencies

| Type | Example | How to Declare |
|------|---------|---------------|
| External CLI tool | `gum`, `fzf`, `jq` | `## Dependencies` section in SKILL.md |
| Other skill | `advanced-ask`, `interactive-tmux` | `## Dependencies` section in SKILL.md |
| System command | `git`, `python3`, `node` | Usually implicit; document if non-obvious |
| File/config | `.env`, `package.json` | `context` field or note in SKILL.md |

## Declaring Dependencies

Add a `## Dependencies` section near the bottom of SKILL.md.

**External tools** — include install instructions:
```
- **gum** — Interactive TUI components (`brew install gum`)
- **jq** — JSON processing (`brew install jq`)
```

**Skill dependencies** — reference by name:
```
- **advanced-ask** skill — For multi-option selection prompts
- **interactive-tmux** skill — For running interactive TUIs
```

**System commands** — only document non-obvious ones:
```
- **ffmpeg** — Video processing (`brew install ffmpeg`)
```

## Checking Dependencies at Runtime

**In shell scripts** — check at the top before proceeding:
```bash
for cmd in gum jq; do
  command -v "$cmd" >/dev/null 2>&1 || { echo "Missing: $cmd"; exit 1; }
done
```

**Skill dependencies** — reference by full path. If the path does not exist, the skill will not load:
```
~/.claude/skills/{skill-name}/
```

**Graceful degradation** — if a dependency is optional, skip that feature with a message rather than failing entirely.

## Inter-Skill Dependencies

Reference other skills by their full path: `~/.claude/skills/{name}/`.

Common dependencies across skills:

| Skill | Provides |
|-------|----------|
| `advanced-ask` | Interactive prompts beyond AskUserQuestion limits |
| `interactive-tmux` | TUI infrastructure for running interactive tools |
| `skillcraft` | Validation and auditing in creation workflows |
| `gum` | Reference for TUI component usage |

**Avoid circular dependencies.** If skill A depends on skill B, skill B must not depend on skill A. Keep the dependency graph a DAG.

## Best Practices

1. **Minimize dependencies** — each one is a potential failure point
2. **Prefer built-in tools** — use `AskUserQuestion` over `advanced-ask` when 4 or fewer options suffice
3. **Document all non-obvious dependencies** — anything beyond `git`, `bash`, standard Unix tools
4. **Test with dependencies missing** — verify the skill produces clear error messages
5. **Pin to behavior, not versions** — tools update; depend on capabilities, not release numbers
6. **Keep install instructions current** — `brew install` for macOS, note alternatives if cross-platform

## Dependency Section Template

```markdown
## Dependencies

- **gum** — Interactive TUI components (`brew install gum`)
- **jq** — JSON processing (`brew install jq`)
- **advanced-ask** skill — For multi-option selection prompts
- **interactive-tmux** skill — For running interactive TUIs
```
