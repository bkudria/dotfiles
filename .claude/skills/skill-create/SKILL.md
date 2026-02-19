---
name: skill-create
description: Interactive wizard to create new Claude Code skills from scratch. Guides through discovery, design, scaffolding, authoring, and validation phases. Use when the user asks to create a new skill, make a skill, build a skill, or scaffold a skill.
argument-hint: "[skill name or topic]"
---

# Skill Create

Interview-driven wizard to create new Claude Code skills. Walks through 5 phases: discovery, design, scaffold, author, validate.

## When to Use

- Creating a brand new Claude Code skill
- Scaffolding a skill directory with proper structure
- Learning what frontmatter fields and features are available

## Quick Reference

| Phase | Purpose | Key Tools | Workflow |
|-------|---------|-----------|----------|
| 1. Discovery | Interview: name, purpose, use cases | `ask-form.sh` | `workflows/phase1-discovery.md` |
| 2. Design | Select features and skill type | `ask-multi.sh`, `ask-choose.sh` | `workflows/phase2-design.md` |
| 3. Scaffold | Create directory and files | `scripts/scaffold.sh` | `workflows/phase3-scaffold.md` |
| 4. Author | Write content collaboratively | Edit tool | `workflows/phase4-author.md` |
| 5. Validate | Run quality checklist | `quick-validate.sh` | `workflows/phase5-validate.md` |

## How to Use

Read the workflow file for the current phase. Start at Phase 1 and proceed sequentially.

1. Read `workflows/phase1-discovery.md` — Gather name, purpose, use cases, triggers
2. Read `workflows/phase2-design.md` — Select skill type, resources, frontmatter features
3. Read `workflows/phase3-scaffold.md` — Run scaffold script to create directory and files
4. Read `workflows/phase4-author.md` — Write content section by section
5. Read `workflows/phase5-validate.md` — Validate against quality checklist

If `$ARGUMENTS` is provided, pre-fill the name/topic and start Phase 1 with that context.

### Alternative: Domain Expertise Skills

If the skill covers a broad domain (framework, platform, API, language ecosystem) and requires exhaustive research and multiple workflows, use `workflows/domain-expertise.md` instead of the standard 5-phase path. This produces router-pattern skills with comprehensive references.

## Dependencies

- **advanced-ask** skill — For interactive interview forms
- **interactive-tmux** skill — For running interactive TUIs
- **skill-improve** skill — For Phase 5 validation (reads its quality checklist)
- **gum** — Interactive TUI components (`brew install gum`)
- **jq** — JSON processing (`brew install jq`)
- **fzf** — File picking (`brew install fzf`)

## Workflows

| File | Purpose |
|------|---------|
| `workflows/phase1-discovery.md` | Concrete examples, interview, validation |
| `workflows/phase2-design.md` | Skill type, resources, frontmatter features |
| `workflows/phase3-scaffold.md` | Run scaffold script, post-scaffold updates |
| `workflows/phase4-author.md` | Section-by-section content authoring |
| `workflows/phase5-validate.md` | Structural check + full audit |
| `workflows/domain-expertise.md` | Domain expertise skill creation (research-intensive, router-pattern) |

## Reference Files

| File | Purpose |
|------|---------|
| `references/official-spec.md` | Official Anthropic skill specification (curated from code.claude.com/docs/en/skills) |
| `references/frontmatter-reference.md` | Complete catalog of all SKILL.md frontmatter fields |
| `references/skill-templates.md` | Starter templates by skill type |
| `references/writing-style.md` | Voice, tone, and formatting rules for skill content |
| `references/interactive-tui.md` | Best practices for using interactive TUI tools in skills |
| `references/resource-usage.md` | Guide for using scripts/, references/, and assets/ directories |
| `references/dynamic-context.md` | Backtick-bang syntax, $ARGUMENTS, and string substitutions |
| `references/naming-conventions.md` | Skill naming, description construction, trigger phrase design |
| `references/dependencies.md` | Handling inter-skill and external tool dependencies |
