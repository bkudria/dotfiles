# Skill Router Pattern Guide

When and how to upgrade a skill from a monolithic SKILL.md to the router pattern.

## When to Upgrade

Upgrade when a skill hits any of these thresholds:

| Signal | Threshold | Why |
|--------|-----------|-----|
| SKILL.md body length | >250 lines | Context budget; Claude loads the whole file |
| Distinct phases/modes | 3+ | Each phase is independently addressable |
| Reference file count | 5+ | Routing logic becomes valuable |
| Phase independence | Phases don't share state | Each can be loaded on demand |

Do NOT upgrade when:
- Skill is under 150 lines
- Content is tightly coupled (every section depends on every other)
- Skill has only 1-2 modes/phases

## Router Pattern Structure

```
skill-name/
├── SKILL.md              # Router (~60-80 lines)
├── workflows/            # Phase/mode files
│   ├── phase1-name.md
│   ├── phase2-name.md
│   └── phase3-name.md
├── references/           # Supporting detail
│   └── ...
└── scripts/              # Executable code
    └── ...
```

## SKILL.md Router Template

The router SKILL.md contains only:

1. **Frontmatter** — Unchanged
2. **Title + summary** — One sentence
3. **When to Use** — Bullet list (unchanged)
4. **Quick Reference table** — With a Workflow column linking to files
5. **Routing instructions** — "Read the workflow file for the current phase"
6. **Dependencies** — Unchanged
7. **Workflows table** — Maps workflow files to purposes
8. **Reference Files table** — Maps reference files to purposes

Total: ~60-80 lines.

## How to Upgrade

1. **Identify phases** — Each `## Phase` or `## Mode` section becomes a workflow file
2. **Create `workflows/` directory**
3. **Extract each phase** — Move content as-is into `workflows/phaseN-name.md`
4. **Add navigation links** — End each workflow with "Next: read `workflows/phaseN+1-name.md`"
5. **Cross-reference** — Each workflow links to relevant references
6. **Rewrite SKILL.md** — Replace phase content with routing table and instructions
7. **Update reference table** — Add workflow files
8. **Validate** — Run `scripts/quick-validate.sh` to check all references resolve

## Example

Before: `skill-create/SKILL.md` at 255 lines with 5 phases inline.

After:
- `SKILL.md` — 68-line router with tables and routing instructions
- `workflows/phase1-discovery.md` — 50 lines
- `workflows/phase2-design.md` — 50 lines
- `workflows/phase3-scaffold.md` — 30 lines
- `workflows/phase4-author.md` — 45 lines
- `workflows/phase5-validate.md` — 35 lines

Each phase loads independently, and SKILL.md stays under 80 lines.

## Naming Conventions

| Pattern | Example | When |
|---------|---------|------|
| `phaseN-name.md` | `phase1-discovery.md` | Sequential phases |
| `mode-name.md` | `mode-lightweight.md` | Parallel modes |
| `action-name.md` | `create-skill.md` | Action-based routing |
