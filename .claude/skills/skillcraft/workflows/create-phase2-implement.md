# Phase 2: Implement

Scaffold the skill, author content, and write eval scenarios — all in one phase.

> **References for this phase:** `references/skill-templates.md` (template variants), `references/writing-style.md` (voice and formatting), `references/frontmatter-reference.md` (field details). Consult `references/dependencies.md` only if the skill has external dependencies. Consult `references/interactive-tui.md` only if the skill uses interactive TUI prompts. Do not read other references unless a specific question arises.

## Step 1: Write a Plan

**Before drafting**, run `scuttlerun --help`, `craboodle --help`, and `pincenez --help` to verify the current schemas for eval config files (scenario.yaml, checks.yaml, base.yaml, craboodle.yaml). Do not rely on skill references alone — the tools are the source of truth.

Enter plan mode. Present a plan covering:

1. **Inferred design decisions** — Skill type, resource directories, invocation strategy (auto-trigger vs manual), frontmatter fields. Justify each choice based on Phase 1 requirements.
2. **Directory structure** — What files will be created (SKILL.md, references, scripts, evals).
3. **Skill content outline** — Section-by-section overview of SKILL.md and any reference files.
4. **Eval scenarios** — 2-3 scenarios derived from Phase 1 success criteria. Each scenario: prompt, checks (from success criteria), context. Include the eval base.yaml.

Exit plan mode for user approval. This is the only user interaction in this phase.

## Step 2: Scaffold

After plan approval, run the scaffold script:

```bash
scripts/scaffold.sh \
    "{name}" --path "{location}" --type "{type}" \
    [--references] [--scripts] [--assets]
```

Type is inferred from Phase 1:
- Pure reference material → `knowledge`
- Multi-step process or wizard → `workflow`
- CLI tool documentation → `tool`
- Multiple concerns → `hybrid`

## Step 3: Author Skill Content

Write SKILL.md and reference files section by section:

1. Frontmatter (name, description, triggers, invocation settings)
2. Main body (When to Use, instructions, quick reference)
3. Reference files (detailed docs that don't fit in the body)
4. Scripts (if applicable)

Follow `references/writing-style.md`: imperative voice, realistic examples, tables for structured data.

## Step 4: Author Eval Scenarios

Co-author evals alongside the skill:

1. Optionally create `evals/base.yaml` if scenarios share common config (model, tools, project settings) — run `craboodle --help` for the schema reference
2. Create scenario directories with `scenario.yaml` files
3. Checks come from Phase 1 success criteria — translate "what good looks like" into binary, verifiable checks
4. For check design rules and anti-patterns, load the `claude-code-evals` skill

The evals define what the skill must achieve; the skill is the implementation.

Evals written in this step are not yet validated. Phase 4 runs and iterates on them. Do not skip Phase 4.

**Next**: Proceed to Phase 3 (read `workflows/create-phase3-validate.md`)
