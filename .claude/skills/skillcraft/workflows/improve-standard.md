# Standard Skill Audit

Full audit of a single skill against the 32-item quality checklist.

## Quick Pre-flight (Optional)

Run `scripts/quick-validate.sh` for fast structural checks before the full audit:

```bash
~/.claude/skills/skillcraft/scripts/quick-validate.sh <skill-directory>
~/.claude/skills/skillcraft/scripts/quick-validate.sh --all  # all skills
```

This catches structural issues (missing files, bad frontmatter, name mismatches) without reading the full checklist. Use the full audit for content and quality checks.

## Step 1: Select Target Skill

If `$ARGUMENTS` specifies a path, use it. Otherwise, pick a skill:

```bash
~/.claude/skills/advanced-ask/scripts/ask-file.sh \
    --glob "SKILL.md" ~/.claude/skills
```

Also check `.claude/skills` in the current project for project-local skills.

## Step 2: Read All Skill Files

Read every file in the skill directory — SKILL.md, all references/, all scripts/. Build a complete picture before auditing.

## Step 3: Apply Quality Checklist

Run all checks from `references/quality-checklist.md` across six categories:

| Category | IDs | Focus |
|----------|-----|-------|
| Structure | S1-S7 | Files, naming, executability |
| Metadata | M1-M8 | Frontmatter correctness |
| Content | C1-C7 | Writing quality, examples |
| Progressive Disclosure | P1-P5 | Body vs references balance |
| Advanced Features | A1-A5 | Dynamic context, hooks, agents |
| Quality | Q1-Q5 | Dedup, formatting, consistency |
| TDD Compliance | T1-T5 | Baseline testing, skill type testing |
| Eval Pipeline | E1-E5 | Behavioral testing & benchmarks |

## Step 4: Present Findings

Structure the report as:

```
## Audit: skill-name

**Score: 24/32 checks passed**

### Passed (24)
S1 ✓ SKILL.md exists
...

### Issues (8)
🔴 Critical (blocks functionality)
- M2: Description empty — skill won't auto-trigger

🟡 Warning (degrades quality)
- C3: Missing "When to Use" section
- P1: Body is 620 lines — move detail to references/

🔵 Suggestion (nice to have)
- Q4: Inconsistent heading levels
```

Also note **Strengths** — things the skill does well worth preserving.

## Step 5: Fix Issues Interactively

For each issue (critical first), present the fix and ask:

```bash
~/.claude/skills/advanced-ask/scripts/ask-choose.sh \
    --header "M2: Description empty" \
    "Fix now" "Skip" "Discuss"
```

- **Fix now** — Apply the fix immediately
- **Skip** — Move to next issue
- **Discuss** — Explain the issue in detail, then re-ask

## Step 6: Re-validate

After all fixes, re-run the checklist. Report final score.

## Step 7: Behavioral Eval

If the skill has no `evals/` directory, bootstrap baseline evals using the Eval Bootstrapping Protocol in `references/testing-guide.md`. All skills should have eval coverage.

Then re-run edit-relevant scenarios to verify improvements haven't introduced regressions. For a full benchmark comparison, read `workflows/create-phase4-refine.md`.

## Anti-Pattern Detection

Consult `references/anti-patterns.md` for common problems. When an anti-pattern is detected, cite it by name and show the before/after fix.

## Fix Resources

When suggesting fixes, consult these references:

| File | Use During Audit |
|------|-----------------|
| `references/official-spec.md` | Verify compliance against official Anthropic spec |
| `references/frontmatter-reference.md` | Fix frontmatter issues (M1-M8) |
| `references/skill-templates.md` | Suggest template-based restructuring |
| `references/writing-style.md` | Fix writing style issues (C1) |
| `references/naming-conventions.md` | Fix naming issues (M1, M3) |
