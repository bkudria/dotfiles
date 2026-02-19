---
name: skill-improve
description: IMPORTANT - this skill MUST be loaded ANY time a SKILL.md file is being edited, created, or reviewed, even as part of other work. Audit, review, and improve Claude Code skills. Also use when reviewing skill quality, fixing skill frontmatter issues, optimizing skill descriptions, checking for anti-patterns in skills, validating skill structure, or maintaining skill collections. Covers frontmatter validation, content quality, progressive disclosure, and best practices.
argument-hint: "[path to skill]"
---

# Skill Improve

Audit, review, and improve existing Claude Code skills against a comprehensive quality checklist.

## When to Use

- **ANY time a SKILL.md file is being edited** — even as a side effect of other work. If you are about to edit a SKILL.md, load this skill first via the Skill tool so the lightweight checks are applied automatically.
- Auditing a skill for quality issues
- Fixing broken frontmatter or references
- Optimizing a skill's description for better auto-triggering
- Maintaining a collection of skills

## Behavior Modes

| Mode | Trigger | Scope |
|------|---------|-------|
| Lightweight | Auto-loaded during SKILL.md editing | Quick frontmatter + anti-pattern check only |
| Standard | `/skill-improve` or `/skill-improve path/to/skill` | Full audit of one skill |
| Bulk | `/skill-improve --all` | Full audit of every installed skill |

## Lightweight Mode (Auto-trigger)

When loaded during SKILL.md editing, apply only these quick checks:

1. **Valid frontmatter** — YAML between `---` delimiters, `name` field present
2. **Name matches directory** — `name` field matches parent directory name
3. **Description present** — Non-empty, 10-1024 characters
4. **Obvious anti-patterns** — Scan for second-person voice ("You should..."), wall-of-text body (>500 lines with no references/), missing "When to Use" section

Report issues inline as suggestions. Do NOT run the full checklist or restructure the skill.

## Standard Audit Workflow

Run on explicit `/skill-improve` invocation.

### Quick Pre-flight (Optional)

Run `scripts/quick-validate.sh` for fast structural checks before the full audit:

```bash
~/.claude/skills/skill-improve/scripts/quick-validate.sh <skill-directory>
~/.claude/skills/skill-improve/scripts/quick-validate.sh --all  # all skills
```

This catches structural issues (missing files, bad frontmatter, name mismatches) without reading the full checklist. Use the full audit for content and quality checks.

### Step 1: Select Target Skill

If `$ARGUMENTS` specifies a path, use it. Otherwise, pick a skill:

```bash
~/.claude/skills/advanced-ask/scripts/ask-file.sh \
    --glob "SKILL.md" ~/.claude/skills
```

Also check `.claude/skills` in the current project for project-local skills.

### Step 2: Read All Skill Files

Read every file in the skill directory — SKILL.md, all references/, all scripts/. Build a complete picture before auditing.

### Step 3: Apply Quality Checklist

Run all checks from `references/quality-checklist.md` across six categories:

| Category | IDs | Focus |
|----------|-----|-------|
| Structure | S1-S7 | Files, naming, executability |
| Metadata | M1-M8 | Frontmatter correctness |
| Content | C1-C7 | Writing quality, examples |
| Progressive Disclosure | P1-P5 | Body vs references balance |
| Advanced Features | A1-A5 | Dynamic context, hooks, agents |
| Quality | Q1-Q5 | Dedup, formatting, consistency |

### Step 4: Present Findings

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

### Step 5: Fix Issues Interactively

For each issue (critical first), present the fix and ask:

```bash
~/.claude/skills/advanced-ask/scripts/ask-choose.sh \
    --header "M2: Description empty" \
    "Fix now" "Skip" "Discuss"
```

- **Fix now** — Apply the fix immediately
- **Skip** — Move to next issue
- **Discuss** — Explain the issue in detail, then re-ask

### Step 6: Re-validate

After all fixes, re-run the checklist. Report final score.

## Bulk Mode

When invoked with `--all`:

1. Find all skills in `~/.claude/skills/` and `.claude/skills/`
2. Run the standard audit on each
3. Present a summary table:

```
| Skill | Score | Critical | Warnings |
|-------|-------|----------|----------|
| gum | 30/32 | 0 | 2 |
| advanced-ask | 28/32 | 0 | 4 |
```

4. Ask which skills to fix interactively

## Anti-Pattern Detection

Consult `references/anti-patterns.md` for common problems. When an anti-pattern is detected, cite it by name and show the before/after fix.

## Related: skill-create References

When suggesting fixes, consult these resources from the `skill-create` skill:

| File | Use During Audit |
|------|-----------------|
| `~/.claude/skills/skill-create/references/official-spec.md` | Verify compliance against official Anthropic spec |
| `~/.claude/skills/skill-create/references/frontmatter-reference.md` | Fix frontmatter issues (M1-M8) |
| `~/.claude/skills/skill-create/references/skill-templates.md` | Suggest template-based restructuring |
| `~/.claude/skills/skill-create/references/writing-style.md` | Fix writing style issues (C1) |
| `~/.claude/skills/skill-create/references/naming-conventions.md` | Fix naming issues (M1, M3) |

## Reference Files

| File | Purpose |
|------|---------|
| `references/quality-checklist.md` | Complete 32-item validation checklist |
| `references/anti-patterns.md` | 10 common anti-patterns with fixes |
| `references/common-fixes.md` | Concrete fix examples for common checklist failures |
| `references/testing-guide.md` | How to test skills after creation or editing |
| `references/router-pattern.md` | When and how to upgrade skills to the router pattern |
| `scripts/quick-validate.sh` | Automated structural validation (fast pre-flight) |
