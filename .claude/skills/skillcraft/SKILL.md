---
name: skillcraft
description: "IMPORTANT - this skill MUST be loaded ANY time any file within a skill directory (~/.claude/skills/*/) is being edited, created, or reviewed, even as part of other work — not just SKILL.md but also scripts, references, workflows, and other skill files. Create new skills with an interactive wizard, audit and improve existing skills, integrate content from external sources, or update skillcraft from upstream sources. Use when creating a skill, building a skill, scaffolding a skill, reviewing skill quality, fixing frontmatter, optimizing descriptions, checking anti-patterns, validating skill structure, maintaining skill collections, editing skill scripts or references, adding scripts to a skill, updating skillcraft, syncing skillcraft from sources, checking for upstream changes to skills, folding in a source, integrating a source, or merging content from an external source."
argument-hint: "[skill name, path, or 'update']"
---

# Skill Craft

Create, audit, improve, and update Claude Code skills.

## When to Use

- **ANY time any file within a skill directory** (`~/.claude/skills/*/`) **is being edited** — not just SKILL.md, but also scripts, references, workflows, and other skill files. Load this skill first so lightweight checks apply automatically.
- Creating a brand new Claude Code skill
- Auditing a skill for quality issues
- Fixing broken frontmatter or references
- Optimizing a skill's description for better auto-triggering
- Maintaining a collection of skills
- Integrating content from external sources into existing skills
- Updating skills from upstream sources

## Modes

| Mode | Trigger | Workflow |
|------|---------|----------|
| Lightweight | Auto-loaded during skill file editing | (inline below) |
| Create | `/skillcraft` or `/skillcraft <name>`, or "create a skill" | See Create Quick Reference |
| Improve | `/skillcraft --improve [path]`, or "audit/improve a skill" | `workflows/improve-standard.md` |
| Bulk Audit | `/skillcraft --all`, or "audit all skills" | `workflows/improve-bulk.md` |
| Update | `/skillcraft update`, or "update skillcraft", "sync from sources", "check upstream" | `workflows/update-from-sources.md` |
| Provenance | "add provenance", "track sources", "add upstream sources" | `workflows/add-provenance.md` |
| Integrate | "fold in source", "integrate source", "merge content from" | `workflows/integrate-source.md` |

**Mode selection**: If the request mentions "fold in", "integrate source", or "merge content from" an external source into an existing skill, use **Integrate** mode. If the request mentions "add provenance", "track sources", or "add upstream sources" for a skill, use **Provenance** mode. If it mentions "update skillcraft", "sync from sources", "upstream changes", or "check sources", use **Update** mode. If it mentions "audit", "review", or "fix" an existing skill without specifying a particular change, use **Improve**. If the request specifies a concrete change to make (e.g., "improve skill X to do Y", "add Z to skill"), use **Lightweight** mode — the edit will be covered by its Behavioral Edit Testing protocol. Otherwise default to **Create**.

### Testing Discipline

**Iron Law**: No skill ships untested — new or edited. Behavioral edits require pre/post verification. See `references/testing-guide.md` for the full TDD framework; see Lightweight Mode below for the edit protocol.

## Lightweight Mode (Auto-trigger)

When loaded during editing of any file within a skill directory, apply only these quick checks:

1. **Valid frontmatter** — YAML between `---` delimiters, `name` field present
2. **Name matches directory** — `name` field matches parent directory name
3. **Description present** — Non-empty, 10-1024 characters
4. **Second-person voice** — Flag second-person directives (phrases addressing the reader) in body text
5. **Body length** — Warn if body >500 lines with no `references/` directory (wall-of-text)
6. **Missing "When to Use"** — Flag if no `## When to Use` heading exists

### Behavioral Edit Testing

**Trivial edits** (typo/spelling, whitespace/formatting, reordering without changing meaning, path updates) skip this section entirely.

**Behavioral edits** (changes to instructions/guidance/rules, adding/removing/modifying sections, changing routing or triggers, modifying scripts, changing `description` or `allowed-tools`) must pass the gate below. When in doubt, it is behavioral.

**GATE — Eval coverage required. Do NOT plan, analyze, or edit until this gate is satisfied.**

1. Check: does the skill have `evals/evals.yml` with scenarios?
2. If NO evals exist: **STOP.** Bootstrap evals before proceeding:
   a. Read all skill files; classify skill type (discipline/technique/pattern/reference)
   b. Draft 3 eval scenarios matching the type (see `references/testing-guide.md` § Eval Bootstrapping Protocol for scenario design by type)
   c. Present scenarios to user for approval via AskUserQuestion
   d. Create `<skill-dir>/evals/evals.yml` with approved scenarios
   e. **Checkpoint**: verify `evals/evals.yml` exists with ≥3 scenarios before continuing
3. If evals exist but no scenario covers the behavior being changed: draft and add 1 scenario targeting that behavior; present to user for approval
4. Run edit-relevant scenario(s) with the current skill loaded; capture output as pre-edit snapshot
5. **NOW** make the edits
6. Re-run the same scenarios; confirm intended improvement without regression
7. If fixing a reported bug, include a scenario that reproduces the original bug pre-edit

**Red flags — STOP if you catch yourself doing any of these before step 5:**
- Listing or analyzing what needs to change
- "The changes are straightforward"
- "I'll create evals after the edit"
- "This is too simple for evals"

Report issues inline as suggestions. Do NOT run the full checklist or restructure the skill.

## Create Quick Reference

| Phase | Purpose | Key Tools | Workflow |
|-------|---------|-----------|----------|
| 0. Baseline | Test without skill (RED phase) | `run-eval.sh run` | `workflows/create-phase0-baseline.md` |
| 1. Discovery | Interview: name, purpose, use cases | `ask-form.sh` | `workflows/create-phase1-discovery.md` |
| 2. Design | Select features and skill type | `ask-multi.sh`, `ask-choose.sh` | `workflows/create-phase2-design.md` |
| 3. Scaffold | Create directory and files | `scripts/scaffold.sh` | `workflows/create-phase3-scaffold.md` |
| 4. Author | Write content collaboratively | Edit tool | `workflows/create-phase4-author.md` |
| 5. Validate | Run quality checklist | `scripts/quick-validate.sh` | `workflows/create-phase5-validate.md` |
| 6. Eval | Behavioral testing & iteration | `run-eval.sh run` | `workflows/create-phase6-eval.md` |

### How to Create

Read the workflow file for the current phase. Start at Phase 0 and proceed sequentially.

0. Read `workflows/create-phase0-baseline.md` — Baseline testing: observe what agents do WITHOUT the skill
1. Read `workflows/create-phase1-discovery.md` — Gather name, purpose, use cases, triggers
2. Read `workflows/create-phase2-design.md` — Select skill type, resources, frontmatter features
3. Read `workflows/create-phase3-scaffold.md` — Run scaffold script to create directory and files
4. Read `workflows/create-phase4-author.md` — Write content section by section
5. Read `workflows/create-phase5-validate.md` — Validate against quality checklist
6. Read `workflows/create-phase6-eval.md` — Run eval scenarios, grade, iterate

If `$ARGUMENTS` is provided (and is not `update`, `--improve`, or `--all`), pre-fill the name/topic and start Phase 1 with that context.

### Alternative: Domain Expertise Skills

If the skill covers a broad domain (framework, platform, API, language ecosystem) and requires exhaustive research and multiple workflows, use `workflows/create-domain-expertise.md` instead of the standard 6-phase path.

## Improve Quick Reference

| Mode | Scope | Workflow |
|------|-------|----------|
| Standard | Full audit of one skill | `workflows/improve-standard.md` |
| Bulk | Audit every installed skill | `workflows/improve-bulk.md` |

### Quick Pre-flight

```bash
~/.claude/skills/skillcraft/scripts/quick-validate.sh <skill-directory>
~/.claude/skills/skillcraft/scripts/quick-validate.sh --all
```

## Update from Upstream Sources

This skill tracks its own provenance — which upstream sources contributed to which files and what curation decisions were made. Run the update workflow to check for upstream changes and incorporate them.

Read `workflows/update-from-sources.md` for the full process. See `provenance.yml` for source mappings.

## Anti-Pattern Detection

Consult `references/anti-patterns.md` for 15 common problems across 4 categories. When an anti-pattern is detected, cite it by name and show the before/after fix.

## Dependencies

- **advanced-ask** skill — For interactive interview forms
- **interactive-tmux** skill — For running interactive TUIs
- **gum** — Interactive TUI components (`brew install gum`)
- **jq** — JSON processing (`brew install jq`)
- **fzf** — File picking (`brew install fzf`)
- **yq** — YAML processing (`brew install yq`)

## Workflows

| File | Purpose |
|------|---------|
| `workflows/create-phase0-baseline.md` | Baseline testing: RED phase before writing |
| `workflows/create-phase1-discovery.md` | Concrete examples, interview, validation |
| `workflows/create-phase2-design.md` | Skill type, resources, frontmatter features |
| `workflows/create-phase3-scaffold.md` | Run scaffold script, post-scaffold updates |
| `workflows/create-phase4-author.md` | Section-by-section content authoring |
| `workflows/create-phase5-validate.md` | Structural check + full audit |
| `workflows/create-phase6-eval.md` | Behavioral eval: paired runs, grading, iteration |
| `workflows/create-domain-expertise.md` | Domain expertise skill creation (research-intensive, router-pattern) |
| `workflows/improve-standard.md` | Full audit of one skill (6-step workflow) |
| `workflows/improve-bulk.md` | Audit every installed skill with summary table |
| `workflows/update-from-sources.md` | Sync curated content from upstream sources |
| `workflows/add-provenance.md` | Add provenance tracking to an existing skill |
| `workflows/integrate-source.md` | Fold content from an external source into an existing skill |

## Reference Files

| File | Purpose |
|------|---------|
| `references/official-spec.md` | Official Anthropic skill specification |
| `references/frontmatter-reference.md` | Complete catalog of all SKILL.md frontmatter fields |
| `references/skill-templates.md` | Starter templates by skill type |
| `references/writing-style.md` | Voice, tone, and formatting rules for skill content |
| `references/interactive-tui.md` | Best practices for using interactive TUI tools in skills |
| `references/resource-usage.md` | Guide for using scripts/, references/, and assets/ directories |
| `references/dynamic-context.md` | Backtick-bang syntax, $ARGUMENTS, and string substitutions |
| `references/naming-conventions.md` | Skill naming, description construction, trigger phrase design |
| `references/dependencies.md` | Handling inter-skill and external tool dependencies |
| `references/quality-checklist.md` | Complete 37-item validation checklist (7 categories) |
| `references/anti-patterns.md` | 15 common anti-patterns across 4 categories |
| `references/bulletproofing.md` | Rationalization resistance for discipline-enforcing skills |
| `references/common-fixes.md` | Concrete fix examples for common checklist failures |
| `references/testing-guide.md` | How to test skills after creation or editing |
| `references/eval-guide.md` | Writing eval scenarios, assertions, and rubrics |
| `agents/grader.md` | Agent prompt for auto-grading eval outputs |
| `agents/comparator.md` | Agent prompt for blind A/B comparison |
| `agents/analyzer.md` | Agent prompt for benchmark analysis |
| `references/router-pattern.md` | When and how to upgrade skills to the router pattern |
| `scripts/quick-validate.sh` | Automated structural validation (fast pre-flight) |
| `references/source-integration.md` | Templates and frameworks for source integration |
| `scripts/check-upstream.sh` | Check upstream sources for changes; optionally update provenance metadata |
| `scripts/run-eval.sh` | Eval pipeline: run, init, status, new-iteration, show, scenarios |
| `scripts/aggregate-results.sh` | Aggregate grading results into benchmark.json |
| `scripts/post-integration-check.sh` | Post-integration content quality validation |
