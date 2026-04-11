# Bootstrap Evals

Add eval coverage to an existing skill that has no `evals/` directory.

> **References for this workflow:** `references/eval-guide.md` (scenario patterns by skill type), `references/testing-guide.md` § Eval Bootstrapping Protocol (tiered approach). Load the `claude-code-evals` skill for check design rules and anti-patterns. Run `craboodle --help` for the canonical scenario.yaml and base.yaml schema reference.

## Step 1: Select Target Skill

If `$ARGUMENTS` specifies a skill name or path, use it. Otherwise, pick a skill:

```bash
~/.claude/skills/advanced-ask/scripts/ask-file.sh \
    --glob "SKILL.md" ~/.claude/skills
```

Also check `.claude/skills` in the current project for project-local skills.

Check the skill's `evals/` directory status:
- **No `evals/` directory** — proceed to Step 2.
- **`evals/` exists with TODO placeholders only** (from `scaffold.sh`) — proceed to Step 2; Step 5 will handle the existing stubs.
- **`evals/` exists with real scenarios** (non-placeholder prompts and checks) — this workflow does not apply. Use Tier 1 or Tier 2 from `references/testing-guide.md` instead.

## Step 2: Read & Classify

Read all files in the skill directory — SKILL.md, all references/, all scripts/. Build a complete picture.

For format reference, read one complete eval suite from a sibling skill of the same type (e.g. `~/.claude/skills/skillcraft/evals/` for a skill-about-skills). The `claude-code-evals` and `skillcraft` skill content is available via loaded skill context — do not spawn sub-agents to re-read their reference files.

Classify the skill type:

| Type | Signal |
|------|--------|
| Discipline | Enforces a rule, resists pressure to skip |
| Technique | Teaches a specific method or diagnostic approach |
| Pattern | Recognizes when a structural pattern applies |
| Reference | Provides syntax, API, or tool documentation |

## Step 3: Propose Scenarios

Generate 3+ scenario proposals matching the skill type:

| Skill Type | Scenario Mix |
|------------|-------------|
| Discipline | 1 pressure, 1 compliance, 1 edge case |
| Technique | 1 application, 1 variation, 1 gap |
| Pattern | 1 recognition, 1 application, 1 counter-example |
| Reference | 1 retrieval, 1 application, 1 completeness |

For each proposed scenario, draft: `id`, `name`, `prompt`, and 3 `checks`. Make scenarios realistic and specific to the skill's actual content — not generic templates.

For auto-triggering skills (no `disable-model-invocation: true`), also propose 1 trigger scenario. See `references/eval-guide.md` § Trigger Testing.

Load the `claude-code-evals` skill for check design rules. Check quality is enforced in Step 6.

## Step 4: Interview (when warranted)

Assess whether the proposed scenarios involve subjective design choices:

| Skill Type | Scenario Predictability | Interview? |
|-----------|------------------------|------------|
| Reference | High — retrieval, application, completeness, trigger are formulaic | Typically skip |
| Pattern | Medium — recognition vs. counter-example selection requires judgment | Typically interview |
| Technique | Medium — application context affects scenario quality | Typically interview |
| Discipline | Low — pressure calibration is subjective | Typically interview |

If the scenarios are straightforward (clear confusion points, well-documented behavior, obvious check targets), proceed directly to Step 5. If there are subjective choices, tradeoffs, or nuance in scenario design, present proposals and ask:

1. "Here are N proposed eval scenarios for [skill-name]. For each: approve as-is, suggest changes, or replace?"
2. "What behaviors are most critical to verify? Anything I missed?"

Revise scenarios based on feedback. Two questions is the target; three is the maximum.

## Step 5: Write Pipeline Config

### If `evals/` does not exist

Scaffold the eval directory:

```bash
craboodle init <skill-dir>/evals
```

This creates `craboodle.yaml`, `base.yaml`, and a `hello-world/` example scenario. Then:

1. **Edit `base.yaml`** — add model, tools, and inject the skill under test via `project.skills`. Include tools the agent needs (typically Bash, Read, Write, Glob, Grep, Edit). Run `scuttlerun --help` for the full schema.
2. **Edit `craboodle.yaml`** — adjust `min_pass_rate` if needed (default is 0.8).
3. **Remove the example scenario** — delete the `hello-world/` directory.

### If `evals/` exists from scaffolding (TODO stubs)

The `scaffold.sh` script already created `craboodle.yaml`, `base.yaml` (with model, tools, and skill injection pre-filled), and placeholder scenario directories. Use what's there:

1. **Review `base.yaml`** — verify model, tools, and skill injection are correct. Adjust if needed.
2. **Edit `craboodle.yaml`** — add `min_pass_rate: 0.8` if not already present.
3. **Delete placeholder scenarios** — remove `scenario-1/` and any other TODO-only directories.

## Step 6: Write Scenario Files

For each approved scenario, create:

- `evals/<scenario-id>/scenario.yaml` — prompt and any scuttlerun overrides (fixtures via `project.files`, tool restrictions, etc.)
- `evals/<scenario-id>/checks.yaml` — context and checks in id-as-key format

**GATE — Apply the Pre-Write Checklist to every check before writing it to a file.** Catching anti-patterns here is free; catching them via `craboodle lint` costs a lint cycle per fix.

| Anti-Pattern | Self-Test | If Yes |
|---|---|---|
| **Compound** | Does this check test two+ independent things? Signals: "and", "both", "as well as". | Split into separate checks. |
| **Vague** | Could two graders disagree on pass/fail? Signals: "valid", "correct", "appropriate" without a concrete example. | Add a concrete element to look for. |
| **Always-passes** | Would Claude do this without the configuration? | Target what the config adds — the delta, not the baseline. |
| **Tautological** | Does this check mirror the prompt wording? | Assert HOW — the specific method or structure — not WHETHER. |
| **Unverifiable** | Can the grader observe this in the output? Signals: "understood", "considered". | Rewrite as observable behavior. |
| **Over-specific** | Does this check mandate a specific function/operator when the outcome matters? | Test the outcome; mention approaches as examples, not requirements. |

**Write incrementally**: Write the first scenario, then lint it with `craboodle lint --scenario <id> <skill-dir>/evals`. Fix any issues before writing the remaining scenarios — anti-pattern tendencies caught on the first scenario won't propagate to the rest. Then write the remaining scenarios in parallel, following the same patterns.

## Step 7: Lint

```bash
craboodle lint <skill-dir>/evals
```

Confirm zero issues across the full suite. If the incremental lint in Step 6 was clean, this should pass on the first run.

## Step 8: First Run

**GATE — Lint validates form; run validates substance. Do NOT report completion or present a final summary until a run has completed and results are reviewed.**

```bash
craboodle run --repeats 1 <skill-dir>/evals
```

Review the output. If all scenarios pass, proceed to Final Report. If failures occur, continue to Step 9.

## Step 9: Iterate

| Exit Code | Meaning | Action |
|-----------|---------|--------|
| 0 | All scenarios at or above `min_pass_rate` | Done |
| 3 | One or more scenarios below `min_pass_rate` | Diagnose and fix |
| 1 | Configuration error | Fix scenario YAML |
| 2 | Infrastructure error | Check tool installation |

For each failing check, diagnose:
- **Skill problem** — The skill doesn't cause the intended behavior. Fix: revise the skill.
- **Check problem** — The skill works but the check doesn't capture it correctly. Fix: revise the check.

To distinguish: read the scuttlerun transcript at `<artifact_dir>/<scenario-id>/rep-<N>/output.yaml` (the `artifact_dir` is printed in craboodle's YAML output).

Iteration rules:
1. Fix one thing at a time (skill OR check, not both)
2. Re-run targeted scenarios after each fix
3. Stop when: exit code 0, or pass rate improvement < 0.05 for 2 iterations

## Final Report

This template requires data from a `craboodle run` — it cannot be filled from lint results alone.

```
## Evals Bootstrapped: {skill-name}

Location: {skill-dir}/evals/
Scenarios: {count}
Checks: {total check count}
Lint: PASS
Run: {PASS or FAIL} (exit code {0 or 3})

### Per-Scenario Results
{paste craboodle run YAML output: scenario id, pass_rate, cost_usd for each}

### Iterations
{count} lint-fix cycles, {count} run-fix cycles
```
