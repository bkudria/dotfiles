# Bootstrap Evals

Add eval coverage to an existing skill that has no `evals/` directory.

> **References for this workflow:** `references/eval-guide.md` (scenario patterns by skill type), `references/testing-guide.md` § Eval Bootstrapping Protocol (tiered approach). Run `craboodle --help` for the canonical scenario.yaml and base.yaml schema reference.

**GATE — Load `claude-code-evals` before proceeding.** This workflow depends on check design rules from the `claude-code-evals` skill. Use the Skill tool to load it now. Do NOT proceed to Step 1 until it is loaded.

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

While reading, note every tool name the skill itself calls (including MCP-prefixed `mcp__*` names). This list feeds Step 5's `tools:` array. Skills that omit a needed tool will fail at smoke-run with "tool not available," and tools the SDK does not recognize will surface as `WARNING: Unknown tool name` on scuttlerun dry-run. After drafting `base.yaml` in Step 5, run `scuttlerun -n <scenario.yaml>` once to validate every tool name before proceeding; the warning output is the authoritative SDK-tool-name list.

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

Check quality is enforced in Step 6.

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

### Model selection

The agent-under-test `model:` determines what your evals measure. Haiku (scuttlerun's default) is cheap and fast but its failure modes may not reflect what the skill does under Sonnet or Opus — the models most users run. Default to Sonnet for representative evals; reserve Haiku for smoke-testing scenario design or iterating cheaply. The synthetic-user `oracle_model` (under `user:`) also defaults to Haiku — raise it when the skill depends on the oracle following nuanced prompts.

### If `evals/` does not exist

Scaffold the eval directory:

```bash
craboodle init <skill-dir>/evals
```

This creates `craboodle.yaml` with `version: "1"` plus commented-out entries for `min_pass_rate`, `max_budget_usd`, and `repeats`. Then:

1. **Create `base.yaml`** — add model, tools, and inject the skill under test via `project.skills`. The `tools:` array must include both scuttlerun's defaults (`Read, Write, Edit, Bash, Glob, Grep, AskUserQuestion, Skill`) **plus** every tool you noted in Step 2 — listing your additions alone replaces the defaults rather than extending them. Run `scuttlerun --help` for the full schema. For how `base.yaml`, `scenario.yaml`, CLI flags, and scuttlerun defaults compose (deep-merge for objects, replace for arrays, defaults applied last), see `claude-code-evals/references/config-precedence.md`.
2. **Edit `craboodle.yaml`** — uncomment `min_pass_rate` and set a reachable value. Reachable pass rates are `k/(checks × reps)`; e.g. with 3 checks × 1 rep the only reachable values are `{0, 0.33, 0.67, 1.0}`, so `0.8` would collapse to requiring a perfect `1.0`.

### If `evals/` exists from scaffolding

The `scaffold.sh` script (when run with `--evals`) created `craboodle.yaml` and `base.yaml` — with model, tools, and skill injection pre-filled. No scenario directories are scaffolded (scenarios are created fresh in Step 6 for exactly the behaviors you want to test). Use what's there:

1. **Review `base.yaml`** — verify model, tools, and skill injection are correct. Adjust if needed.
2. **Edit `craboodle.yaml`** — set `min_pass_rate` to a reachable value for your check count × rep count (rates are `k/(checks × reps)`; `0.8` at 3 checks × 1 rep is unreachable and acts as `1.0`).

## Step 6: Write Scenario Files

For each approved scenario, create:

- `evals/<scenario-id>/scenario.yaml` — prompt and any scuttlerun overrides (fixtures via `project.files`, tool restrictions, etc.)
- `evals/<scenario-id>/checks.yaml` — context and checks in id-as-key format

**GATE — STOP. Before writing any check, you MUST Read `claude-code-evals/references/check-design.md` § Pre-Write Checklist AND § Common Slips in this session.** The reference contains six self-tests that every check must pass before being written. Apply every self-test to every check — do not skip self-tests on checks that "look obviously fine"; over-specific and compound are the most common lint failures and they hide in checks that look concrete. Catching anti-patterns here is free; catching them via `craboodle lint` has previously cost 5 lint cycles and several minutes of re-authoring when the checklist was skipped at first-pass authorship.

Loading the `claude-code-evals` skill (done in Step 1) is not the same as having the Pre-Write Checklist in context. If you have not Read the § Pre-Write Checklist AND § Common Slips sections of check-design.md during this session, Read them now before continuing.

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

**GATE — When exit code is 3, you MUST produce a per-check diagnosis table before the Final Report. Do NOT emit "Bootstrap complete" or any final summary until every failing check has a Skill-vs-Check attribution recorded in the Diagnosis section of the report template.**

For each failing check, diagnose:
- **Skill problem** — The skill doesn't cause the intended behavior. Fix: revise the skill.
- **Check problem** — The skill works but the check doesn't capture it correctly. Fix: revise the check.

To distinguish: read the scuttlerun transcript at `<artifact_dir>/<scenario-id>/rep-<N>/output.yaml` (the `artifact_dir` is printed in craboodle's YAML output).

Iteration rules:
1. Fix one thing at a time (skill OR check, not both)
2. When a rewrite responds to a lint flag, re-apply the full Pre-Write Checklist to the rewrite before moving on — rewrites commonly reintroduce a different anti-pattern (see `claude-code-evals/references/check-design.md` § Common Slips)
3. Re-run targeted scenarios after each fix
4. Stop when: exit code 0, or pass rate improvement < 0.05 for 2 iterations

## Final Report

This template requires data from a `craboodle run` — it cannot be filled from lint results alone.

**Output handling**: redirect `craboodle run` to a file; never pipe it through `grep`, `head`, or `tail` before reading for the report. Filtered streams drop scenarios silently. See `claude-code-evals/references/results-interpretation.md` § Preserving Full Output.

```
## Evals Bootstrapped: {skill-name}

Location: {skill-dir}/evals/
Scenarios: {count}
Checks: {total check count}
Lint: PASS
Run: {PASS or FAIL} (exit code {0 or 3})

### Per-Scenario Results
{paste craboodle run YAML output: scenario id, pass_rate, cost_usd for each}

### Diagnosis
{Required when Run: FAIL. One row per failing check. Omit this section only when all scenarios passed (exit code 0).}

| Scenario | Check | Pass rate | Attribution (Skill / Check) | Notes |
|----------|-------|-----------|----------------------------|-------|
| ...      | ...   | ...       | ...                        | ...   |

### Iterations
{count} lint-fix cycles, {count} run-fix cycles
```

**Before submitting**: verify every scenario directory under `evals/` appears above with a numeric `pass_rate`. If any row shows `?`, `unknown`, or "succeeded" without a number, the run output was filtered — re-read it from the redirect file or artifact directory and re-populate that row.
