# Results Interpretation

Read eval results, diagnose failures, and iterate effectively.

---

## Reading craboodle Output

```yaml
artifact_dir: /tmp/craboodle-run-a1b2c3     # Intermediate files for debugging
scenarios:
  - id: email-validator
    checks:
      - check: "Output validates email format"
        pass_rate: 1.0                        # Passed all reps — compact
      - check: "Handles edge cases"
        pass_rate: 0.67                       # Passed 2 of 3 reps
        failures:                             # Per-rep failure evidence
          - rep: 1
            evidence: "No empty string handling found in the output"
    pass_rate: 0.83                           # Mean of check pass_rates
    cost_usd: 0.029                           # Agent + grading cost
    errors: []                                # Infrastructure errors (if any)
```

Key fields:
- **`pass_rate`** (per-check) — fraction of reps where the check passed
- **`pass_rate`** (per-scenario) — mean of check pass_rates
- **`failures`** — per-rep evidence explaining why a check failed (only present when pass_rate < 1.0)
- **`errors`** — infrastructure failures (scuttlerun crash, pincenez timeout). Failed reps are excluded from averaging

### Preserving Full Output

`craboodle run` streams one YAML document per scenario. **Never** pipe it through `grep`, `head`, or `tail` when the output will be used to decide pass/fail or populate a report — a truncated stream can omit scenarios entirely, and you cannot tell which are missing from the filtered view.

Redirect to a file, then query:

```bash
craboodle run evals > /tmp/results.yaml
yq '.scenarios[] | {id, pass_rate, cost_usd}' /tmp/results.yaml
```

This preserves the full run record, keeps the tool output small, and guarantees every scenario appears in the view.

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Pipeline completed. Individual failures are in the output, not the exit code |
| 1 | Configuration error (invalid YAML, missing fields) |
| 2 | Infrastructure error (tools not found, zero scenarios, all reps failed) |
| 3 | Threshold failure — a scenario fell below `min_pass_rate` in evals.yaml |

---

## Decision Framework

| Pass Rate | Action |
|-----------|--------|
| >= 0.8 | **Ship it** — configuration works as intended |
| 0.5 - 0.8 | **Revise** — some checks failing. Read failure evidence, adjust config or checks |
| < 0.5 | **Major revision** — configuration isn't achieving its goal |
| All pass trivially | **Suspect checks** — they may not test config-specific value. Ask: "would Claude do this without the config?" |

---

## Diagnosing Failures

When a check fails, read the `evidence` field. It tells you *why* the grader judged the check as failed.

Common causes:

| Symptom | Likely Cause | Fix |
|---------|-------------|-----|
| Check fails inconsistently (pass_rate 0.3-0.7) | Config adds tendency but doesn't enforce it strongly enough | Strengthen the config instruction, or relax the check |
| Check always fails (pass_rate 0.0) | Config doesn't address this behavior, or check is too strict | Verify the config actually teaches this; try loosening the check wording |
| Check always passes (pass_rate 1.0) | May test baseline behavior, not config value | Run without the config — if it still passes, the check is an always-passes anti-pattern |
| Infrastructure errors in `errors` array | scuttlerun or pincenez failed, not the config | Check the artifact directory for raw logs |

### Config problem or check problem?

When a check fails, the issue is in one of two places:

1. **Config problem** — the configuration doesn't cause the behavior you expected. Fix: revise the configuration.
2. **Check problem** — the configuration works, but the check doesn't capture the behavior correctly. Fix: revise the check.

To distinguish: read the scuttlerun transcript in the artifact directory. If the agent *did* follow the config but the check missed it, it's a check problem. If the agent *didn't* follow the config, it's a config problem.

---

## Iteration Workflow

```
1. Run evals
2. Review results
3. Read failure evidence
4. Identify: is it a config problem or a check problem?
5. Revise the config OR the check (not both at once)
6. Re-run — check `craboodle run --help` for options to target specific scenarios
7. Review pass rates — look for improvements and regressions
8. Repeat until pass_rate >= 0.8 or plateau
```

**One change at a time.** If you revise both config and checks simultaneously, you can't attribute improvement to either change.

---

## When to Stop

- **Ship**: Pass rate >= 0.8 across all scenarios
- **Plateau**: Pass rate improvement < 0.05 for 2 consecutive iterations — further changes aren't helping
- **Diminishing returns**: Cost of running more evals exceeds expected quality gain

Not every configuration needs to reach 1.0. A pass rate of 0.8-0.9 means the configuration works reliably. Chasing 1.0 often means over-fitting checks to specific output patterns rather than testing meaningful behavior.

---

## Lint vs Run: Different Signals

Lint validates check **form** (clarity, specificity, independence). Eval runs validate check **substance** (does the config actually produce this behavior?). Both can pass while the other fails:

- Checks that pass lint can fail at runtime (too strict, wrong expectation)
- Checks that fail lint can pass at runtime (domain-appropriate language works for the grader)

When iterating, diagnose whether the problem is **form** (fix check wording) or **substance** (fix config or eval design). Never change both simultaneously — you can't attribute improvement to either change.

### First Run Fast

Get to substance quickly: run once with `--repeats 1` immediately after writing checks. Use the result to calibrate — then lint, then run the full suite with standard reps. Runtime signal is more valuable than lint signal for diagnosing real problems.

---

## Common Pitfalls

- **Iterating on the config when the check is the problem** — always check the transcript first
- **Over-fitting to specific failure evidence** — fix the *pattern*, not the specific wording the grader complained about
- **Running many reps when you should revise first** — if pass_rate is 0.2 after 3 reps, more reps won't help. Revise, then re-run
- **Not linting checks before running** — `craboodle lint` catches anti-patterns cheaply. Always lint before the first run
- **Using unclear scenario names** — use descriptive scenario directory names so results are easy to interpret
- **Filtering `craboodle run` output with grep/head/tail** — silently drops scenarios before they reach the report. Redirect to a file and query with yq instead (see § Preserving Full Output)
