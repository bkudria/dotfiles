# Results Interpretation

Read eval results, diagnose failures, and iterate effectively.

---

## Reading craboodle Output

```yaml
artifact_dir: /tmp/craboodle-run-a1b2c3     # Intermediate files for debugging
scenarios:
  - id: email-validator
    labels:
      config: optimized
    assertions:
      - check: "Output validates email format"
        pass_rate: 1.0                        # Passed all reps — compact
      - check: "Handles edge cases"
        pass_rate: 0.67                       # Passed 2 of 3 reps
        failures:                             # Per-rep failure evidence
          - rep: 1
            evidence: "No empty string handling found in the output"
    pass_rate: 0.83                           # Mean of assertion pass_rates
    cost_usd: 0.029                           # Agent + grading cost
    errors: []                                # Infrastructure errors (if any)
```

Key fields:
- **`pass_rate`** (per-assertion) — fraction of reps where the assertion passed
- **`pass_rate`** (per-scenario) — mean of assertion pass_rates
- **`failures`** — per-rep evidence explaining why an assertion failed (only present when pass_rate < 1.0)
- **`errors`** — infrastructure failures (scuttlerun crash, pincenez timeout). Failed reps are excluded from averaging

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Pipeline completed. Individual failures are in the output, not the exit code |
| 1 | Configuration error (invalid YAML, missing fields) |
| 2 | Infrastructure error (tools not found, zero scenarios, all reps failed) |
| 3 | Threshold failure — a scenario fell below `min_pass_rate` in base.yml |

---

## Decision Framework

| Pass Rate | Action |
|-----------|--------|
| >= 0.8 | **Ship it** — configuration works as intended |
| 0.5 - 0.8 | **Revise** — some assertions failing. Read failure evidence, adjust config or assertions |
| < 0.5 | **Major revision** — configuration isn't achieving its goal |
| All pass trivially | **Suspect assertions** — they may not test config-specific value. Ask: "would Claude do this without the config?" |

---

## Diagnosing Failures

When an assertion fails, read the `evidence` field. It tells you *why* the grader judged the assertion as failed.

Common causes:

| Symptom | Likely Cause | Fix |
|---------|-------------|-----|
| Assertion fails inconsistently (pass_rate 0.3-0.7) | Config adds tendency but doesn't enforce it strongly enough | Strengthen the config instruction, or relax the assertion |
| Assertion always fails (pass_rate 0.0) | Config doesn't address this behavior, or assertion is too strict | Verify the config actually teaches this; try loosening the assertion wording |
| Assertion always passes (pass_rate 1.0) | May test baseline behavior, not config value | Run without the config — if it still passes, the assertion is an always-passes anti-pattern |
| Infrastructure errors in `errors` array | scuttlerun or pincenez failed, not the config | Check the artifact directory for raw logs |

### Config problem or assertion problem?

When an assertion fails, the issue is in one of two places:

1. **Config problem** — the configuration doesn't cause the behavior you expected. Fix: revise the configuration.
2. **Assertion problem** — the configuration works, but the assertion doesn't capture the behavior correctly. Fix: revise the assertion.

To distinguish: read the scuttlerun transcript in the artifact directory. If the agent *did* follow the config but the assertion missed it, it's an assertion problem. If the agent *didn't* follow the config, it's a config problem.

---

## Iteration Workflow

```
1. Run evals
2. Review results
3. Read failure evidence
4. Identify: is it a config problem or an assertion problem?
5. Revise the config OR the assertion (not both at once)
6. Re-run — check `craboodle run --help` for options to target specific scenarios
7. Compare pass rates to previous run
8. Repeat until pass_rate >= 0.8 or plateau
```

**One change at a time.** If you revise both config and assertions simultaneously, you can't attribute improvement to either change.

---

## When to Stop

- **Ship**: Pass rate >= 0.8 across all scenarios
- **Plateau**: Pass rate improvement < 0.05 for 2 consecutive iterations — further changes aren't helping
- **Diminishing returns**: Cost of running more evals exceeds expected quality gain

Not every configuration needs to reach 1.0. A pass rate of 0.8-0.9 means the configuration works reliably. Chasing 1.0 often means over-fitting assertions to specific output patterns rather than testing meaningful behavior.

---

## Lint vs Run: Different Signals

Lint checks assertion **form** (clarity, specificity, independence). Eval runs check assertion **substance** (does the config actually produce this behavior?). Both can pass while the other fails:

- Assertions that pass lint can fail at runtime (too strict, wrong expectation)
- Assertions that fail lint can pass at runtime (domain-appropriate language works for the grader)

When iterating, diagnose whether the problem is **form** (fix assertion wording) or **substance** (fix config or eval design). Never change both simultaneously — you can't attribute improvement to either change.

### First Run Fast

Get to substance quickly: run once with `--repeats 1` immediately after writing assertions. Use the result to calibrate — then lint, then run the full suite with standard reps. Runtime signal is more valuable than lint signal for diagnosing real problems.

---

## Common Pitfalls

- **Iterating on the config when the assertion is the problem** — always check the transcript first
- **Over-fitting to specific failure evidence** — fix the *pattern*, not the specific wording the grader complained about
- **Running many reps when you should revise first** — if pass_rate is 0.2 after 3 reps, more reps won't help. Revise, then re-run
- **Not linting assertions before running** — `craboodle lint` catches anti-patterns cheaply. Always lint before the first run
- **Comparing across runs without labels** — use labels to tag variants so downstream comparison is unambiguous
