# Phase 4: Refine

Lint, run evals, and iterate until the skill passes. This phase is autonomous — no user interaction needed.

> **References for this phase:** `references/eval-guide.md` (check patterns by skill type). Load the `claude-code-evals` skill for general eval methodology. Run `craboodle --help` for CLI reference. Do not read other references unless a specific question arises.

## Prerequisites

- `craboodle` and `pincenez` available on PATH

## Step 1: Lint Checks

```bash
craboodle lint <skill-dir>/evals
```

Fix any flagged issues. Run `craboodle lint --help` for options.

## Step 2: Run Evals

```bash
craboodle run <skill-dir>/evals
```

Run `craboodle run --help` for all available options.

## Step 3: Iterate

| Exit Code | Meaning | Action |
|-----------|---------|--------|
| 0 | All scenarios at or above `min_pass_rate` | Done |
| 3 | One or more scenarios below `min_pass_rate` | Diagnose and fix |
| 1 | Configuration error | Fix scenario YAML |
| 2 | Infrastructure error | Check tool installation |

For each failing check, diagnose:
- **Skill problem** — The skill doesn't cause the intended behavior. Fix: revise the skill.
- **Check problem** — The skill works but the check doesn't capture it correctly. Fix: revise the check.

To distinguish: read the scuttlerun transcript in the artifact directory.

Iteration rules:
1. Fix one thing at a time (skill OR check, not both)
2. Re-run targeted scenarios after each fix
3. Stop when: exit code 0, or pass rate improvement < 0.05 for 2 iterations

## Final Report

```
## Created: {skill-name}

Location: {path}
Type: {skill-type}
Files: {count}

### Eval Results
Iterations: {count}
Final pass rate: {overall_pass_rate}
```
