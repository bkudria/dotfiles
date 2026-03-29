# Phase 6: Eval & Iterate

Run behavioral evaluation to verify the skill performs as expected. The eval runner (craboodle) spawns scuttlerun sessions with the skill loaded, grades results via pincenez, and reports pass rates.

## When to Do This

- After Phase 5 quality checklist passes
- **Strongly recommended** for discipline-enforcing and technique skills
- **Optional** for reference and pattern skills
- When improving existing skills (to verify fixes work)

## Prerequisites

- `craboodle` installed (from `~/code/craboodle`)
- `pincenez` installed (from `~/code/pincenez`)
- For assertion design rules, anti-patterns, and results interpretation: load the `claude-code-evals` skill

---

## Step 1: Define Eval Scenarios

Create the `evals/` directory with scenario subdirectories, each containing a `scenario.yml`. Run `craboodle --help` for the canonical scenario.yml and base.yml schema reference. Consult `references/eval-guide.md` for assertion design patterns and examples by skill type.

### Create `evals/base.yml`

Each skill needs a committed `base.yml` that configures craboodle for skill evaluation:

```yaml
min_pass_rate: 0.8
project:
  claude_md: "Use relative paths. Do not use absolute paths."
  skills:
    - "~/.claude/skills/<skill-name>"
tools:
  - Read
  - Write
  - Bash
  - Glob
  - Grep
  - Skill
user:
  turn_policy: single
```

Replace `<skill-name>` with the actual skill directory name. This file is committed with the skill and never regenerated.

### Scenario Design by Skill Type

| Skill Type | Scenario Focus |
|------------|---------------|
| Discipline | Pressure to violate the rule under realistic conditions |
| Technique | Apply the technique to a new, realistic problem |
| Pattern | Recognize when the pattern applies (and when it doesn't) |
| Reference | Retrieve and correctly apply documented information |

---

## Step 2: Lint Assertions

Before running the eval, lint assertions for quality anti-patterns (vague, compound, tautological, always_passes, unverifiable):

```bash
craboodle lint <skill-dir>/evals
```

Fix any flagged issues before proceeding. This catches bad assertions before spending money on agent sessions.

---

## Step 3: Run the Eval

```bash
craboodle run <skill-dir>/evals
```

All craboodle options pass through directly:
- `--agent-model MODEL` — Model for agent sessions
- `--grader-model MODEL` — Model for grading
- `--repeats N` — Run each scenario N times (default: 3)
- `--concurrency N` — Max parallel work items (default: 10)

Craboodle streams YAML results to stdout as scenarios complete, showing per-assertion pass rates and evidence for failures.

---

## Step 4: Review & Iterate

### Interpreting Results

| Exit Code | Meaning | Action |
|-----------|---------|--------|
| 0 | All scenarios at or above `min_pass_rate` | Skill performs as expected — done |
| 3 | One or more scenarios below `min_pass_rate` | Revise skill content, re-run |
| 1 | Configuration error | Fix scenario YAML |
| 2 | Infrastructure error | Check tool installation |

### Iteration Loop

1. Review failing assertions in craboodle's YAML output — check the failure evidence
2. Revise the skill content to address specific failures
3. Re-run the eval
4. Compare pass rates across runs
5. Stop when: exit code 0, or plateau (pass rate improvement < 0.05 for 2 iterations)

---

## Final Report

After eval completes, update the Phase 5 final report with eval results:

```
### Eval Results (Phase 6)
Iterations: {count}
Final pass rate: {overall_pass_rate}
```
