# Phase 6: Eval & Iterate

Run behavioral evaluation to verify the skill actually improves Claude's output. The headless eval runner spawns paired scuttlerun sessions (with and without the skill), grades results via pincenez, and aggregates a benchmark.

## When to Do This

- After Phase 5 quality checklist passes
- **Strongly recommended** for discipline-enforcing and technique skills
- **Optional** for reference and pattern skills
- When improving existing skills (to verify fixes work)

## Prerequisites

- Skill passes Phase 5 structural validation
- `yq` installed (`brew install yq`)
- `jq` installed (`brew install jq`)
- `claude` CLI installed

---

## Step 1: Define Eval Scenarios

Initialize the eval directory and write scenarios:

```bash
~/.claude/skills/skillcraft/scripts/run-eval.sh init <skill-directory>
```

Edit `evals/evals.yml` to define 3-10 scenarios. Each scenario needs:

| Field | Description |
|-------|-------------|
| `id` | Unique kebab-case identifier |
| `name` | Human-readable description |
| `prompt` | The exact task for both with-skill and without-skill runs |
| `assertions` | 3-5 objectively verifiable pass/fail checks |

Consult `references/eval-guide.md` for assertion design patterns and examples by skill type.

### Scenario Design by Skill Type

| Skill Type | Scenario Focus |
|------------|---------------|
| Discipline | Pressure to violate the rule under realistic conditions |
| Technique | Apply the technique to a new, realistic problem |
| Pattern | Recognize when the pattern applies (and when it doesn't) |
| Reference | Retrieve and correctly apply documented information |

---

## Step 2: Run the Eval

```bash
~/.claude/skills/skillcraft/scripts/run-eval.sh run <skill-directory>
```

Options:
- `--model MODEL` — Use a specific model (e.g., `claude-haiku-4-5` for cost savings)
- `--parallel` — Run with/without skill variants in parallel
- `--iteration N` — Reuse an existing iteration directory
- `--skip-grading` — Run scenarios only, skip grading
- `--skip-aggregate` — Skip aggregation step

The script handles all steps automatically:
1. Creates the iteration directory
2. Runs paired with/without-skill scenarios via scuttlerun
3. Grades each scenario using pincenez
4. Aggregates results into `benchmark.json`

For without-skill runs, it temporarily hides SKILL.md to prevent skill loading.

After the run completes, review results with:
```bash
~/.claude/skills/skillcraft/scripts/run-eval.sh show <skill-directory>
```

---

## Step 3: Review & Iterate

### Decision Framework

| Result | Action |
|--------|--------|
| PASS (delta >= 0.2, rate >= 0.8) | Skill is effective — done |
| PARTIAL (good delta, low rate) | Revise skill content to address failures, re-run |
| WEAK (low delta) | Assertions may be wrong, or skill needs major revision |
| REGRESSION (negative delta) | Skill is harmful — investigate and fix |

### Iteration Loop

1. Review failing scenarios — identify what the skill should have caused
2. Revise the skill content to address specific failures
3. Re-run the headless eval (it creates a new iteration automatically)
4. Compare benchmark.json across iterations
5. Stop when: PASS verdict, or plateau (delta improvement < 0.05 for 2 iterations)

### Checking Status

```bash
~/.claude/skills/skillcraft/scripts/run-eval.sh status <skill-directory>
~/.claude/skills/skillcraft/scripts/run-eval.sh show <skill-directory> [iteration]
```

---

## Final Report

After eval completes, update the Phase 5 final report with eval results:

```
### Eval Results (Phase 6)
Iterations: {count}
Final pass rate: {with_skill_pass_rate}
Baseline rate: {without_skill_pass_rate}
Delta: {mean_delta}
Verdict: {PASS|PARTIAL|WEAK|REGRESSION}
```
