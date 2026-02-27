# Phase 6: Eval & Iterate

Run behavioral evaluation to verify the skill actually improves Claude's output. Spawn paired subagents (with and without the skill), grade results, and iterate.

## When to Do This

- After Phase 5 quality checklist passes
- **Strongly recommended** for discipline-enforcing and technique skills
- **Optional** for reference and pattern skills
- When improving existing skills (to verify fixes work)

## Prerequisites

- Skill passes Phase 5 structural validation
- `yq` installed (`brew install yq`)
- `jq` installed (`brew install jq`)

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
| `prompt` | The exact task for both subagents |
| `assertions` | 3-5 objectively verifiable pass/fail checks |
| `rubric` | Qualitative grading criteria (numbered list) |

Consult `references/eval-guide.md` for assertion design patterns and examples by skill type.

### Scenario Design by Skill Type

| Skill Type | Scenario Focus |
|------------|---------------|
| Discipline | Pressure to violate the rule under realistic conditions |
| Technique | Apply the technique to a new, realistic problem |
| Pattern | Recognize when the pattern applies (and when it doesn't) |
| Reference | Retrieve and correctly apply documented information |

---

## Step 2: Create Iteration Directory

```bash
~/.claude/skills/skillcraft/scripts/run-eval.sh new-iteration <skill-directory>
```

This creates `evals/iteration-N/` with subdirectories for each scenario.

---

## Step 3: Run Paired Subagents

For each scenario in evals.yml, spawn **two subagents in parallel**:

### With-Skill Subagent

Use the Task tool with `subagent_type: "general-purpose"`. The prompt should:
1. Load the skill being evaluated (read the SKILL.md)
2. Execute the scenario prompt
3. Save the full output

Write the subagent's output to: `evals/iteration-N/<scenario-id>/with_skill/output.md`

### Without-Skill Subagent

Use the Task tool with `subagent_type: "general-purpose"`. The prompt should:
1. **Not** load the skill
2. Execute the same scenario prompt
3. Save the full output

Write the subagent's output to: `evals/iteration-N/<scenario-id>/without_skill/output.md`

### Parallelization

Spawn both subagents for a scenario in the same turn to minimize iteration time. Process multiple scenarios in parallel when possible.

---

## Step 4: Grade

For each completed scenario, spawn a **grader subagent**:

1. Read `agents/grader.md` for grading instructions
2. Provide the grader with:
   - The scenario definition (prompt, assertions, rubric) from evals.yml
   - The with-skill output (`with_skill/output.md`)
   - The without-skill output (`without_skill/output.md`)
3. The grader writes `grading.json` in the scenario directory

The grader evaluates:
- Each assertion against both outputs (pass/fail with evidence)
- Rubric dimensions on a 1-5 scale
- Assertion discrimination (does it differentiate with vs without skill?)
- Implicit quality claims beyond explicit assertions

---

## Step 5: Aggregate

After all scenarios are graded:

```bash
~/.claude/skills/skillcraft/scripts/aggregate-results.sh <skill-directory> <iteration-number>
```

This produces `benchmark.json` with:
- Per-scenario pass rates and deltas
- Overall summary statistics
- Discrimination ratios
- A verdict (PASS / PARTIAL / WEAK / REGRESSION)

Review the benchmark table output.

---

## Step 6: Review & Iterate

### Decision Framework

| Result | Action |
|--------|--------|
| PASS (delta ≥ 0.2, rate ≥ 0.8) | Skill is effective — done |
| PARTIAL (good delta, low rate) | Revise skill content to address failures, re-run |
| WEAK (low delta) | Assertions may be wrong, or skill needs major revision |
| REGRESSION (negative delta) | Skill is harmful — investigate and fix |

### Iteration Loop

1. Review failing scenarios — identify what the skill should have caused
2. Revise the skill content to address specific failures
3. Run `new-iteration` to create the next iteration directory
4. Re-run Steps 3-5
5. Compare benchmark.json across iterations
6. Stop when: PASS verdict, or plateau (delta improvement < 0.05 for 2 iterations)

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
