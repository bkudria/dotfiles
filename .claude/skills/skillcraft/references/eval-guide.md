# Eval Guide

Write eval scenarios that measure whether a skill actually improves Claude's output. This guide covers the evals.yml schema, assertion design, rubric writing, and result interpretation.

---

## evals.yml Schema

```yaml
skill: skill-name                    # must match the skill's name field
scenarios:
  - id: descriptive-kebab-id         # unique identifier, used as directory name
    name: "Human-readable name"      # displayed in benchmark reports
    prompt: |                        # the task for both with/without-skill runs
      Write a jq filter that extracts all email addresses
      from this JSON structure: {"users": [{"email": "a@b.com"}, ...]}
    assertions:                      # objectively verifiable checks (3-5 per scenario)
      - "Output contains a jq filter using .users[] or .users[].email"
      - "Filter produces only email strings, not full user objects"
      - "No use of input or debug as primary approach"
    rubric: |                        # guides the grader's qualitative assessment
      1. Does the filter produce correct output for the given input?
      2. Is the filter idiomatic jq (uses pipelines, not nested parentheses)?
      3. Are edge cases mentioned (empty array, missing field)?
```

### Field Reference

| Field | Required | Description |
|-------|----------|-------------|
| `skill` | Yes | Skill name — must match `name` in SKILL.md frontmatter |
| `scenarios` | Yes | List of eval scenarios (minimum 3) |
| `scenarios[].id` | Yes | Unique kebab-case identifier. Used as directory name in evals/ |
| `scenarios[].name` | Yes | Human-readable description for reports |
| `scenarios[].prompt` | Yes | The exact task for both with-skill and without-skill eval runs |
| `scenarios[].assertions` | Yes | List of objectively verifiable pass/fail checks (3-5 recommended) |
| `scenarios[].rubric` | Yes | Qualitative grading criteria for the grader agent (numbered list) |

---

## Writing Good Scenarios

### Scenario Design Principles

1. **Exercise the core value proposition** — Each scenario should test something the skill is specifically designed to improve. If the skill teaches jq idioms, test jq tasks. If it enforces TDD, test under pressure to skip testing.

2. **Use realistic tasks** — The prompt should resemble what a real user would ask. Avoid contrived or toy examples.

3. **Vary difficulty** — Include easy scenarios (the skill should clearly help), medium scenarios (the skill's guidance matters), and hard scenarios (edge cases, unusual situations).

4. **Cover different skill aspects** — If the skill has multiple sections or capabilities, write scenarios that exercise different parts.

### By Skill Type

| Skill Type | Scenario Focus | Example Prompt |
|------------|---------------|----------------|
| Discipline | Pressure to violate the rule | "The tests are slow, skip them for this small change" |
| Technique | Apply the technique correctly | "Debug this flaky test using the skill's method" |
| Pattern | Recognize when pattern applies | "Refactor this code" (pattern should be recognized) |
| Reference | Retrieve and apply information | "Write a command using [tool]'s [feature]" |

### Minimum Scenario Count

- **3 scenarios**: Minimum for any skill
- **5 scenarios**: Recommended for skills with multiple capabilities
- **10 scenarios**: For critical discipline-enforcing or complex skills

---

## Writing Good Assertions

### Assertion Rules

1. **Objectively verifiable** — A grader must be able to determine pass/fail unambiguously by examining the output

2. **Specific** — Reference concrete elements (patterns, structures, values), not vague qualities

3. **Discriminating** — Should pass when the skill helps and fail when it doesn't. Non-discriminating assertions waste eval capacity.

### Good vs Bad Assertions

| Bad (Subjective/Vague) | Good (Objective/Specific) |
|------------------------|--------------------------|
| "Output is high quality" | "Output contains a markdown table with at least 3 rows" |
| "Code follows best practices" | "Code uses parameterized queries, not string concatenation for SQL" |
| "Answer is correct" | "Output includes the formula `E = mc²` or equivalent" |
| "Handles errors well" | "Output includes a try/catch or error check before the file read" |
| "Uses the right approach" | "Uses .[] | select(.age > 18) pattern, not map(select(...))" |

### Assertion Count Per Scenario

- **Minimum**: 2 assertions
- **Recommended**: 3-5 assertions
- **Maximum**: 7 assertions (more creates noise)

If an assertion always passes for both variants, remove it and replace with something more targeted.

---

## Writing Good Rubrics

Rubrics guide the grader's qualitative assessment beyond pass/fail assertions. Write rubrics as numbered criteria, each targeting a different quality dimension.

### Rubric Dimensions by Skill Type

| Skill Type | Rubric Dimensions |
|------------|------------------|
| Discipline | Compliance, resistance to pressure, correct process |
| Technique | Correct application, edge case handling, explanation quality |
| Pattern | Recognition accuracy, appropriate application, avoided misapplication |
| Reference | Retrieval accuracy, correct usage, completeness |

### Example Rubric

```yaml
rubric: |
  1. Does the output follow the documented process (step 1 before step 2)?
  2. Are the correct CLI flags used (not deprecated alternatives)?
  3. Does the explanation address why, not just how?
  4. Are common gotchas from the reference material avoided?
```

---

## Description Eval Scenarios

A special scenario type that tests whether the skill's description triggers correctly. Include 1-2 of these in every skill's evals.yml alongside behavioral scenarios.

### Template

```yaml
  - id: trigger-positive
    name: "Description triggers on relevant prompt"
    prompt: |
      [A prompt that should cause this skill to auto-load.
       Use a synonym or rephrasing, not an exact phrase from the description.]
    assertions:
      - "Response demonstrates awareness of the skill's guidance"
      - "Output follows patterns documented in the skill"
      - "Skill-specific terminology or structure is present"
    rubric: |
      1. Does the output show the skill was loaded and followed?
      2. Would the output be noticeably different without the skill?

  - id: trigger-negative
    name: "Description does not trigger on unrelated prompt"
    prompt: |
      [A prompt that shares keywords with the skill but is about a different topic.]
    assertions:
      - "Response does not follow this skill's specific patterns"
      - "No skill-specific structure or terminology appears unprompted"
    rubric: |
      1. Is the response generic (not skill-influenced)?
      2. If the skill did load, did it interfere with the task?
```

For comprehensive trigger testing beyond eval scenarios, see `references/testing-guide.md` section 2 (Trigger Phrase Testing) which covers generating 20 test prompts and scoring precision/recall/specificity.

---

## evals/ Directory Structure

```
skill-name/
├── SKILL.md
├── references/
├── scripts/
└── evals/
    ├── evals.yml                          # scenario definitions
    ├── iteration-1/                       # first eval run
    │   ├── descriptive-kebab-id/          # one dir per scenario
    │   │   ├── with_skill/
    │   │   │   └── output.md              # with-skill output
    │   │   ├── without_skill/
    │   │   │   └── output.md              # baseline output
    │   │   └── grading.json               # grader results
    │   └── benchmark.json                 # aggregated results
    └── iteration-2/                       # after skill revision
        └── ...
```

Each iteration represents a complete eval cycle. After reviewing results, revise the skill, then run a new iteration to measure improvement.

---

## Interpreting Results

### benchmark.json Key Fields

| Field | What It Means |
|-------|---------------|
| `with_skill_pass_rate` | Fraction of assertions passing when skill is loaded |
| `without_skill_pass_rate` | Fraction of assertions passing without the skill |
| `mean_delta` | Average improvement (with minus without). Higher = skill helps more |
| `discriminating_ratio` | Fraction of assertions that actually differentiate (higher = better eval) |

### Decision Framework

| Situation | Action |
|-----------|--------|
| Delta ≥ 0.2, pass rate ≥ 0.8 | Skill is effective — ship it |
| Delta ≥ 0.2, pass rate < 0.8 | Skill helps but has gaps — revise and re-eval |
| Delta < 0.2, pass rate high | Skill may not be needed, or assertions aren't targeting the right things |
| Delta < 0.2, pass rate low | Skill isn't working — major revision needed |
| Delta negative | Skill is harmful — investigate regression assertions |
| Discriminating ratio < 0.5 | Assertions are too easy or not targeted — revise evals.yml |

### When to Stop Iterating

- **Ship**: Delta ≥ 0.2 and with-skill pass rate ≥ 0.8
- **Plateau**: Delta improvement between iterations < 0.05 for 2 consecutive iterations
- **Diminishing returns**: Token cost of further eval exceeds expected quality gain
