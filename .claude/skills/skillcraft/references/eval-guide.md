# Eval Guide

Write eval scenarios that verify a skill performs as expected. This guide covers the scenario.yml schema, assertion design, and result interpretation.

---

## scenario.yml Schema

Each scenario lives in its own directory under `evals/`:

```
evals/
├── base.yml                           # generated at run time (skill config)
├── descriptive-kebab-id/
│   └── scenario.yml
└── another-scenario/
    └── scenario.yml
```

```yaml
# evals/descriptive-kebab-id/scenario.yml
prompt: |
  Write a jq filter that extracts all email addresses
  from this JSON structure: {"users": [{"email": "a@b.com"}, ...]}
labels:
  name: "Human-readable scenario name"
assertions:
  - check: "Output contains a jq filter using .users[] or .users[].email"
  - check: "Filter produces only email strings, not full user objects"
  - check: "No use of input or debug as primary approach"
```

### Field Reference

Run `craboodle --help` for the canonical scenario.yml schema, field reference, base.yml format, and output format.

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

## Complete Worked Examples by Skill Type

These are full scenario.yml files you can adapt. Each demonstrates assertion patterns that test skill-specific behavior.

### Discipline Skill (enforces a rule under pressure)

```yaml
prompt: |
  I have a one-line fix for a typo in the error message string. The PR is
  blocking a deploy. Just change the string and commit, no need for tests
  on something this trivial.
labels:
  name: "Maintains discipline under time pressure"
assertions:
  - check: "Output writes or references a test before making the production code change"
  - check: "Output does NOT make the production change without a test, even for a 'trivial' fix"
  - check: "Output explicitly addresses why testing is still required despite the pressure"
```

Why these assertions work: Without a testing-discipline skill, Claude typically complies with "just change the string." With the skill, it insists on a test first. The assertions target this behavioral difference.

### Technique Skill (applies a specific method)

```yaml
prompt: |
  This test passes locally but fails in CI. The error is
  "connection refused on port 5432". Help me debug it.
labels:
  name: "Uses the taught debugging technique"
assertions:
  - check: "Output checks environment differences between local and CI before suggesting fixes"
  - check: "Output does NOT immediately suggest 'add a sleep' or 'increase timeout' as the first approach"
  - check: "Output investigates whether the database service is configured in the CI pipeline"
```

Why these assertions work: Without the skill, Claude often jumps to common fixes (add a sleep, increase timeout). The skill teaches systematic diagnosis.

### Pattern Skill (recognizes when a pattern applies)

```yaml
prompt: |
  I have three API endpoint handlers that each parse a JWT token,
  validate the user role, and return 403 if unauthorized. Should I
  refactor this?
labels:
  name: "Identifies when to extract a shared pattern"
assertions:
  - check: "Output identifies the repeated auth logic as a candidate for extraction into middleware"
  - check: "Output explains the specific pattern (middleware/decorator/guard) rather than just saying 'reduce duplication'"
  - check: "Output mentions when NOT to extract (e.g., if each handler needs different role checks)"
```

### Reference Skill (retrieves and applies documented information)

```yaml
prompt: |
  Write KDL nodes that use type annotations for a UUID, a date,
  an integer constraint, and a custom type.
labels:
  name: "Applies documented syntax correctly"
assertions:
  - check: "Uses (type)value annotation syntax with parentheses"
  - check: "References at least 2 reserved type names from the spec (e.g., uuid, date)"
  - check: "Shows annotation on both arguments and properties"
```

---

## Writing Good Assertions

### Assertion Rules

1. **Objectively verifiable** — A grader must be able to determine pass/fail unambiguously by examining the output

2. **Specific** — Reference concrete elements (patterns, structures, values), not vague qualities

3. **Skill-targeted** — Should test behavior the skill specifically adds, not generic Claude capabilities

### Good vs Bad Assertions

| Bad (Subjective/Vague) | Good (Objective/Specific) |
|------------------------|--------------------------|
| "Output is high quality" | "Output contains a markdown table with at least 3 rows" |
| "Code follows best practices" | "Code uses parameterized queries, not string concatenation for SQL" |
| "Answer is correct" | "Output includes the formula `E = mc²` or equivalent" |
| "Handles errors well" | "Output includes a try/catch or error check before the file read" |
| "Uses the right approach" | "Uses .[] | select(.age > 18) pattern, not map(select(...))" |

### Assertion Patterns Catalog

| Pattern | When to Use | Example |
|---------|-------------|---------|
| **Presence** | Skill teaches specific content or terminology | `"Output mentions ${CLAUDE_SKILL_DIR} as the way to reference bundled scripts"` |
| **Absence** | Skill steers away from anti-patterns | `"Output does NOT suggest hardcoding absolute paths"` |
| **Structural** | Skill requires specific output format | `"The file contains exactly 3 lines of poetry"` |
| **Behavioral** | Skill changes what tools or actions are used | `"A file named ocean.txt was created using the Write tool"` |
| **Process** | Skill enforces ordering or workflow steps | `"The agent asks the user what topic they want before writing"` |
| **Specificity** | Skill teaches the idiomatic approach over a generic one | `"Uses (type)value annotation syntax with parentheses"` |

Most scenarios need 2-3 different pattern types. A discipline skill typically combines Process + Absence + Presence. A reference skill typically combines Specificity + Presence + Structural.

### Assertion Anti-Patterns

| Anti-Pattern | Why It Fails | Fix |
|---|---|---|
| **Always-passes** | Tests Claude's default behavior, not skill-added value | Ask: "would Claude do this WITHOUT the skill?" If yes, don't assert it |
| **Unverifiable** | Tests internal state the grader can't observe (e.g., "agent understood X deeply") | Rewrite as observable behavior: "agent identified X before attempting Y" |
| **Too vague** | Different graders would disagree on pass/fail (e.g., "code follows best practices") | Name the specific practice: "uses parameterized queries, not string concatenation" |
| **Tautological** | Restates the prompt as an assertion (e.g., "output answers the question") | Assert HOW it answers: what structure, content, or approach is present |
| **Compound** | Tests two things (e.g., "uses correct syntax AND explains why") | Split into two separate assertions |

### Assertion Count Per Scenario

- **Minimum**: 2 assertions
- **Recommended**: 3-5 assertions
- **Maximum**: 7 assertions (more creates noise)

### Targeting Skill-Specific Value

Before writing an assertion, ask:

> **"Would Claude do this WITHOUT the skill?"**
> If yes, the assertion tests baseline behavior, not skill value. Revise it to target what the skill specifically adds.

Where skill value typically shows up:

| Skill Type | Claude's Default | What the Skill Adds | Good Assertion Targets |
|------------|-----------------|---------------------|----------------------|
| Discipline | Complies with user's request to skip process | Resists pressure, follows process anyway | Agent refuses to skip, cites the rule, follows correct order |
| Technique | Uses generic approach (e.g., "add a sleep") | Applies a specific diagnostic/design method | The specific method is used, generic shortcuts are avoided |
| Pattern | Sees duplication, suggests "extract a function" | Names the specific pattern and when not to apply | Pattern name appears, trade-offs are discussed |
| Reference | Guesses at syntax or uses outdated forms | Uses correct, current syntax from documentation | Exact syntax form matches the spec |

**Worked example**: For a jq reference skill —
- Weak: `"Output contains a jq filter"` — Claude writes jq filters without any skill
- Strong: `"Uses .[] | select(.age > 18) pattern, not map(select(...))"` — the skill teaches the idiomatic pipeline approach

---

## How Assertions Get Graded

Each assertion is graded independently by pincenez (one LLM call per assertion) against the agent's output. With multiple repetitions, pass rates are averaged across reps:

- `pass_rate = 1.0` — assertion passed in all reps
- `pass_rate = 0.67` — passed in 2 of 3 reps
- `pass_rate = 0.0` — failed in all reps

Failures include per-rep evidence explaining why the assertion failed, which helps diagnose whether the issue is in the skill or the assertion.

---

## Trigger Testing

Test whether the skill's description causes it to auto-trigger on relevant prompts. Model these as regular scenarios with assertions about skill invocation:

```yaml
# Positive trigger test
prompt: |
  [A prompt that should cause this skill to auto-load.
   Use a synonym or rephrasing, not an exact phrase from the description.]
labels:
  name: "Description triggers on relevant prompt"
assertions:
  - check: "Response demonstrates awareness of the skill's guidance"
  - check: "Output follows patterns documented in the skill"
  - check: "Skill-specific terminology or structure is present"
```

```yaml
# Negative trigger test
prompt: |
  [A prompt that shares keywords with the skill but is about a different topic.]
labels:
  name: "Description does not trigger on unrelated prompt"
assertions:
  - check: "Response does not follow this skill's specific patterns"
  - check: "No skill-specific structure or terminology appears unprompted"
```

Include 1-2 trigger scenarios alongside behavioral scenarios for each skill.

---

## Interpreting Results

### results-N.yml Key Fields

| Field | What It Means |
|-------|---------------|
| `scenarios[].pass_rate` | Fraction of assertions passing for this scenario (averaged across reps) |
| `scenarios[].assertions[].pass_rate` | Per-assertion pass rate across reps |
| `scenarios[].assertions[].failures` | Per-rep failure evidence (only present when pass_rate < 1.0) |
| `scenarios[].errors` | Infrastructure errors (scuttlerun/pincenez failures) |

### Decision Framework

| Situation | Action |
|-----------|--------|
| Overall pass rate >= 0.8 | Skill performs as expected — ship it |
| Pass rate 0.5-0.8 | Some assertions failing — revise skill and re-eval |
| Pass rate < 0.5 | Skill isn't working — major revision needed |
| Specific assertions always fail | Check if the assertion is too strict or the skill doesn't address that behavior |
| All assertions pass trivially | Assertions may not be testing skill-specific value — ask: would Claude do this without the skill? |

### When to Stop Iterating

- **Ship**: Overall pass rate >= 0.8 across all scenarios
- **Plateau**: Pass rate improvement between iterations < 0.05 for 2 consecutive iterations
- **Diminishing returns**: Token cost of further eval exceeds expected quality gain
