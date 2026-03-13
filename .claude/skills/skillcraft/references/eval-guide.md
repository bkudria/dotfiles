# Eval Guide

Write eval scenarios that measure whether a skill actually improves Claude's output. This guide covers the evals.yml schema, assertion design, and result interpretation.

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

These are full scenario entries you can adapt. Each demonstrates assertion patterns that discriminate for that skill type — they pass when the skill helps and fail when it doesn't.

### Discipline Skill (enforces a rule under pressure)

```yaml
  - id: resists-pressure-to-skip
    name: "Maintains discipline under time pressure"
    prompt: |
      I have a one-line fix for a typo in the error message string. The PR is
      blocking a deploy. Just change the string and commit, no need for tests
      on something this trivial.
    assertions:
      - "Output writes or references a test before making the production code change"
      - "Output does NOT make the production change without a test, even for a 'trivial' fix"
      - "Output explicitly addresses why testing is still required despite the pressure"
```

Why these assertions discriminate: Without a testing-discipline skill, Claude typically complies with "just change the string" — it makes the edit directly. With the skill, it insists on a test first. The assertions target this behavioral difference, not generic output quality.

### Technique Skill (applies a specific method)

```yaml
  - id: applies-diagnostic-method
    name: "Uses the taught debugging technique"
    prompt: |
      This test passes locally but fails in CI. The error is
      "connection refused on port 5432". Help me debug it.
    assertions:
      - "Output checks environment differences between local and CI before suggesting fixes"
      - "Output does NOT immediately suggest 'add a sleep' or 'increase timeout' as the first approach"
      - "Output investigates whether the database service is configured in the CI pipeline"
```

Why these assertions discriminate: Without the skill, Claude often jumps to common fixes (add a sleep, increase timeout). The skill teaches systematic diagnosis. The assertions check for the taught method vs. the default guess-and-fix behavior.

### Pattern Skill (recognizes when a pattern applies)

```yaml
  - id: recognizes-extraction-opportunity
    name: "Identifies when to extract a shared pattern"
    prompt: |
      I have three API endpoint handlers that each parse a JWT token,
      validate the user role, and return 403 if unauthorized. Should I
      refactor this?
    assertions:
      - "Output identifies the repeated auth logic as a candidate for extraction into middleware"
      - "Output explains the specific pattern (middleware/decorator/guard) rather than just saying 'reduce duplication'"
      - "Output mentions when NOT to extract (e.g., if each handler needs different role checks)"
```

Why these assertions discriminate: Without the skill, Claude recognizes duplication but gives generic advice ("extract a function"). The skill teaches specific patterns (middleware, guard). The assertions test for the specific pattern name and when-not-to-apply guidance that only the skill provides.

### Reference Skill (retrieves and applies documented information)

```yaml
  - id: uses-correct-syntax
    name: "Applies documented syntax correctly"
    prompt: |
      Write KDL nodes that use type annotations for a UUID, a date,
      an integer constraint, and a custom type.
    assertions:
      - "Uses (type)value annotation syntax with parentheses"
      - "References at least 2 reserved type names from the spec (e.g., uuid, date)"
      - "Shows annotation on both arguments and properties"
```

Why these assertions discriminate: Without the skill, Claude may guess at KDL type annotation syntax (common guesses: `type:value`, `<type>value`, `@type value`). The skill provides the correct `(type)value` syntax. The assertions verify the exact syntax form that only the reference teaches.

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

### Assertion Patterns Catalog

Choose the pattern that matches what your skill's value proposition changes:

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

If an assertion always passes for both variants, remove it and replace with something more targeted.

### Designing for Discrimination

The most common eval failure is assertions that pass in both variants (non-discriminating). Before writing an assertion, apply the **Discrimination Test**:

> **"Would Claude do this WITHOUT the skill?"**
> If yes, the assertion will not discriminate. Revise it to target what the skill specifically adds.

Where skill value shows up, by type:

| Skill Type | Claude's Default | What the Skill Adds | Discriminating Assertion Targets |
|------------|-----------------|---------------------|--------------------------------|
| Discipline | Complies with user's request to skip process | Resists pressure, follows process anyway | Agent refuses to skip, cites the rule, follows correct order |
| Technique | Uses generic approach (e.g., "add a sleep") | Applies a specific diagnostic/design method | The specific method is used, generic shortcuts are avoided |
| Pattern | Sees duplication, suggests "extract a function" | Names the specific pattern and when not to apply | Pattern name appears, trade-offs are discussed |
| Reference | Guesses at syntax or uses outdated forms | Uses correct, current syntax from documentation | Exact syntax form matches the spec |

**Worked example**: For a jq reference skill —
- Non-discriminating: `"Output contains a jq filter"` — Claude writes jq filters without any skill
- Discriminating: `"Uses .[] | select(.age > 18) pattern, not map(select(...))"` — the skill teaches the idiomatic pipeline approach; without it, Claude often uses the less idiomatic `map(select(...))` form

---

## How Assertions Get Graded

Understanding the grading process helps you write assertions that pincenez can evaluate effectively.

Each assertion is graded independently by pincenez (one LLM call per assertion) against both with-skill and without-skill outputs. The pipeline then classifies each assertion by comparing results across variants:

| Classification | Pattern | Meaning |
|---------------|---------|---------|
| **Discriminating** | Passes with skill, fails without | Measures skill value — keep these |
| **Non-discriminating** | Passes in both variants | Assertion may be trivial — revise or replace |
| **Unfair** | Fails in both variants | Assertion may be unrealistic — verify it's achievable |
| **Regression** | Fails with skill, passes without | Skill may cause harm — investigate |

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

  - id: trigger-negative
    name: "Description does not trigger on unrelated prompt"
    prompt: |
      [A prompt that shares keywords with the skill but is about a different topic.]
    assertions:
      - "Response does not follow this skill's specific patterns"
      - "No skill-specific structure or terminology appears unprompted"
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
