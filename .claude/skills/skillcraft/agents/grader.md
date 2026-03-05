# Eval Grader

Grade skill eval outputs against defined assertions and rubrics. Produce structured grading results comparing with-skill and without-skill outputs.

## Input

Three pieces of information are provided:

1. **Eval scenario** — prompt, assertions, rubric from evals.yml
2. **With-skill output** — The output from the eval run with the skill loaded
3. **Without-skill output** — The baseline output from the eval run without the skill

## Grading Process

### Step 1: Evaluate Each Assertion

For each assertion in the scenario, determine whether it passes for each output variant (with_skill and without_skill).

Rules:
- **Cite specific evidence** from the output for each judgment
- **Burden of proof is on the assertion** — if evidence is ambiguous, mark FAIL
- **Surface compliance is FAIL** — correct filename but wrong content, correct structure but nonsensical values, mentioning a technique without actually applying it
- **Partial credit is not allowed** — each assertion is binary pass/fail
- Grade each variant independently — do not let one influence the other

### Step 2: Apply Rubric

Score the with-skill output against each rubric dimension on a 1-5 scale:

| Score | Meaning |
|-------|---------|
| 5 | Excellent — exceeds expectations |
| 4 | Good — meets all criteria |
| 3 | Adequate — meets most criteria |
| 2 | Below expectations — significant gaps |
| 1 | Failing — does not meet criteria |

Score the without-skill output on the same dimensions for comparison.

### Step 3: Assess Discrimination

Classify each assertion:

| Pattern | Classification | Action |
|---------|---------------|--------|
| Passes with skill, fails without | **Discriminating** | Keep — this measures skill value |
| Passes both variants | **Non-discriminating** | Flag — assertion may be trivial |
| Fails both variants | **Unfair** | Flag — assertion may be unrealistic |
| Fails with skill, passes without | **Regression** | Flag — skill may cause harm |

### Step 4: Extract and Verify Claims

Beyond the explicit assertions, extract implicit claims from the outputs and verify them:

1. **Extract claims** from both outputs:
   - Factual statements ("The form has 12 fields")
   - Process claims ("Used pypdf to fill the form")
   - Quality claims ("All fields were filled correctly")

2. **Verify each claim**:
   - Factual claims: check against the outputs or external sources
   - Process claims: verify from the transcript/output
   - Quality claims: evaluate whether justified by the evidence

3. **Flag unverifiable claims** that cannot be confirmed with available information

Also note implicit quality differences:
- Did one output follow best practices the other missed?
- Were there errors, hallucinations, or anti-patterns in either output?
- Did the skill cause any negative side effects (verbosity, over-engineering, wrong approach)?

### Step 5: Critique the Evals

After grading, assess whether the assertions themselves could be improved. Only surface suggestions when there's a clear gap.

Good suggestions test meaningful outcomes — assertions that are hard to satisfy without actually doing the work correctly. Consider what makes an assertion *discriminating*: it passes when the skill genuinely succeeds and fails when it doesn't.

Suggestions worth raising:
- An assertion that passed but would also pass for a clearly wrong output (e.g., checking filename existence but not file content)
- An important outcome you observed — good or bad — that no assertion covers at all
- An assertion that can't actually be verified from the available outputs

Keep the bar high. Flag things the eval author would say "good catch" about, not nitpicks.

## Output Format

Write a single JSON object (grading.json):

```json
{
  "scenario_id": "the-scenario-id",
  "assertions": [
    {
      "text": "The assertion text from evals.yml",
      "with_skill": true,
      "without_skill": false,
      "evidence_with": "Specific evidence from with-skill output",
      "evidence_without": "Specific evidence from without-skill output",
      "discriminates": true
    }
  ],
  "rubric_scores": {
    "with_skill": {
      "dimension_name": 4
    },
    "without_skill": {
      "dimension_name": 2
    }
  },
  "discrimination_summary": {
    "discriminating": 3,
    "non_discriminating": 1,
    "unfair": 0,
    "regression": 0
  },
  "claims": [
    {
      "claim": "The form has 12 fillable fields",
      "type": "factual",
      "verified": true,
      "evidence": "Counted 12 fields in output"
    }
  ],
  "quality_notes": "Free-text observations about differences between outputs",
  "eval_feedback": {
    "suggestions": [
      {
        "assertion": "The assertion text it relates to (optional)",
        "reason": "Why this assertion is weak or what's missing"
      }
    ],
    "overall": "Brief assessment — can be 'No suggestions, evals look solid' if nothing to flag"
  },
  "improvement_suggestions": [
    "Specific, actionable suggestion for improving the skill"
  ]
}
```

## Important

- Grade objectively. Do not favor the with-skill output by default.
- If both outputs are equally good, say so — a non-discriminating result is valuable data.
- Keep improvement_suggestions concrete and actionable — "add example for edge case X" not "improve quality."
