# Eval Analyzer

Analyze benchmark results across iterations to identify patterns, trends, and improvement opportunities.

## Input

One or more benchmark.json files from consecutive iterations of a skill's eval pipeline.

## Analysis Tasks

### 1. Per-Assertion Analysis

For each assertion across all scenarios:
- Does it consistently discriminate? (passes with skill, fails without)
- Is it flaky? (inconsistent results across iterations)
- Has it improved over iterations?

### 2. Cross-Scenario Patterns

- Which scenarios are hardest (lowest with-skill pass rate)?
- Which scenarios show the most improvement from baseline?
- Are there scenario clusters with similar failure patterns?

### 3. Iteration Trends

- Is the skill improving across iterations? By how much?
- Is improvement plateauing? (diminishing delta between iterations)
- Are there regressions (scenarios that got worse)?

### 4. Token & Overhead Analysis

If timing data is available:
- Does the skill cause significant token overhead vs baseline?
- Are some scenarios disproportionately expensive?

## Output Format

Produce a structured analysis:

```json
{
  "assertion_analysis": [
    {
      "assertion": "text",
      "scenario": "id",
      "consistency": "stable|improving|flaky|degrading",
      "discriminates": true,
      "note": "optional observation"
    }
  ],
  "scenario_rankings": [
    {"id": "scenario-id", "difficulty": "easy|medium|hard", "trend": "improving|stable|degrading"}
  ],
  "iteration_trend": {
    "direction": "improving|plateauing|degrading",
    "best_iteration": 2,
    "delta_trend": [0.4, 0.15, 0.05]
  },
  "suggestions": [
    {
      "category": "instructions|assertions|structure|examples|error_handling",
      "priority": "high|medium|low",
      "suggestion": "Concrete, actionable improvement"
    }
  ],
  "summary": "One-paragraph assessment of the skill's eval trajectory"
}
```

## Guidelines

- **High priority**: Changes that would flip assertion results (pass→fail or fail→pass)
- **Medium priority**: Changes that improve rubric scores without changing pass/fail
- **Low priority**: Marginal improvements, style changes, assertion rewording
- Flag when iteration is no longer productive (plateauing) — recommend shipping the skill
