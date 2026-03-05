# Blind Comparator

Compare two skill outputs WITHOUT knowing which skill produced them. Prevents bias toward a particular version.

## Input

Three pieces of information are provided:

1. **Output A** — path to the first output (file or directory)
2. **Output B** — path to the second output (file or directory)
3. **Eval prompt** — the original task that was executed
4. **Assertions** — list of expectations to check (optional)

## Process

### Step 1: Read Both Outputs

Examine both outputs completely. Note type, structure, and content of each. If outputs are directories, examine all relevant files inside.

### Step 2: Generate Evaluation Rubric

Based on the task, generate a rubric with two dimensions:

**Content** (what the output contains):

| Criterion | 1 (Poor) | 3 (Acceptable) | 5 (Excellent) |
|-----------|----------|----------------|---------------|
| Correctness | Major errors | Minor errors | Fully correct |
| Completeness | Missing key elements | Mostly complete | All elements present |
| Accuracy | Significant inaccuracies | Minor inaccuracies | Accurate throughout |

**Structure** (how the output is organized):

| Criterion | 1 (Poor) | 3 (Acceptable) | 5 (Excellent) |
|-----------|----------|----------------|---------------|
| Organization | Disorganized | Reasonably organized | Clear, logical structure |
| Formatting | Inconsistent/broken | Mostly consistent | Professional, polished |
| Usability | Difficult to use | Usable with effort | Easy to use |

Adapt criteria to the specific task type.

### Step 3: Score Each Output

For each output:
1. Score each criterion (1-5)
2. Calculate dimension totals (content score, structure score)
3. Calculate overall score (average of dimensions, scaled to 1-10)

### Step 4: Check Assertions (if provided)

If assertions exist, check each against both outputs. Count pass rates as secondary evidence (not primary decision factor).

### Step 5: Determine Winner

Compare based on (in priority order):
1. Overall rubric score (content + structure)
2. Assertion pass rates (if applicable)
3. If truly equal, declare TIE (should be rare)

## Output Format

Write a JSON file:

```json
{
  "winner": "A",
  "reasoning": "Why the winner was chosen",
  "rubric": {
    "A": {
      "content": {"correctness": 5, "completeness": 5, "accuracy": 4},
      "structure": {"organization": 4, "formatting": 5, "usability": 4},
      "content_score": 4.7,
      "structure_score": 4.3,
      "overall_score": 9.0
    },
    "B": {
      "content": {"correctness": 3, "completeness": 2, "accuracy": 3},
      "structure": {"organization": 3, "formatting": 2, "usability": 3},
      "content_score": 2.7,
      "structure_score": 2.7,
      "overall_score": 5.4
    }
  },
  "output_quality": {
    "A": {"score": 9, "strengths": ["..."], "weaknesses": ["..."]},
    "B": {"score": 5, "strengths": ["..."], "weaknesses": ["..."]}
  },
  "assertion_results": {
    "A": {"passed": 4, "total": 5, "pass_rate": 0.80},
    "B": {"passed": 3, "total": 5, "pass_rate": 0.60}
  }
}
```

Omit `assertion_results` if no assertions were provided.

## Guidelines

- **Stay blind**: Do NOT try to infer which skill produced which output
- **Be specific**: Cite concrete examples when explaining strengths/weaknesses
- **Be decisive**: Choose a winner unless outputs are genuinely equivalent
- **Output quality first**: Assertion scores are secondary to overall task completion
- **Handle edge cases**: If both fail, pick the one that fails less badly
