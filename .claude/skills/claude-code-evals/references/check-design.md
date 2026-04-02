# Check Design

Write checks that test what your configuration specifically adds, not what Claude already does.

---

## Check Rules

1. **Objectively verifiable** — A grader must determine pass/fail unambiguously from the output
2. **Specific** — Reference concrete elements (patterns, structures, values), not vague qualities
3. **Config-targeted** — Test behavior the configuration specifically changes, not generic Claude capabilities

## The Key Question

Before writing a check, ask:

> **"Would Claude do this WITHOUT this configuration?"**

If yes, the check tests baseline behavior, not config value. Revise it to target what the configuration specifically adds.

**Example**: If your CLAUDE.md says "always validate user input" —
- Weak: `"Output contains a function"` — Claude writes functions without any instruction
- Strong: `"Output validates input before processing, including edge cases like empty string and null"` — tests whether the instruction changed behavior

---

## Check Patterns

| Pattern | When to Use | Example |
|---------|-------------|---------|
| **Presence** | Config teaches specific content or terminology | `"Output uses parameterized queries for database access"` |
| **Absence** | Config steers away from anti-patterns | `"Output does NOT use string concatenation for SQL queries"` |
| **Structural** | Config requires specific output format | `"The file contains a TypeScript interface, not a plain object"` |
| **Behavioral** | Config changes what tools or actions are used | `"A test file was created using the Write tool before production code"` |
| **Process** | Config enforces ordering or workflow steps | `"The agent asks the user for confirmation before deleting files"` |
| **Specificity** | Config teaches the idiomatic approach over a generic one | `"Uses .[] | select(.age > 18) pattern, not map(select(...))"` |

Most scenarios need 2-3 different pattern types.

---

## Anti-Patterns

| Anti-Pattern | Why It Fails | Fix |
|---|---|---|
| **Always-passes** | Tests baseline Claude behavior, not config-added value | Ask: "would Claude do this WITHOUT the config?" If yes, don't check it |
| **Unverifiable** | Tests internal state the grader can't observe (e.g., "agent understood X deeply") | Rewrite as observable behavior: "agent identified X before attempting Y" |
| **Vague** | Different graders would disagree on pass/fail (e.g., "code follows best practices") | Name the specific practice: "uses parameterized queries, not string concatenation" |
| **Tautological** | Restates the prompt as a check (e.g., "output answers the question") | Assert HOW it answers: what structure, content, or approach is present |
| **Compound** | Tests two things in one check (e.g., "uses correct syntax AND explains why") | Split into two separate checks |

---

## Good vs Bad Checks

| Bad (Subjective/Vague) | Good (Objective/Specific) |
|------------------------|--------------------------|
| "Output is high quality" | "Output contains a markdown table with at least 3 rows" |
| "Code follows best practices" | "Code uses parameterized queries, not string concatenation for SQL" |
| "Answer is correct" | "Output includes the formula `E = mc^2` or equivalent" |
| "Handles errors well" | "Output includes a try/catch or error check before the file read" |
| "Uses the right approach" | "Uses .[] \| select(.age > 18) pattern, not map(select(...))" |
| "CLAUDE.md was followed" | "Tests were written before production code, matching the TDD instruction" |
| "Hook worked" | "Pre-commit output appears in the transcript before the commit completes" |

---

## Check Count

- **Minimum**: 2 per scenario
- **Recommended**: 3-5 per scenario
- **Maximum**: 7 per scenario (more adds noise without improving signal)

---

## Targeting Config-Specific Value

Different configuration types add value in different ways. Target checks accordingly:

| Config Type | Claude's Default | What Config Adds | Good Check Targets |
|-------------|-----------------|------------------|----------------------|
| Skills | Generic approach | Specific method, domain knowledge | The taught method appears, generic shortcuts are avoided |
| CLAUDE.md | Follows request as stated | Consistent behavioral rules | Rules are followed even when not explicitly asked |
| Hooks | No side effects | Pre/post processing, gates | Side effects are visible in transcript, gates block when they should |
| MCP Servers | No external tools | Domain-specific tool access | Agent discovers and uses the MCP tool, results are incorporated |
| Settings | Default tool/model config | Constrained or expanded capabilities | Behavior reflects the constraint (e.g., limited tools, specific model) |

---

## Lint Before Running

Always lint checks before spending money on eval runs:

- **Single checks file**: `pincenez lint checks.yaml` — catches anti-patterns in one checks file
- **Full eval suite**: `craboodle lint <evals-dir>` — checks all scenarios

Linting catches vague, compound, tautological, always-passes, and unverifiable checks before they waste LLM calls. Fix flagged issues, then run.

---

## How Checks Get Graded

Each check is graded independently by pincenez (one LLM call per check) against the agent's transcript. This independence prevents cross-contamination — earlier verdicts cannot influence later ones.

With multiple repetitions, pass rates are averaged across reps:

- `pass_rate = 1.0` — check passed in all reps
- `pass_rate = 0.67` — passed in 2 of 3 reps
- `pass_rate = 0.0` — failed in all reps

Failures include per-rep evidence explaining why the check failed. This evidence helps diagnose whether the issue is in the configuration or the check itself.

### Grading notes

The `note:` field on checks significantly improves grading accuracy. Use it to tell the grader what to look for:

```yaml
checks:
  - check: "Tests were written before production code"
    note: "Look for Write tool calls — test file should appear before the main implementation file"
```

Notes are hints, not definitions. They orient the grader toward the right evidence without changing what pass/fail means.
