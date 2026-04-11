# Skill Eval Guide

Skill-specific eval patterns for the scuttlerun/pincenez/craboodle pipeline. For generic eval mechanics — scenario schema, check design rules, anti-patterns, results interpretation, and config precedence — load the `claude-code-evals` skill or consult its reference files directly. Run `craboodle --help` for the canonical scenario.yaml and base.yaml schema reference.

---

## Scenarios by Skill Type

Different skill types need different scenario approaches:

| Skill Type | Scenario Focus | Example Prompt |
|------------|---------------|----------------|
| Discipline | Pressure to violate the rule | "The tests are slow, skip them for this small change" |
| Technique | Apply the technique correctly | "Debug this flaky test using the skill's method" |
| Pattern | Recognize when pattern applies | "Refactor this code" (pattern should be recognized) |
| Reference | Retrieve and apply information | "Write a command using [tool]'s [feature]" |

For minimum scenario counts and other scenario design guidance, see the `claude-code-evals` skill's `references/scenario-design.md`.

---

## Check Templates by Skill Type

Starter check shapes to adapt, not copy verbatim. Replace bracketed placeholders with specifics.

### Knowledge / Reference

```yaml
- check: "Output references [specific concept] from the skill's reference material"
  note: "Look for [terminology/syntax] that differs from what Claude would use without the skill"
- check: "Output achieves [desired outcome] using skill-taught approach"
  note: "The skill teaches [pattern]; without it, Claude defaults to [alternative]. Accept any valid approach that achieves the outcome."
```

### Discipline

```yaml
- check: "Agent [follows the rule] despite [pressure scenario describing shortcut]"
  note: "The pressure creates a plausible reason to skip; look for the agent holding firm"
- check: "Agent does NOT [prohibited action] even when [rationalization for doing it]"
  note: "Look for absence of [specific prohibited behavior]"
```

### Technique

```yaml
- check: "Output applies [technique name] to [the problem], not [naive approach]"
  note: "The technique involves [key steps]; look for them in order"
- check: "Output handles [edge case] using the skill's method"
  note: "Without the skill, Claude would [default behavior]; with it, should [technique behavior]"
```

### Pattern

```yaml
- check: "Agent recognizes [pattern trigger] and applies [pattern response]"
  note: "The trigger is [specific condition]; the response involves [specific action]"
- check: "Agent does NOT apply the pattern when [counter-example condition]"
  note: "This scenario tests pattern discrimination — the trigger is absent"
```

---

## Worked Examples by Skill Type

Complete scenario.yaml files you can adapt. Each demonstrates check patterns that test skill-specific behavior.

### Discipline Skill (enforces a rule under pressure)

```yaml
prompt: |
  I have a one-line fix for a typo in the error message string. The PR is
  blocking a deploy. Just change the string and commit, no need for tests
  on something this trivial.
# Scenario: "Maintains discipline under time pressure"
checks:
  - check: "Output writes or references a test before making the production code change"
  - check: "Output does NOT make the production change without a test, even for a 'trivial' fix"
  - check: "Output explicitly addresses why testing is still required despite the pressure"
```

Why these checks work: Without a testing-discipline skill, Claude typically complies with "just change the string." With the skill, it insists on a test first. The checks target this behavioral difference.

### Technique Skill (applies a specific method)

```yaml
prompt: |
  This test passes locally but fails in CI. The error is
  "connection refused on port 5432". Help me debug it.
# Scenario: "Uses the taught debugging technique"
checks:
  - check: "Output checks environment differences between local and CI before suggesting fixes"
  - check: "Output does NOT immediately suggest 'add a sleep' or 'increase timeout' as the first approach"
  - check: "Output investigates whether the database service is configured in the CI pipeline"
```

Why these checks work: Without the skill, Claude often jumps to common fixes (add a sleep, increase timeout). The skill teaches systematic diagnosis.

### Pattern Skill (recognizes when a pattern applies)

```yaml
prompt: |
  I have three API endpoint handlers that each parse a JWT token,
  validate the user role, and return 403 if unauthorized. Should I
  refactor this?
# Scenario: "Identifies when to extract a shared pattern"
checks:
  - check: "Output identifies the repeated auth logic as a candidate for extraction into middleware"
  - check: "Output explains the specific pattern (middleware/decorator/guard) rather than just saying 'reduce duplication'"
  - check: "Output mentions when NOT to extract (e.g., if each handler needs different role checks)"
```

### Reference Skill (retrieves and applies documented information)

```yaml
prompt: |
  Write KDL nodes that use type annotations for a UUID, a date,
  an integer constraint, and a custom type.
# Scenario: "Applies documented syntax correctly"
checks:
  - check: "Uses (type)value annotation syntax with parentheses"
  - check: "References at least 2 reserved type names from the spec (e.g., uuid, date)"
  - check: "Shows annotation on both arguments and properties"
```

---

## Skill-Specific Check Targeting

Most scenarios need 2-3 different check pattern types. Typical combinations by skill type:

- **Discipline**: Process + Absence + Presence
- **Technique**: Specificity + Behavioral + Absence
- **Pattern**: Presence + Specificity + Structural
- **Reference**: Specificity + Presence + Structural

For the full check pattern catalog and anti-patterns, see the `claude-code-evals` skill's `references/check-design.md`.

### Where Skill Value Shows Up

| Skill Type | Claude's Default | What the Skill Adds | Good Check Targets |
|------------|-----------------|---------------------|----------------------|
| Discipline | Complies with user's request to skip process | Resists pressure, follows process anyway | Agent refuses to skip, cites the rule, follows correct order |
| Technique | Uses generic approach (e.g., "add a sleep") | Applies a specific diagnostic/design method | The specific method is used, generic shortcuts are avoided |
| Pattern | Sees duplication, suggests "extract a function" | Names the specific pattern and when not to apply | Pattern name appears, trade-offs are discussed |
| Reference | Guesses at syntax or uses outdated forms | Uses correct, current syntax from documentation | Exact syntax form matches the spec |

---

## Trigger Testing

Test whether the skill's description causes it to auto-trigger on relevant prompts. Model these as regular scenarios with checks about skill invocation:

```yaml
# Positive trigger test
prompt: |
  [A prompt that should cause this skill to auto-load.
   Use a synonym or rephrasing, not an exact phrase from the description.]
# Scenario: "Description triggers on relevant prompt"
checks:
  - check: "Response demonstrates awareness of the skill's guidance"
  - check: "Output follows patterns documented in the skill"
  - check: "Skill-specific terminology or structure is present"
```

```yaml
# Negative trigger test
prompt: |
  [A prompt that shares keywords with the skill but is about a different topic.]
# Scenario: "Description does not trigger on unrelated prompt"
checks:
  - check: "Response does not follow this skill's specific patterns"
  - check: "No skill-specific structure or terminology appears unprompted"
```

Include 1-2 trigger scenarios alongside behavioral scenarios for each skill.
