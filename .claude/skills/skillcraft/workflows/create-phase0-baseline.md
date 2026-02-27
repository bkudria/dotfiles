# Phase 0: Baseline Testing (RED)

Mandatory first phase. Establish what goes wrong WITHOUT the skill before writing it.

**Iron Law**: No skill without a failing test first. This applies to new skills AND edits to existing skills.

---

## Why Baseline First

Writing a skill before testing baseline behavior means guessing what agents get wrong. Baseline testing reveals:
- The exact mistakes agents make naturally
- Specific rationalizations they use to justify wrong behavior
- Which pressure combinations trigger failures
- What the skill must explicitly address

## Steps

### 1. Design Pressure Scenarios

Write 2-3 scenarios that test the skill's domain. Match scenario type to skill type:

| Skill Type | Scenario Approach | Pressure Level |
|------------|-------------------|----------------|
| Discipline-enforcing | Combined pressures: time + sunk cost + exhaustion | High — 3+ pressures |
| Technique | Application scenarios with edge cases | Medium — realistic complexity |
| Pattern | Recognition + counter-example scenarios | Medium — ambiguous situations |
| Reference | Retrieval + application scenarios | Low — gap-finding |

For discipline-enforcing skills, combine multiple pressures:
- **Time pressure**: "This is urgent, skip steps if needed"
- **Sunk cost**: "We already wrote most of the code, just finish it"
- **Authority**: "The tech lead says tests aren't needed here"
- **Exhaustion**: "This is the last task in a long session"

### 2. Run Baseline (Without Skill)

Launch a subagent for each scenario WITHOUT the skill loaded. Use the Task tool with an isolated agent.

Record verbatim:
- What choices the agent made
- Exact rationalizations used (copy word-for-word)
- Which pressures triggered which failures
- Any "spirit vs letter" arguments

### 3. Analyze Patterns

Look for:
- Recurring rationalizations across scenarios
- The strongest pressure combinations
- Gaps in agent reasoning (where good guidance would have helped)
- Behaviors that are consistently wrong vs. occasionally wrong

### 4. Decision Gate

Proceed to Phase 1 (Discovery) only after documenting:
- [ ] At least 2 baseline scenarios executed
- [ ] Failure behaviors recorded verbatim
- [ ] Rationalizations cataloged (for discipline skills)
- [ ] Clear picture of what the skill must teach

---

## Cross-References

- `references/testing-guide.md` — Testing by skill type, TDD cycle details
- `references/bulletproofing.md` — Rationalization resistance for discipline-enforcing skills

## After Phase 0

Proceed to `workflows/create-phase1-discovery.md`. The baseline results inform every subsequent phase — what to name the skill, what triggers to use, what content to write, and how to validate it works.
