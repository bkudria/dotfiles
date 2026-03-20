# Bulletproofing Skills Against Rationalization

Techniques for making discipline-enforcing skills resist agent rationalization under pressure.

---

## When to Bulletproof

Bulletproof skills that enforce rules agents are tempted to skip under pressure. Examples: TDD enforcement, verification-before-completion, design-before-coding.

Skills that are purely informational (API references, syntax guides) do not need bulletproofing.

---

## Core Principle

**Violating the letter of the rules IS violating the spirit of the rules.**

State this early in any discipline-enforcing skill. It cuts off an entire class of "I'm following the spirit" rationalizations.

---

## Techniques

### 1. Close Every Loophole Explicitly

Do not just state the rule — forbid specific workarounds discovered during baseline testing.

**Weak:**
```markdown
Write code before test? Delete it.
```

**Strong:**
```markdown
Write code before test? Delete it. Start over.

**No exceptions:**
- Not for "simple additions"
- Not for "just adding a section"
- Not for "documentation updates"
- Don't keep untested changes as "reference"
- Don't "adapt" while running tests
- Delete means delete
```

### 2. Build Rationalization Tables

Capture every excuse from testing. Every rationalization agents make goes in the table:

```markdown
| Excuse | Reality |
|--------|---------|
| "Too simple to test" | Simple code breaks. Test takes 30 seconds. |
| "I'll test after" | Tests passing immediately prove nothing. |
| "Tests after achieve same goals" | Tests-after = "what does this do?" Tests-first = "what should this do?" |
| "This is different because..." | It's not. Follow the process. |
```

Add new rows each time the REFACTOR phase discovers new rationalizations.

### 3. Create Red Flags Lists

Give agents a self-check for when they are about to rationalize:

```markdown
## Red Flags — STOP and Start Over

- Code before test
- "I already manually tested it"
- "Tests after achieve the same purpose"
- "It's about spirit not ritual"
- "This is different because..."
- "It's just a small change"

**All of these mean: Delete. Start over.**
```

### 4. Address "Spirit vs Letter" Arguments

Agents will argue they are following the "spirit" while violating specific rules. Counter this with:

```markdown
**Violating the letter of the rules is violating the spirit of the rules.**
```

Place this near the top of the skill, before any specific rules.

### 5. Update Description for Violation Symptoms

Add triggering conditions that fire when agents are ABOUT to violate the rule:

```yaml
description: Use when implementing any feature or bugfix, before writing implementation code
```

The phrase "before writing implementation code" activates the skill at the moment of greatest temptation.

---

## Persuasion Principles for Skill Design

Research on persuasion (Cialdini, 2021; Meincke et al., 2025) identifies principles that strengthen discipline-enforcing skills:

| Principle | Application to Skills |
|-----------|----------------------|
| **Authority** | Cite authoritative sources for the rule. "The official spec requires..." carries more weight than "You should..." |
| **Commitment/Consistency** | Once an agent acknowledges the rule, leverage that commitment. "As stated in the Iron Law above..." |
| **Scarcity** | Frame the cost of violation. "Untested skills waste hours. Testing takes minutes." |
| **Social proof** | Frame the practice as standard. "All skills in this collection follow TDD." |
| **Unity** | Frame shared identity. "As a skill author, maintain the quality standard." |

Use these principles to strengthen rule statements, not to manipulate. The goal is compliance with genuinely beneficial practices.

---

## REFACTOR Cycle

Bulletproofing is iterative:

1. Run pressure scenarios WITH the skill present
2. Look for new rationalizations the agent finds
3. Add explicit counters for each new rationalization
4. Update the rationalization table
5. Re-test until the agent complies under maximum pressure
6. Stop when no new rationalizations emerge across 2+ test runs
