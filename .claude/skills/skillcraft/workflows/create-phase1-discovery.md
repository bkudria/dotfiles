# Phase 1: Discovery

Gather requirements for the new skill. If `$ARGUMENTS` is provided, pre-fill the name/topic.

> **References for this phase:** `references/naming-conventions.md` (for name validation). Do not read other references unless a specific question arises.

## Step 1: Concrete Examples

Ask the user to describe 2-3 concrete examples of how they'd use this skill:

> "Give me 2-3 examples of when you'd want this skill to activate, or how you'd invoke it. Be specific — describe the situation and what you'd expect the skill to do."

Use these examples to:
- Derive a clear skill name and purpose
- Extract natural trigger phrases
- Identify use cases
- Infer skill type (knowledge, workflow, tool, hybrid) — do NOT ask the user to classify

## Step 2: Success Criteria

Ask the user what "good" looks like:

> "What does good output look like when this skill is working? Describe 2-3 examples of the skill performing correctly — what should change about Claude's behavior?"

Optionally, ask about anti-patterns:

> "Is there anything the skill should NOT do, or behaviors to avoid?"

These answers become eval assertions in Phase 2.

## Step 3: Interview Form

```bash
~/.claude/skills/advanced-ask/scripts/ask-form.sh --inline '{
  "questions": [
    {"question": "Skill name (hyphen-case)?", "type": "input", "key": "name", "placeholder": "my-skill-name"},
    {"question": "One-sentence purpose?", "type": "input", "key": "purpose", "placeholder": "What does this skill do?"},
    {"question": "Where should it live?", "type": "choose", "key": "location", "options": ["~/.claude/skills/ (global)", ".claude/skills/ (project-local)"]},
    {"question": "Trigger phrases (comma-separated)", "type": "input", "key": "triggers", "placeholder": "editing Dockerfiles, debugging containers, building images"}
  ]
}'
```

## Pre-fill from Arguments

If `$ARGUMENTS` is non-empty:
- If it looks like a hyphen-case name, pre-fill the name field
- If it's a topic phrase, pre-fill both name (converted to hyphen-case) and purpose
- Still run the interview for remaining fields

## Validation

Before proceeding, verify:
- Name is hyphen-case, ≤64 characters (see `references/naming-conventions.md`)
- Purpose is a clear single sentence
- At least 2 concrete use cases collected
- At least 3 trigger phrases
- At least 2 success criteria (these become eval assertions)

## Output

Summarize before proceeding:
1. **Requirements**: name, purpose, use cases, triggers
2. **Success criteria**: what good output looks like (→ eval assertions)
3. **Anti-patterns**: what to avoid (→ absence assertions)
4. **Inferred design decisions**: skill type, resource needs, invocation strategy

**Next**: Proceed to Phase 2 (read `workflows/create-phase2-implement.md`)
