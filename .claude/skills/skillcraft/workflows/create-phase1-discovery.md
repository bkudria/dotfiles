# Phase 1: Discovery

Gather core information about the new skill. If `$ARGUMENTS` is provided, pre-fill the name/topic.

## Step 1: Concrete Examples First

Before jumping into metadata, ask the user to describe 2-3 concrete examples of how they'd use this skill. For example:

> "Give me 2-3 examples of when you'd want this skill to activate, or how you'd invoke it. Be specific — describe the situation and what you'd expect the skill to do."

Use these examples to:
- Derive a clear skill name and purpose
- Extract natural trigger phrases (how the user described the situation)
- Identify use cases and the skill type (knowledge vs workflow vs tool)
- Pre-fill as many interview fields as possible

## Step 2: Interview Form

```bash
~/.claude/skills/advanced-ask/scripts/ask-form.sh --inline '{
  "questions": [
    {"question": "Skill name (hyphen-case)?", "type": "input", "key": "name", "placeholder": "my-skill-name"},
    {"question": "One-sentence purpose?", "type": "input", "key": "purpose", "placeholder": "What does this skill do?"},
    {"question": "Where should it live?", "type": "choose", "key": "location", "options": ["~/.claude/skills/ (global)", ".claude/skills/ (project-local)"]},
    {"question": "Describe 2-3 use cases", "type": "write", "key": "use_cases", "placeholder": "When would someone need this skill?"},
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
- At least 2 use cases provided
- At least 3 trigger phrases

**Next**: Proceed to Phase 2 (read `workflows/create-phase2-design.md`)
