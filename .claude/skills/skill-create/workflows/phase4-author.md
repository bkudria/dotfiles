# Phase 4: Author

Write content collaboratively, section by section.

## For Each Section

1. Show the section heading and its purpose (from the template)
2. Draft initial content using Phase 1 data (use cases, triggers)
3. Present the draft and ask for approval or edits

## Key Sections to Author

| Section | Source Data | Guidance |
|---------|-----------|----------|
| Introduction | Purpose | One paragraph, imperative voice |
| When to Use | Use cases + triggers | Bullet list of scenarios |
| Quick Reference | Skill type | Table summarizing key commands/info |
| Workflow/Usage | Use cases | Step-by-step with code examples |
| Reference Files | Selected resources | Table linking to references/ |

## Writing Guidelines

Follow `references/writing-style.md` for all content. Key rules:

- Imperative voice throughout ("Run the command", not "You should run")
- Tables for structured data (commands, flags, options)
- Realistic code examples from the use cases — no foo/bar
- Keep SKILL.md ≤300 lines; move detail to references/
- Link to every file in references/ and scripts/

## Reference Files

If references/ was selected, create focused reference files:
- One topic per file
- Named descriptively (`api-reference.md`, not `ref1.md`)
- Each starts with a title heading matching its topic

See `references/resource-usage.md` for detailed guidance on organizing reference, script, and asset files.

## Dependencies Section

If the skill uses external tools or other skills, add a Dependencies section. See `references/dependencies.md` for format and best practices.

## Interactive Elements

If the skill uses interactive TUI prompts, see `references/interactive-tui.md` for integration patterns.

**Next**: Proceed to Phase 5 (read `workflows/phase5-validate.md`)
