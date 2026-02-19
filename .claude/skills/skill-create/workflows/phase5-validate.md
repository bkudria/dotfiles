# Phase 5: Validate

Run the quality checklist from skill-improve against the newly created skill.

## Quick Structural Check

First, run the automated validator for fast structural checks:

```bash
~/.claude/skills/skill-improve/scripts/quick-validate.sh <skill-directory>
```

Fix any structural issues before proceeding to the full audit.

## Full Audit

1. Read `~/.claude/skills/skill-improve/references/quality-checklist.md`
2. Apply all 32 checks to the new skill
3. For any failures, fix immediately (no need to ask — this is a fresh skill)
4. Re-validate until all checks pass

## Final Report

Present a summary:

```
## Created: {skill-name}

Location: {path}
Type: {skill-type}
Files: {count}

Quality: {score}/32 checks passed

### Files Created
- SKILL.md (N lines)
- references/foo.md (N lines)
- scripts/bar.sh

### Next Steps
- Test with: /skill-name
- Improve later with: /skill-improve path/to/skill
```

See `references/testing-guide.md` (in skill-improve) for how to test the skill after creation.
