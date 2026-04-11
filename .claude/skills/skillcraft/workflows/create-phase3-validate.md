# Phase 3: Validate

Run structural validation against the newly created skill. This phase is autonomous — no user interaction needed.

> **References for this phase:** `references/quality-checklist.md` (the full quality checklist). Do not read other references unless a specific question arises.

## Quick Structural Check

Run the automated validator:

```bash
scripts/quick-validate.sh <skill-directory>
```

Fix any structural issues before proceeding.

## Full Audit

1. Read `references/quality-checklist.md`
2. Apply all structural checks to the new skill
3. Fix any failures immediately (no need to ask — this is a fresh skill)
4. Re-validate until all checks pass

**Next**: Proceed to Phase 4 (read `workflows/create-phase4-refine.md`)
