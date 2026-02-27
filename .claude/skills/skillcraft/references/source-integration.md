# Source Integration Reference

Templates, frameworks, and checklists for integrating content from external sources into existing skills.

---

## Gap Analysis Template

For each topic in the new source, fill in this table:

| Topic | Coverage | Existing File | Quality Comparison | Recommended Action |
|-------|----------|---------------|--------------------|--------------------|
| *Topic name* | `fully covered` / `partially covered` / `not covered` | Path or `—` | See options below | `merge` / `new file` / `skip` |

**Coverage statuses:**

| Status | Meaning |
|--------|---------|
| `fully covered` | Existing skill already addresses this topic completely |
| `partially covered` | Existing skill touches this topic but new source adds material |
| `not covered` | New content with no existing equivalent |

**Quality comparison options:**

- **Redundant** — New source says the same thing. Likely skip.
- **Different angle** — Both cover it but from different perspectives. Consider merging the unique angle.
- **New source better** — New source is clearer, more complete, or more accurate. Consider replacing or heavily supplementing.
- **Complementary** — New source adds material that pairs well with existing content. Merge.

---

## Placement Decision Framework

Use these heuristics to decide where integrated content belongs:

| Situation | Placement |
|-----------|-----------|
| New concept with no existing home | New reference file in `references/` |
| Overlaps existing file by >50% | Merge into that file |
| New workflow or multi-step process | New or modified workflow file in `workflows/` |
| New validation check | Add to `references/quality-checklist.md` |
| New anti-pattern | Add to `references/anti-patterns.md` |
| New writing guidance | Add to `references/writing-style.md` |
| New frontmatter field documentation | Add to `references/frontmatter-reference.md` |
| New naming/description guidance | Add to `references/naming-conventions.md` |
| New template or skill type | Add to `references/skill-templates.md` |

When in doubt, prefer merging into an existing file over creating a new one. New files add navigation overhead.

---

## Provenance-Aware Editing

When modifying a file during integration, update its `curation_decisions` entry in `provenance.yml` as part of the same editing step.

### Adding a Source to an Existing Entry

If the file already has curation_decisions entries, append a new entry to its list:

```yaml
  references/example.md:
    - source: existing-source        # existing entry — leave as-is
      sections: ["*"]
      decision: kept
      rationale: Original content from existing source.
    - source: new-source-id          # new entry — add this
      sections: ["Section-Name"]
      decision: synthesized
      rationale: >-
        Added X from new source. Merged with existing Y content.
```

### Creating a New Entry

For newly created files:

```yaml
  references/new-file.md:
    - source: new-source-id
      sections: ["*"]
      decision: <kept|simplified|altered|synthesized>
      rationale: >-
        Description of what was taken and how it was adapted.
```

For new files combining content from the new source with original material:

```yaml
  references/new-file.md:
    - source: new-source-id
      sections: ["Section-A", "Section-B"]
      decision: synthesized
      rationale: >-
        Combined source sections A and B with original framing.
    - decision: original
      rationale: Original organizational structure and connecting material.
```

---

## Content Deduplication Checklist

After completing integration, verify:

1. **No concept duplication** — Search for key terms from the integrated content across all skill files. If a concept appears in two files, consolidate into one and cross-reference from the other.
2. **Bidirectional cross-references** — If file A references file B by name, check whether B should reference A back. Add the reverse reference if logically appropriate.
3. **Terminology consistency** — Verify the new content uses the same terms as existing content for the same concepts. Align on one term and use it everywhere.
4. **SKILL.md references** — Every new file in `references/` or `scripts/` must appear in SKILL.md's Reference Files table. Every new workflow must appear in the Workflows table.

---

## Integration Report Template

After integration, report this summary:

```text
## Integration Summary

**Source**: <source-id> (<url>)
**Target skill**: <skill-name>
**Date**: <today>

### Changes
- Files created: N
  - <list>
- Files modified: N
  - <list>
- Files skipped: N (topics from source not integrated)

### Gap Analysis Results
- Topics fully covered (skipped): N
- Topics partially covered (merged): N
- Topics not covered (new content): N

### Curation Decisions
- Entries added: N
- Entries updated: N
- Breakdown: N kept, N simplified, N altered, N synthesized, N original

### Validation
- quick-validate.sh: N passed, N failed, N warnings
- post-integration-check.sh: N passed, N failed, N warnings
```
