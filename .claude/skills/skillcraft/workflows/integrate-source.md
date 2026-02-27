# Integrate Source into Existing Skill

Fold content from an external source into an existing skill's files and provenance.

**When to use**: "fold in source", "integrate source", "merge content from", or when a new upstream source should be incorporated into an existing skill that already has content.

---

## Phase 1: Fetch & Analyze

1. Identify the source: URL, GitHub repo, or local file path.
2. Fetch the full content:
   - **Web**: `WebFetch` the URL
   - **GitHub**: `gh api repos/{owner}/{repo}/contents/{path}` or read files directly
   - **Local**: Read the file(s)
3. Read the source content completely. Do not skim.
4. List every distinct topic or section the source covers. For each topic, note:
   - Topic name / heading
   - Approximate scope (1-2 sentences)
   - Whether it overlaps with the existing skill's domain

---

## Phase 2: Gap Analysis

Compare each topic from the new source against the existing skill's content. Use the structured template from `references/source-integration.md` § Gap Analysis Template.

For each topic in the new source, determine:

| Field | Value |
|-------|-------|
| Topic | Name from source |
| Coverage | `fully covered` / `partially covered` / `not covered` |
| Existing file | Path if covered, `—` if not |
| Quality comparison | New source better? Different angle? Redundant? |
| Recommended action | `merge` / `new file` / `skip` |

Read every file in the target skill that might overlap before marking coverage status. Do not guess from file names alone.

Present the complete gap analysis table to the user before proceeding.

---

## Phase 3: Interview

Three rounds. Use `AskUserQuestion` or the `advanced-ask` skill for each round.

### Round 1: High-Level Decisions

- What overall philosophy should guide integration? (conservative/additive/comprehensive)
- Which topics from the gap analysis should be included? Excluded?
- For the source's key concepts, does the existing skill's stance differ? Which stance wins?

### Round 2: Per-Topic Placement

For each topic the user approved in Round 1:

- Merge into an existing file, or create a new file?
- If merging, which section of the target file?
- If new file, proposed name and location (`references/` or `workflows/`)?

Use the Placement Decision Framework from `references/source-integration.md` to suggest defaults, but let the user override.

### Round 3: Remaining Details

- Ordering preferences within modified files
- Cross-reference additions needed
- Terminology alignment (if the source uses different terms for the same concept)
- Any content to intentionally elide (with rationale)

---

## Phase 4: Plan

1. Enter plan mode.
2. Produce a file-by-file change spec based on the interview decisions:
   - For each file to create: proposed name, purpose, content outline
   - For each file to modify: specific sections to add/change, with brief descriptions
   - For provenance.yml: new source entry metadata, curation_decisions to add/update
3. List the complete set of files to create and modify.
4. Exit plan mode and wait for user approval.

---

## Phase 5: Implement

Execute the changes file by file.

**Provenance-aware editing**: After modifying or creating each file, immediately update its `curation_decisions` entry in `provenance.yml`. Do not defer provenance updates to the end. See `references/source-integration.md` § Provenance-Aware Editing for the entry template.

For each file:

1. Make the content change (Edit or Write)
2. Update `provenance.yml` curation_decisions for that file
3. Confirm both changes before moving to the next file

Use the curation decision taxonomy from `workflows/add-provenance.md`:

| Decision | Meaning |
|----------|---------|
| `kept` | Taken as-is (formatting changes only) |
| `simplified` | Reduced complexity while preserving meaning |
| `elided` | Intentionally excluded (rationale required) |
| `altered` | Changed in meaning or approach from source |
| `synthesized` | Combined from multiple sources into something new |
| `original` | Not derived from any upstream source |

---

## Phase 6: Provenance

1. Add the new source to the `sources:` section of `provenance.yml`:

```yaml
  <source-id>:
    url: <url>
    type: <web|github>
    last_checked: "<today>"
    # GitHub sources only:
    owner: <org>
    repo: <repo>
    path: <path>
    last_checked_sha: "<sha>"
```

2. For GitHub sources, fetch the current SHA:
```bash
gh api "repos/{owner}/{repo}/commits?path={path}&per_page=1" --jq '.[0].sha'
```

3. Verify all curation_decisions entries are complete — every file that was created or modified in Phase 5 must have an entry.

4. Update `last_full_update` to today's date.

---

## Phase 7: Validate

Run both validation scripts:

```bash
# Structural validation
~/.claude/skills/skillcraft/scripts/quick-validate.sh "$SKILL_DIR"

# Post-integration content checks
~/.claude/skills/skillcraft/scripts/post-integration-check.sh "$SKILL_DIR"
```

Confirm:
- All `quick-validate.sh` checks pass (0 failures)
- `post-integration-check.sh` reports no failures (warnings acceptable)
- Provenance checks PV1-PV6 all pass

Report a summary:

- Source integrated: `<source-id>` (`<url>`)
- Files created: N (list them)
- Files modified: N (list them)
- Curation decisions added/updated: N
- Validation: pass/warn/fail counts
