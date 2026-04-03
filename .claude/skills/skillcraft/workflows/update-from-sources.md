# Update from Upstream Sources

Check upstream sources for changes and incorporate them into curated content.

This skill tracks its provenance in `provenance.yml` — which upstream sources contributed to which files, and what curation decisions (kept, simplified, elided, altered, synthesized, original) were made.

## Prerequisites

- `gh` CLI authenticated (`brew install gh`)
- `yq` for YAML parsing (`brew install yq`)

Verify before starting:
```bash
gh auth status && command -v yq
```

## Step 1: Load Provenance

Read `provenance.yml` from this skill's root directory. Build a mental model of:
- All upstream sources and their last-checked dates/SHAs
- All `curation_decisions` mappings (file → source → decision → rationale)

## Step 2: Check All Sources for Changes

Run the automated detection script:

```bash
scripts/check-upstream.sh <skill-directory>
```

This checks all GitHub sources via `gh api compare` and flags web sources for manual review. The output shows which sources have changes, which files changed, and new HEAD SHAs.

For any web sources flagged as `MANUAL`:
1. WebFetch the URL shown in the output
2. Read the corresponding curated file (e.g., `references/official-spec.md`)
3. Compare the fetched content against the curated content
4. Identify: new sections, removed content, changed guidance, new fields, updated examples
5. Ignore: formatting-only changes, minor rewording with same meaning

For GitHub sources marked `CHANGED`, fetch the changed file contents:
```bash
gh api "repos/{owner}/{repo}/contents/{path}/{filename}" \
  --jq '.content' | base64 -d
```
Or use WebFetch on `https://raw.githubusercontent.com/{owner}/{repo}/HEAD/{path}/{filename}`.

## Step 3: Map Changes to Curated Files

Cross-reference upstream changes against `curation_decisions` in provenance.yml.

For each upstream change, determine:
1. Which curated file(s) it affects (via the `source` and `sections` fields)
2. The original curation decision
3. Whether the change falls within the scope of what was kept/synthesized

Categorize each change:

| Category | Meaning | Action |
|----------|---------|--------|
| **Relevant** | Affects content we `kept` or `synthesized` | Present for review |
| **Elided** | Changes to sections within a source that we chose to `elide` | Present as FYI (rationale may need revisiting) |
| **New** | Content that didn't exist when we last curated | Present for decision |
| **Original scope** | Changes to upstream content we `altered` significantly | Flag — may want to re-evaluate our alteration |

## Step 4: Present Changes

Label each change with its category from Step 3 (Relevant, Elided, New, Original scope).

For each affected curated file, present a summary:

```
### references/frontmatter-reference.md

**Upstream change** (official-docs):
  New field added: `plan-mode-required` — boolean, default false.
  Allows skills to require plan approval before execution.

**Current curation decision**: kept
**Rationale**: "Core field table and substitutions from official docs."

**Recommendation**: Add to the frontmatter field catalog.

[Incorporate] [Skip] [Discuss]
```

Group changes by curated file. Show elided changes in a separate "FYI" section — these are changes to sections within a used source that were intentionally excluded (section-level elision). They are informational and do not require action, but the user may want to revisit their exclusion decision.

For `New` upstream content that doesn't map to any existing curated file:

```
### New upstream content (anthropic-skills)

**What's new**: New file `references/output-patterns.md` added upstream.
Covers structured output patterns for skills.

**No existing curated file** — this content didn't exist when we last curated.

[Create new reference file] [Incorporate into existing file] [Skip]
```

## Step 5: Apply Approved Changes

For each approved change:

1. Read the curated file
2. Edit to incorporate the upstream change, respecting the curation decision:
   - **kept**: Add the new content as-is
   - **simplified**: Add with appropriate simplification (trim examples, condense prose)
   - **synthesized**: Integrate into the existing synthesis (maintain voice and structure)
   - **altered**: Consider whether our alteration still makes sense given the upstream change

**Full rewrites**: If a change is large enough to require rewriting the entire file
(e.g., correcting a fundamental error), show the user a summary of what will change
before applying. Use Edit for incremental changes, Write only when the structure must
change fundamentally.

If creating a new curated file for `New` content:
1. Write the file with appropriate content
2. Add entry to `curation_decisions` in provenance.yml
3. Add row to the Reference Files table in SKILL.md

## Step 6: Update Provenance Metadata

Run the automated metadata update:

```bash
scripts/check-upstream.sh <skill-directory> --update-metadata
```

This updates `last_full_update`, all `sources.*.last_checked` dates, and
`last_checked_sha` values for GitHub sources with changes.

Then manually update `curation_decisions` in provenance.yml:

- [ ] Add/update entries for any newly incorporated content
- [ ] Update rationale text if the curation approach changed

## Step 7: Regression Check (Optional)

If the skill has `evals/` with scenarios, verify the update didn't degrade behavior:

```bash
craboodle run <skill-dir>/evals
```

Review pass rates — flag any scenarios with degraded results. If regressions are found, review the upstream changes that caused them and consider reverting or adjusting.

## Step 8: Verify Provenance Updates

Run the detection script again to confirm all dates are current:

```bash
scripts/check-upstream.sh <skill-directory>
```

Verify that all `last_checked` dates show today's UTC date. Also confirm
any new curation decisions are present and have rationale text.

Report summary:
- Sources checked: N
- Sources with changes: N
- Changes incorporated: N
- Changes skipped: N (with reasons)
- New content decisions: N
- Files modified: [list]

## Curation Decision Taxonomy

| Decision | Meaning | Update strategy |
|----------|---------|-----------------|
| `kept` | Taken as-is | Add new upstream content as-is |
| `simplified` | Reduced complexity | Add with similar simplification |
| `elided` | Sections intentionally excluded | Skip unless user revisits decision |
| `altered` | Changed in meaning | Re-evaluate alteration against new upstream |
| `synthesized` | Combined from multiple sources | Integrate maintaining existing synthesis |
| `original` | Not from any source | No upstream to check |
