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
- Any `watch` entries (content we excluded but want to monitor)

## Step 2: Check GitHub Sources for Changes

Check all GitHub sources **in parallel** for efficiency.

For each source with `type: github`, check if there are changes since the stored commit SHA:

```bash
gh api "repos/{owner}/{repo}/compare/{last_checked_sha}...HEAD" \
  --jq '.files[] | select(.filename | startswith("{path}/")) | {filename, status, patch}'
```

If the compare returns no matching files, the source has not changed — skip it.
The stored SHA is still current; no need to re-fetch it.

If the compare returns changes:

1. Note which upstream files changed
2. Fetch their current content:
   ```bash
   gh api "repos/{owner}/{repo}/contents/{path}/{filename}" \
     --jq '.content' | base64 -d
   ```
   Or use WebFetch on `https://raw.githubusercontent.com/{owner}/{repo}/HEAD/{path}/{filename}`.

3. Capture the new HEAD SHA (only needed when changes were found):
   ```bash
   gh api "repos/{owner}/{repo}/commits?path={path}&per_page=1" --jq '.[0].sha'
   ```

## Step 3: Check Web Source for Changes

For sources with `type: web` (e.g., official-docs):

1. WebFetch the URL
2. Read the corresponding curated file (e.g., `references/official-spec.md`)
3. Compare the fetched content against the curated content
4. Identify: new sections, removed content, changed guidance, new fields, updated examples
5. Ignore: formatting-only changes, minor rewording with same meaning

## Step 4: Map Changes to Curated Files

Cross-reference upstream changes against `curation_decisions` in provenance.yml.

For each upstream change, determine:
1. Which curated file(s) it affects (via the `source` and `sections` fields)
2. The original curation decision
3. Whether the change falls within the scope of what was kept/synthesized

Categorize each change:

| Category | Meaning | Action |
|----------|---------|--------|
| **Relevant** | Affects content we `kept` or `synthesized` | Present for review |
| **Watch** | Affects a `watch` entry we're monitoring | Present as FYI |
| **Elided** | Changes to content we chose to `elide` | Present as FYI (rationale may need revisiting) |
| **New** | Content that didn't exist when we last curated | Present for decision |
| **Original scope** | Changes to upstream content we `altered` significantly | Flag — may want to re-evaluate our alteration |

## Step 5: Present Changes

Label each change with its category from Step 4 (Relevant, Watch, Elided, New, Original scope).

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

Group changes by curated file. Show elided/watch changes in a separate "FYI" section — these are informational and do not require action, but the user may want to revisit their exclusion decision.

For `New` upstream content that doesn't map to any existing curated file:

```
### New upstream content (anthropic-skills)

**What's new**: New file `references/output-patterns.md` added upstream.
Covers structured output patterns for skills.

**No existing curated file** — this content didn't exist when we last curated.

[Create new reference file] [Incorporate into existing file] [Skip]
```

## Step 6: Apply Approved Changes

For each approved change:

1. Read the curated file
2. Edit to incorporate the upstream change, respecting the curation decision:
   - **kept**: Add the new content as-is
   - **simplified**: Add with appropriate simplification (trim examples, condense prose)
   - **synthesized**: Integrate into the existing synthesis (maintain voice and structure)
   - **altered**: Consider whether our alteration still makes sense given the upstream change
3. Update the file's provenance header date (`> Last curated:` line)

**Full rewrites**: If a change is large enough to require rewriting the entire file
(e.g., correcting a fundamental error), show the user a summary of what will change
before applying. Use Edit for incremental changes, Write only when the structure must
change fundamentally.

If creating a new curated file for `New` content:
1. Write the file with appropriate content
2. Add provenance header
3. Add entry to `curation_decisions` in provenance.yml
4. Add row to the Reference Files table in SKILL.md

## Step 7: Update Provenance Metadata

**CRITICAL — do not skip any of these.** This step is easy to overlook after
the substantive work of Steps 5-6. Complete every item in the checklist.

First, determine today's date in UTC:

```bash
date -u +%Y-%m-%d
```

Use this UTC date for all updates below. Do NOT use the `currentDate` from
conversation context — it may reflect a different timezone and be off by a day.

Update `provenance.yml` with the following checklist:

- [ ] `last_full_update` → today's UTC date
- [ ] `sources.*.last_checked` → today's UTC date (for every source checked, even if unchanged)
- [ ] `sources.*.last_checked_sha` → current HEAD SHA (for each GitHub source — capture these in Step 2)
- [ ] `curation_decisions` — add/update entries for any newly incorporated content
- [ ] `curation_decisions` — update rationale text if the curation approach changed
- [ ] Provenance headers (`> Last curated:` line) updated on all modified curated files

## Step 8: Verify Provenance Updates

**Do not skip this step.** Re-read `provenance.yml` in full after editing.
Confirm every item:

1. All `last_checked` dates match the UTC date from Step 7
2. `last_full_update` matches the UTC date from Step 7
3. All `last_checked_sha` values are current HEAD SHAs (not the old values)
4. Any new curation decisions are present and have rationale text

If anything is stale or missing, fix it now before reporting.

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
| `elided` | Intentionally excluded | Skip unless user revisits decision |
| `altered` | Changed in meaning | Re-evaluate alteration against new upstream |
| `synthesized` | Combined from multiple sources | Integrate maintaining existing synthesis |
| `original` | Not from any source | No upstream to check |
| `watch` | Monitoring only | Present as FYI |
