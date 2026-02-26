# Add Provenance to an Existing Skill

Add structured upstream source tracking to a skill that doesn't yet have a `provenance.yml`.

**CRITICAL**: Provenance is ALWAYS a standalone `provenance.yml` file at the skill root — NEVER a markdown section in SKILL.md. Do not add `## Provenance` or similar sections to the SKILL.md body.

## Prerequisites

- `gh` CLI authenticated (for GitHub sources): `gh auth status`
- `yq` for YAML validation (optional): `command -v yq`

## Step 1: Identify Target Skill

If `$ARGUMENTS` specifies a path or skill name, resolve it. Otherwise, pick:

```bash
~/.claude/skills/advanced-ask/scripts/ask-file.sh \
    --glob "SKILL.md" ~/.claude/skills
```

Set `SKILL_DIR` to the skill's root directory.

## Step 2: Check for Existing Provenance

If `$SKILL_DIR/provenance.yml` already exists, inform the user and redirect:

> This skill already has provenance tracking. To check for upstream changes, use the **Update** workflow instead:
> Read `workflows/update-from-sources.md`

Stop here if provenance.yml exists.

## Step 3: Inventory Skill Files

List all files in the skill directory (SKILL.md, references/*, scripts/*). These are the files that need curation decisions in provenance.yml.

## Step 4: Interview for Upstream Sources

Ask the user about the skill's origins. Key questions:

1. **What sources contributed content to this skill?** — URLs of documentation, blog posts, GitHub repos, or other materials that content was actually derived from. Only include sources whose content made it into the skill — provenance tracks derivation, not bibliography.
2. **Source types** — For each source: is it a GitHub repository or a web page?
3. **Which files came from which sources?** — Map each skill file to its upstream source(s)
4. **What curation was applied?** — For each file-source mapping, what was the decision?

Use the curation decision taxonomy:

| Decision | Meaning |
|----------|---------|
| `kept` | Taken as-is (formatting changes only) |
| `simplified` | Reduced complexity while preserving meaning |
| `elided` | Intentionally excluded (rationale required) |
| `altered` | Changed in meaning or approach from source |
| `synthesized` | Combined from multiple sources into something new |
| `original` | Not derived from any upstream source |

**Note on `elided`**: This applies to sections *within* a source that was partially used — e.g., "used sections A and B, elided section C." If an entire source was considered but not used at all, simply omit it from `sources:`. Provenance tracks what IS in the skill, not what was considered and rejected.

If the user is unsure about specific mappings, help by reading the skill files and comparing against the upstream sources (use WebFetch for web sources, `gh api` for GitHub sources).

## Step 5: Research Source Metadata

For each upstream source, gather metadata:

**GitHub sources:**
```bash
# Get latest commit SHA for the relevant path
gh api "repos/{owner}/{repo}/commits?path={path}&per_page=1" --jq '.[0].sha'
```

**Web sources:**
- Record the URL
- Note today's date as `last_checked`

## Step 6: Generate provenance.yml

Create `$SKILL_DIR/provenance.yml` with this structure:

```yaml
schema_version: 1
skill: <skill-name>
last_full_update: "<today UTC, YYYY-MM-DD>"

# Upstream sources this skill was curated from.
sources:
  <source-id>:
    url: <url>
    type: <web|github>
    last_checked: "<today>"
    # GitHub sources only:
    owner: <org>
    repo: <repo>
    path: <path/to/relevant/dir>
    last_checked_sha: "<sha>"

# Maps each curated file to its upstream source(s) and curation decision.
#
# Decision taxonomy:
#   kept        - Taken as-is (formatting changes only)
#   simplified  - Reduced complexity while preserving meaning
#   elided      - Intentionally excluded (rationale required)
#   altered     - Changed in meaning or approach from source
#   synthesized - Combined from multiple sources into something new
#   original    - Not derived from any upstream source
curation_decisions:
  <relative-file-path>:
    - source: <source-id>
      sections: ["<section-id>" | "*"]
      decision: <kept|simplified|elided|altered|synthesized|original>
      rationale: >-
        Explanation of curation choice.
```

Guidelines:
- Every file in the skill should have at least one curation decision entry
- Files not derived from any source use `decision: original` (no `source` field needed)
- Only list sources in `sources:` that contributed content to at least one curated file. If a source was considered but not used, omit it entirely.
- The `sections` field maps which parts of the upstream source contributed to this file
- Use `"*"` for sections when the entire source document was used
- Rationale should be concise but explain WHY the curation choice was made

## Step 7: Validate

Run the validation script to confirm provenance checks pass:

```bash
~/.claude/skills/skillcraft/scripts/quick-validate.sh "$SKILL_DIR"
```

Confirm:
- **PV1** passes: all files in curation_decisions exist on disk
- **PV2** passes: last_full_update is recent (today)

## Step 8: Report

Summarize what was created:

- Sources tracked: N
- Files with curation decisions: N
- Curation breakdown: N kept, N simplified, N altered, N synthesized, N original
- Provenance file: `$SKILL_DIR/provenance.yml`
