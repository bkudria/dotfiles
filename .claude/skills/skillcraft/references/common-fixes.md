# Skill Common Fixes

Concrete before/after fixes for frequently failed checklist items. For design-level anti-patterns, see `anti-patterns.md`.

## Metadata

### M2: Description Quality

**Before** -- vague, restates the name:
```yaml
description: A skill for working with Docker
```
**After** -- specific actions and context:
```yaml
description: Build, debug, and optimize Dockerfiles. Use when creating multi-stage builds, diagnosing container startup failures, reducing image size, or converting docker-compose services to Kubernetes manifests.
```
State the concrete actions, not just the domain.

### M3: Trigger Phrases

**Before** -- no actionable triggers:
```yaml
description: Helps with database operations and queries
```
**After** -- verb+noun phrases matching real requests:
```yaml
description: Write and optimize SQL queries. Use when writing complex joins, debugging slow queries with EXPLAIN, adding indexes, or converting raw SQL to an ORM.
```
Write 3-5 "verb + specific noun" phrases. Test: "If a user said this, should this skill activate?"

## Content

### C1: Imperative Writing Style

**Before**:
```markdown
## Workflow
You should start by reading the config file. Once you've reviewed it,
you can run the linter. If you find issues, you should fix them before
committing. You might also want to check the CI output.
```
**After**:
```markdown
## Workflow
1. Read the config file
2. Run the linter
3. Fix all reported issues before committing
4. Check CI output for remaining warnings
```
Delete every "you" and rewrite as a direct command or statement of fact.

### C3: "When to Use" Section

**Before** -- jumps straight to steps:
```markdown
## Steps
1. Read the OpenAPI spec
2. Generate client code
```
**After** -- adds scope guidance:
```markdown
## When to Use
- Adding a new third-party API integration
- OpenAPI spec exists but no typed client is available
- Existing hand-written client is out of sync with the spec

## When NOT to Use
- GraphQL APIs (use `graphql-codegen` skill instead)
- APIs without an OpenAPI spec

## Steps
1. Read the OpenAPI spec
2. Generate client code
```
Add 3-5 bullets describing the triggering situation, not the skill's features.

### C5: Runnable Code Examples

**Before**:
```bash
./scripts/migrate.sh --source foo --target bar --dry-run
```
**After**:
```bash
./scripts/migrate.sh --source postgres://localhost:5432/myapp_dev \
                     --target postgres://localhost:5432/myapp_test \
                     --dry-run
```
Replace every placeholder with a domain-realistic value.

## Progressive Disclosure

### P1: Body Not Bloated

**Before** -- detailed config inline for each item:
```markdown
## Supported Linters
### ESLint
ESLint checks JavaScript and TypeScript files...
[40 lines of configuration details]
### Prettier
[35 lines of configuration details]
```
**After** -- summary table, detail extracted:
```markdown
## Supported Linters

| Linter | Languages | Config File |
|--------|-----------|-------------|
| ESLint | JS, TS | `.eslintrc.*` |
| Prettier | JS, TS, CSS, MD | `.prettierrc` |
| Stylelint | CSS, SCSS | `.stylelintrc` |

For detailed configuration, see `references/linter-configs.md`.
```
Keep a scannable table in the body. Move per-item detail into a reference file.

### P3: SKILL.md References Supporting Files

**Before** -- files exist but body never mentions them:
```markdown
## Workflow
1. Run the audit
2. Fix issues
3. Re-run
```
(`references/error-catalog.md` and `scripts/validate.sh` sit unreferenced)

**After** -- files linked inline and in a table:
```markdown
## Workflow
1. Run `scripts/validate.sh` to audit the project
2. Fix issues using `references/error-catalog.md`
3. Re-run validation

## Reference Files

| File | Purpose |
|------|---------|
| `references/error-catalog.md` | Error codes with explanations |
| `scripts/validate.sh` | Automated validation runner |
```
Reference files inline where relevant and add a summary table.

## Advanced Features

### A1: Dynamic Context Commands

**Before** -- fragile, platform-specific:
```markdown
!`readlink -f ~/.config/myapp/settings.json`
```
**After** -- portable with error handling:
```markdown
!`cat ~/.config/myapp/settings.json 2>/dev/null || echo "No config found"`
```
Use POSIX-compatible commands. Handle failures with `2>/dev/null || fallback`. Avoid GNU-only flags (`readlink -f`, `sed -i` without `''`).

## Quality

### Q3: Consistent Formatting

Common inconsistencies and their fixes:

| Problem | Before | After |
|---------|--------|-------|
| Skipped heading level | `#### Config` under `## Setup` | `### Config` under `## Setup` |
| Missing language tag | ` ``` ` | ` ```bash ` |
| Inconsistent caps | `### running tests` | `### Running Tests` |
| Bold for commands | `**npm test**` | `` `npm test` `` |
| Emphasis for names | `*config* file` | `config file` or `` `config` `` |

Fix: Sequential heading levels (never skip). Language tags on all code fences. Pick one heading capitalization style. Backticks for commands, bold only for emphasis.

### Q2: Tables for Structured Data

**Before** -- bullet list with implicit structure:
```markdown
- `lint` - runs the linter on all files
- `fix` - auto-fixes linter warnings
- `check` - runs lint without modifying files
```
**After** -- table exposing additional dimensions:
```markdown
| Command | Description | Modifies Files? |
|---------|-------------|-----------------|
| `lint` | Run linter on all files | No |
| `fix` | Auto-fix linter warnings | Yes |
| `check` | Run lint without modifying | No |
```
When items share structure (name + description), use a table. Add columns for dimensions hidden in flat lists.
