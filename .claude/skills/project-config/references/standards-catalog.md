# Standards Catalog

Detailed check logic for each standard. Standards are checked in the order listed.

**All file checks are project-root-only** unless a standard explicitly says otherwise. Never search recursively into `node_modules/`, `vendor/`, `.git/`, `dist/`, or other dependency/build directories. When checking "Files checked" lists below, look only in the project root directory.

## readme

**Default check**: A README file exists, is non-empty, and has at least one heading.

**Files checked** (first match wins):
- `README.md`
- `README`
- `README.txt`
- `README.rdoc`
- `README.org`

**Configurable options**:

| Option | Type | Description |
|--------|------|-------------|
| `sections` | list of strings | Required section headings (case-insensitive match). Respects the standard's severity — FAIL if the readme standard is required, WARN if recommended. |
| `recommended_sections` | list of strings | Section headings that should be present (always WARN if missing, never FAIL) |
| `references` | list of standard names | README must link to these documents (e.g., `[goals, spec, docs]` requires links to GOALS.md, SPEC.md, and docs/) |

**Section check**: Scan for markdown headings (`## Section Name`) matching the declared sections. Missing required `sections` use the standard's severity (FAIL or WARN). Missing `recommended_sections` are always WARN.

**Reference check**: Scan README content for links or references to the specified documents. A bare mention of the filename counts (e.g., "See GOALS.md" or `[Goals](GOALS.md)`).

---

## gitignore

**Default check**: `.gitignore` exists in the project root.

**Public visibility check**: When `visibility: public`, also checks that `.gitignore` contains patterns whose absence creates real risk for a public repo. Currently this is just `.env` (committed secrets). A missing pattern is reported as a WARN (not FAIL).

Personal/editor patterns (`.vscode/`, `.idea/`, swap files, OS metadata like `.DS_Store`, etc.) are intentionally **not** recommended here — those belong in a developer's personal gitignore (`git config --global core.excludesFile`), not in the project's `.gitignore`.

---

## license

**Default check**: A LICENSE file exists and its content matches the declared SPDX identifier.

**Files checked** (first match wins):
- `LICENSE`
- `LICENSE.md`
- `LICENSE.txt`
- `COPYING`
- `COPYING.md`

**Configurable options**:

| Option | Type | Description |
|--------|------|-------------|
| `spdx` | string | SPDX license identifier (e.g., `MIT`, `Apache-2.0`, `GPL-3.0-only`) |
| `current_year` | boolean | When true, verify the LICENSE file contains the current year in its copyright line. WARN if not found (since year conventions vary). |

**SPDX check**: If `spdx` is declared, verify the license file content matches the expected license. Use known license text patterns:
- `MIT` — look for "MIT License" or "Permission is hereby granted"
- `Apache-2.0` — look for "Apache License" and "Version 2.0"
- `GPL-3.0-only` — look for "GNU GENERAL PUBLIC LICENSE" and "Version 3"

If `spdx` is not declared, just check file existence.

---

## tests

**Default check**: A test directory exists with at least one file.

**Configurable options**:

| Option | Type | Description |
|--------|------|-------------|
| `framework` | string | Test framework name (for metadata/display) |
| `directory` | string | Path to the test directory |
| `config` | string | Path to the test framework config file |

**Script behavior**: If `directory` is declared, checks that directory exists and counts files. If `config` is declared, checks that file exists. If `directory` is not declared, the check is SKIPped for manual verification.

**Manual verification** (SKIP): When the script cannot determine the test setup from declared fields, verify that a test directory exists with test files and that the test framework is properly configured.

---

## claude-md

**Default check**: `CLAUDE.md` or `AGENTS.md` exists and is non-empty. Follows symlinks.

**Files checked** (first match wins, following symlinks):
- `CLAUDE.md`
- `AGENTS.md`

**Configurable options**:

| Option | Type | Description |
|--------|------|-------------|
| `sections` | list of strings | Required section headings (case-insensitive heading match) |

**Section check**: Scan the resolved file content for markdown headings matching the declared sections. Typical sections:
- `Build commands`
- `Test commands`
- `Project overview`
- `Architecture`
- `Key decisions`

**Public visibility review**: When `visibility: public`, emit an additional SKIP row (`claude-md.review`) reminding to manually review CLAUDE.md content for internal-only references (private URLs, credentials, internal project names) before public release.

---

## goals

**Default check**: `GOALS.md` exists. This is the WHY document — project motivation, vision, non-goals.

**Files checked** (first match wins; root takes precedence over `docs/`):
- `GOALS.md`
- `goals.md`
- `goals.yaml`
- `docs/GOALS.md`
- `docs/goals.md`
- `docs/goals.yaml`

No additional configuration.

---

## spec

**Default check**: `SPEC.md` exists. This is the HOW document — technical specification, architecture, data model, API design.

**Files checked** (first match wins; root takes precedence over `docs/`):
- `SPEC.md`
- `spec.md` (root only — and only if no `spec/` test directory exists — disambiguate)
- `specification.md`
- `design.md`
- `docs/SPEC.md`
- `docs/spec.md` (always safe under `docs/` — cannot be confused with a test directory)
- `docs/specification.md`
- `docs/design.md`

No additional configuration.

---

## linter

**Default check**: A linter is configured for the project.

**Configurable options**:

| Option | Type | Description |
|--------|------|-------------|
| `tool` | string | Linter tool name (for metadata/display) |
| `config` | string | Path to the linter config file |

**Script behavior**: If `config` is declared, checks that file exists. If `config` is not declared, the check is SKIPped for manual verification.

**Manual verification** (SKIP): When the script cannot determine the linter setup from declared fields, verify that a linter is installed and configured for the project.

---

## ci

**Default check**: Any CI configuration file or directory exists.

**Files/directories checked**:
- `.github/workflows/` (with at least one `.yml` or `.yaml` file)
- `.gitlab-ci.yml`
- `.circleci/`
- `.travis.yml`
- `Jenkinsfile`
- `.buildkite/`

This is a flag-based standard — project.yaml declares `ci: { required: true }` and the skill just checks existence.

---

## coverage

**Default check**: Coverage is configured with an enforced ratchet (minimum threshold).

A coverage ratchet is a minimum threshold that fails the build if coverage drops. Coverage configured without a ratchet is a **FAIL** — the ratchet is what makes coverage enforceable.

**Configurable options**:

| Option | Type | Description |
|--------|------|-------------|
| `config` | string | Path to the file containing coverage configuration |
| `ratchet_pattern` | string | Grep pattern to detect ratchet/threshold in the config file |

**Script behavior**: If `config` is declared, checks that file exists. If `ratchet_pattern` is also declared, greps the config file for that pattern. PASS requires both config existence and ratchet detection. If `config` is not declared, the check is SKIPped for manual verification.

**Manual verification** (SKIP): When the script cannot determine the coverage setup from declared fields, verify that coverage tooling is configured and that a minimum threshold/ratchet is enforced.

---

## changelog

**Default check**: A CHANGELOG file exists in Keep-a-Changelog format.

**Files checked** (first match wins):
- `CHANGELOG.md`
- `CHANGELOG`
- `HISTORY.md`
- `CHANGES.md`

**Format check** (Keep-a-Changelog):
- Must have at least one `## [version]` section (`## [X.Y.Z]` or `## [Unreleased]`)
- Version sections should follow `## [X.Y.Z] - YYYY-MM-DD` format
- Entries should use change type headings: `### Added`, `### Changed`, `### Deprecated`, `### Removed`, `### Fixed`, `### Security`

**`[Unreleased]` check**: If version sections exist, the script additionally checks for an `## [Unreleased]` section (case-insensitive). PASS if present, WARN if missing — confirms the changelog is actively maintained, not abandoned at a prior release.

Report format violations as warnings, not failures — the file existing is the primary check.

---

## contributing

**Default check**: `CONTRIBUTING.md` exists. Required only when `visibility: public` in project.yaml metadata.

**Files checked**:
- `CONTRIBUTING.md`
- `CONTRIBUTING`

If `visibility` is not `public`, this standard is automatically skipped (not reported as a failure).

---

## editorconfig

**Default check**: `.editorconfig` exists in the project root.

No additional configuration.

---

## docs

**Default check**: A `docs/` or `doc/` directory exists with at least one file.

**Directories checked**:
- `docs/`
- `doc/`

---

## issue-templates

**Default check**: Structured issue templates exist for the project's forge.

**Check logic** (first match wins):
1. `.github/ISSUE_TEMPLATE/` directory contains at least one `.md`, `.yml`, or `.yaml` template file (excluding `config.yml` / `config.yaml`, which is the template chooser, not a template). **PASS**.
2. `.gitlab/issue_templates/` directory contains at least one `.md` file. **PASS**.
3. `.github/ISSUE_TEMPLATE.md` (legacy single-file form) exists. **WARN** with note: "legacy single-file form — consider migrating to `.github/ISSUE_TEMPLATE/` directory".
4. Otherwise, FAIL/WARN based on severity.

No configurable options.

---

## pr-template

**Default check**: A Pull Request template exists for the project's forge.

**Check logic** (first match wins):
1. `.github/PULL_REQUEST_TEMPLATE.md` or `.github/pull_request_template.md` — **PASS**.
2. `.github/PULL_REQUEST_TEMPLATE/` directory with at least one `.md` file (multi-template form) — **PASS**.
3. `PULL_REQUEST_TEMPLATE.md` or `docs/PULL_REQUEST_TEMPLATE.md` (root / docs variants — GitHub searches these too) — **PASS**.
4. `.gitlab/merge_request_templates/` directory with at least one `.md` file — **PASS**.
5. Otherwise, FAIL/WARN based on severity.

No configurable options.

---

## commit-convention

**Default check**: The project enforces or documents a commit message convention.

**Check logic** (first match wins):
1. A commitlint config file exists — **PASS**.
2. A commitizen config (`.czrc`, `.cz.json`) or `package.json` with `"commitlint"` key exists — **PASS**.
3. `CONTRIBUTING.md` contains a mention of "conventional commit", "commit message format", "commit convention", or "angular commit" (case-insensitive grep) — **PASS**.
4. Otherwise, FAIL/WARN based on severity.

**Commitlint config files checked**:
- `commitlint.config.js`, `commitlint.config.cjs`, `commitlint.config.mjs`, `commitlint.config.ts`
- `.commitlintrc`, `.commitlintrc.json`, `.commitlintrc.yml`, `.commitlintrc.yaml`, `.commitlintrc.js`, `.commitlintrc.cjs`, `.commitlintrc.ts`

No configurable options.

---

## release-automation

**Default check**: Release automation tooling is configured.

**Check logic** (first match wins):
1. release-please config: `release-please-config.json`, `.release-please-manifest.json` — **PASS**.
2. semantic-release config: `.releaserc`, `.releaserc.json`, `.releaserc.yml`, `.releaserc.yaml`, `.releaserc.js`, `.releaserc.cjs`, `release.config.js`, `release.config.cjs`, `release.config.ts` — **PASS**.
3. changesets config: `.changeset/config.json` — **PASS**.
4. GoReleaser config: `.goreleaser.yml`, `.goreleaser.yaml`, `goreleaser.yml`, `goreleaser.yaml` — **PASS**.
5. `package.json` with `"release"` key (semantic-release embedded config) — **PASS**.
6. Otherwise, FAIL/WARN based on severity.

No configurable options.

---

## dependency-updates

**Default check**: Automated dependency update tooling is configured.

**Check logic** (first match wins):
1. `.github/dependabot.yml` or `.github/dependabot.yaml` — **PASS**.
2. Renovate config: `renovate.json`, `renovate.json5`, `.renovaterc`, `.renovaterc.json`, `.github/renovate.json`, `.github/renovate.json5` — **PASS**.
3. `package.json` with `"renovate"` key — **PASS**.
4. Otherwise, FAIL/WARN based on severity.

No configurable options.

---

## readme-badges

**Default check**: The README contains at least one status badge (build status, version, license, coverage, etc.).

**Check logic**: Grep the first README file found for any of these badge URL patterns:
- `img.shields.io` (shields.io badges)
- `badge.svg` (GitHub Actions status badges)
- `codecov.io` (coverage badges)
- `badgen.net` (badgen badges)
- `img src=.*badge` or `!\[.*badge` (generic badge image patterns)

**PASS** if any badge pattern is found. FAIL/WARN if none found.

No configurable options.

---

## lockfile

**Default check**: A language-appropriate lockfile is committed to the repository.

**Lockfiles by language**:
- `typescript` / `javascript`: `package-lock.json`, `yarn.lock`, `pnpm-lock.yaml`, `bun.lockb`, `bun.lock`
- `ruby`: `Gemfile.lock`
- `python`: `poetry.lock`, `Pipfile.lock`, `uv.lock`
- `rust`: `Cargo.lock`
- `go`: `go.sum`
- `php`: `composer.lock`
- `elixir`: `mix.lock`
- `swift`: `Package.resolved`
- Unknown language: SKIPped for manual verification.

No configurable options — auto-detected based on project language.

**Note**: For libraries, some ecosystems recommend NOT committing the lockfile (so downstream consumers test with their own resolved versions). Projects may set `lockfile: { recommended: true }` or omit the standard entirely if this applies.

---

## support

**Default check**: A `SUPPORT.md` file exists, telling users where to get help (discussions, chat, paid support) so issues don't become a catch-all help desk.

**Files checked** (first match wins):
- `SUPPORT.md`
- `.github/SUPPORT.md`
- `docs/SUPPORT.md`

No configurable options.

---

## code-of-conduct

**Default check**: A CODE_OF_CONDUCT file exists.

**Files checked** (first match wins):
- `CODE_OF_CONDUCT.md`
- `CODE_OF_CONDUCT`
- `CODE_OF_CONDUCT.txt`

---

## security-policy

**Default check**: A security policy file exists, telling users how to report vulnerabilities privately.

**Files checked** (first match wins):
- `SECURITY.md`
- `SECURITY`
- `SECURITY.txt`
- `.github/SECURITY.md`

No additional configuration.

---

## package-metadata

**Default check**: The project's distribution manifest declares metadata required for public release (name, version, license, repository URL, description).

**Configurable options**:

| Option | Type | Description |
|--------|------|-------------|
| `manifest` | string | Path to the manifest file (e.g., `package.json`, `Cargo.toml`, `pyproject.toml`, `*.gemspec`) |

**Script behavior**: If `manifest` is declared, checks that file exists. If the project language is known, attempts to validate key fields (name, version, license, repository/homepage) in the manifest. Missing fields are reported as WARN with a list. If `manifest` is not declared, the check is SKIPped for manual verification.

**Known manifest field checks** (ecosystem-specific, best-effort):
- `package.json`: name, version, license, repository, description (via jq)
- `Cargo.toml`: [package] name, version, license, repository (via yq)
- `pyproject.toml`: [project] name, version, license, urls (via yq)
- Other formats: file existence only, field validation SKIPped

---

## publish-config

**Default check**: The project explicitly controls which files are included in distributed packages, preventing accidental inclusion of tests, internal docs, secrets, or development artifacts.

No configurable options — auto-detected based on project language.

**Check logic** (best-effort, ecosystem-specific):
- `typescript` / `javascript`: `.npmignore` exists OR `package.json` has a `"files"` field
- `ruby`: a `.gemspec` file exists with a `files` attribute
- `rust`: `Cargo.toml` has `[package]` exclude or include
- `python`: `MANIFEST.in` exists OR `pyproject.toml` has `[tool.setuptools.packages]`
- Unknown language: SKIPped for manual verification

---

## Runtime Verification Standards

These are sub-standards that extend the infrastructure checks above. They appear as `.run` rows in the compliance table (e.g., `tests.run`, `linter.run`, `coverage.run`). A `.run` row is only present when its parent infrastructure check passed.

All `.run` standards are binary PASS/FAIL — no WARN.

### linter.run

**Prerequisite**: `linter` infrastructure check passed.

**Check**: Run the project's linter and report results.

**PASS**: Linter exits 0 (no violations). **FAIL**: Violations found.

**Detail format**: violation count or "clean"

### tests.run

**Prerequisite**: `tests` infrastructure check passed.

**Check**: Run the test suite and report pass/fail counts.

**PASS**: All tests pass (exit 0). **FAIL**: Any test fails.

**Detail format**: pass/fail counts (e.g., "14/14 passing" or "79/81 passing, 2 failures")

### coverage.run

**Prerequisite**: `coverage` infrastructure check passed.

**Check**: Generate a coverage report and extract the line coverage percentage.

**PASS**: Coverage tool runs successfully. **FAIL**: Coverage tool fails to run.

**Detail format**: coverage percentage (e.g., "94.2% line coverage")

---

## Standard Severity Levels

Each standard in project.yaml can be declared as:

| Level | Meaning | Audit behavior |
|-------|---------|----------------|
| `required: true` | Must pass | Reported as **FAIL** if missing |
| `recommended: true` | Should pass | Reported as **WARN** if missing |
| Not declared | Not tracked | Not included in audit output |

If a standard appears in project.yaml without `required` or `recommended`, it defaults to `required: true`.
