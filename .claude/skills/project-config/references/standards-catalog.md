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
| `sections` | list of strings | Required section headings (case-insensitive match) |
| `references` | list of standard names | README must link to these documents (e.g., `[goals, spec, docs]` requires links to GOALS.md, SPEC.md, and docs/) |

**Section check**: Scan for markdown headings (`## Section Name`) matching the declared sections. Report missing sections as failures.

**Reference check**: Scan README content for links or references to the specified documents. A bare mention of the filename counts (e.g., "See GOALS.md" or `[Goals](GOALS.md)`).

---

## gitignore

**Default check**: `.gitignore` exists in the project root.

No additional configuration.

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

---

## goals

**Default check**: `GOALS.md` exists. This is the WHY document — project motivation, vision, non-goals.

**Files checked** (first match wins):
- `GOALS.md`
- `goals.md`
- `goals.yaml`

No additional configuration.

---

## spec

**Default check**: `SPEC.md` exists. This is the HOW document — technical specification, architecture, data model, API design.

**Files checked** (first match wins):
- `SPEC.md`
- `spec.md` (only if no `spec/` test directory exists — disambiguate)
- `specification.md`
- `design.md`

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
- Must have an `## [Unreleased]` section (or at least one `## [version]` section)
- Version sections should follow `## [X.Y.Z] - YYYY-MM-DD` format
- Entries should use change type headings: `### Added`, `### Changed`, `### Deprecated`, `### Removed`, `### Fixed`, `### Security`

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

## code-of-conduct

**Default check**: A CODE_OF_CONDUCT file exists.

**Files checked** (first match wins):
- `CODE_OF_CONDUCT.md`
- `CODE_OF_CONDUCT`
- `CODE_OF_CONDUCT.txt`

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
