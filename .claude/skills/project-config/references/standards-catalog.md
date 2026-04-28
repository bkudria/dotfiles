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

**Supported reference mappings** (standard name → file/path scanned in README):
- `goals` → `GOALS.md`
- `spec` → `SPEC.md`
- `docs` → `docs/`
- `contributing` → `CONTRIBUTING.md`
- `code-of-conduct` → `CODE_OF_CONDUCT.md`
- `security-policy` → `SECURITY.md`
- `support` → `SUPPORT.md`
- `changelog` → `CHANGELOG.md`
- any other name → matched as a literal case-insensitive substring against the README

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

## formatter

**Default check**: A code formatter is configured for the project — separate from the linter, since lint covers correctness while a formatter covers style consistency (whitespace, line wrapping, quote style).

**Configurable options**:

| Option | Type | Description |
|--------|------|-------------|
| `tool` | string | Formatter tool name (for metadata/display) |
| `config` | string | Path to the formatter config file |

**Script behavior**: If `config` is declared, checks that file exists. If `config` is not declared, the check is SKIPped for manual verification.

**Manual verification** (SKIP): When the script cannot determine the formatter setup from declared fields, verify that a formatter is installed and configured for the project (e.g., Prettier, Biome, dprint, Black, rustfmt, gofmt — declared in package.json/pyproject.toml/etc. or as a config file at the project root).

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

## release-process

**Default check**: The project documents its end-to-end release process where contributors will look — typically `CONTRIBUTING.md`, a dedicated `RELEASING.md`, or a `docs/` page.

No configurable options — this standard intentionally stays content-agnostic. The audit emits `SKIP` for manual verification.

**Manual verification** (SKIP): Confirm the docs answer the questions a first-time releaser would ask, end-to-end:

- **Commit conventions** — what message format gates the next release (Conventional Commits, Angular, etc.)?
- **Release trigger** — who or what opens the release PR (release-please, semantic-release, manual)? What does merging it cause?
- **Versioning** — how is the next version chosen, and how do contributors signal a bump (commit prefix, changeset entry, manual edit)?
- **Publishing** — where does the artifact land (npm, PyPI, GitHub Releases, container registry) and what credentials are required?
- **CHANGELOG** — how do new entries appear (auto-generated from commits, hand-edited, both)?

A bare `we use release-please` line is not a documented process. The bar is whether a new contributor could ship a release after reading the doc, with no out-of-band help.

This standard complements `release-automation` (which only checks that *some* tooling is configured) by ensuring the human side of releases — conventions, expectations, recovery — is written down.

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

## runtime-version

**Default check**: The project declares its required runtime/language version somewhere a contributor or CI step would look — e.g., a manifest field, a dotfile, a version-manager config, or a Requirements/Prerequisites section in the README. Without this, new contributors and CI runners have no authoritative answer to "which runtime version should I install?"

No configurable options — this standard intentionally stays language-agnostic. The audit emits `SKIP` for manual verification.

**Manual verification** (SKIP): Confirm that the project declares its required runtime/language version somewhere contributors will look. Common places:
- Manifest fields (e.g., `engines` in `package.json`, `requires-python` in `pyproject.toml`, `rust-version` in `Cargo.toml`, the `go` directive in `go.mod`, a `ruby` line in `Gemfile`, etc.)
- Dotfiles (`.nvmrc`, `.node-version`, `.python-version`, `.ruby-version`, `rust-toolchain[.toml]`, etc.)
- Version-manager configs (`.tool-versions` for asdf/mise)
- A "Requirements" or "Prerequisites" section in the README

A repo with none of these gets a fresh contributor stuck on "which version?" — this standard exists to flag that gap for review.

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

## security-automation

**Default check**: The project has automated security scanning configured to surface vulnerabilities in source code, dependencies, and supply-chain hygiene before they ship — running on pull requests and on a schedule against the main branch.

No configurable options — this standard intentionally stays language-agnostic. The audit emits `SKIP` for manual verification.

**Manual verification** (SKIP): Confirm that the project has automation covering each of the three layers below, wired into CI so that results are visible to reviewers and the build fails on high-severity findings.

- **SAST (static analysis of source code)**: e.g., CodeQL workflow (free for public GitHub repos), Semgrep, Bandit (Python), gosec (Go), brakeman (Ruby), or equivalents in your ecosystem. Should run on PRs and on a schedule against the main branch.
- **SCA (dependency vulnerability scanning)**: e.g., GitHub's `dependency-review-action` on PRs, `npm audit --audit-level=high`, `pip-audit`, `cargo-audit`, `bundle-audit`, or the equivalent for your ecosystem — failing the build on known high-severity CVEs.
- **Supply-chain hardening**: e.g., OSSF Scorecard (`ossf/scorecard-action`) for repo-hygiene scoring, signed releases (sigstore/cosign, npm `--provenance`, PEP 740), pinned third-party action versions (commit SHA, not tag).

A project without automated security scanning ships unknown vulnerabilities and is invisible to supply-chain ecosystem signals like Scorecard. This standard exists to flag that gap for review.

---

## privacy-posture

**Default check**: The project explicitly states its privacy posture in user-facing docs (typically `README.md` and/or `SECURITY.md`).

No configurable options — this standard intentionally stays content-agnostic. The audit emits `SKIP` for manual verification.

**Manual verification** (SKIP): Confirm that the project's docs answer, in plain language, the questions a security-conscious user would ask before installing or running it:

- What data leaves the user's machine when the project runs (none, prompts/outputs, telemetry, crash reports, etc.)?
- Where does that data go — directly to a third-party API, to the project's own servers, to an analytics provider?
- What is collected, retained, or shared by the project itself (vs. by upstream services it calls)?
- Is there an opt-out, and how do users invoke it?

A vague mention of "we care about privacy" is not a posture. The bar is whether a user can answer the questions above without reading source code.

A project without an explicit privacy posture invites worst-case assumptions ("it phones home, sends my prompts to an unnamed server, retains them indefinitely"). This standard exists to flag that gap for review.

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

## package-metadata-complete

**Default check**: Beyond the bare-required fields covered by `package-metadata`, the project's distribution manifest fills in the discoverability and governance metadata an outside contributor or package-registry visitor expects — keywords/topics, author/maintainer, bugs URL, homepage URL, contributors, funding, and similar.

No configurable options — this standard intentionally stays language-agnostic. The audit emits `SKIP` for manual verification.

**Manual verification** (SKIP): Confirm that the project's manifest includes more than the bare minimum. Common discoverability/governance fields:
- **Keywords / topics**: enable package-registry search (`keywords` in `package.json`, `keywords` in `Cargo.toml`'s `[package]`, `keywords` in `pyproject.toml`'s `[project]`, etc.). Repos may also use GitHub/GitLab topics as a complement.
- **Author / maintainer**: who to contact about the package (`author` / `authors` / `maintainers` field, or a `MAINTAINERS` file).
- **Bug tracker URL**: where to file issues (`bugs` in `package.json`, `repository.issues` link, or a section in CONTRIBUTING/SUPPORT).
- **Homepage URL**: project landing page (`homepage` in `package.json`, `homepage` in `Cargo.toml`, `[project.urls].Homepage` in `pyproject.toml`).
- **Contributors / funding** (optional but signals project health): `contributors`, `funding`, `sponsors`, etc.

A manifest with only the legally-required fields gets contributors stuck on "who do I contact?", "where do I file bugs?", and prevents discoverability via package-registry search. This standard exists to flag that gap for review.

---

## metadata-quality

**Default check**: Project metadata text — not just whether fields are filled in, but whether the prose is accurate, informative, and representative — describes what the project actually does, in language that helps a stranger decide whether it's useful to them.

No configurable options — quality is a human judgment. The audit emits `SKIP` for manual verification.

**Manual verification** (SKIP): Confirm that the project's user-facing prose accurately and richly describes the project. Common surfaces to review:
- **Manifest description** (e.g., `description` in `package.json`, `description` in `Cargo.toml`'s `[package]`, `description` in `pyproject.toml`'s `[project]`). Should mention the *what* (function), the *audience* (who it's for), and the *form-factor* (CLI, library, service) — not just a 4-word summary.
- **Repo description** on GitHub/GitLab (the field beneath the project name).
- **README opening paragraph / tagline**: usually the first thing a visitor reads. Should match the manifest description in tone and substance.
- **Keywords / topics**: should be searchable terms an actual user would type, not internal jargon.
- **Tagline consistency**: manifest description, repo description, README tagline, and any `homepage` page should agree on what the project is — otherwise visitors see contradictory framings.

A project where the description reads like a 5-second placeholder ("foo CLI", "TODO", "Multi-turn driver") fails this standard even if all required fields are filled in (item `package-metadata-complete` would still PASS). Surface this gap so the maintainer can rewrite for clarity before public release.

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

## shell-completion

**Default check**: A project that distributes a CLI ships shell completion for at least bash and zsh.

No configurable options — this standard intentionally stays language- and packaging-agnostic. The audit emits `SKIP` for manual verification.

**Manual verification** (SKIP): Confirm one of the following:

- A built-in subcommand prints a completion script (e.g., `<cli> completion bash`, `<cli> completion zsh`, `<cli> completion fish`) and the README documents how to source or install the output.
- Static completion scripts ship with the package and land in a discoverable location at install time (e.g., `share/bash-completion/completions/`, `share/zsh/site-functions/`, or a documented copy step).

A CLI with no completion forces users to remember every subcommand and flag by hand. This standard exists in the optional `cli` profile (`profiles: [base, public, cli]`) so that non-CLI projects don't see a noisy WARN they can't act on.

---

## visual-demo

**Default check**: The README features an embedded visual demo of the project, prominently placed (ideally near the top).

No configurable options — this standard intentionally stays format- and language-agnostic. The audit emits `SKIP` for manual verification.

**Manual verification** (SKIP): Confirm the README embeds at least one of:

- An animated GIF or short MP4/WebM clip showing the tool in action.
- An asciinema cast (`*.cast`) embedded via the asciinema player or linked prominently.
- For CLIs, a reproducible recording (e.g., `charmbracelet/vhs` `.tape` file rendered to GIF) so the demo can be regenerated.
- A representative screenshot for libraries or UIs where motion is not the point.

Static text alone (a code block of sample output) is not a visual demo. The point is to communicate what the project does at a glance; readers who skim the README without a visual cue often bounce.

---

## comparison

**Default check**: The README discusses how the project compares to similar or alternative tools.

No configurable options — this standard intentionally stays format-agnostic. The audit emits `SKIP` for manual verification.

**Manual verification** (SKIP): Confirm the README contains at least one of:

- A comparison table contrasting the project with named peers on the dimensions readers care about (use case, philosophy, key features).
- A bullet list of differences against specific alternatives.
- A short prose section that names peer projects and explains the project's distinct positioning.

Lead with what makes the project distinct, not feature parity. Acknowledge cases where alternatives are a better fit — readers are looking for honest positioning, not marketing copy. Public projects in crowded niches benefit most; novel projects can simply state that no direct alternatives exist and explain why.

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
