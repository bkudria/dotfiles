# Scaffold Mode

> **References:** `references/project-yaml-schema.md` (full schema), `profiles/` (every standard YAML in each profile directory describes what that profile activates).

**STOP. Do not create any files until the interview is complete and project.yaml is written.**

Scaffold mode has a strict ordering. The first file written must be `project.yaml`. Every other file is created based on what `project.yaml`'s selected profiles activate.

## Step 1: Interview (mandatory)

Ask the user:

1. **Which profiles to enable.** The profiles available are the directories under `profiles/`. Each profile's standards are the YAML files inside its directory.
2. **Whether any standards should be disabled** for this project (with reasoning). A disabled standard must specify why — empty reasons fail lint. Disabling is for legitimate exemptions (e.g., "single-maintainer pre-1.0 project; CoC adoption deferred"), not for hiding inconvenient truths.
3. **Whether any suggested standards should be upgraded to required** (severity tightened from `SUGG` to `FAIL`). List them under `required:`. Capture the rationale in conversation; the file itself stores no reason. Use this when the project enforces a standard that its profile ships as a suggestion (e.g., a Bun project committing the lockfile).

Do not ask about project name, description, language, status, visibility, repo, or any per-standard parameters. The new schema does not support them. The skill is intentionally language- and tool-agnostic; standards inspect the project itself rather than reading declarations.

## Step 2: Generate project.yaml (mandatory, first file created)

The first file written is `project.yaml`. The schema accepts exactly three top-level keys:

- `profiles:` — list of profile names matching directories under `profiles/`.
- `disabled:` — optional map of `<profile>/<basename>` → non-empty reason string.
- `required:` — optional list of `<profile>/<basename>` whose unmet result is upgraded from `SUGG` to `FAIL` for this project.

Minimal example:

```yaml
profiles: [base]
```

Public OSS project disabling a standard:

```yaml
profiles: [base, public]

disabled:
  public/code-of-conduct: "Single-maintainer pre-1.0 project; CoC adoption deferred until v1.0."
  public/comparison: "Novel project — no direct alternatives exist."
```

Anything else (`name`, `description`, `language`, `status`, `visibility`, per-standard knobs like `tests.framework` or `license.spdx`) fails lint. The standards inspect the project directly to figure out what they need.

After writing `project.yaml`, run `scripts/lint-project-yaml.sh <project-root>/project.yaml`. Lint must exit 0 before continuing.

## Step 3: Create standard files

For each standard activated by the selected profiles (and not disabled in `project.yaml`), create the files it expects. Read the standard's YAML to understand what it checks. For example:

- `base/readme.yaml` checks for a README.md (or other conventional README filename) with a heading → create `README.md`.
- `base/license.yaml` checks for a LICENSE file → create `LICENSE`.
- `base/gitignore.yaml` checks for `.gitignore` → create `.gitignore`.
- `base/claude-md.yaml` checks for CLAUDE.md → create `CLAUDE.md`.
- `base/goals.yaml`, `base/spec.yaml` → create `GOALS.md`, `SPEC.md`.
- `public/contributing.yaml` → create `CONTRIBUTING.md`.
- `public/changelog.yaml` → create `CHANGELOG.md`.
- `public/code-of-conduct.yaml` → create `CODE_OF_CONDUCT.md` (skip if disabled).
- `public/security-policy.yaml` → create `SECURITY.md`.
- `public/editorconfig.yaml` → create `.editorconfig`.

For standards that need configuration files (linter config, formatter config, CI config, package manifest, etc.), choose a sensible default appropriate to the project's actual language/tooling and create it.

## Step 4: Verify

Run an immediate audit (see `workflows/audit.md`). The audit must show zero `FAIL` rows for the scaffold to be considered successful. `SUGG` rows are non-blocking but worth reviewing.

## Profiles

Profiles are directories under `profiles/`. Selecting `profiles: [base]` activates **every YAML file** in `profiles/base/`. Profiles do not merge or override each other — if both `profiles/base/readme.yaml` and `profiles/public/readme-sections.yaml` exist and both profiles are selected, both checks run independently.

To disable a profile-inherited standard for a specific project, list its `<profile>/<basename>` in the project's `disabled:` map with a non-empty reason. To strengthen a check (e.g., require additional README sections beyond what `base/readme.yaml` enforces), add a separate standard YAML to a profile directory rather than parameterizing an existing one.

### Adding a new profile

Create `profiles/<profile-name>/` and drop self-contained standard YAMLs into it. Each standard YAML must specify `required: bool`, `description: <one-line>`, and exactly one of `check.script` or `check.prompt`. Optional `notes:` carries maintainer-facing context. See `references/project-yaml-schema.md` for the full standard YAML schema.
