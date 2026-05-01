# project.yaml Schema

`project.yaml` lives in the project root and declares which profiles apply and which inherited standards (if any) are disabled. The schema is intentionally minimal — every other detail (which language, which framework, which sections to require) is the responsibility of individual standard YAMLs under `profiles/`.

## Top-level keys

Exactly two top-level keys are accepted. Anything else fails `scripts/lint-project-yaml.sh`.

| Key | Type | Required | Description |
|-----|------|----------|-------------|
| `profiles` | list of strings | yes | Names of profiles to activate. Each must match a directory under `profiles/`. |
| `disabled` | map of strings | no | Map of `<profile>/<basename>` → non-empty reason string. |

There is no metadata block (no `name`, `description`, `language`, `status`, `visibility`, `repo`). There is no `standards:` block — standards are not configurable per-project.

## `profiles:`

A non-empty list of profile names. Each name must match an existing directory under `profiles/`. Selecting a profile activates **every** standard YAML in that directory. Profiles do not merge: if two profiles both define a standard with the same basename, both run independently and report independently. Standard identity in the audit is `<profile>/<basename>`.

```yaml
profiles: [base, public]
```

## `disabled:`

A map whose keys are `<profile>/<basename>` strings (matching the audit identity of an activated standard) and whose values are non-empty reason strings. Disabled standards are omitted from the audit table entirely — they have no row, no status, and do not contribute to any count. The reasons live in `project.yaml`; the audit table only surfaces a count line ("N standards disabled in project.yaml").

```yaml
disabled:
  public/code-of-conduct: "Single-maintainer pre-1.0 project; CoC adoption deferred until v1.0."
  public/comparison: "Novel project — no direct alternatives exist."
```

Lint fails on:

- Empty/missing reason.
- A `disabled:` key that does not match `<profile>/<basename>`.
- A `disabled:` key whose profile is not in the project's `profiles:` list.
- A `disabled:` key whose `<basename>` does not exist as a YAML file in the named profile.

## Examples

**Minimal — base profile only:**

```yaml
profiles: [base]
```

**Public OSS project with selective disables:**

```yaml
profiles: [base, public]

disabled:
  public/code-of-conduct: "Single-maintainer pre-1.0 project; CoC adoption deferred until v1.0."
  public/comparison: "Novel project — no direct alternatives exist."
```

**CLI tool:**

```yaml
profiles: [base, public, cli]
```

## What is NOT in this file

The new schema deliberately drops the following — none of them are accepted, all of them fail lint:

- Metadata: `name`, `description`, `language`, `status`, `visibility`, `repo`.
- Per-standard parameters: `tests.framework`, `tests.directory`, `license.spdx`, `coverage.ratchet`, `coverage.config`, `linter.tool`, `linter.config`, `claude-md.sections`, `readme.sections`, `readme.references`, `package-metadata.manifest`, etc.
- Severity enums: `recommended:` is gone; standards declare `required: true` (failure is FAIL) or `required: false` (failure is SUGG) inside their own YAML.
- Profile composition operators: there is no deep-merge or "later overrides earlier" — each profile's standards run independently.

If you need a stricter check, add a separate standard YAML in a profile directory (e.g., `profiles/public/readme-sections.yaml` is a separate file from `profiles/base/readme.yaml`).

## Standard YAML schema

Each YAML file under `profiles/<profile>/` is a self-contained standard. The standard's identity is its filename without `.yaml`; there is no `name:` field.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `required` | boolean | yes | `true` ⇒ unmet causes audit failure (`FAIL`). `false` ⇒ unmet is reported as a suggestion (`SUGG`), does not fail audit. |
| `description` | string | yes | One-line prose explaining what this standard verifies. Surfaced in the remediation list and lint summary. |
| `check` | object | yes | Exactly one of `check.script` or `check.prompt`. Never both, never neither. |
| `check.script` | string | — | Bash script executed under `set -euo pipefail` with `$PROJECT_ROOT` set. Exit 0 = met; non-zero = unmet. The last non-empty stdout line becomes the row's `Detail`. |
| `check.prompt` | string | — | Prompt rendered with `$PROJECT_ROOT` substituted, then sent to a sub-agent for verification. The sub-agent returns a `{"met": bool, "detail": string}` JSON block. |
| `notes` | string | no | Multi-paragraph maintainer-facing context (file precedence rules, why this standard exists, edge cases, links). Never surfaced in audit output. |

### Deterministic example

```yaml
required: true
description: A README file exists, is non-empty, and has at least one heading.
notes: |
  Looks for any of these filenames in priority order: README.md, README,
  README.txt, README.rdoc, README.org. The first match wins; subsequent
  variants are not checked. An empty file or one with no headings counts
  as unmet.
check:
  script: |
    cd "$PROJECT_ROOT"
    for f in README.md README README.txt README.rdoc README.org; do
      [[ -e "$f" ]] || continue
      [[ -s "$f" ]] || { echo "$f exists but is empty"; exit 1; }
      grep -q '^#' "$f" || { echo "$f exists but has no headings"; exit 1; }
      echo "$f exists, has heading"
      exit 0
    done
    echo "No README file found"
    exit 1
```

### Prompt-based example

```yaml
required: false
description: The project commits a language-appropriate lockfile.
notes: |
  Prompt-based because conventional lockfile filenames vary by ecosystem
  (Cargo.lock, package-lock.json, yarn.lock, pnpm-lock.yaml, Gemfile.lock,
  poetry.lock, uv.lock, etc.). The verifier determines the language from
  manifest files and checks for the conventional lockfile of that ecosystem.
check:
  prompt: |
    Verify that the project at $PROJECT_ROOT commits a language-appropriate
    lockfile. Determine the language and package manager from manifest
    files in the project root, then check for the conventional lockfile of
    that ecosystem. Report met (with the lockfile path found) or unmet
    (with what was looked for and not found).
```

### Script contract

- `$PROJECT_ROOT` is set when the script runs.
- Script runs under `set -euo pipefail`. Standards may relax that internally if needed.
- Exit 0 = met. Exit non-zero = unmet.
- Stdout's last non-empty line = the row's `Detail`.

### Prompt contract

- `$PROJECT_ROOT` placeholder is interpolated at runtime (literal string substitution, before sending the prompt to the sub-agent).
- Manual verification (sub-agent) returns `{"met": bool, "detail": string}` in a fenced JSON code block. The audit workflow combines `met` with the standard's `required:` flag to produce `PASS`/`FAIL`/`SUGG`. There is no intermediate `MANUAL` row in the audit table — prompt-based standards resolve to one of the three statuses before the table is rendered.

## Lint

`scripts/lint-project-yaml.sh <project-root>/project.yaml` validates a project.yaml: top-level keys, profile existence, disabled keys/values, and that each disabled key resolves to an existing standard in a selected profile.

`scripts/lint-project-yaml.sh --skill` (no project.yaml needed) validates **every** standard YAML across all profile directories: required fields, exactly-one-of check shape, `notes:` non-empty if present, no unknown keys.
