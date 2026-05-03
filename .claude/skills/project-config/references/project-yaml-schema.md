# project.yaml Schema

`project.yaml` lives in the project root and declares which profiles apply and which inherited standards (if any) are disabled. The schema is intentionally minimal — every other detail (which language, which framework, which sections to require) is the responsibility of individual standard YAMLs under `profiles/`.

## Top-level keys

Exactly three top-level keys are accepted. Anything else fails `scripts/lint-project-yaml.sh`.

| Key | Type | Required | Description |
|-----|------|----------|-------------|
| `profiles` | list of strings | yes | Names of profiles to activate. Each must match a directory under `profiles/`. |
| `disabled` | map of strings | no | Map of `<profile>/<basename>` → non-empty reason string. |
| `required` | list of strings | no | List of `<profile>/<basename>` whose unmet result is upgraded from `SUGG` to `FAIL` for this project. |

There is no metadata block (no `name`, `description`, `language`, `status`, `visibility`, `repo`). There is no `standards:` block — checks themselves are not parameterized per-project; only their severity is.

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

## `required:`

A list of `<profile>/<basename>` strings (matching the audit identity of an activated standard) whose unmet result is upgraded from `SUGG` to `FAIL` for this project. Useful when a standard is shipped as suggested in its profile but the project wants to enforce it. Symmetric with `disabled:`: both override an inherited standard's severity, in opposite directions.

```yaml
required:
  - base/lockfile
  - public/release-automation
```

The override only changes how an *unmet* result is reported. A standard that already passes is unaffected. Standards already declared `required: true` in their YAML must not be listed here — that's a no-op.

When an audit completely passes (zero `FAIL`, zero `SUGG`), `--render` appends a "lock-in" suggestion block listing every PASSing standard whose underlying YAML has `required: false` AND that is not already in this list. The suggestion is purely informational — adopting it tightens future regression severity from `SUGG` to `FAIL`.

Lint fails on:

- An entry that does not match `<profile>/<basename>`.
- An entry whose profile is not in the project's `profiles:` list.
- An entry whose `<basename>` does not exist as a YAML file in the named profile.
- An entry whose underlying standard is already `required: true` (no-op).
- An entry that also appears as a key in `disabled:` (mutually exclusive — a standard cannot be both disabled and upgraded).
- A `required:` value that is not a list (e.g., a map or scalar).

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

**Project that tightens a suggested standard into a required one:**

```yaml
profiles: [base, public]

required:
  - base/lockfile
  - public/release-automation
```

**CLI tool:**

```yaml
profiles: [base, public, cli]
```

## What is NOT in this file

The schema deliberately drops the following — none of them are accepted, all of them fail lint:

- Metadata: `name`, `description`, `language`, `status`, `visibility`, `repo`.
- Per-standard parameters: `tests.framework`, `tests.directory`, `license.spdx`, `coverage.ratchet`, `coverage.config`, `linter.tool`, `linter.config`, `claude-md.sections`, `readme.sections`, `readme.references`, `package-metadata.manifest`, etc. The behaviour of a check is fully described inside its own YAML and is not parameterized per-project.
- Severity enums: `recommended:` is gone. A standard declares `required: true` (failure is FAIL) or `required: false` (failure is SUGG) in its own YAML. A project may upgrade individual standards via `required:` (this file), but cannot loosen them — that's what `disabled:` is for.
- Profile composition operators: there is no deep-merge or "later overrides earlier" — each profile's standards run independently.

If you need a stricter *check* (not just stricter severity), add a separate standard YAML in a profile directory (e.g., `profiles/public/readme-sections.yaml` is a separate file from `profiles/base/readme.yaml`).

## Standard YAML schema

Each YAML file under `profiles/<profile>/` is a self-contained standard. The standard's identity is its filename without `.yaml`; there is no `name:` field.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `required` | boolean | yes | `true` ⇒ unmet causes audit failure (`FAIL`). `false` ⇒ unmet is reported as a suggestion (`SUGG`), does not fail audit. |
| `description` | string | yes | One-line prose explaining what this standard verifies. Persisted into `collect-required.json` / `collect-suggested.json` and `merged.json`; consulted during fix-plan synthesis (`workflows/audit.md` step 4) to give plan items the standard's intent. Not printed by `--render` directly. |
| `check` | object | yes | Exactly one of `check.script` or `check.prompt`. Never both, never neither. |
| `check.script` | string | — | Bash script executed under `set -euo pipefail` with `$PROJECT_ROOT` set. Exit 0 = met; non-zero = unmet. The last non-empty stdout line becomes the row's `Detail`. |
| `check.prompt` | string | — | Prompt rendered with `$PROJECT_ROOT` substituted, then sent to a sub-agent for verification. The sub-agent returns a `{"met": bool, "detail": string}` JSON block. |
| `notes` | string | no | Multi-paragraph maintainer-facing context (file precedence rules, why this standard exists, edge cases, links). For prompt-based standards, notes are threaded into the rendered verifier prompt as a labeled background section between `project_context` and the check body. For script-based standards, notes are not surfaced at runtime (there is no verifier to read them). Never surfaced in the rendered audit table or `--render` output. |

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
- The prompt's responsibility is **what** to verify and what evidence to surface. The audit workflow (`workflows/audit.md` step 2) wraps every prompt with a response-format directive that requires the verifier to use the Write tool to save its verdict — exactly one raw JSON object of the form `{"met": bool, "detail": string}`, with no fenced code block and no surrounding prose — to its `response_path`. **Do not specify a response format inside the prompt itself** — the wrapper handles it, and a duplicated/conflicting instruction in the prompt would compete with the wrapper.
- Convention: phrase the reporting expectation as "Report met (with `<evidence>`) or unmet (with `<gap>`)". This produces a natural one-line `detail` that the verifier emits as the JSON object's `detail` field. Every existing prompt-based standard follows this pattern.
- The audit workflow combines `met` with the standard's `required:` flag to produce `PASS`/`FAIL`/`SUGG`. There is no intermediate `MANUAL` row in the audit table — prompt-based standards resolve to one of the three statuses before the table is rendered.

### When to use script vs prompt

Pick `check.script` when met/unmet reduces to file/dir presence or pattern matching on fixed paths — anything bash can verify without parsing structured config formats or interpreting content semantically. Pick `check.prompt` when the check requires detecting the ecosystem to know where to look, parsing structured config (JSON/TOML/YAML) to inspect a value, or interpreting human-written content for substantive meaning.

| Pick `script` when | Pick `prompt` when |
|---|---|
| File or directory presence at fixed paths (`[[ -e ]]`, `[[ -d ]]`) | Determining the file/field to inspect requires knowing the ecosystem |
| Grepping for a literal pattern in a fixed file (`grep -q '^# ' README.md`) | Reading a value out of structured config (JSON/TOML/YAML) |
| Search space is finite, listable, language-independent | Interpreting natural-language content for meaning |
| One mechanism is canonical across ecosystems | Multiple equally-valid mechanisms exist (Dependabot vs Renovate vs scheduled CI) |

Worked examples:

- **`base/tests` (script)** — looks for `spec/`, `test/`, `tests/`, or `__tests__/`. Four fixed directories, finite, language-independent.
- **`base/coverage` (prompt)** — verifies that a coverage threshold is configured. The threshold lives in language-specific config (`jest.config.*`, `pyproject.toml`, `.coveragerc`, `vitest.config.*`, etc.) and requires reading a *value*, not just presence — a script would need to parse several structured formats.
- **`base/runtime-version` (prompt)** — declared via the `engines` field in `package.json`, OR `.nvmrc`/`.node-version`, OR `.tool-versions`/`mise.toml`, OR README prose. The dotfile sources are pure presence checks, but the `engines` source requires JSON parsing — and the standard's intent is to verify *any* of these, so the whole check tips into prompt territory.
- **`public/security-policy` (script)** — looks for `SECURITY.md`, `SECURITY`, `SECURITY.txt`, or `.github/SECURITY.md`. Four fixed paths, no parsing, language-independent → script.

When a check straddles the line, pick `prompt`. The sub-agent dispatch is cheap (haiku tier per `workflows/audit.md`); the cost of an incorrect script is a brittle, language-specific check that silently fails the moment the project onboards a new ecosystem.

## Lint

`scripts/lint-project-yaml.sh <project-root>/project.yaml` validates a project.yaml: top-level keys, profile existence, disabled keys/values, and that each disabled key resolves to an existing standard in a selected profile.

`scripts/lint-project-yaml.sh --skill` (no project.yaml needed) validates **every** standard YAML across all profile directories: required fields, exactly-one-of check shape, `notes:` non-empty if present, no unknown keys.
