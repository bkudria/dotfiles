# project.yaml Schema

Complete schema for the project configuration file. Place `project.yaml` in the project root.

## Metadata Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | string | yes | Project name |
| `description` | string | yes | One-line project description |
| `language` | string | no | Primary language (`ruby`, `javascript`, `typescript`, `python`, `rust`, `go`, `lua`, `shell`, etc.) |
| `status` | enum | no | `active`, `archived`, `experimental` (default: `experimental`) |
| `visibility` | enum | no | `public`, `private` (default: `private`) |
| `repo` | string | no | Repository URL |

## Profiles Field

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `profiles` | list of strings | no | Names of profiles from `references/profiles/`. Later profiles override earlier ones. |

Profiles provide both **metadata defaults** (under `defaults:`) and **standard configurations** (under `standards:`). Any explicit entries in project.yaml override profile values. **DRY rule**: Only include entries that differ from or add to the profile — this applies to both metadata fields and standards. If the profile defaults `status: experimental`, do not repeat it. If the profile sets `required: true` for a standard, do not repeat it.

## Standards Configuration

The `standards:` key maps standard names to their configuration. Each standard accepts:

### Common Fields (all standards)

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `required` | boolean | `true` | Whether this standard must pass. If `false`, use `recommended`. |
| `recommended` | boolean | `false` | Whether to warn (not fail) on absence. Mutually exclusive with `required: true`. |

### Standard-Specific Fields

#### readme

| Field | Type | Description |
|-------|------|-------------|
| `sections` | list of strings | Required heading names (case-insensitive) |
| `references` | list of standard names | Must contain links to these documents |

#### license

| Field | Type | Description |
|-------|------|-------------|
| `spdx` | string | SPDX license identifier |

#### tests

| Field | Type | Description |
|-------|------|-------------|
| `framework` | string | Test framework name (for metadata/display) |
| `directory` | string | Path to the test directory |
| `config` | string | Path to the test framework config file |

#### linter

| Field | Type | Description |
|-------|------|-------------|
| `tool` | string | Linter tool name (for metadata/display) |
| `config` | string | Path to the linter config file |

#### coverage

| Field | Type | Description |
|-------|------|-------------|
| `config` | string | Path to the file containing coverage configuration |
| `ratchet_pattern` | string | Grep pattern to detect ratchet/threshold in the config file |

#### claude-md

| Field | Type | Description |
|-------|------|-------------|
| `sections` | list of strings | Required heading names in CLAUDE.md |

#### ci

No standard-specific fields — just `required: true/false`.

#### All other standards

No standard-specific fields — just `required` or `recommended`.

## Complete Example (No Profile)

```yaml
# === Metadata ===
name: chorearch
description: A choreography architecture framework for Ruby
language: ruby
status: active
visibility: public
repo: https://github.com/bkudria/chorearch

# === Standards ===
standards:
  readme:
    required: true
    sections:
      - Installation
      - Usage
      - Development
    references: [goals, spec, docs]

  gitignore:
    required: true

  license:
    required: true
    spdx: MIT

  tests:
    required: true
    framework: rspec
    directory: spec

  claude-md:
    required: true
    sections:
      - Build commands
      - Test commands
      - Project overview

  goals:
    required: true

  spec:
    required: true

  linter:
    required: true

  ci:
    required: true

  coverage:
    required: true

  changelog:
    recommended: true

  contributing:
    required: true  # public project

  editorconfig:
    recommended: true

  docs:
    required: true

  code-of-conduct:
    recommended: true
```

## Minimal Example (with base profile)

The base profile provides sensible defaults — most projects only need to declare what's unique:

```yaml
name: scuttlerun
description: Multi-turn Claude session driver
language: typescript
profiles: [base]

standards:
  tests:
    framework: vitest
    directory: tests
```

The base profile handles: readme, gitignore, license (MIT), tests, claude-md, goals, spec, linter, coverage (all required), plus metadata defaults (status: experimental, visibility: private).

## Minimal Example (no profile)

```yaml
name: my-script
description: A small utility script
language: shell
status: experimental

standards:
  readme:
    required: true
  gitignore:
    required: true
  tests:
    recommended: true
```

## Profile Override Example (DRY)

When using profiles, only include overrides and additions — never repeat profile defaults.

```yaml
name: chorearch
description: A choreography architecture framework for Ruby
language: ruby
status: active       # override — base defaults to experimental
visibility: public   # override — base defaults to private
profiles: [base]

standards:
  # OVERRIDES — fields that differ from the profile
  license:
    spdx: Apache-2.0  # base defaults to MIT
  tests:
    framework: rspec
    directory: spec

  # ADDITIONS — standards the profile does not include
  ci:
    required: true
  contributing:
    required: true
  changelog:
    recommended: true
```

### What NOT to Write

```yaml
# BAD — redundant entries when using profiles
profiles: [base]
status: experimental         # REDUNDANT — matches base default
visibility: private          # REDUNDANT — matches base default
standards:
  readme:
    required: true           # REDUNDANT — base already sets this
  gitignore:
    required: true           # REDUNDANT — fully matches profile, remove entire entry
  license:
    required: true           # REDUNDANT — base already sets this
    spdx: MIT                # REDUNDANT — base already sets this
  tests:
    required: true           # REDUNDANT — base already sets this
    framework: vitest        # OK — override (not in profile)
```

## Redundancy Lint

Run `scripts/lint-project-yaml.sh <path-to-project.yaml>` to detect fields that duplicate profile defaults. Use `--fix` to auto-remove redundant entries.

A field is redundant when:
- The project declares `profiles:`
- The profile defines the same field (metadata default or standard field)
- The values are identical

Fields NOT in the profile are additions (always kept). Fields with different values from the profile are overrides (always kept).
