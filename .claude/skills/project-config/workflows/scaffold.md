# Scaffold Mode

> **References:** `references/project-yaml-schema.md` (complete schema, examples, DRY rules), `references/profiles/` (profile definitions), `references/standards-catalog.md` (standard details for file creation).

**STOP. Do not create any files until the interview is complete and project.yaml is written.**

Scaffold mode has a strict ordering. Violating this order (e.g., creating README.md before project.yaml exists) is incorrect.

## Step 1: Interview (mandatory)

The `base` profile is always included. **Only ask about fields the profile does NOT default**:

- **Ask:** name, description, language
- **Ask:** which additional standards to enable beyond the profile (present the full catalog from `references/standards-catalog.md`)
- **Ask:** standard-specific overrides only (e.g., test framework/directory if not the language default)
- **Do NOT ask about:** status, visibility, license SPDX, or any other field the selected profile already defaults. Assume profile defaults unless the user volunteers an override.

Do not create any files until the interview is complete.

## Step 2: Generate project.yaml (mandatory, first file created)

The **first file written** must be `project.yaml`. It contains:
- Only metadata that differs from profile defaults (typically just name, description, language)
- A `profiles:` declaration (e.g., `profiles: [base]`)
- Under `standards:`, **only overrides and additions** — NOT profile defaults

**DRY rule**: When a profile is selected, do NOT repeat anything the profile already provides — this applies to both metadata defaults and standards. Only include:
- **Metadata overrides**: Fields with values different from the profile defaults (e.g., `status: active` when the profile defaults to `experimental`)
- **Standard additions**: Standards not in the profile (e.g., `ci`) — these need `required: true`
- **Standard overrides**: Fields with values different from the profile (e.g., specific `framework`) — omit `required: true` if the profile already sets it

This is the central artifact. All other files are derived from what project.yaml declares.

Example using the base profile:

```yaml
name: scuttlerun
description: Multi-turn Claude session driver
language: typescript
profiles: [base]

# Only overrides — profile handles the rest
standards:
  tests:
    framework: vitest
    directory: tests
```

The base profile provides: readme, gitignore, license (MIT), tests, claude-md, goals, spec, linter, coverage — all required. It also defaults status to `experimental` and visibility to `private`. None of that needs repeating.

## Step 3: Create standard files

Based on what project.yaml declares (including profile-inherited standards), create the required files:
README.md, GOALS.md, SPEC.md, LICENSE, .gitignore, CLAUDE.md, CONTRIBUTING.md, etc.

## Step 4: Verify

Run an immediate audit (see `workflows/audit.md`) to confirm all declared standards pass.

## Profiles

Profiles are presets stored in `references/profiles/` as YAML files. A profile provides metadata defaults and standard configurations that can be overridden per-project.

### Built-in Profiles

| Profile | Metadata Defaults | Required Standards |
|---------|-------------------|-------------------|
| `base` | status: experimental, visibility: private | readme, gitignore, license (MIT), tests, claude-md, goals, spec, linter, coverage |

### Using a Profile

Reference it in project.yaml with `profiles: [base]`. Profile values are defaults — any explicit entries in project.yaml override the profile (both metadata and standards). To keep project.yaml DRY, only include entries that differ from the profile. Use `scripts/lint-project-yaml.sh` to check for redundant entries.

### Creating Custom Profiles

1. Create `references/profiles/{profile-name}.yaml`
2. Define `defaults:` for metadata and `standards:` for standard configurations
3. Reference it in project.yaml with `profiles: [{profile-name}]`

Multiple profiles can be composed: `profiles: [base, ruby-gem]`. Later profiles override earlier ones.
