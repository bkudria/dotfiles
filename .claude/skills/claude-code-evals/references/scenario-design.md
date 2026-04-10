# Scenario Design

Structure eval scenarios to test meaningful behavior changes from your configurations.

---

## Design Principles

1. **Exercise the core value proposition** — Each scenario should test something the configuration is specifically designed to improve
2. **Use realistic tasks** — Prompts should resemble what a real user would ask. Avoid contrived or toy examples
3. **Vary difficulty** — Include easy scenarios (config clearly helps), medium (config's guidance matters), and hard (edge cases)
4. **Cover different aspects** — If the config has multiple behavioral effects, write scenarios that exercise different parts

---

## Directory Structure

```
evals/
├── craboodle.yaml                # Pipeline config (version, min_pass_rate, repeats, etc.)
├── base.yaml                     # Scuttlerun defaults (model, tools, user, project)
├── descriptive-scenario-id/
│   ├── scenario.yaml             # Scuttlerun config (prompt + overrides)
│   └── checks.yaml               # Pincenez config (context + checks)
├── another-scenario/
│   ├── scenario.yaml
│   └── checks.yaml
└── with-fixtures/
    ├── scenario.yaml
    ├── checks.yaml
    └── seed-data.json            # Fixture files (injected via project.files)
```

Scenario IDs are the directory names. Use descriptive kebab-case names that indicate what the scenario tests.

---

## craboodle.yaml

Pipeline-level configuration. Lives at the root of the evals directory:

```yaml
version: "1"                      # Required
min_pass_rate: 0.8                # Optional ratchet — exit 3 if any scenario below this
max_budget_usd: 5.0               # Optional spending limit
repeats: 5                        # Default repetitions per scenario
```

This file contains only craboodle-specific fields. Scuttlerun defaults belong in `base.yaml`.

---

## base.yaml

Shared scuttlerun defaults for all scenarios. Contains only fields that scuttlerun understands — no craboodle keys like `version` or `min_pass_rate`:

```yaml
model: claude-sonnet-4-6          # Default model
tools:                            # Default tools
  - Read
  - Write
  - Bash
  - Glob
  - Grep
  - Edit
project:
  claude_md: |                    # Shared CLAUDE.md (scenarios can override)
    Use relative paths.
```

Run `craboodle --help` for the full base.yaml field reference.

---

## scenario.yaml

Each scenario's scuttlerun configuration. Contains only scuttlerun fields — prompt plus any per-scenario overrides. Overrides are top-level fields (no nested `scuttlerun:` block):

```yaml
prompt: |                         # Required: realistic user task
  Write a function that validates email addresses.

model: claude-sonnet-4-6         # Optional: override base.yaml model
project:
  claude_md: |                    # Optional: override base.yaml CLAUDE.md
    Always validate user input before processing.
```

Do not put checks, context, repeats, or labels in scenario.yaml. Those belong in checks.yaml or craboodle.yaml.

Run `craboodle --help` for the full scenario.yaml field reference.

---

## checks.yaml

Each scenario's pincenez configuration. Contains checks and optional grading context:

```yaml
context: |                        # Optional: orients the grader (defaults to prompt)
  The agent was asked to write an email validator with input validation enabled.

checks:                           # Required: at least 1 check, id-as-key format
  - validates-input:
      check: "Output validates input format before processing"
      note: "Look for regex or string parsing that checks for @ and domain"
  - handles-edge-cases:
      check: "Function handles edge cases like empty string"
  - includes-tests:
      check: "Output includes at least one test or example usage"
```

Checks use id-as-key format: each list item is a single-key object where the key is the check ID and the value contains `check:` (required) and `note:` (optional).

---

## Suite Evolution

An eval suite is a living artifact that evolves alongside the configuration it tests.

### When to Add Scenarios

- **New behavior** — Added a new instruction or skill section? Add a scenario exercising it
- **Discovered gaps** — A check always passes? You may be missing a scenario that pressures the config
- **Edge cases** — Found a prompt where the config fails? Capture it as a scenario
- **Different dimensions** — A config with multiple effects needs scenarios covering each one

### When to Improve Existing Scenarios

- **Flaky checks** (pass_rate 0.3-0.7) — The check or the config needs tightening. Read failure evidence and iterate
- **Always-passes checks** — May test baseline behavior, not config value. Revise to target what the config specifically adds
- **Poor targeting** — Checks that test outcomes without testing the *method* the config teaches

### When to Retire Scenarios

- **Config changed** — The scenario tests behavior the config no longer covers
- **Redundant** — Two scenarios test the same dimension with no additional signal
- **Stale prompts** — The task is no longer realistic or representative

### Running After Changes

Run your eval suite after config changes to catch regressions:

```bash
craboodle run my-evals/
```

Review any scenarios with degraded pass rates. Diagnose whether the regression is a config problem or a check problem (see `results-interpretation.md`).

---

## Minimum Scenario Count

- **3 scenarios**: Minimum for any eval suite
- **5 scenarios**: Recommended for configs with multiple behaviors
- **10 scenarios**: For critical configs or configs with many behavioral dimensions

---

## Prompt Design

- **Realistic tasks** — Use tasks a real user would ask. Avoid meta-prompts like "demonstrate that you follow the CLAUDE.md instructions"
- **Don't give away the answer** — If testing whether a config causes TDD behavior, don't say "write tests first" in the prompt. Let the config do the work
- **Vary pressure** — Include at least one scenario that tempts the agent to skip the configured behavior (e.g., "this is trivial, just do it quickly")
- **Match the config's domain** — If the config is about database access, use database-related tasks

---

## Fixture Files

Place additional files alongside `scenario.yaml` and inject them via `project.files`:

```yaml
# scenario.yaml
prompt: "Review the code in app.py and suggest improvements."
project:
  files:
    app.py: |
      import subprocess
      def run_command(user_input):
          return subprocess.run(user_input, shell=True)  # Security issue
    tests/test_app.py: |
      def test_run_command():
          result = run_command("echo hello")
          assert result.returncode == 0
```

Fixtures create realistic project contexts. Use them to provide code for review, seed data for processing, or existing files that the agent should interact with.
