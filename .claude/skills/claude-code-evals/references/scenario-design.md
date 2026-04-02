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
user:
  turn_policy: single             # Single-turn by default
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

## Comparison Patterns

Eval's real power is comparing behavior across configurations. Define variant scenarios, then compare results downstream.

### Before/After

Test the same scenarios before and after a config change:

```bash
# Before: run and save results
craboodle run my-evals/ > results-before.yaml

# Make your config change, then:
craboodle run my-evals/ > results-after.yaml

# Compare pass rates
diff <(yq '.scenarios[].pass_rate' results-before.yaml) \
     <(yq '.scenarios[].pass_rate' results-after.yaml)
```

### With/Without a Config

Two scenarios testing the same task — one with the configuration, one baseline. Each scenario has a separate `scenario.yaml` and `checks.yaml`:

```yaml
# with-tdd-instruction/scenario.yaml
prompt: |
  Write a function called isPrime. Save it to prime.js.
project:
  claude_md: |
    Always write tests before production code.
```

```yaml
# with-tdd-instruction/checks.yaml
context: |
  The agent was asked to write an isPrime function with TDD instructions.

checks:
  - test-before-code:
      check: "Tests were written before or alongside production code"
      note: "Look for Write tool calls — test file should appear before the main implementation file"
  - test-validates-behavior:
      check: "At least one test validates prime behavior"
```

```yaml
# without-tdd-instruction/scenario.yaml
prompt: |
  Write a function called isPrime. Save it to prime.js.
# No project.claude_md — baseline behavior
```

```yaml
# without-tdd-instruction/checks.yaml
context: |
  The agent was asked to write an isPrime function with no special instructions.

checks:
  - test-before-code:
      check: "Tests were written before or alongside production code"
  - test-validates-behavior:
      check: "At least one test validates prime behavior"
```

If the "with" scenario passes at 0.9 and the "without" at 0.3, the instruction demonstrably changes behavior.

### Model Comparison

Same scenarios, different models:

```yaml
# craboodle.yaml
version: "1"
```

```yaml
# base.yaml — shared scuttlerun defaults
user:
  turn_policy: single
```

```yaml
# sonnet-variant/scenario.yaml
prompt: "Write a function to merge two sorted arrays efficiently."
model: claude-sonnet-4-6
```

```yaml
# sonnet-variant/checks.yaml
checks:
  - efficient-algorithm:
      check: "Uses O(n) two-pointer approach, not O(n log n) concat+sort"
```

```yaml
# haiku-variant/scenario.yaml
prompt: "Write a function to merge two sorted arrays efficiently."
model: claude-haiku-4-5
```

```yaml
# haiku-variant/checks.yaml
checks:
  - efficient-algorithm:
      check: "Uses O(n) two-pointer approach, not O(n log n) concat+sort"
```

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
