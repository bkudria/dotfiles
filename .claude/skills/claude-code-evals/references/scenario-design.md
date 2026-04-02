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
├── base.yml                      # Shared config (optional but recommended)
├── descriptive-scenario-id/
│   └── scenario.yml              # One scenario per directory
├── another-scenario/
│   └── scenario.yml
└── with-fixtures/
    ├── scenario.yml
    └── seed-data.json            # Fixture files (injected via project.files)
```

Scenario IDs are the directory names. Use descriptive kebab-case names that indicate what the scenario tests.

---

## base.yml

Shared defaults for all scenarios. Contains craboodle settings and scuttlerun defaults:

```yaml
version: "1"                      # Required when base.yml exists
min_pass_rate: 0.8                # Optional ratchet — exit 3 if any scenario below this
model: claude-sonnet-4-6          # Scuttlerun default model
tools:                            # Scuttlerun default tools
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

Run `craboodle --help` for the full base.yml field reference.

---

## scenario.yml

Each scenario defines a prompt, checks, and optional config overrides:

```yaml
prompt: |                         # Required: realistic user task
  Write a function that validates email addresses.

labels:                           # Optional: metadata for grouping/comparison
  name: "Email validator"
  config: optimized

context: |                        # Optional: orients the grader (defaults to prompt)
  The agent was asked to write an email validator with input validation enabled.

checks:                       # Required: at least 1 check
  - check: "Output validates input format before processing"
    note: "Look for regex or string parsing that checks for @ and domain"
  - check: "Function handles edge cases like empty string"
  - check: "Output includes at least one test or example usage"

repeats: 5                        # Optional: override --repeats for this scenario

scuttlerun:                       # Optional: config overrides (passthrough to scuttlerun)
  model: claude-sonnet-4-6
  project:
    claude_md: |
      Always validate user input before processing.
```

Run `craboodle --help` for the full scenario.yml field reference.

---

## Comparison Patterns

Eval's real power is comparing behavior across configurations. Use labels to tag variants, then compare results downstream.

### Before/After

Test the same scenarios before and after a config change:

```bash
# Before: run and save results
craboodle run my-evals/ > results-before.yml

# Make your config change, then:
craboodle run my-evals/ > results-after.yml

# Compare pass rates
diff <(yq '.scenarios[].pass_rate' results-before.yml) \
     <(yq '.scenarios[].pass_rate' results-after.yml)
```

### With/Without a Config

Two scenarios testing the same task — one with the configuration, one baseline:

```yaml
# with-tdd-instruction/scenario.yml
prompt: |
  Write a function called isPrime. Save it to prime.js.
labels:
  name: "With TDD instruction"
  config: with-tdd
checks:
  - check: "Tests were written before or alongside production code"
  - check: "At least one test validates prime behavior"
scuttlerun:
  project:
    claude_md: |
      Always write tests before production code.
```

```yaml
# without-tdd-instruction/scenario.yml
prompt: |
  Write a function called isPrime. Save it to prime.js.
labels:
  name: "Baseline (no TDD instruction)"
  config: baseline
checks:
  - check: "Tests were written before or alongside production code"
  - check: "At least one test validates prime behavior"
# No scuttlerun.project.claude_md — baseline behavior
```

If the "with" scenario passes at 0.9 and the "without" at 0.3, the instruction demonstrably changes behavior.

### Model Comparison

Same scenarios, different models:

```yaml
# base.yml — shared checks and prompt
version: "1"
user:
  turn_policy: single
```

```yaml
# sonnet-variant/scenario.yml
prompt: "Write a function to merge two sorted arrays efficiently."
labels:
  name: "Merge sorted arrays"
  model: sonnet-4-6
checks:
  - check: "Uses O(n) two-pointer approach, not O(n log n) concat+sort"
scuttlerun:
  model: claude-sonnet-4-6
```

```yaml
# haiku-variant/scenario.yml
prompt: "Write a function to merge two sorted arrays efficiently."
labels:
  name: "Merge sorted arrays"
  model: haiku-4-5
checks:
  - check: "Uses O(n) two-pointer approach, not O(n log n) concat+sort"
scuttlerun:
  model: claude-haiku-4-5
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

## Labels

Labels are key-value metadata that pass through to craboodle's output. Craboodle doesn't interpret them — they enable downstream grouping and comparison.

Common label keys:

| Key | Purpose | Example Values |
|-----|---------|---------------|
| `name` | Human-readable scenario title | "TDD under time pressure" |
| `config` | Which config variant | "with-skill", "baseline", "optimized" |
| `model` | Which model | "sonnet-4-6", "haiku-4-5" |
| `variant` | General variant identifier | "strict-mode", "permissive" |
| `concern` | What aspect is being tested | "correctness", "style", "safety" |

---

## Fixture Files

Place additional files alongside `scenario.yml` and inject them via `scuttlerun.project.files`:

```yaml
prompt: "Review the code in app.py and suggest improvements."
scuttlerun:
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
