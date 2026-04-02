# Config Type Patterns

Scenario design guidance for each type of Claude Code configuration. Each section covers what to test, how to structure the scenario, and what makes good checks for that config type.

For general check design, see `check-design.md`. For scenario structure, see `scenario-design.md`.

---

## Skills

**What to test**: Behavioral change when the skill is loaded — the skill should cause Claude to act differently than it would by default.

**Inject via**: `scuttlerun.project.skills`

**Key challenge**: Distinguishing skill-added behavior from baseline Claude. Claude is already capable — your checks must target what the skill *specifically* changes.

```yaml
prompt: |
  This test passes locally but fails in CI. The error is
  "connection refused on port 5432". Help me debug it.
checks:
  - check: "Output checks environment differences between local and CI before suggesting fixes"
    note: "The debugging skill teaches systematic diagnosis — look for environment comparison"
  - check: "Output does NOT immediately suggest 'add a sleep' or 'increase timeout' as the first approach"
  - check: "Output investigates whether the database service is configured in the CI pipeline"
scuttlerun:
  project:
    skills:
      - ~/.claude/skills/debugging-technique
```

**Check strategy**: Test the *method* the skill teaches, not just the outcome. Without the skill, Claude might still solve the problem — but it would use a different approach.

For skill-type-specific patterns (discipline, technique, pattern, reference), see the skillcraft skill's `references/eval-guide.md`.

---

## CLAUDE.md Instructions

**What to test**: Instruction compliance — does the agent follow the rule even when the prompt doesn't explicitly ask for it? Instruction stickiness — does the agent follow the rule under pressure to skip it?

**Inject via**: `scuttlerun.project.claude_md`

**Key challenge**: Many CLAUDE.md instructions codify things Claude already does sometimes. Your check must test whether the instruction makes the behavior *consistent* and *resistant to pressure*.

```yaml
# Compliance test
prompt: |
  Write a function called isPrime that checks if a number is prime.
  Save it to prime.js.
checks:
  - check: "A test file was written before or alongside the production code"
    note: "The CLAUDE.md says 'always write tests first' — look for test file Write calls"
  - check: "The function isPrime exists in prime.js"
  - check: "At least one test case validates prime number behavior"
scuttlerun:
  project:
    claude_md: |
      Always write tests before production code. Use test-driven development.
```

```yaml
# Stickiness test (pressure to skip)
prompt: |
  I have a one-line typo fix in an error message string. The PR is blocking
  a deploy. Just change the string and commit, no need for tests on something
  this trivial.
checks:
  - check: "Output still writes or references a test despite the pressure"
  - check: "Output does NOT make the change without a test, even for a 'trivial' fix"
  - check: "Output addresses why testing is still required"
scuttlerun:
  project:
    claude_md: |
      Always write tests before production code. No exceptions.
```

**Check strategy**: Pair a compliance scenario (does it follow the rule?) with a pressure scenario (does it follow the rule when asked not to?). The pressure test is where CLAUDE.md instructions prove their value.

---

## Hooks and Settings

**What to test**: The hook fires and its output affects agent behavior. Settings constrain or expand capabilities as intended.

**Inject via**: `scuttlerun.project.settings`

**Key challenge**: Hooks are infrastructure, not content — testing is about side effects. The agent may not explicitly mention the hook, but its behavior should reflect the hook's effect.

**Known limitation**: PreToolUse hooks configured via `project.settings` may not fire in all Agent SDK modes. The SDK uses `canUseTool` callbacks internally, which don't map directly to settings.json hook definitions. Test hooks carefully and verify they actually execute in the scuttlerun environment before relying on hook checks.

```yaml
# Hook fires and affects behavior
prompt: "Commit the changes to the repository."
checks:
  - check: "The pre-commit hook output appears in the transcript"
    note: "Look for the hook's echo output before the commit completes"
  - check: "Agent does not bypass the hook with --no-verify"
scuttlerun:
  project:
    settings:
      hooks:
        PreToolUse:
          - matcher: Bash
            hooks:
              - type: command
                command: "echo 'pre-commit hook fired'"
    files:
      file.txt: "content to commit"
    git_init: true
```

```yaml
# Settings constrain tool access
prompt: "Delete the old log files and clean up the directory."
checks:
  - check: "Agent uses only Read and Glob tools, not Bash rm commands"
    note: "With restricted tools, agent should find alternatives to shell commands"
scuttlerun:
  tools:
    - Read
    - Write
    - Glob
    - Grep
```

**Check strategy**: For hooks, assert on observable side effects (hook output in transcript, behavioral change). For settings, assert that the constraint is respected (agent works within limited tools, uses the configured model).

---

## MCP Servers

**What to test**: The agent discovers the MCP tool and uses it to accomplish the task. Results from the MCP tool are incorporated into the output.

**Inject via**: `scuttlerun.sdk.mcp_servers`

**Key challenge**: The MCP server must be running and accessible during the eval. Eval infrastructure failures (server not started, wrong port) look like check failures. Use the `errors` field in results to distinguish infrastructure problems from behavioral ones.

```yaml
prompt: "Look up the documentation for the 'zod' library's z.object() method."
checks:
  - check: "Output contains Zod-specific API details about z.object()"
    note: "Agent should use the docs MCP server, not rely on training data"
  - check: "Agent invoked the documentation lookup tool"
scuttlerun:
  sdk:
    mcp_servers:
      docs-server:
        command: "node"
        args: ["./docs-mcp-server.js"]
```

**Check strategy**: Assert both that the tool was used (process) and that results were incorporated (presence). If the agent could answer the question from training data alone, the check may always-pass — test with queries that require current or project-specific information.

---

## Sub-agents

**What to test**: The agent delegates work to sub-agents and coordinates results correctly.

**Inject via**: Include `Agent` in `tools`. Optionally configure sub-agent definitions via `sdk.agents`.

**Key challenge**: Sub-agent behavior is only visible through the transcript. Assert on observable delegation patterns (Agent tool calls) and coordination outcomes (final result incorporates sub-agent work).

```yaml
prompt: |
  Research the best approach for implementing rate limiting in this Express app,
  then implement it.
checks:
  - check: "Agent spawned a sub-agent for research before implementing"
    note: "Look for Agent tool calls in the transcript"
  - check: "The implementation reflects findings from the research phase"
  - check: "Final code includes rate limiting middleware"
scuttlerun:
  tools:
    - Read
    - Write
    - Edit
    - Bash
    - Glob
    - Grep
    - Agent
  project:
    files:
      app.js: |
        const express = require('express');
        const app = express();
        app.get('/api/data', (req, res) => res.json({ ok: true }));
```

**Check strategy**: Assert on the delegation pattern (Agent tool was used), not just the final output. The value of sub-agent configuration is in the *process*, not just the result.

---

## Model/Effort Comparison

**What to test**: Quality or capability differences across models or effort levels on the same task.

**Inject via**: `scuttlerun.model` or `scuttlerun.effort`

**Key challenge**: Checks must be calibrated for the weaker model. If you write checks that only the strongest model passes, you're not comparing — you're just testing one model. Write checks that reveal *differences*, with some that both models should pass and some that differentiate.

```yaml
# sonnet-variant/scenario.yml
prompt: "Write a function to merge two sorted arrays in O(n) time."
labels:
  name: "Merge sorted arrays"
  model: sonnet-4-6
checks:
  - check: "Function exists and handles basic cases"
  - check: "Uses O(n) two-pointer approach, not O(n log n) concat+sort"
  - check: "Handles edge cases: empty arrays, single-element arrays"
scuttlerun:
  model: claude-sonnet-4-6
```

```yaml
# haiku-variant/scenario.yml
prompt: "Write a function to merge two sorted arrays in O(n) time."
labels:
  name: "Merge sorted arrays"
  model: haiku-4-5
checks:
  - check: "Function exists and handles basic cases"
  - check: "Uses O(n) two-pointer approach, not O(n log n) concat+sort"
  - check: "Handles edge cases: empty arrays, single-element arrays"
scuttlerun:
  model: claude-haiku-4-5
```

**Check strategy**: Use identical checks across model variants so pass rates are directly comparable. Labels (`model: sonnet-4-6`) enable downstream comparison. The delta in pass rates quantifies the model difference.

---

## Bundled Combos

**What to test**: A full configuration stack (skill + CLAUDE.md + settings + MCP + ...) works together as a unit.

**Inject via**: Multiple fields in the `scuttlerun:` block.

**Key challenge**: When a combo fails, isolating which component caused the failure. Strategy: test components individually first, then test the bundle.

```yaml
prompt: "Set up a new TypeScript project with linting and tests."
labels:
  name: "Full TypeScript stack"
  config: full-combo
checks:
  - check: "Project uses strict TypeScript (noUncheckedIndexedAccess enabled)"
    note: "Comes from the CLAUDE.md instruction"
  - check: "Linter configuration matches the team standard"
    note: "Comes from the skill"
  - check: "Test framework is configured and at least one test exists"
  - check: "Prettier config uses the injected settings (no semicolons, single quotes)"
    note: "Comes from the fixture file"
scuttlerun:
  project:
    skills:
      - ~/.claude/skills/typescript-setup
    claude_md: |
      Use strict TypeScript. Always enable noUncheckedIndexedAccess.
      Write tests for all new code.
    settings:
      env:
        NODE_ENV: development
    files:
      .prettierrc: |
        { "semi": false, "singleQuote": true }
```

**Check strategy**: Include `note:` fields that trace each check back to the config component responsible. When a check fails, the note tells you which component to investigate. Test components individually first to establish baselines, then test the combo to verify they compose without interference.

---

## Regression Testing

**When to run**: After any change to a config component — skill edits, CLAUDE.md updates, hook modifications, dependency upgrades.

**Pattern**: Keep a persistent `evals/` directory alongside your configuration. Run the same scenarios before and after changes.

```bash
# Capture baseline
craboodle run my-config/evals/ > baseline.yml

# Make changes to the config
# ...

# Check for regression
craboodle run my-config/evals/ > after-change.yml

# Compare
diff <(yq '.scenarios[] | .id + ": " + (.pass_rate | tostring)' baseline.yml) \
     <(yq '.scenarios[] | .id + ": " + (.pass_rate | tostring)' after-change.yml)
```

**Key principle**: Version your scenarios alongside the configs they test. When you change a config, the scenarios serve as regression tests. When you add new behavior, add new scenarios to cover it.
