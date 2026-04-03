---
name: claude-code-evals
description: "Evaluate Claude Code configurations (skills, CLAUDE.md, hooks, MCP servers, settings, sub-agents) using the scuttlerun/pincenez/craboodle eval pipeline. Use when evaluating a configuration, writing eval scenarios, designing checks, interpreting eval results, benchmarking configurations, testing CLAUDE.md effectiveness, verifying hooks work, regression testing configs, or building an eval suite."
---

# Claude Code Evals

Evaluate Claude Code configurations with behavioral evidence.

## When to Use

- Verifying a skill, CLAUDE.md instruction, hook, or MCP server works as intended
- Regression testing after changes to any config component
- Iterating on scenario coverage and check quality
- Building confidence before sharing or publishing a configuration
- Deciding whether a configuration change actually improved behavior

## The Eval Mindset

Evaluation replaces intuition with behavioral evidence. Instead of reading your configuration and guessing whether it works, you run Claude against realistic tasks and measure outcomes.

**The key question**: "Would Claude behave the same way without this configuration?" If yes, your configuration may not be adding value — or your checks may not be targeting what the configuration adds.

**When to evaluate:**
- After any behavioral change to a configuration
- Before sharing or publishing configs for others to use
- When debugging inconsistent behavior — evals reveal whether the problem is the config or the task

**When NOT to evaluate:**
- Trivial formatting-only changes (whitespace, comments, reordering)
- Configs with no behavioral expectations (e.g., editor settings)
- During initial exploratory drafting — write first, evaluate when the config stabilizes

**Evaluation is probabilistic.** LLM behavior is non-deterministic. Evals run multiple repetitions and report pass *rates*, not binary pass/fail. A pass rate of 0.8 means the configuration works reliably, not that it works every time.

## The Three Tools

```
craboodle (orchestrator)
  ├── scuttlerun (session driver) ── runs Claude, produces transcript
  └── pincenez (grader) ────────── grades transcript, produces verdict
```

- **scuttlerun** runs a headless Claude session with a synthetic user (LLM oracle), producing a full transcript. It handles multi-turn interactions and project scaffolding.
- **pincenez** grades one transcript against a checks file. Each check gets an independent LLM call — binary pass/fail with evidence. No cross-contamination between verdicts.
- **craboodle** discovers scenarios, runs each through scuttlerun N times, grades each run with pincenez, averages results, and streams YAML output.

Run `<tool> --help` for CLI flags, YAML schemas, and field references.

## Quick Start

Evaluate whether a CLAUDE.md instruction changes behavior:

**1. Create the eval directory:**
```
my-evals/
├── craboodle.yaml
├── base.yaml
└── tdd-instruction/
    ├── scenario.yaml
    └── checks.yaml
```

**2. Write `craboodle.yaml`** (pipeline config):
```yaml
version: "1"
```

**3. Write `base.yaml`** (scuttlerun defaults):
```yaml
model: claude-sonnet-4-6
tools: [Read, Write, Bash, Glob, Grep, Edit]
user:
  turn_policy: single
```

**4. Write `tdd-instruction/scenario.yaml`** (scuttlerun config only):
```yaml
prompt: |
  Write a function called isPrime that checks if a number is prime.
  Save it to prime.js.
project:
  claude_md: |
    Always write tests before production code. Use test-driven development.
```

**5. Write `tdd-instruction/checks.yaml`** (pincenez config only):
```yaml
checks:
  - test-before-code:
      check: "A test file was written before or alongside the production code"
      note: "Look for a test file created via the Write tool"
  - function-exists:
      check: "The function isPrime exists in prime.js"
  - tests-validate:
      check: "At least one test case validates prime number behavior"
```

**6. Lint checks** (catches anti-patterns before spending money):
```bash
craboodle lint my-evals/
```

**7. Run:**
```bash
craboodle run my-evals/
```

**8. Interpret results** — see `references/results-interpretation.md`.

## What Can Be Evaluated

Any Claude Code configuration that changes agent behavior. Each `scenario.yaml` contains scuttlerun fields that define the configuration under test:

| Config Type | Inject Via | What to Test |
|-------------|-----------|--------------|
| Skills | `project.skills` | Behavioral change with skill loaded |
| CLAUDE.md | `project.claude_md` | Instruction compliance, stickiness under pressure |
| Hooks/Settings | `project.settings` | Hook fires, affects behavior |
| MCP Servers | `sdk.mcp_servers` | Tool discovery and usage |
| Sub-agents | `tools: [Agent]` + `sdk.agents` | Delegation and coordination |
| Model/Effort | `model`, `effort` | Quality at different cost points |
| Bundled Combos | Multiple fields | Combined config stack works as a unit |

See `references/config-type-patterns.md` for scenario examples and design guidance for each type.

## Suite Lifecycle

An eval suite evolves alongside the configuration it tests:

- **Growing the suite** — Add scenarios when you add new behavior, discover edge cases, or find untested dimensions
- **Improving checks** — Iterate on check quality; fix always-passes anti-patterns, tighten vague checks, add missing `note:` fields
- **Running after changes** — Run your suite after config changes to catch regressions. Review any scenarios with degraded pass rates
- **Retiring scenarios** — Remove stale scenarios that no longer test meaningful behavior (e.g., config changed, scenario tests removed functionality)

See `references/scenario-design.md` for scenario structure and suite evolution guidance.

## Reference Files

| File | Purpose |
|------|---------|
| `references/check-design.md` | Check patterns, anti-patterns, quality criteria |
| `references/scenario-design.md` | Scenario structure, suite evolution |
| `references/config-type-patterns.md` | Per-config-type eval guidance with examples |
| `references/results-interpretation.md` | Reading results, decision framework, iteration |
| `references/config-precedence.md` | Full config precedence chain across all three tools |

## Dependencies

- **craboodle** — eval orchestrator (`craboodle --help`)
- **scuttlerun** — session driver (`scuttlerun --help`)
- **pincenez** — checks grader (`pincenez --help`)
- Optional: **skillcraft** — for skill-specific eval patterns (pressure testing by skill type, trigger testing, worked examples per skill category)
