---
name: testing-strategy
description: "MANDATORY before writing code in projects with tests. TRIGGER when: project has test/, tests/, spec/, or __tests__/ directory, AND you are about to write or plan production code changes. DO NOT TRIGGER when: project has no tests, or work is read-only investigation/analysis with no code changes. Covers TDD red-green-refactor workflow, implementation protocol with pre-edit gates, test level decisions, purity vs extent, and test economy."
---

# Testing Strategy

A framework for making good testing decisions: what to test, at what level, how to follow a test-first workflow, and how to keep test suites fast and maintainable.

## When to Use

This skill is MANDATORY in any project with tests. Load it before writing an implementation plan or production code.

**Always load when:**
- The project has a test directory (spec/, test/, tests/, __tests__/) or co-located test files
- You are about to plan or write production code changes (not read-only analysis)
- A session transitions from investigation to implementation

**Also useful for:**
- Deciding between unit and integration tests for a new feature
- Choosing what level to test a behavioral requirement at
- Evaluating whether existing tests are too coupled to implementation
- Understanding why a test suite is slow
- Designing test strategy for a new project or module

## Core Principles

1. **Test-first** -- Write the test before the code. If you did not watch the test fail, you do not know it tests the right thing.
2. **Tests as specification** -- If it is not tested, it is not guaranteed.
3. **Test at boundaries** -- Test the public interface at each level, not the implementation behind it.
4. **Fixtures as truth** -- For complex outputs, store expected results as version-controlled fixtures or inline snapshots. Use assertions for simple invariants.
5. **Optimize for purity** -- IO causes slowness, not code volume. Minimize impure tests while maximizing coverage.

## TDD: Red-Green-Refactor

Write a failing test. Write minimal code to pass it. Clean up. Repeat.

```
RED    → write one failing test for the next behavior
         verify it fails for the expected reason (not typos or errors)
GREEN  → write the simplest code that makes it pass
         verify all tests pass
REFACTOR → clean up duplication, improve names, extract helpers
           verify all tests still pass
```

**Strict rule**: no new behavior without a failing test first. If code is written before a test, delete it and start the cycle from Red. See `references/implementation-protocol.md` for during-coding guardrails and red flags.

**Exceptions** (confirm with your collaborator before applying):
- Throwaway prototypes explicitly intended to be discarded
- Generated code (code generators, scaffolding)
- Configuration files

See `references/tdd-workflow.md` for the complete workflow with examples, verification steps, and guidance on why test-first order matters.

## Planning for TDD

Plans encode execution order. A plan that lists code changes first and tests second will be executed code-first -- violating TDD even when the author intends to follow it.

**Rule**: Each planned change is a test-code pair. The test comes first.

| Anti-pattern | Correct |
|-------------|---------|
| "Changes" section (all code), then separate "Tests" section | Each change specifies: (1) which test fails first (RED), (2) which code makes it pass (GREEN) |
| "Fix the bug in X, then add tests for X" | "Write a test reproducing the bug in X, then fix X to make it pass" |
| Tests grouped at the bottom of the plan | Tests interleaved with each change, always preceding the code |

Before finalizing any implementation plan, verify: *Does every planned change specify which test will fail first?* If any change lacks a RED phase, restructure before proceeding.

See `references/tdd-workflow.md` § Plan Review Checklist for the full verification checklist.

## Bug Fix Workflow

Bug investigations that lead to code changes are the highest-risk scenario for skipping TDD, because the fix often seems obvious after investigation. Follow this sequence:

1. **Investigate** the bug (read code, run experiments, trace the cause) -- this phase produces understanding, not code edits
2. **Write a failing test** that reproduces the bug -- the first edit is always to a test file
3. **Fix** the bug with minimal production code, verify all tests pass

Never fix a bug without first writing a failing test that demonstrates it. See `references/tdd-workflow.md` § Bug Fix Workflow and `references/implementation-protocol.md` § Bug Fix Protocol for full details.

## Test Design: Interfaces Over Implementation

Test **interfaces and intentions** rather than implementation details.

| Test | Avoid |
|------|-------|
| Public interfaces and contracts | Tests coupled to internal data structures |
| Domain logic and behavior | Tests verifying *how* rather than *what* |
| Observable outcomes | Tests that break from internal refactoring |

Apply the **Refactoring Test**: if a test fails after an internal change that preserves external behavior, the test is too coupled. Apply the **Neural Network Test**: if replacing the code with a black box would break the test, it is testing implementation. See `references/decision-framework.md` for full details.

## Why Tests Are Fast or Slow

Speed depends on **purity** (IO involvement), not **extent** (code exercised). Each step up the purity ladder (pure → filesystem → process → network) adds roughly 5x runtime. Do not mock internal code to reduce extent -- this reduces fidelity without improving speed. See `references/purity-vs-extent.md` for the full purity ladder and optimization strategies.

## Choosing Test Level

Ask: **"What is the observable requirement?"** Test at the level where that requirement is observable.

| Situation | Test Level |
|-----------|------------|
| Behavioral requirement at system boundary | Integration test |
| Complex internal logic with many edge cases | Unit test |
| Refactoring that preserves behavior | Existing tests suffice |
| Uncertain | Prefer integration level |

Behavioral requirements (atomicity, error messages, output format) are system behaviors -- test them at the integration level. Internal logic (parsing, algorithms, transformations) with many paths belongs at the unit level. See `references/decision-framework.md` for the full decision flow.

## Test Economy

Integration tests are inherently slow due to IO. Balance coverage and speed:

- **Prefer extending** an existing test scenario over creating new test infrastructure
- **Create new tests** when extending would make a scenario incoherent or require fundamentally different setup

See `references/decision-framework.md` for detailed guidance.

## Reference Files

| File | Contents |
|------|----------|
| `references/tdd-workflow.md` | Complete Red-Green-Refactor cycle, verification steps, why test-first matters, bug fix workflow |
| `references/purity-vs-extent.md` | Purity levels, extent misconceptions, correct optimization strategies |
| `references/decision-framework.md` | Refactoring test, neural network test, behavioral vs internal logic, test economy |
| `references/implementation-protocol.md` | Pre-edit gate, per-task ordering, red flags for TDD violations |
