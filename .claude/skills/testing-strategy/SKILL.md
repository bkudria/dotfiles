---
name: testing-strategy
description: Testing strategy for choosing test levels, designing good tests, following test-driven development, and optimizing test suites. Use when deciding between unit and integration tests, choosing what level to test at, designing test strategy for a new feature, following a TDD workflow, implementing Red/Green/Refactor, writing test-first code, evaluating whether to write unit tests or integration tests, understanding why tests are slow, or assessing test quality and coupling. Covers TDD, purity vs extent, the refactoring test, the neural network test, and test economy.
---

# Testing Strategy

A framework for making good testing decisions: what to test, at what level, how to follow a test-first workflow, and how to keep test suites fast and maintainable.

## When to Use

- Following a test-driven development (TDD) workflow
- Implementing a feature or bug fix with test-first discipline
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

**Strict rule**: no new behavior without a failing test first. If code is written before a test, delete it and start the cycle from Red.

**Exceptions** (confirm with your collaborator before applying):
- Throwaway prototypes explicitly intended to be discarded
- Generated code (code generators, scaffolding)
- Configuration files

See `references/tdd-workflow.md` for the complete workflow with examples, verification steps, and guidance on why test-first order matters.

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
