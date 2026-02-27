# TDD Workflow: Red-Green-Refactor

A detailed guide to the test-driven development cycle, including verification steps, rationale for test-first order, and practical guidance for common situations.

## The Cycle

Each iteration adds one behavior through three phases, each followed by a mandatory verification step.

```
┌─────────────────────────────────────────────┐
│                                             │
│  RED ──verify──► GREEN ──verify──► REFACTOR │
│   ▲                                    │    │
│   └────────────────────────────────────┘    │
│                                             │
└─────────────────────────────────────────────┘
```

The cycle produces small, verified increments. Each phase has a specific purpose and a verification gate that must pass before moving to the next.

## RED: Write a Failing Test

Write one minimal test that describes the next behavior the code should have.

**What the test should do:**
- Assert one behavior (if the test name contains "and", split it)
- Use a clear name that describes the expected behavior
- Show how the code should be called (the test designs the API)
- Use real code paths, not mocks, unless external IO is unavoidable

```
test "rejects empty input":
    result = process(input: "")
    assert result.error == "Input required"
```

A good test at this stage demonstrates the *desired API* -- the test is a specification of what the code should do, written before the code exists.

## Verify RED

**Mandatory. Do not skip.**

Run the test and confirm:

1. The test **fails** (not errors -- a failure is an unmet assertion, an error is broken test code)
2. The failure message matches what you expect ("expected 'Input required', got nil" -- not a syntax error)
3. The failure is because the feature is missing, not because of a typo or misconfiguration

| Outcome | Action |
|---------|--------|
| Test fails with expected message | Proceed to GREEN |
| Test errors (syntax, import, setup) | Fix the test code, re-run until it fails correctly |
| Test passes immediately | The behavior already exists -- this test adds no value. Write a different test |

## GREEN: Write Minimal Code

Write the simplest code that makes the failing test pass.

```
function process(input):
    if input is empty:
        return error("Input required")
    ...
```

**Discipline:**
- Write just enough to pass the test -- no more
- Do not add features the test does not require
- Do not refactor other code
- Do not add configuration, options, or generalization beyond what the test demands

Over-engineering at this stage adds untested behavior. If additional behavior is needed, it belongs in the next Red phase with its own test.

## Verify GREEN

**Mandatory.**

Run the test suite and confirm:

1. The new test passes
2. All existing tests still pass
3. No warnings or errors in output

| Outcome | Action |
|---------|--------|
| All tests pass | Proceed to REFACTOR |
| New test fails | Fix the production code (not the test) |
| Other tests fail | Fix now -- do not defer regressions |

## REFACTOR: Clean Up

With all tests green, improve the code:

- Remove duplication
- Improve names and readability
- Extract helpers or shared logic
- Simplify structure

**Constraints:**
- Do not add new behavior (that belongs in the next Red phase)
- Re-run tests after each change to confirm they stay green
- If a refactoring breaks a test, undo and reconsider

## Why Test-First Order Matters

The order of the cycle is not ritual -- it serves a specific epistemic purpose.

### Tests written after code are biased

A test written after implementation answers: *"Does this code do what I built?"* It verifies remembered behavior, not required behavior. The implementer unconsciously tests what they know the code does, not what it should do. Edge cases that were not considered during implementation are also not considered during testing.

### Tests written before code are specifications

A test written before implementation answers: *"What should this code do?"* It forces explicit enumeration of requirements before any implementation exists. Edge cases emerge during test design because the author is thinking about the interface, not defending an implementation.

### Watching the test fail proves it tests something

A test that passes immediately proves nothing. It might test existing behavior, test the wrong thing, or have a bug that makes it vacuously true. Watching it fail -- and confirming it fails for the expected reason -- establishes that the test will actually catch a regression.

### Common objections analyzed

| Objection | Analysis |
|-----------|----------|
| "I'll write tests after to verify" | Tests passing immediately provide no evidence they catch bugs. You never see them fail, so you never confirm they test the right thing. |
| "I already manually tested the edge cases" | Manual testing is ad-hoc with no record, no repeatability, and no regression protection. It answers "it worked when I tried it" rather than "it works." |
| "Deleting working code is wasteful" | Sunk cost. The choice is between code you can trust (rewritten with TDD) and code you cannot (written without test-first verification). |
| "I'll keep code as reference while writing tests" | Adapting existing code to fit tests is testing after implementation. The bias of having seen the implementation cannot be undone by writing tests around it. |
| "TDD slows me down" | TDD moves debugging time forward into test design. The total time is typically lower because bugs are caught before they compound. |
| "This is too simple to need TDD" | Simple code breaks. A test for simple behavior takes seconds to write and provides permanent regression protection. |
| "Exploration requires code first" | Exploration is valid. Discard the exploration code, then start the real implementation with TDD. Exploration code is not production code. |

## Exceptions

TDD is the default for all production code. The following are recognized exceptions -- confirm with your collaborator before applying them:

- **Throwaway prototypes** -- Code explicitly intended to be discarded after learning. If the prototype becomes production code, stop and restart with TDD.
- **Generated code** -- Output from code generators, scaffolding tools, or automated migrations.
- **Configuration files** -- Static configuration that does not contain logic.

If the exception feels like it might be a rationalization, it probably is. Apply the test: would you be comfortable explaining to a colleague why TDD does not apply here?

## When Stuck

| Problem | Approach |
|---------|----------|
| Cannot figure out how to test it | Write the assertion first (what should be true?), then work backwards to the setup. If still stuck, the interface may be too complex -- simplify it. |
| Test is too complicated | The design is too complicated. A test that is hard to write indicates an interface that is hard to use. Simplify the interface. |
| Must mock everything | The code is too coupled. Introduce dependency injection or restructure so that the core logic is pure and testable without mocks. |
| Test setup is enormous | Extract test helpers for common setup. If setup is still large, the system under test may have too many responsibilities -- consider splitting it. |

## Bug Fix Workflow

When a bug is reported:

1. **RED** -- Write a test that reproduces the bug (the test should fail, demonstrating the bug exists)
2. **Verify RED** -- Confirm the test fails for the reason described in the bug report
3. **GREEN** -- Fix the bug with minimal code
4. **Verify GREEN** -- Confirm the test passes and no other tests broke
5. **REFACTOR** -- Clean up if needed

This produces a regression test that permanently prevents the bug from recurring. Never fix a bug without first writing a failing test that demonstrates it.

## Debugging Integration

When debugging reveals a problem:

- Do not fix it inline. Write a failing test first.
- The test documents the bug and proves the fix works.
- The test prevents the bug from silently recurring.

If the bug is in code without tests, this is an opportunity to add test coverage for the area. Write a test for the correct behavior, watch it fail (confirming the bug), then fix it.
