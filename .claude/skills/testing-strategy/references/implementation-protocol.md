# Implementation Protocol

During-coding guardrails for maintaining TDD discipline. Apply these checks at the moment of each edit, not just at planning time.

## Pre-Edit Gate

Before each edit to a production file, answer this question:

> **Is there a failing test for the behavior being added or changed?**

| Answer | Action |
|--------|--------|
| Yes — a test exists and fails for the expected reason | Proceed with the edit |
| No — no test exists yet | STOP. Write the failing test first |
| Unsure | STOP. Run the test suite to confirm a failure exists |

This gate applies to every production file edit. No exceptions for "simple" changes, "obvious" fixes, or "just adding a line."

## Per-Task Implementation Order

For each discrete change (bug fix, feature, refactor):

1. **Write one failing test** that specifies the behavior
2. **Run the test** — confirm it fails for the expected reason (not a syntax error or missing import)
3. **Edit the production code** — write the minimal code to make the test pass
4. **Run all tests** — confirm the new test passes and no existing tests broke
5. **Refactor** if needed — clean up with all tests green

Steps 1-2 must complete before step 3 begins. There is no "write the code, then add tests" variant of this sequence.

## Red Flags

These phrases and patterns indicate TDD has been abandoned. If any appear during implementation, stop and restart the current change from step 1.

**Phrases that reveal code-first ordering:**
- "Now add tests for..."
- "Let me also add tests"
- "Now add round-trip tests to..."
- "Now add the compiler tests"
- "Tests for this change..."  (as a follow-up to code edits)

**Patterns that indicate a missing RED phase:**
- All tests pass on the first run after writing both code and tests together — no prior failure was observed
- Tests are added in a separate step, commit, or task from the code they cover
- Test file is edited after the production file in the same task

**Patterns that indicate over-implementation:**
- Multiple production files edited before any test is run
- Production code handles edge cases not yet covered by any test

## Rationalization Table

| Excuse | Reality |
|--------|---------|
| "The fix is obvious — tests after achieve the same goal" | Tests written after code verify what was built, not what should be built. The bias of having seen the implementation cannot be undone. |
| "I'll keep the code as reference while writing tests" | Adapting existing code to fit tests is testing after implementation. Delete the code and start from RED. |
| "This is too simple to need TDD" | Simple code breaks. A test for simple behavior takes seconds to write and provides permanent regression protection. |
| "I already manually verified it works" | Manual testing is ad-hoc with no record, no repeatability, and no regression protection. |
| "Writing tests first would slow down this task" | TDD moves debugging time forward into test design. Skipping it creates untested behavior that compounds into harder bugs later. |
| "The test would just be the inverse of the code" | If the test is trivially derivable from the code, the code is trivially derivable from the test. Write the test first — it takes the same effort. |

## Bug Fix Protocol

Bug fixes are the highest-risk scenario for skipping TDD, because the fix often seems obvious after investigation. Follow this sequence strictly:

1. **Investigate** the bug (read code, run experiments, trace the cause)
2. **Write a failing test** that reproduces the bug — the test fails, demonstrating the bug exists
3. **Verify** the test fails for the reason described in the bug report
4. **Fix** the bug with minimal code
5. **Verify** the test passes and no other tests broke

Never fix a bug without first writing a failing test that demonstrates it. The investigation phase (step 1) does not produce code edits — it produces understanding. The first edit is always to a test file.
