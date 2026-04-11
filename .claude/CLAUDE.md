- When presenting options, approaches, or possible alternatives, always note any trade-offs
- Always clarify when there are multiple valid approaches: use AskUserQuestion for quick clarifications, or EnterPlanMode for design decisions that need codebase exploration first.
- Prefer asking over guessing. Use AskUserQuestion (or `advanced-ask` when its limits are hit) whenever in doubt — for design decisions, ambiguous requirements, implementation choices, or anything where you'd otherwise be making an assumption. This applies in all modes, not just Plan mode.
- Treat the phrase "interview me" (or close variants like "interview me about") as a strong signal to ask many clarifying questions using AskUserQuestion / `advanced-ask`. When this phrase appears, lean heavily toward asking questions before acting.

## TDD Gate

**Before writing any implementation plan or production code** in a project that has tests (spec/, test/, tests/, __tests__/, or similar), you MUST load the `testing-strategy` skill. No exceptions.

This applies:
- Even when a session starts as investigation and transitions into implementation
- Even when the change seems small or obvious
- Even for bug fixes where the fix is already clear from investigation
- Even when editing source files alongside configuration or YAML changes

The skill must be loaded **before the plan is written**, not after. Follow its TDD workflow, implementation protocol, and plan review checklist. If you find yourself writing production code without a failing test, stop, delete the code, and start from RED.

**Detection checkpoint:** Before the first Edit or Write to a non-test source file, check whether the project has a test directory (spec/, test/, tests/, __tests__/) or test files. If yes and `testing-strategy` has not been loaded, STOP and load it before proceeding.
