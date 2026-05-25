- When presenting options, approaches, or possible alternatives, always note any trade-offs
- Always clarify when there are multiple valid approaches: use AskUserQuestion for quick clarifications, or EnterPlanMode for design decisions that need codebase exploration first.
- Prefer asking over guessing. Use AskUserQuestion (or `advanced-ask` when its limits are hit) whenever in doubt — for design decisions, ambiguous requirements, implementation choices, or anything where you'd otherwise be making an assumption. This applies in all modes, not just Plan mode.
- Treat the phrase "interview me" (or close variants like "interview me about") as a strong signal to ask many clarifying questions using AskUserQuestion / `advanced-ask`. When this phrase appears, lean heavily toward asking questions before acting.
- Keep ephemeral/local references out of persisted artifacts (commits, branch names, PR/issue bodies, code comments, docs). This covers task IDs, plan phases/step numbers, subagent IDs or names, and callbacks to prior turns/sessions. External tracker IDs (JIRA/Linear/GH issue #) are fine, and the rule only applies to artifacts — chat with the user is unaffected.

## TDD Gate

**Before writing any implementation plan or production code** in a project that has tests (spec/, test/, tests/, __tests__/, or similar), you MUST load the `testing-strategy` skill. No exceptions.

This applies:
- Even when a session starts as investigation and transitions into implementation
- Even when the change seems small or obvious
- Even for bug fixes where the fix is already clear from investigation
- Even when editing source files alongside configuration or YAML changes

The skill must be loaded **before the plan is written**, not after. Follow its TDD workflow, implementation protocol, and plan review checklist. If you find yourself writing production code without a failing test, stop, delete the code, and start from RED.

**Detection checkpoint:** Before the first Edit or Write to a non-test source file, check whether the project has a test directory (spec/, test/, tests/, __tests__/) or test files. If yes and `testing-strategy` has not been loaded, STOP and load it before proceeding.

## Committing and pushing

Replaces the built-in "never commit unless explicitly asked" rule with an **ask-once, then act** model, scoped to the current conversation. Applies to every git repo, including dotfile/config repos like `~/.claude/`.

### The upfront commit question

Right before the first commit-worthy change lands (skip pure-investigation sessions), ask whether to commit as we go and mirror the repo's commit style. A "yes" is the explicit authorization the built-in rule requires and covers commits for **this conversation only**. A "no" reverts to built-in behavior.

In the same exchange:
- If the working tree has pre-existing uncommitted changes, ask whether to include them in the first commit.
- If the current branch is `main`/`master`/`develop`, ask whether to branch first.

### Commit cadence

Once authorized:
- Commit at the end of each logical change, as Claude judges it.
- Briefly announce what's about to be committed before running it, so the user can interrupt.
- Mirror message style from the repo's recent `git log`.
- For test/commit policy, defer to the `testing-strategy` skill.

### The push + PR question

Ask once per session whether to push and open PRs as tasks complete — not at end-of-session. Separate from the commit question; asked at most once per session. A "yes" authorizes pushing + opening a PR each time Claude judges *a task* complete (not the whole session). Ask regardless of remote host — if `gh` fails (non-GitHub), the push still landed; report that PR creation must be done manually.

**What counts as a "task"** — any logical unit of work, judged ad-hoc. When processing a list-driven workflow (e.g. `/triage:iterate`), each item is one task. For a free-form user request, the whole request is typically one task. When in doubt, prefer finer granularity (more, smaller PRs) over coarser.

**Cross-repo tasks** — when one task touches multiple repos, open one PR per repo touched. Each PR is self-contained so it can merge independently.

**Timing** — push + open the PR immediately when the task is marked complete, so review can start in parallel with the next task. Briefly announce the push + PR before running it so the user can interrupt.

**Branch-chained tasks** — when task B's branch was started from task A's branch (rather than main), ask per case before opening B's PR: rebase B onto main when feasible (independent review), or open B as a dependent PR with the dependency noted in the body when the chain is load-bearing.

### Opt-out and pause

Watch for natural-language opt-outs ("stop committing" or similar). Treat as a pause: stop auto-committing and re-ask before resuming.

### Boundary

Standing approval covers ordinary commits and (separately) push+PR per task. Destructive or history-rewriting git operations (force push, `reset --hard`, `--amend`, `--no-verify`, rebase, etc.) still require per-action confirmation per the built-in safety rules.
