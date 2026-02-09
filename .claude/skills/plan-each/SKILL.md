---
name: plan-each
description: Iterate through a list of items, planning and implementing each one-by-one. Use when batch-processing improvements, applying a series of changes, working through audit findings, or implementing multiple enhancements from a review.
disable-model-invocation: true
argument-hint: "[filter or guidance, e.g. '1,3,5' or 'only security-related']"
---

## When to Use

- A previous step produced a list of items to process (audit findings, improvements, refactors)
- Batch-processing multiple changes that each need individual planning and approval
- Working through review feedback item-by-item
- Implementing a series of enhancements where each may be accepted or declined independently

## Dependencies

This skill uses: `TaskCreate`, `TaskUpdate`, `TaskList`, `AskUserQuestion`, `EnterPlanMode` / `ExitPlanMode`, and the `Task` tool (sub-agents for research).

---

## Arguments: `$ARGUMENTS`

Parse any provided arguments into two categories:

1. **Filter/selection criteria** — Item numbers (e.g., "1,3,5"), indices, or selection keywords (e.g., "only security-related", "skip documentation"). Apply these to filter which items to process.
2. **Additional context/instructions** — Guidance to apply throughout (e.g., "focus on performance", "be thorough with tests", "prefer minimal changes").

Apply both when present. Ask the user to clarify ambiguous arguments. When no arguments are provided, process all items with default behavior.

---

## Step 0: Initialize Progress Tracking

**CRITICAL — Do this before any other work.**

Run `TaskList` to check for existing progress from a previous invocation or compaction recovery:

- **Tasks already exist**: Resume from the first `pending` task. Skip all `completed` tasks. Do NOT re-create tasks.
- **No tasks exist**: Create a `TaskCreate` for **every item** to process (after applying any argument filters). Each task needs:
  - `subject`: A concise description of the item/improvement
  - `description`: Full context including the problem, proposed improvement, and any user-provided guidance
  - `activeForm`: Present-continuous description (e.g., "Implementing X improvement")

---

## Step 1: Process Items One-by-One

**Before each item**, run `TaskList` to confirm current state. Then for each pending item:

1. **Mark in-progress**: `TaskUpdate` the task to `in_progress`
2. **Explore the item** — Before presenting anything to the user, use sub-agents or direct reads to investigate the item. Read relevant source files, understand current state, check for complications, and gather concrete context. The goal is to give the user an informed summary, not just parrot back the task description.
3. **Present the item** — Summarize what you found: the current state, what the proposed change involves concretely, any complications or trade-offs discovered, and your assessment of complexity. This should give the user enough context to make an informed decision.
4. **Ask the user how to proceed** — use `AskUserQuestion` with these options for **every** item, regardless of complexity:
   - **Implement** — Proceed directly (for straightforward items)
   - **Plan first, then implement** — Enter plan mode, design the approach, get approval, then implement (for items needing design decisions or exploration)
   - **Skip** — `TaskUpdate` the task subject to `[DECLINED] <original subject>` and mark `completed`; move to next item
5. **If "Implement"**: Implement the change directly
6. **If "Plan first"**:
   - Enter plan mode and plan the proposed improvement, incorporating user-provided context
   - Use sub-agents as needed to explore, research, or otherwise support planning
   - Use `AskUserQuestion` as many times as needed for questions, design decisions, or other choices
   - Exit plan mode and implement
7. **Mark completed**: `TaskUpdate` the task to `completed` (immediately — never batch updates)
8. **Repeat** from step 1 for the next pending item

**IMPORTANT**: Never skip the `AskUserQuestion` in step 4. Every item must be explicitly approved or declined by the user before any implementation work begins.

---

## Step 2: Final Summary

After all tasks are `completed`, run `TaskList` one final time and summarize:
- Which items were **implemented** (completed without [DECLINED] prefix)
- Which items were **declined** (completed with [DECLINED] prefix)
- Which items were **filtered out** by arguments (never created as tasks)

Follow this procedure closely, one task at a time.
