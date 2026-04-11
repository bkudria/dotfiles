---
name: iterate
description: Iterate through a list of items, planning and implementing each one-by-one. Use when batch-processing improvements, applying a series of changes, working through audit findings, or implementing multiple enhancements from a review.
disable-model-invocation: true
argument-hint: "[filter or guidance, e.g. '1,3,5' or 'only security-related']"
---

# Iterate

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

**Identify the list.** Scan the conversation for the most recent list of actionable items. Before creating any tasks, briefly state what you found — e.g., "I found 5 items from the audit's Prioritized Remediation section." If multiple candidate lists exist or the source is ambiguous, ask the user which list to process.

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
2. **Investigate the item** — Do NOT present to the user or ask how to proceed until investigation is complete.
   - **Required**: Use at least one investigative tool (Read, Grep, Bash, or sub-agent) to check the current state of what the item concerns. No exceptions for "simple" items or items you already analyzed earlier in the conversation.
   - **Artifact rule**: Your presentation (step 3) must reference specific findings from this investigation — file paths, current values, concrete state you discovered. A presentation that only restates the task description or earlier analysis means the investigation was skipped.
   - **Decomposition**: If investigation reveals the item has 3+ independently-implementable parts, split it — narrow the current task to the first part (update its subject/description), create new tasks for the rest, and note the split in your presentation.
   - **Red flags — restart investigation if you catch yourself doing any of these**:
     - Presenting without any tool calls since marking in-progress
     - No file paths or line numbers from investigation in your presentation
     - Phrases like "From my earlier review", "As noted above", "I already analyzed this"
     - Paraphrasing the task description instead of reporting what you found now
3. **Present the item** — Summarize what you found: the current state, what the proposed change involves concretely, any complications or trade-offs discovered, and your assessment of complexity. This should give the user enough context to make an informed decision.
4. **Ask the user how to proceed** — use `AskUserQuestion` with these options for **every** item, regardless of complexity:
   - **Implement** — Proceed directly (for straightforward items)
   - **Plan first, then implement** — Enter plan mode, design the approach, get approval, then implement (for items needing design decisions or exploration)
   - **Skip** — `TaskUpdate` the task subject to `[DECLINED] <original subject>` and mark `completed`; move to next item
5. **If scope changes**: When planning or implementation reveals the item is larger or different than originally described, update the task's subject and description via `TaskUpdate` to reflect the actual scope before proceeding.
6. **If "Implement"**: Implement the change directly
7. **If "Plan first"**:
   - Enter plan mode and plan the proposed improvement, incorporating user-provided context
   - Use sub-agents as needed to explore, research, or otherwise support planning
   - Use `AskUserQuestion` as many times as needed for questions, design decisions, or other choices
   - Exit plan mode and implement
8. **Mark completed**: `TaskUpdate` the task to `completed` (immediately — never batch updates)
9. **Repeat** from step 1 for the next pending item

**IMPORTANT**: Process exactly one item per cycle through steps 1-9. Never combine, group, or present multiple items together — even if they seem related. Every item gets its own investigation, presentation, and user approval before any implementation begins.

---

## Step 2: Final Summary

After all tasks are `completed`, run `TaskList` one final time and summarize:
- Which items were **implemented** (completed without [DECLINED] prefix)
- Which items were **declined** (completed with [DECLINED] prefix)
- Which items were **filtered out** by arguments (never created as tasks)

Follow this procedure exactly. Do not propose alternative strategies, comment on the number of remaining items, or suggest changing the approach mid-session.
