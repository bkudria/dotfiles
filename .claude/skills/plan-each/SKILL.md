---
name: plan-each
description: Iterate through items one-by-one, planning and implementing each improvement
disable-model-invocation: true
---

## Arguments: `{{ARGUMENTS}}`

If arguments were provided above (i.e., not empty), first parse them to determine:
1. **Filter/selection criteria** - If arguments contain item numbers (e.g., "1,3,5"), indices, or selection keywords (e.g., "only security-related", "skip documentation"), use these to filter which items to process
2. **Additional context/instructions** - Any other text should be treated as guidance to apply throughout (e.g., "focus on performance", "be thorough with tests", "prefer minimal changes")

If both are present, apply both. If the arguments are unclear, ask the user to clarify. If no arguments were provided, proceed with all items using default behavior.

---

Let's go through these one-by-one (applying any filters and context from arguments if provided). Use your Task tools, and for each item or suggested improvement:

1. Restate the problem and proposed improvement, with complete detail and context (incorporating any user-provided guidance if present)
2. Assess complexity:
   2a. For trivial changes:
       - Ask a quick question and implement directly if approved
       - If declined, note the decision
   2b. For non-trivial changes:
       - Enter plan mode and carefully plan the proposed improvement, keeping any user-provided context in mind
       - First, use sub-agents as needed to explore, research, or otherwise support planning
       - Then, use the AskUserQuestion tool as many times as needed for any questions, design decisions, or other choices
       - Exit plan mode and implement
       - If declined, note the decision
3. Repeat from step 1 for the next proposed improvement

After all items, summarize what was implemented vs. declined (and note which items were filtered out based on arguments, if applicable).

Think carefully to ensure you follow this procedure closely, one task at a time.
