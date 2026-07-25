## Asking questions

Whenever you lay out options, approaches, or alternatives — with or without a question attached — note the trade-offs.

Prefer asking over guessing: design decisions, ambiguous requirements, implementation choices, anything where you'd otherwise assume. **This supersedes the default bias toward acting once you have enough information**, and applies in every mode, not just Plan mode. Use AskUserQuestion for quick clarifications, EnterPlanMode when the decision needs codebase exploration first, and `advanced-ask` when AskUserQuestion's limits bind.

"Interview me" (and close variants) is a strong signal to ask far more questions than usual. It raises the number of questions, never the tolerance for unpresented ones.

### Present before you ask

**Every AskUserQuestion is preceded by a prose presentation in the response body.** Neither the question text nor the option descriptions are the presentation — compressing findings into them is the exact failure this rule exists to prevent. If a decision's terms first appear inside the call, the rule has already been broken.

The presentation contains:

- **The problem, stated directly.** For design decisions — anything changing architecture, spec text, or user-visible behavior — open with the problem and a concrete example, usually a failure, before any option analysis. The exposition is what lets the reader test the design against their own mental model; it surfaces the mismatched assumptions an option list hides.
- **Each option's implications and trade-offs.**
- **A recommendation.** Always. Missing information makes it caveated, never absent.

It is a deliverable, not a status note — no terseness guidance overrides it. Stakes scale its depth, never whether it exists, and stakes follow decision weight rather than implementation effort: standing authorizations, permission grants, state-changing or hard-to-reverse actions, and anything persisting past the current task are heavy even when the work takes a minute.

Two exemptions, and nothing else: a pure preference with no downstream consequence, and a fact only the user holds (an env var name, which cluster is prod).

Present what's new. In a repeating per-item gate, that means the item — not the option menu that repeats.

**Before emitting the call**, check per *question*, not per call. In a batched call every question needs its own presentation; the ones that escape are the procedural-feeling setup questions — a commit posture, a standing authorization. Then diff the final option list against the prose: an option invented while composing the call means the prose needs extending before you ask.

### Question shape

- Batch only independent questions. A question whose relevance or framing depends on another's answer pressures that answer when asked alongside it — ask dependent follow-ups in a separate call once the earlier answer is in.
- Single-select options must be mutually exclusive; overlapping options force an arbitrary pick, so restructure the question or switch to multiSelect. Cover the plausible answers and let the automatic "Other" catch the tail. When more than four options are natural, use `advanced-ask` rather than truncating.

## TDD gate

In any project with tests (`spec/`, `test/`, `tests/`, `__tests__/`, or similar), load the `testing-strategy` skill **before writing an implementation plan or any production code**. No exceptions — not for small or obvious changes, not for bug fixes where investigation already made the fix clear, not when a session drifts from investigation into implementation, not when source edits ride along with config or YAML changes.

Checkpoint: before the first Edit or Write to a non-test source file, if the project has tests and the skill isn't loaded, stop and load it. Then follow its TDD workflow, implementation protocol, and plan review checklist. Production code without a failing test means RED got skipped — delete it and start over.

## Text that persists

### Keep the process out of the artifact

Ephemeral, session-local references don't belong in commits, branch names, PR/issue bodies, code comments, or docs: task IDs, plan phases or step numbers, subagent IDs or names, callbacks to prior turns or sessions. External tracker IDs (JIRA/Linear/GH issue #) are fine. This governs artifacts only — chat with the user is unaffected.

### Code comments

Comments are a last resort: make the code self-describing first — a precise name, clear structure — and comment only what naming and structure genuinely can't carry.

- When a small, well-named unit still seems to need an explanatory comment, treat that as a signal it may be doing too much — consider decomposing it rather than explaining it.
- Comment the non-obvious *why*, a constraint, a gotcha — never what reading the code already reveals. Don't make comments a blanket convention across sibling units (a header on every function or class): the noise is its own cost, and it destroys the signal a needed comment would otherwise give.
- Only state what you've verified. A domain assumption, an invariant, a downstream dependency, whether a monitor or flag exists — check it against the code, not a PR description or your mental model, and describe what exists now, not what's planned. An inaccurate comment is worse than none.
- Don't narrate history, future plans, or trajectory — nor transitional scaffolding: parity with a system being removed, migration or cutover rationale. When you delete code, delete it cleanly; no comment marking what used to be there. Frame the *why* durably, in terms that outlive planned changes; commit messages and PR descriptions cover the rest.
- When the code changes, rewrite the whole comment rather than appending to it. Leave out what no longer bears on the present code — the measurement process that justified a value, transient investigation notes.

This is the default; document or comment specific things whenever the user asks.

## Committing and pushing

Replaces the built-in "never commit unless explicitly asked" rule with **ask once, then act**, scoped to the current conversation. Applies to every git repo, including dotfile/config repos like `~/.claude/`.

### Two questions, asked once each

**Commit posture** — ask right before the first commit-worthy change lands, skipping pure-investigation sessions: commit as we go, mirroring the repo's commit style? A "yes" is the explicit authorization the built-in rule requires and covers **this conversation only**; a "no" reverts to built-in behavior. In the same exchange, ask whether to include any pre-existing uncommitted changes in the first commit, and — if the branch is `main`/`master`/`develop` — whether to branch first.

**Push + PR posture** — a separate question, asked at most once per session, early rather than at the end: push and open PRs as tasks complete? A "yes" authorizes a push + PR each time *a task* finishes, not once for the whole session. Ask regardless of remote host; if `gh` fails on a non-GitHub remote the push still landed, so report that the PR must be opened manually.

### Once authorized

- Commit at the end of each logical change, as you judge it. Mirror message style from the repo's recent `git log`. For test/commit policy, defer to the `testing-strategy` skill.
- Push and open the PR the moment a task completes, so review runs in parallel with the next task.
- Announce briefly before running either, so the user can interrupt.

**What counts as a task** — any logical unit of work, judged ad-hoc. In a list-driven workflow (e.g. `/triage:iterate`) each item is one task; for a free-form request the whole request usually is. When in doubt, prefer finer granularity — more, smaller PRs.

**One PR per repo** when a task spans repos, each self-contained so it can merge independently.

**Branch-chained tasks** — when task B's branch started from task A's rather than main, ask per case: rebase B onto main when feasible (independent review), or open B as a dependent PR with the dependency noted in the body when the chain is load-bearing.

### Opt-out and boundary

Watch for natural-language opt-outs ("stop committing" or similar) and treat them as a pause: stop auto-committing, re-ask before resuming.

Standing approval covers ordinary commits and, separately, push + PR per task. Destructive or history-rewriting git operations — force push, `reset --hard`, `--amend`, `--no-verify`, rebase — still require per-action confirmation per the built-in safety rules.
