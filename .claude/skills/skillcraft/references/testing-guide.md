# Skill Testing & Iteration Guide

Skill creation IS Test-Driven Development applied to documentation. Test before writing, test after writing, and iterate until bulletproof.

**Terminology**: "Testing" covers all verification (manual invocation, trigger phrases, edge cases, scripts). "Evals" are the subset that runs through the automated craboodle pipeline (§7).

---

## The Iron Law

```
NO SKILL WITHOUT A FAILING TEST FIRST
```

This applies to new skills AND edits to existing skills. Write skill before testing? Delete it. Start over. Edit skill without testing? Same violation.

**No exceptions:**
- Not for "simple additions"
- Not for "just adding a section"
- Not for "documentation updates"
- Don't keep untested changes as "reference"
- Don't "adapt" while running tests
- Delete means delete

---

## TDD Cycle for Skills

| TDD Concept | Skill Creation |
|-------------|----------------|
| **Test case** | Pressure scenario with subagent |
| **Production code** | Skill document (SKILL.md) |
| **Test fails (RED)** | Agent violates rule without skill (baseline) |
| **Test passes (GREEN)** | Agent complies with skill present |
| **Refactor** | Close loopholes while maintaining compliance |
| **Write test first** | Run baseline scenario BEFORE writing skill |
| **Watch it fail** | Document exact rationalizations agent uses |
| **Minimal code** | Write skill addressing those specific violations |
| **Watch it pass** | Verify agent now complies |
| **Refactor cycle** | Find new rationalizations → plug → re-verify |

---

## Testing by Skill Type

Different skill types need different test approaches.

### Discipline-Enforcing Skills

Skills that enforce rules: TDD, verification-before-completion, design-before-coding.

**Test with:**
- Academic questions: Do agents understand the rules?
- Pressure scenarios: Do agents comply under stress?
- Combined pressures: time + sunk cost + exhaustion + authority
- Identify rationalizations and add explicit counters

**Success criteria:** Agent follows rule under maximum pressure.

See `references/bulletproofing.md` for rationalization resistance techniques.

### Technique Skills

Skills that teach methods: condition-based-waiting, root-cause-tracing, defensive-programming.

**Test with:**
- Application scenarios: Can agents apply the technique correctly?
- Variation scenarios: Do agents handle edge cases?
- Missing information tests: Do instructions have gaps?

**Success criteria:** Agent successfully applies technique to new scenario.

### Pattern Skills

Skills that teach mental models: reducing-complexity, information-hiding, flatten-with-flags.

**Test with:**
- Recognition scenarios: Do agents recognize when pattern applies?
- Application scenarios: Can agents use the mental model?
- Counter-examples: Do agents know when NOT to apply?

**Success criteria:** Agent correctly identifies when and how to apply pattern.

### Reference Skills

Skills that document APIs or tools: command references, library guides, syntax references.

**Test with:**
- Retrieval scenarios: Can agents find the right information?
- Application scenarios: Can agents use what they found correctly?
- Gap testing: Are common use cases covered?

**Success criteria:** Agent finds and correctly applies reference information.

---

## Common Rationalizations for Skipping Testing

| Excuse | Reality |
|--------|---------|
| "Skill is obviously clear" | Clear to you ≠ clear to other agents. Test it. |
| "It's just a reference" | References can have gaps, unclear sections. Test retrieval. |
| "Testing is overkill" | Untested skills have issues. Always. 15 min testing saves hours. |
| "I'll test if problems emerge" | Problems = agents can't use skill. Test BEFORE deploying. |
| "Too tedious to test" | Testing is less tedious than debugging bad skill in production. |
| "I'm confident it's good" | Overconfidence guarantees issues. Test anyway. |
| "Academic review is enough" | Reading ≠ using. Test application scenarios. |
| "No time to test" | Deploying untested skill wastes more time fixing it later. |

---

## 1. Manual Invocation Testing

| Test | How | Expected |
|------|-----|----------|
| Basic invocation | `/skill-name` | Skill loads, runs default behavior |
| With arguments | `/skill-name path/to/file` | Skill receives and uses arguments |
| Multiple arguments | `/skill-name arg1 arg2 --flag` | Arguments parsed as single `$ARGUMENTS` string |
| Empty arguments | `/skill-name` (no args, but skill expects them) | Graceful handling or prompt for input |
| Quoted arguments | `/skill-name "multi word arg"` | Quotes preserved in `$ARGUMENTS` |

For auto-invocable skills, trigger by mentioning relevant phrases in conversation instead of using `/skill-name`.

## 2. Trigger Phrase Testing

Test that the `description` field drives correct auto-invocation.

### Step 1: Generate Test Prompts

Create two lists of 10 prompts each:

**Should-trigger prompts** (skill should load):
- 3 prompts using exact phrases from the description
- 3 prompts using synonyms or rephrasings
- 2 prompts using partial matches or related terminology
- 2 prompts describing the problem the skill solves (not the skill itself)

**Should-NOT-trigger prompts** (skill should NOT load):
- 3 prompts on related but different topics (shared keywords, different intent)
- 3 prompts that a different installed skill should handle instead
- 2 prompts using the same domain but outside this skill's scope
- 2 general prompts with no relation to the skill

### Step 2: Test Each Prompt

For each prompt, start a new conversation (or use a subagent) and observe:
- Does the skill auto-load? (Check system messages or skill loading indicators)
- If it loads, was that correct?
- If it doesn't load, was that correct?

### Step 3: Score

| Metric | Formula | Target |
|--------|---------|--------|
| Recall | (correct triggers) / (total should-trigger) | ≥ 0.7 |
| Precision | (correct triggers) / (total actual triggers) | ≥ 0.8 |
| Specificity | (correct non-triggers) / (total should-NOT-trigger) | ≥ 0.8 |

### Step 4: Iterate Description

If recall < 0.7: Add more specific trigger phrases matching the missed prompts.
If precision < 0.8: Make trigger phrases more specific (verb+noun pairs, not single keywords).
If specificity < 0.8: Remove overly generic terms that cause false triggers.

Repeat Steps 2-4 up to 3 iterations. Restart the conversation after each description change (description is cached at load time).

### Quick Checklist

- [ ] Fires on 3+ distinct positive trigger phrasings
- [ ] Fires on synonym/rephrasings (not just exact matches)
- [ ] Does NOT fire on 3+ unrelated topics sharing keywords
- [ ] Does NOT fire when another skill is more appropriate
- [ ] `disable-model-invocation: true` skills never auto-trigger

## 3. Edge Case Testing

| Scenario | Test | Watch for |
|----------|------|-----------|
| Empty arguments | `/skill-name` with no args | Crashes, undefined variable errors |
| Long arguments | Paste 500+ character string as argument | Truncation, context overflow |
| Special characters | Arguments with `"`, `'`, `$`, `` ` ``, `\n` | Shell injection, broken parsing |
| Missing dependency | Remove a tool the skill expects (e.g., `gum`) | Clear error message vs cryptic failure |
| Missing reference file | Rename a `references/*.md` temporarily | Skill degrades gracefully or reports error |
| Missing script | Remove execute permission from a script | Permission error caught and reported |
| Non-existent path | Pass a path argument that doesn't exist | Error handling, not silent failure |

## 4. Script Testing

Test scripts outside of Claude Code first, then inside.

```bash
bash -n scripts/my-script.sh                    # syntax check
./scripts/my-script.sh "valid input"; echo $?   # expect 0
./scripts/my-script.sh ""; echo $?              # expect non-zero
PATH="" ./scripts/my-script.sh "input"          # expect clear error
```

- [ ] Has shebang (`#!/bin/bash`) and execute permission
- [ ] Exit code 0 on success, non-zero on failure
- [ ] Output format matches what SKILL.md describes
- [ ] stderr for errors, stdout for results
- [ ] No hardcoded paths that differ across machines

## 5. Interactive Element Testing

For skills using `gum`, `fzf`, or form-based prompts via `interactive-tmux`.

| Test | Expected |
|------|----------|
| Normal selection -- pick an option | Correct value returned |
| Cancel/Escape during prompt | Handled gracefully, no crash |
| Empty selection -- submit with no choice | Fallback or re-prompt |
| Unexpected input in select prompt | Input rejected or handled |
| Form JSON validity | Valid JSON, all fields present |

Interactive prompts require `interactive-tmux`. Verify SKILL.md specifies this in `allowed-tools` if needed.

## 6. Iteration Workflow

### Standard cycle

```
Edit SKILL.md --> test manually --> run /skillcraft --improve --> fix findings --> repeat
```
### When to restart the conversation

| Situation | Action |
|-----------|--------|
| Changed frontmatter fields | Restart -- skill metadata is loaded at conversation start |
| Changed body text only | No restart needed -- re-invoke to pick up changes |
| Changed reference files | No restart needed -- files read on demand |
| Changed scripts | No restart needed -- scripts executed fresh each time |
| Skill not auto-triggering after description change | Restart -- description cached at load time |

### Common iteration patterns

| Pattern | Cycle |
|---------|-------|
| Trigger tuning | Edit `description` -> restart -> test phrases -> repeat |
| Script debugging | Run in terminal -> fix -> test via skill -> repeat |
| Reference extraction | Move to references/ -> link from SKILL.md -> `/skillcraft --improve` |
| Tool restriction | Add `allowed-tools` -> invoke -> fix permission errors -> repeat |

### Post-iteration validation

- [ ] Run `/skillcraft --improve` with full audit -- all checks pass
- [ ] Invoke skill manually -- produces expected output
- [ ] Trigger phrases work (if auto-invocable)
- [ ] Edge cases handled gracefully
- [ ] Scripts exit cleanly with correct codes

---

## 7. Eval Bootstrapping Protocol

When a behavioral edit targets a skill with no `evals/` directory, bootstrap baseline evals before the edit proceeds. This is **blocking** — the edit cannot complete without eval coverage.

### Tiered Approach

| Tier | Condition | Action | Time |
|------|-----------|--------|------|
| 1 | `evals/` exists, scenario covers edit | Run matching scenarios pre/post | ~2 min |
| 2 | `evals/` exists, no scenario covers edit | Add 1 edit-specific scenario, run pre/post | ~5 min |
| 3 | No `evals/` directory | Bootstrap interview + Tier 1 or 2 | ~10 min |

Determine the tier at the start of every behavioral edit. Tier 3 happens at most **once per skill** — after bootstrap, all future edits are Tier 1 or 2.

### Tier 3: Bootstrap Interview

When a skill has no `evals/` at all:

**Step 1 — Read the skill.** Read all files in the skill directory. Classify the skill type (discipline, technique, pattern, reference) per § Testing by Skill Type above.

**Step 2 — Propose scenarios.** Generate 3 scenario proposals matching the skill type:

| Skill Type | Scenario Mix |
|------------|-------------|
| Discipline | 1 pressure, 1 compliance, 1 edge case |
| Technique | 1 application, 1 variation, 1 gap |
| Pattern | 1 recognition, 1 application, 1 counter-example |
| Reference | 1 retrieval, 1 application, 1 completeness |

For each proposed scenario, draft: `id`, `name`, `prompt`, and 3 `checks`. Make scenarios realistic and specific to the skill's actual content — not generic templates. Consult the `claude-code-evals` skill for check quality rules.

**Step 3 — Interview the user.** Present the proposals and ask:

1. "Here are 3 proposed eval scenarios for [skill-name]. For each: approve as-is, suggest changes, or replace?"
2. "What behaviors are most critical to verify? Anything I missed?"

Revise scenarios based on feedback. Two questions is the target; three is the maximum.

**Step 4 — Write scenario files.** Create `<skill-dir>/evals/<scenario-id>/scenario.yml` for each scenario. Run `craboodle --help` for the scenario.yml schema.

**Step 5 — Run initial eval (optional).** If time permits and the user agrees, run the eval scenarios to establish an initial baseline. This confirms the scenarios work as expected before the skill is written.

### Tier 2: Edit-Specific Scenario

When `evals/` exists but no scenario covers the behavior being edited:

1. Identify the specific behavior the edit changes.
2. Draft 1 new scenario exercising that behavior — with `id`, `prompt`, and `checks`.
3. Present to user: "This edit changes [behavior]. I propose this eval scenario: [summary]. Approve or modify?"
4. Create `<skill-dir>/evals/<scenario-id>/scenario.yml` for the new scenario.

### Pre/Post Edit Verification with Evals

Once eval scenarios exist (from any tier), use them for Lightweight Mode items 7-9:

| Lightweight Mode Item | Eval-Based Execution |
|----------------------|---------------------|
| 7. Pre-edit snapshot | Run edit-relevant scenario(s) with current skill as a subagent; save output |
| 8. Post-edit verification | Re-run same scenarios with edited skill; compare outputs |
| 9. Bug fix validation | Ensure at least one scenario reproduces the reported bug pre-edit |

This replaces informal "run 1-2 scenarios" with structured, repeatable eval execution. Outputs can optionally be saved into an `evals/iteration-N/` directory for long-term tracking.

### Quick Path

To minimize bootstrap time:
- Accept proposed scenarios without modification (skip interview question 2)
- Skip the optional baseline iteration (Step 5)
- Minimum viable bootstrap: ~5 minutes for Tier 3
