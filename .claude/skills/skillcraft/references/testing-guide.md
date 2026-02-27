# Skill Testing & Iteration Guide

Skill creation IS Test-Driven Development applied to documentation. Test before writing, test after writing, and iterate until bulletproof.

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

See `workflows/create-phase0-baseline.md` for the RED phase workflow.

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

Test that `description` field drives correct auto-invocation.

| Direction | What to test | Example |
|-----------|-------------|---------|
| Positive | Exact phrases from description | "audit this skill" |
| Positive | Synonyms and rephrasings | "review my skill quality" |
| Positive | Partial matches | "check skill frontmatter" |
| Negative | Related but different topic | "audit this code" (not a skill) |
| Negative | Shared keywords, different intent | "improve performance" vs "improve skills" |

- [ ] Fires on 3+ distinct positive trigger phrasings
- [ ] Does NOT fire on 3+ unrelated topics sharing keywords
- [ ] Does NOT fire when another skill is more appropriate
- [ ] `disable-model-invocation: true` skills never auto-trigger

If triggers are too broad or too narrow, revise `description`. More specific verb+noun phrases improve precision.

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
