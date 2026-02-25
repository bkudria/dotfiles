> Sources: original (not derived from upstream)
> Created: 2026-02-06

# Skill Testing & Iteration Guide

Quick reference for testing Claude Code skills after creation or modification.

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
