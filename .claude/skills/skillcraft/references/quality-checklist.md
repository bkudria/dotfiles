# Skill Quality Checklist

42 checks across 8 categories. Each item has: ID, description, pass/fail criteria, fix guidance.

Apply all checks when running a full audit. For lightweight mode, apply only S1, S3, M1, M2, M3 and scan for anti-patterns.

---

## Structure (S1-S7)

### S1: SKILL.md Exists
- **Check**: Skill directory contains a file named exactly `SKILL.md`
- **Pass**: File exists with exact case
- **Fail**: Missing, or wrong case (`skill.md`, `Skill.md`)
- **Fix**: Rename to `SKILL.md` (case-sensitive)

### S2: Name Matches Directory
- **Check**: `name` frontmatter field matches the parent directory name
- **Pass**: `name: my-skill` in directory `my-skill/`
- **Fail**: Name and directory differ
- **Fix**: Update `name` field to match directory, or rename directory

### S3: Valid YAML Frontmatter
- **Check**: File starts with `---`, contains valid YAML, closes with `---`
- **Pass**: YAML parses without errors, contains at least `name`
- **Fail**: Missing delimiters, invalid YAML, or no `name` field
- **Fix**: Add or fix the frontmatter block

### S4: Only Valid Frontmatter Fields
- **Check**: All frontmatter fields are recognized Claude Code skill fields
- **Pass**: Only uses: `name`, `description`, `disable-model-invocation`, `user-invocable`, `allowed-tools`, `model`, `context`, `agent`, `argument-hint`, `hooks`
- **Fail**: Contains unknown fields (typos, invented fields)
- **Fix**: Remove or correct invalid fields

### S5: Referenced Files Exist
- **Check**: Every file path mentioned in SKILL.md exists on disk
- **Pass**: All referenced paths resolve
- **Fail**: Broken references to scripts or reference files
- **Fix**: Create missing files, fix paths, or remove stale references

### S6: Scripts Executable
- **Check**: All `.sh` files in `scripts/` have execute permission
- **Pass**: `chmod +x` on all scripts
- **Fail**: Scripts lack execute permission
- **Fix**: `chmod +x scripts/*.sh`

### S7: No Orphan Files
- **Check**: Every file in `references/` and `scripts/` is referenced from SKILL.md
- **Pass**: All files mentioned or linked
- **Fail**: Files exist but are never referenced
- **Fix**: Add references in SKILL.md or remove orphaned files

---

## Metadata (M1-M8)

### M1: Name Format
- **Check**: `name` is hyphen-case, max 64 characters, lowercase
- **Pass**: `my-skill-name` (hyphen-case, ≤64 chars)
- **Fail**: `MySkill`, `my_skill`, or >64 characters
- **Fix**: Convert to hyphen-case

### M2: Description Present
- **Check**: `description` field exists and is 10-1024 characters
- **Pass**: Meaningful description within length bounds
- **Fail**: Missing, empty, too short (<10), or too long (>1024)
- **Fix**: Write a description with trigger phrases (see M3)

### M3: Description Has Trigger Phrases
- **Check**: Description contains specific action verbs and context nouns that enable auto-triggering
- **Pass**: Description mentions concrete scenarios: "Use when editing Dockerfiles", "for debugging API errors"
- **Fail**: Generic description: "A helpful skill for various tasks"
- **Fix**: Add 3-5 specific trigger phrases. Think: what would a user say or do when they need this skill?

### M4: Third-Person / Imperative Voice
- **Check**: Description uses third-person or imperative voice
- **Pass**: "Audit and improve skills" / "This skill audits..."
- **Fail**: "You can use this to audit..." / "I will help you..."
- **Fix**: Rewrite in imperative: "Audit and improve Claude Code skills. Use when..."

### M5: Argument Hint If Arguments Used
- **Check**: If SKILL.md body references `$ARGUMENTS` or `{{ARGUMENTS}}`, frontmatter includes `argument-hint`
- **Pass**: `argument-hint` present when arguments are used
- **Fail**: Body uses `$ARGUMENTS` but no hint provided
- **Fix**: Add `argument-hint: "[description of expected argument]"`

### M6: Minimal Allowed Tools
- **Check**: If `allowed-tools` is set, it includes all tools the skill's instructions require
- **Pass**: Allowed tools sufficient for described operations, or field omitted (all tools)
- **Fail**: Instructions say "read the file" but `allowed-tools` doesn't include Read
- **Fix**: Add missing tools or remove the restriction if most tools are needed

### M7: Invocation Settings Coherent
- **Check**: `disable-model-invocation` and `user-invocable` settings make sense together
- **Pass**: At least one invocation path enabled. Settings match intended use.
- **Fail**: Both set to disable (invisible skill), or auto-trigger on a heavy wizard
- **Fix**: Enable at least one path. Set `disable-model-invocation: true` for heavy workflows.

### M8: Model Override Justified
- **Check**: If `model` is set, there's a clear reason (cost, capability, speed)
- **Pass**: Model override documented in body with rationale, or not set
- **Fail**: Model override with no explanation
- **Fix**: Add a comment explaining why, or remove the override

---

## Content (C1-C7)

### C1: Imperative Writing Style
- **Check**: Body text uses imperative/declarative voice, not second-person
- **Pass**: "Run the audit.", "Apply all checks.", "The skill validates..."
- **Fail**: "You should run the audit.", "You can apply checks."
- **Fix**: Rewrite sentences in imperative. Remove "you" throughout.

### C2: Appropriate Length
- **Check**: SKILL.md body is 50-500 lines (excluding frontmatter)
- **Pass**: Within range — enough detail to be useful, not overwhelming
- **Fail**: <50 lines (too sparse) or >500 lines (too dense, needs splitting)
- **Fix**: If too short, add examples and sections. If too long, extract to references/.

### C3: "When to Use" Section
- **Check**: Body contains a section explaining when the skill applies
- **Pass**: Has "When to Use", "When This Skill Applies", or equivalent heading
- **Fail**: No guidance on when to use
- **Fix**: Add a "When to Use" section with 3-5 bullet points

### C4: Quick Reference Table
- **Check**: Key information is summarized in a table for quick scanning
- **Pass**: Has at least one reference table (commands, scripts, options, etc.)
- **Fail**: All information in prose paragraphs
- **Fix**: Extract key reference data into a markdown table

### C5: Runnable Code Examples
- **Check**: Code examples are complete and copy-pasteable
- **Pass**: Examples include full command with realistic arguments
- **Fail**: Pseudocode, `...` placeholders, or `foo/bar` toy examples
- **Fix**: Replace with realistic, runnable examples

### C6: Accurate Cross-References
- **Check**: References to other skills, tools, or files are accurate
- **Pass**: All mentioned skills exist, paths are correct
- **Fail**: References to non-existent skills or wrong paths
- **Fix**: Verify and correct all cross-references

### C7: Dependencies Documented
- **Check**: External dependencies (CLI tools, other skills) are listed
- **Pass**: Has a Dependencies section listing required tools
- **Fail**: Uses `gum`, `jq`, etc. without mentioning them
- **Fix**: Add a Dependencies section

---

## Progressive Disclosure (P1-P5)

### P1: Body Not Bloated
- **Check**: SKILL.md body stays focused on essential workflow and quick reference
- **Pass**: Body ≤300 lines, detailed reference in references/
- **Fail**: Body >300 lines with detailed reference material inline
- **Fix**: Extract detailed content to references/ files

### P2: Detail in References
- **Check**: Detailed specifications, catalogs, and deep-dive content live in references/
- **Pass**: references/ contains supporting detail files
- **Fail**: All detail crammed into SKILL.md, no references/
- **Fix**: Create references/ files for detailed content. Link from SKILL.md.

### P3: SKILL.md References Supporting Files
- **Check**: SKILL.md mentions and links to its references/ and scripts/ files
- **Pass**: Body contains paths or links to all supporting files
- **Fail**: Supporting files exist but aren't mentioned
- **Fix**: Add a "Reference Files" table or links in relevant sections

### P4: Each Reference Focused
- **Check**: Each file in references/ covers one coherent topic
- **Pass**: `anti-patterns.md` covers anti-patterns, `checklist.md` covers checks
- **Fail**: Single monolithic reference file covering everything
- **Fix**: Split into focused files, one topic per file

### P5: Scripts Have Usage Headers
- **Check**: Scripts in scripts/ start with a usage comment
- **Pass**: First lines show `#!/bin/bash` and `# Usage: script.sh [args]`
- **Fail**: Scripts with no usage documentation
- **Fix**: Add a usage comment block at the top

---

## Advanced Features (A1-A5)

### A1: Dynamic Context Commands Valid
- **Check**: If SKILL.md uses `` !`command` `` syntax, the commands are valid and portable
- **Pass**: Commands work on macOS and Linux, handle errors
- **Fail**: Commands use platform-specific tools without fallback
- **Fix**: Use portable commands or add platform checks

### A2: String Substitutions Documented
- **Check**: If using `$ARGUMENTS`, `$N`, or `${CLAUDE_SESSION_ID}`, their meaning is documented in the body
- **Pass**: Body explains what arguments are expected
- **Fail**: Uses `$ARGUMENTS` with no explanation of expected format
- **Fix**: Add argument documentation near the top of the body

### A3: Context Fork Justified
- **Check**: If `context: fork` is set, there's a reason (prevents context pollution)
- **Pass**: Forking justified — skill is reference-heavy or has large context
- **Fail**: Fork set with no clear reason
- **Fix**: Document why forking is needed, or remove if unnecessary

### A4: Hooks Structured Correctly
- **Check**: If `hooks` field is used, it follows the Claude Code hooks schema
- **Pass**: Valid hook configuration
- **Fail**: Malformed hooks
- **Fix**: Correct the hooks structure per Claude Code documentation

### A5: Agent Type Valid
- **Check**: If `agent` field is used, it's a valid agent type
- **Pass**: One of: `Explore`, `Plan`, `general-purpose`, `Bash`, or a custom agent defined in `.claude/agents/`
- **Fail**: Invalid or misspelled agent type
- **Fix**: Use a valid agent type

---

## Quality (Q1-Q5)

### Q1: No Duplicated Content
- **Check**: Same information isn't repeated across SKILL.md and references/
- **Pass**: Each piece of information appears once
- **Fail**: Body repeats what's in a reference file
- **Fix**: Keep one copy, reference the other location

### Q2: Tables for Structured Data
- **Check**: Structured data (lists of options, commands, flags) uses tables
- **Pass**: Flags, options, and catalogs in table format
- **Fail**: Structured data in bullet lists or prose
- **Fix**: Convert to markdown tables

### Q3: Consistent Formatting
- **Check**: Headings, code blocks, and emphasis follow a consistent style
- **Pass**: Uniform heading levels, consistent code fence style
- **Fail**: Mixed `#` levels, inconsistent emphasis
- **Fix**: Standardize formatting throughout

### Q4: No Broken References
- **Check**: All markdown links and file references resolve
- **Pass**: Every `[text](path)` link works
- **Fail**: Links to non-existent files or anchors
- **Fix**: Fix or remove broken links

### Q5: Real-World Examples
- **Check**: Examples use realistic scenarios, not foo/bar placeholders
- **Pass**: Examples reflect actual use cases for the skill
- **Fail**: Generic placeholder examples
- **Fix**: Replace with examples matching the skill's actual use cases

---

## TDD Compliance (T1-T5)

### T1: Compliance Verified
- **Check**: Skill tested with presence — agents follow the guidance
- **Pass**: Eval scenarios run with skill loaded; agents comply
- **Fail**: Skill present but agents still fail scenarios
- **Fix**: Revise skill content to address specific failures. Do not add speculative content.

### T2: Loopholes Closed
- **Check**: REFACTOR iterations completed; rationalization table populated (discipline skills)
- **Pass**: For discipline-enforcing skills: rationalization table has entries, red flags list created, no new rationalizations found in 2+ test runs
- **Fail**: Discipline skill has no rationalization resistance
- **Fix**: See `references/bulletproofing.md` for techniques. Add loophole counters iteratively.
- **Note**: Non-discipline skills (technique, pattern, reference) may skip this check.

### T3: Skill Type Testing
- **Check**: Appropriate test approach used for the skill's type
- **Pass**: Test approach matches skill type per `references/testing-guide.md` (discipline → pressure, technique → application, pattern → recognition, reference → retrieval)
- **Fail**: Wrong test approach (e.g., pressure-testing a reference skill)
- **Fix**: Review testing-by-type guidance and re-test with appropriate approach

---

## Eval Pipeline (E1-E5)

### E1: Eval Scenarios Defined
- **Check**: Skill has evals.yml with ≥3 scenarios
- **Pass**: evals.yml exists with id, prompt, assertions for each scenario
- **Fail**: No evals.yml or <3 scenarios
- **Fix**: Create scenario.yml files — run `craboodle --help` for schema

### E2: Assertions Target Skill Value
- **Check**: Assertions test behavior the skill specifically adds, not generic Claude capabilities
- **Pass**: For each assertion, "would Claude do this without the skill?" is answered "no"
- **Fail**: Assertions test baseline behavior (e.g., "output contains valid code")
- **Fix**: Revise assertions using the "Where Skill Value Shows Up" table in `references/eval-guide.md`

### E3: Eval Cycle Completed
- **Check**: At least one full eval cycle completed (run → grade → review)
- **Pass**: Eval results reviewed, with pass rates for all scenarios
- **Fail**: Eval started but not reviewed, or never run
- **Fix**: Complete the Phase 4 workflow (`workflows/create-phase4-refine.md`)

### E4: Production Readiness
- **Check**: Pass rate meets minimum threshold
- **Pass**: All scenarios pass at or above `min_pass_rate`
- **Fail**: One or more scenarios below threshold
- **Fix**: Revise skill content, re-run eval, iterate
