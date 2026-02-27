# Skill Anti-Patterns

Common problems in Claude Code skills with diagnosis and fixes. Organized by category.

---

## Structural

### 1. Wall of Text

**Problem**: Everything crammed into SKILL.md with no references/ directory. Body exceeds 500 lines.

**Symptoms**: SKILL.md is a monolith. Hard to scan. Claude loads excessive context.

**Before**:
```
SKILL.md (800 lines)
├── Frontmatter
├── Full API reference (300 lines)
├── Complete examples catalog (200 lines)
├── Troubleshooting guide (150 lines)
└── Configuration reference (150 lines)
```

**After**:
```
SKILL.md (150 lines) — workflow + quick reference
references/
├── api-reference.md
├── examples.md
├── troubleshooting.md
└── configuration.md
```

**Fix**: Extract detailed content into focused reference files. Keep SKILL.md as a concise workflow guide with a reference table linking to supporting files.

---

### 2. Missing Map

**Problem**: references/ and scripts/ files exist but SKILL.md never mentions them.

**Symptoms**: Claude loads SKILL.md but doesn't know supporting files exist. Users can't discover them.

**Before**:
```markdown
## Usage
Run the audit command to check your skill.
```
(Meanwhile `references/checklist.md` and `scripts/validate.sh` sit unused)

**After**:
```markdown
## Reference Files

| File | Purpose |
|------|---------|
| `references/checklist.md` | Complete validation checklist |
| `scripts/validate.sh` | Automated validation script |
```

**Fix**: Add a "Reference Files" or "Scripts" table at the bottom of SKILL.md. Reference specific files inline where relevant.

---

### 3. Orphaned Resources

**Problem**: Files in references/ or scripts/ that nothing references.

**Symptoms**: `ls references/` shows files. `grep -r` for those filenames in SKILL.md returns nothing.

**Diagnosis**:
```bash
# Find orphans
for f in references/* scripts/*; do
  basename=$(basename "$f")
  grep -q "$basename" SKILL.md || echo "Orphan: $f"
done
```

**Fix**: Either reference the file from SKILL.md or delete it. Every file should earn its place.

---

### 4. Kitchen Sink Frontmatter

**Problem**: Frontmatter includes unnecessary fields, cluttering the metadata.

**Before**:
```yaml
---
name: my-skill
description: Does a thing
disable-model-invocation: false
user-invocable: true
allowed-tools: Bash, Read, Write, Edit, Glob, Grep, WebFetch, WebSearch, Task
model: sonnet
context: shared
---
```

**After**:
```yaml
---
name: my-skill
description: Does a thing
---
```

**Fix**: Only include fields that differ from defaults. `disable-model-invocation: false`, `user-invocable: true`, and unrestricted `allowed-tools` are all defaults — omit them. Only set `model` with justification.

---

### 5. Over-Restricted Tools

**Problem**: `allowed-tools` is too narrow for what the instructions describe.

**Before**:
```yaml
allowed-tools: Read
```
```markdown
## Workflow
1. Read the file
2. Edit the configuration  ← Edit not allowed!
3. Run the tests           ← Bash not allowed!
```

**After**:
```yaml
allowed-tools: Read, Edit, Bash
```

**Fix**: Audit every action the skill describes. Ensure each maps to an allowed tool. If most tools are needed, omit `allowed-tools` entirely.

---

### 6. Invisible Skill

**Problem**: Both invocation paths disabled — skill can never be loaded.

**Before**:
```yaml
disable-model-invocation: true
user-invocable: false
```

**Diagnosis**: Skill exists on disk but Claude can never load it. No `/skill-name` command, no auto-trigger.

**Fix**: Enable at least one path:
- `user-invocable: true` (default) — enables `/skill-name`
- Remove `disable-model-invocation: true` — enables auto-trigger
- For heavy workflows, keep manual-only: `disable-model-invocation: true` with `user-invocable: true` (default)

---

## Content

### 7. Second Person Habit

**Problem**: Body text uses "you should" instead of imperative voice.

**Before**:
```markdown
## Usage
You should first read the configuration file. Then you can run the
validation command. If you encounter errors, you should check the logs.
```

**After**:
```markdown
## Usage
1. Read the configuration file
2. Run the validation command
3. On errors, check the logs
```

**Fix**: Remove "you" globally. Rewrite as imperative commands or declarative statements.

---

### 8. Duplicate Knowledge

**Problem**: Same content appears in both SKILL.md body and a reference file.

**Symptoms**: Updating one copy but not the other causes drift. Extra context consumption.

**Diagnosis**: Compare overlapping sections between SKILL.md and references/ files.

**Fix**: Keep the canonical version in one place:
- Quick reference / summary → SKILL.md
- Complete detail → references/
- Never both

---

### 9. Narrative Example

**Problem**: Examples tell a story about a specific session instead of showing a reusable pattern.

**Before**:
```markdown
In session 2025-10-03, we found that empty projectDir caused the build
to fail. After debugging for 20 minutes, we discovered...
```

**After**:
```markdown
## Empty Project Directory

**Symptom**: Build fails with "no input files" error.
**Cause**: `projectDir` is empty or unset.
**Fix**: Validate `projectDir` before invoking the build.
```

**Fix**: Extract the reusable pattern. Strip session-specific narrative, dates, and debugging stories. Present the problem, cause, and fix directly.

---

### 10. Multi-Language Dilution

**Problem**: Same example implemented in 5+ languages, diluting quality and creating maintenance burden.

**Before**:
```
examples/
├── example-js.js
├── example-py.py
├── example-go.go
├── example-rust.rs
└── example-java.java
```

**After**:
```
SKILL.md — one excellent, well-commented example in the most relevant language
```

**Fix**: Pick the language most relevant to the skill's domain. Write one complete, realistic, well-commented example. Claude can port to other languages on demand.

---

## Naming & Discovery

### 11. Vague Trigger

**Problem**: Description is too generic for Claude to auto-trigger the skill.

**Before**:
```yaml
description: A useful skill for helping with various development tasks
```

**After**:
```yaml
description: Audit and improve Claude Code skills. Use when editing SKILL.md files, reviewing skill quality, fixing skill frontmatter issues, optimizing skill descriptions, or maintaining skill collections.
```

**Fix**: Include 3-5 specific verb+noun phrases that match what users actually say or do. Think: "What would I be doing when I need this skill?"

---

### 12. Workflow Summary Description

**Problem**: Description summarizes the skill's workflow, causing Claude to follow the description instead of reading the full skill body.

**Before**:
```yaml
description: Use when executing plans — dispatches subagent per task with code review between tasks
```

**After**:
```yaml
description: Use when executing implementation plans with independent tasks in the current session
```

**Diagnosis**: Claude performs only some of the skill's steps — it followed the description's summary instead of reading the body. Verified by testing: changing description to triggering-conditions-only restored correct behavior.

**Fix**: Description = triggering conditions only. Never summarize what the skill does step-by-step. See `references/naming-conventions.md` for CSO guidance.

---

## Examples & Formatting

### 13. Toy Examples

**Problem**: Code examples use foo/bar placeholders instead of realistic scenarios.

**Before**:
```bash
my-tool --input foo.txt --output bar.txt
my-tool --name "example" --type "thing"
```

**After**:
```bash
my-tool --input src/config.yaml --output dist/config.json
my-tool --name "user-auth" --type "middleware"
```

**Fix**: Replace every placeholder with a realistic value that matches the skill's domain. Show examples that users can actually adapt.

---

### 14. Code in Flowcharts

**Problem**: Code snippets placed inside flowchart node labels where they can't be copy-pasted.

**Before**:
```dot
step1 [label="import fs"];
step2 [label="const data = fs.readFileSync('config.json')"];
step3 [label="JSON.parse(data)"];
```

**After**: Use a fenced code block for the code, and a flowchart only for the decision logic.

**Fix**: Flowcharts show decisions and flow. Code blocks show code. Never mix them. If a step involves code, describe it in the node label and show the code in a separate code block.

---

### 15. Generic Labels

**Problem**: Flowchart nodes, list items, or table entries use meaningless labels like `helper1`, `step3`, `pattern4`.

**Before**:
```
helper1 → helper2 → helper3
```

**After**:
```
validate-input → transform-data → write-output
```

**Fix**: Every label should have semantic meaning. Name things by what they do, not by their position in a sequence.
