# Skill Templates

Starter templates for four common skill types. Apply the matching template in Phase 3 (Scaffold), then customize in Phase 4 (Author).

---

## 1. Knowledge Skill

Pure reference material. Auto-triggers when the topic comes up. Read-only tools.

```markdown
---
name: {name}
description: {purpose}. Use when {trigger1}, {trigger2}, or {trigger3}.
---

# {Title}

{One-sentence summary of what this reference covers.}

## When This Skill Applies

- {Use case 1}
- {Use case 2}
- {Use case 3}

## Quick Reference

| Area | Description |
|------|-------------|
| {Topic 1} | {Brief summary} |
| {Topic 2} | {Brief summary} |
| {Topic 3} | {Brief summary} |

## Instructions

1. Consult `references/{detail-file}.md` for complete details before {acting}
2. {Key instruction 2}
3. {Key instruction 3}
4. Common gotchas: {list of pitfalls}

## Reference Files

| File | Purpose |
|------|---------|
| `references/{detail-file}.md` | {Description} |
```

**Characteristics**: Short body (~50-100 lines), detail in references/, auto-triggers freely, read-only safe.

---

## 2. Workflow Skill

Multi-step procedure with distinct phases. Usually manual invoke to avoid accidental triggering.

```markdown
---
name: {name}
description: {purpose}. Guides through {workflow summary}.
disable-model-invocation: true
argument-hint: "[{expected argument}]"
---

# {Title}

{One-sentence summary of the workflow and what it produces.}

## When to Use

- {Scenario 1}
- {Scenario 2}
- {Scenario 3}

## Quick Reference

| Phase | Purpose | Key Tools |
|-------|---------|-----------|
| 1. {Phase 1} | {What it does} | {Tools used} |
| 2. {Phase 2} | {What it does} | {Tools used} |
| 3. {Phase 3} | {What it does} | {Tools used} |

---

## Phase 1: {Phase Name}

{Description of what happens in this phase.}

### Steps

1. {Step 1}
2. {Step 2}
3. {Step 3}

### Example

```bash
{Realistic, runnable example}
```

---

## Phase 2: {Phase Name}

{Description of what happens in this phase.}

### Steps

1. {Step 1}
2. {Step 2}

---

## Phase 3: {Phase Name}

{Description of what happens in this phase.}

### Output

{What the user gets at the end.}

---

## Dependencies

- **{tool}** — {Why it's needed} (`brew install {tool}`)

## Reference Files

| File | Purpose |
|------|---------|
| `references/{file}.md` | {Description} |
```

**Characteristics**: Longer body (~150-250 lines), manual invoke, phased structure, interactive prompts.

---

## 3. Tool Integration Skill

Wraps an external CLI tool. Provides command reference + usage examples. Auto-triggers when the tool is mentioned.

```markdown
---
name: {name}
description: Reference for using the `{tool}` CLI tool. Use when {trigger1}, {trigger2}, or {trigger3}. {Important caveat if any.}
---

# {Tool} CLI Reference

{Tool} is {one-sentence description of what the tool does}.

## Critical Notes

{Any critical warnings, gotchas, or behavioral notes about the tool.}

## Command Reference

### {command1} — {Brief description}
```bash
# Basic usage
{tool} {command1} {basic args}

# With common flags
{tool} {command1} --flag value {args}

# Advanced usage
{tool} {command1} --flag1 --flag2 {args}
```

Key flags:
- `--flag1` — {Description}
- `--flag2` — {Description}

### {command2} — {Brief description}
```bash
{tool} {command2} {args}
```

Key flags:
- `--flag` — {Description}

## Common Patterns

### {Pattern 1 name}
```bash
{Complete, runnable example}
```

### {Pattern 2 name}
```bash
{Complete, runnable example}
```

## Environment Variables

{If applicable — environment variable configuration.}

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | {Error condition} |
```

**Characteristics**: Medium body (~200-400 lines), auto-triggers, command-reference structure, many code examples.

---

## 4. Hybrid Skill

Combines knowledge, workflow, and/or scripts. Full-featured template for complex skills.

```markdown
---
name: {name}
description: {purpose}. Use when {trigger1}, {trigger2}, or {trigger3}. {Additional context.}
argument-hint: "[{expected argument}]"
---

# {Title}

{One-sentence summary covering both the knowledge and workflow aspects.}

## When to Use

- {Scenario 1 — knowledge aspect}
- {Scenario 2 — workflow aspect}
- {Scenario 3 — script aspect}

## Quick Reference

| Feature | Description |
|---------|-------------|
| {Feature 1} | {Summary} |
| {Feature 2} | {Summary} |
| {Feature 3} | {Summary} |

## Workflow

### Step 1: {Step Name}

{Description and instructions.}

```bash
{Example using scripts/}
```

### Step 2: {Step Name}

{Description and instructions.}

### Step 3: {Step Name}

{Description and instructions.}

## Script Reference

| Script | Purpose | Output |
|--------|---------|--------|
| `scripts/{script1}.sh` | {What it does} | {What it returns} |
| `scripts/{script2}.sh` | {What it does} | {What it returns} |

## Dependencies

- **{tool}** — {Why it's needed} (`brew install {tool}`)
- **{skill}** skill — {Why it's needed}

## Reference Files

| File | Purpose |
|------|---------|
| `references/{file1}.md` | {Description} |
| `references/{file2}.md` | {Description} |
```

**Characteristics**: Medium body (~200-300 lines), may or may not auto-trigger, combines multiple resource types, scripts directory.
