# Skill Templates

Starter templates for seven skill types. Apply the matching template in Phase 3 (Scaffold), then customize in Phase 4 (Author).

## Type Comparison

| Type | Purpose | Body Size | Auto-trigger | Key Feature |
|------|---------|-----------|-------------|-------------|
| Knowledge | Curated reference on a topic | 50-100 lines | Yes | Summary + references/ |
| Workflow | Multi-step procedure | 150-250 lines | Usually no | Phased steps, interactive |
| Tool Integration | CLI tool documentation | 200-400 lines | Yes | Command reference, examples |
| Hybrid | Combines knowledge + workflow | 200-300 lines | Maybe | Multiple resource types |
| Technique | Concrete reusable method | 100-200 lines | Yes | Before/after comparison |
| Pattern | Mental model / way of thinking | 50-150 lines | Yes | Recognition criteria |
| Reference | Exhaustive API/syntax docs | 50-100 lines (body) | Yes | Large references/ directory |

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

---

## 5. Technique Skill

Concrete, reusable method with steps. Differs from Workflow in being context-independent — a technique applies across projects, not as a fixed procedure.

```markdown
---
name: {name}
description: {purpose}. Use when {trigger1}, {trigger2}, or {trigger3}.
---

# {Technique Name}

{One-sentence core principle.}

## When to Use

- {Symptom or situation 1}
- {Symptom or situation 2}
- {Symptom or situation 3}

## The Pattern

### Before
```{lang}
{Code showing the problem or naive approach}
```

### After
```{lang}
{Code showing the technique applied}
```

## Implementation Steps

1. {Step 1 — identify the condition}
2. {Step 2 — apply the technique}
3. {Step 3 — verify the result}

## Common Mistakes

| Mistake | Fix |
|---------|-----|
| {Mistake 1} | {How to fix} |
| {Mistake 2} | {How to fix} |
```

**Characteristics**: Medium body (~100-200 lines), auto-triggers on symptoms, before/after comparison is central, reusable across contexts.

---

## 6. Pattern Skill

Mental model or way of thinking about problems. Lighter than a Technique — focuses on the insight rather than specific implementation steps.

```markdown
---
name: {name}
description: {purpose}. Use when {trigger1}, {trigger2}, or {trigger3}.
---

# {Pattern Name}

{One-sentence insight — the core mental model.}

## When This Pattern Applies

- {Recognition signal 1}
- {Recognition signal 2}
- {Recognition signal 3}

## When NOT to Apply

- {Counter-example 1 — looks similar but this pattern is wrong}
- {Counter-example 2}

## The Insight

{2-3 paragraphs explaining the mental model. Why it works. What it changes about how you think about the problem.}

## Application

### Recognizing the Pattern
{How to identify when this pattern is relevant.}

### Applying the Pattern
{How to use the mental model in practice.}

## Examples

### Applies
{Scenario where the pattern correctly applies, with brief explanation.}

### Does NOT Apply
{Scenario that looks similar but where the pattern would be wrong.}
```

**Characteristics**: Short-medium body (~50-150 lines), auto-triggers, focuses on recognition and judgment, counter-examples are essential.

---

## 7. Reference Skill

Exhaustive API documentation, syntax guide, or tool reference. Differs from Knowledge in being comprehensive rather than curated — the value is completeness, not brevity.

```markdown
---
name: {name}
description: {purpose} reference. Use when {trigger1}, {trigger2}, or {trigger3}.
---

# {Subject} Reference

{One-sentence summary of what this reference covers.}

## Quick Reference

| Area | Description | Detail |
|------|-------------|--------|
| {Topic 1} | {Brief summary} | `references/{topic1}.md` |
| {Topic 2} | {Brief summary} | `references/{topic2}.md` |
| {Topic 3} | {Brief summary} | `references/{topic3}.md` |

## Common Operations

### {Operation 1}
```{lang}
{Minimal working example}
```

### {Operation 2}
```{lang}
{Minimal working example}
```

## Reference Files

| File | Purpose |
|------|---------|
| `references/{topic1}.md` | {Comprehensive docs for topic 1} |
| `references/{topic2}.md` | {Comprehensive docs for topic 2} |
| `references/{topic3}.md` | {Comprehensive docs for topic 3} |
```

**Characteristics**: Short body (~50-100 lines), large references/ directory, auto-triggers on subject keywords, read-only safe, value is in completeness of reference files.
