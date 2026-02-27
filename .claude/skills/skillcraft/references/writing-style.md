# Skill Writing Style Guide

Voice, tone, and formatting rules for authoring Claude Code skill content.

---

## Voice and Tone

| Rule | Correct | Incorrect |
|------|---------|-----------|
| Imperative voice | "Run the tests" | "You should run the tests" |
| No second person | "Check the output" | "You can check the output" |
| Active voice | "The script creates a directory" | "A directory is created by the script" |
| No hedging | "This fails when..." | "This might possibly fail when..." |
| No filler | "Set the flag" | "It's important to note that the flag should be set" |

Treat every sentence as an instruction or a statement of fact. Strip qualifiers like "simply", "just", "basically", "actually", and "please".

---

## Structure and Formatting

### Heading Hierarchy

| Level | Use For | Example |
|-------|---------|---------|
| `#` | Skill title (one per file) | `# Docker CLI Reference` |
| `##` | Major sections or phases | `## Phase 2: Design` |
| `###` | Subsections within a phase | `### Step 1: Configure` |
| `####` | Rare; sub-subsections only | Avoid when possible |

### Choosing the Right Format

| Content Type | Format | Example Use |
|--------------|--------|-------------|
| Structured data with columns | Table | Commands, flags, options, comparisons |
| Ordered steps | Numbered list | Workflow phases, setup instructions |
| Unordered items | Bullet list | Use cases, prerequisites, notes |
| Explanation or rationale | Paragraph | Introduction, context, trade-offs |
| Commands or code | Fenced code block | Shell commands, config snippets |

### Section Separators

Use horizontal rules (`---`) between major phases or top-level sections. Do not use them between subsections within the same phase.

### Code Block Language Tags

Always specify the language tag on fenced code blocks.

| Content | Tag |
|---------|-----|
| Shell commands | ` ```bash ` |
| YAML config | ` ```yaml ` |
| Skill content examples | ` ```markdown ` |
| JSON data | ` ```json ` |
| Generic/mixed output | ` ```text ` |

---

## Code Examples

### Philosophy

**One excellent example beats many mediocre ones.** Choose the most relevant language for the skill's domain and write one complete, realistic example rather than implementing in multiple languages.

### Requirements

- Realistic and runnable. Never use `foo`, `bar`, `baz`, `example.com`, or `test123` as placeholders.
- Drawn from the skill's actual use cases collected in Phase 1.
- Complete and copy-pasteable, not fill-in-the-blank templates.
- Well-commented explaining WHY, not WHAT.
- Show basic usage first, then advanced variations.
- Include expected output when it clarifies behavior.

### Structure

```bash
# Basic usage
docker build -t myapp:latest .

# With build arguments
docker build --build-arg NODE_ENV=production -t myapp:latest .

# Multi-stage build targeting a specific stage
docker build --target builder -t myapp:builder .
```

### Comment Rules

- Use comments to label distinct variations (as above), not to explain obvious syntax.
- One comment per block of related commands. Do not annotate every line.
- Never use comments as prose paragraphs inside code blocks.

---

## Content Length

| Skill Frequency | Word Target | Rationale |
|----------------|-------------|-----------|
| Frequently-loaded / getting-started | <200 words | Loaded in every conversation — minimize token cost |
| Standard skills | <500 words | Loaded on demand — be concise |
| Reference-heavy skills | SKILL.md <500 words, references/ unlimited | Body stays lean, detail in references/ |
| Frontmatter `description` | 50-200 chars (hard limit 1024) | Brief but keyword-rich |

See `references/naming-conventions.md` for token compression techniques and CSO guidance.

### Paragraph Discipline

- One idea per paragraph.
- Maximum 3-4 sentences per paragraph.
- Prefer a table or list over a dense paragraph when presenting multiple items.
- When a section exceeds 40 lines, split detail into a reference file and link to it.

---

## Linking and References

### Rules

1. Link to every file in `references/` and `scripts/` from SKILL.md.
2. Use relative paths: `` `references/api-guide.md` ``, not absolute paths.
3. Place a **Reference Files** table at the bottom of SKILL.md.
4. Name files descriptively: `api-reference.md`, not `ref1.md` or `notes.md`.

### Reference Table Format

```markdown
## Reference Files

| File | Purpose |
|------|---------|
| `references/frontmatter-reference.md` | Complete catalog of all frontmatter fields |
| `references/writing-style.md` | Voice, tone, and formatting rules |
| `scripts/scaffold.sh` | Creates skill directory structure |
```

---

## Common Mistakes

| Mistake | Fix |
|---------|-----|
| "You should run the tests" | "Run the tests" |
| `foo/bar/baz` placeholder names | Use realistic names from the skill's domain |
| Wall of text without structure | Break into headed sections, tables, or lists |
| Orphaned reference file | Add an entry in SKILL.md's Reference Files table |
| Missing language tag on code block | Add ` ```bash `, ` ```yaml `, etc. |
| `####` heading overuse | Restructure; three levels is usually enough |
| Long frontmatter description | Keep under 1024 chars; move detail to the body |
| Comments on every line of code | One comment per variation block |
