# Skill Resource Usage Guide

How to effectively use the `references/`, `scripts/`, and `assets/` directories within a Claude Code skill.

## Directory Purposes

- **references/** -- Documentation and reference material that Claude reads into context. Supplements SKILL.md with detailed information without bloating the main file.
- **scripts/** -- Executable code (bash, python) that Claude runs via the Bash tool. Automates repetitive or multi-step tasks.
- **assets/** -- Static files (templates, configs, data) used as output or input. These are NOT loaded into context automatically.

## When to Use Each

| Use Case | Directory | Example |
|----------|-----------|---------|
| Detailed docs too long for SKILL.md | references/ | API reference, checklists |
| Automated task | scripts/ | Scaffolding, validation, formatting |
| Template files to copy/customize | assets/ | Config templates, boilerplate |
| Static data files | assets/ | Word lists, schema definitions |

## References Best Practices

- One topic per file.
- Name descriptively (`api-reference.md`, not `ref1.md`).
- Start with a title heading matching the topic.
- Keep focused -- if a reference exceeds 300 lines, split it into separate files.
- Always mention references in SKILL.md with a reference table so Claude knows when to consult them.
- Use progressive disclosure: SKILL.md has the summary, the reference file has the detail.

## Scripts Best Practices

- Include a shebang line (`#!/usr/bin/env bash`).
- Set strict mode (`set -euo pipefail`).
- Include a usage comment at the top describing purpose and arguments.
- Accept arguments for flexibility rather than hardcoding values.
- Print clear status messages so Claude can interpret results.
- Use exit codes properly: 0 for success, non-zero for errors.
- Make scripts executable (`chmod +x`).
- Document each script in SKILL.md's script reference table.

Example header:

```bash
#!/usr/bin/env bash
# Usage: scaffold-component.sh <name> [--with-tests]
# Creates a new component with optional test file.
set -euo pipefail
```

## Assets Best Practices

- Assets are NOT read into context automatically -- they do not consume context budget.
- Use the Write tool or scripts to copy and customize them for the user.
- Name files with the intended output format (`template.yml`, not `template.txt`).
- Include placeholder markers (`TODO`, `{{variable}}`) so Claude knows what to fill in.
- Assets have no size limit since they are not loaded into context.

## File Size Guidelines

| File Type | Recommended Max | Reason |
|-----------|----------------|--------|
| SKILL.md | 300 lines | Context budget; this is always loaded |
| Single reference | 300 lines | Focus and readability; split if larger |
| Script | 200 lines | Maintainability; decompose if larger |
| Asset | No limit | Not loaded into context |

## How They Work Together

A typical skill uses all three directories in concert:

1. **SKILL.md** provides the high-level instructions and a table listing available references, scripts, and assets.
2. **references/** files are read on demand when Claude needs deeper detail on a topic mentioned in SKILL.md.
3. **scripts/** are invoked via the Bash tool to automate tasks described in SKILL.md.
4. **assets/** are copied or transformed by scripts or the Write tool to produce final output.

## Key Trade-offs

- Putting too much in SKILL.md wastes context on every invocation. Move detail to references.
- Putting too little in SKILL.md means Claude may not know a reference or script exists. Always include a summary table.
- Scripts add power but add maintenance burden. Use them for tasks that are repetitive, error-prone, or require exact formatting.
- Assets are free in terms of context cost but invisible to Claude unless SKILL.md describes them.
