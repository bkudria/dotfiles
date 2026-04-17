#!/usr/bin/env bash
# scaffold.sh — Generate a skill directory with type-specific SKILL.md template
#
# Usage:
#   scaffold.sh <skill-name> --path <base-path> --type <knowledge|workflow|tool|hybrid> \
#     [--references] [--scripts] [--assets]
#
# Examples:
#   scaffold.sh docker-helper --path ~/.claude/skills --type tool --references
#   scaffold.sh pr-review --path .claude/skills --type workflow --references --scripts
#   scaffold.sh rust-ref --path ~/.claude/skills --type knowledge --references --provenance

set -euo pipefail

# --- Argument parsing ---

SKILL_NAME=""
BASE_PATH=""
SKILL_TYPE=""
WANT_REFERENCES=false
WANT_SCRIPTS=false
WANT_ASSETS=false
WANT_PROVENANCE=false
WANT_EVALS=false

usage() {
  cat <<'USAGE'
Usage: scaffold.sh <skill-name> --path <base-path> --type <knowledge|workflow|tool|hybrid> [--references] [--scripts] [--assets]

Arguments:
  <skill-name>    Hyphen-case identifier (e.g., docker-helper). Lowercase, digits, hyphens only. Max 64 chars.
  --path          Base directory where the skill folder will be created
  --type          Skill type: knowledge, workflow, tool, or hybrid
  --references    Create references/ directory with placeholder
  --scripts       Create scripts/ directory with placeholder
  --assets        Create assets/ directory with placeholder
  --provenance    Create provenance.yml template for tracking upstream sources
  --evals         Create evals/ directory with template evals.yml for behavioral testing
USAGE
  exit 1
}

[[ $# -lt 5 ]] && usage

SKILL_NAME="$1"
shift

while [[ $# -gt 0 ]]; do
  case "$1" in
    --path)       BASE_PATH="$2"; shift 2 ;;
    --type)       SKILL_TYPE="$2"; shift 2 ;;
    --references) WANT_REFERENCES=true; shift ;;
    --scripts)    WANT_SCRIPTS=true; shift ;;
    --assets)     WANT_ASSETS=true; shift ;;
    --provenance) WANT_PROVENANCE=true; shift ;;
    --evals)      WANT_EVALS=true; shift ;;
    *)            echo "Unknown option: $1"; usage ;;
  esac
done

# --- Validation ---

if [[ -z "$SKILL_NAME" || -z "$BASE_PATH" || -z "$SKILL_TYPE" ]]; then
  echo "Error: skill-name, --path, and --type are required."
  usage
fi

if ! echo "$SKILL_NAME" | grep -qE '^[a-z0-9]([a-z0-9-]*[a-z0-9])?$'; then
  echo "Error: Skill name must be hyphen-case (lowercase letters, digits, hyphens only)."
  exit 1
fi

if [[ ${#SKILL_NAME} -gt 64 ]]; then
  echo "Error: Skill name must be 64 characters or fewer."
  exit 1
fi

case "$SKILL_TYPE" in
  knowledge|workflow|tool|hybrid) ;;
  *) echo "Error: --type must be one of: knowledge, workflow, tool, hybrid"; exit 1 ;;
esac

# Expand ~ in path
BASE_PATH="${BASE_PATH/#\~/$HOME}"

SKILL_DIR="$BASE_PATH/$SKILL_NAME"

if [[ -d "$SKILL_DIR" ]]; then
  echo "Error: Directory already exists: $SKILL_DIR"
  exit 1
fi

# --- Title generation ---

title_case() {
  echo "$1" | tr '-' ' ' | awk '{for(i=1;i<=NF;i++) $i=toupper(substr($i,1,1)) substr($i,2)}1'
}

SKILL_TITLE=$(title_case "$SKILL_NAME")

# --- Template selection ---

generate_knowledge_body() {
  cat <<TEMPLATE
---
name: ${SKILL_NAME}
description: TODO - Complete description. Use when TODO, TODO, or TODO.
---

# ${SKILL_TITLE}

TODO - One-sentence summary of what this reference covers.

## When This Skill Applies

- TODO - Use case 1
- TODO - Use case 2
- TODO - Use case 3

## Quick Reference

| Area | Description |
|------|-------------|
| TODO | TODO |
| TODO | TODO |

## Instructions

1. TODO - Key instruction 1
2. TODO - Key instruction 2
3. Common gotchas: TODO

## Reference Files

| File | Purpose |
|------|---------|
| \`references/TODO.md\` | TODO |
TEMPLATE
}

generate_workflow_body() {
  cat <<TEMPLATE
---
name: ${SKILL_NAME}
description: TODO - Complete description. Guides through TODO workflow.
disable-model-invocation: true
argument-hint: "[TODO - expected argument]"
---

# ${SKILL_TITLE}

TODO - One-sentence summary of the workflow and what it produces.

## When to Use

- TODO - Scenario 1
- TODO - Scenario 2
- TODO - Scenario 3

## Quick Reference

| Phase | Purpose | Key Tools |
|-------|---------|-----------|
| 1. TODO | TODO | TODO |
| 2. TODO | TODO | TODO |
| 3. TODO | TODO | TODO |

---

## Phase 1: TODO

TODO - Description of what happens in this phase.

### Steps

1. TODO
2. TODO
3. TODO

---

## Phase 2: TODO

TODO - Description of what happens in this phase.

### Steps

1. TODO
2. TODO

---

## Phase 3: TODO

TODO - Description of what happens in this phase.

### Output

TODO - What the user gets at the end.

---

## Dependencies

- **TODO** - Why it's needed

## Reference Files

| File | Purpose |
|------|---------|
| \`references/TODO.md\` | TODO |
TEMPLATE
}

generate_tool_body() {
  cat <<TEMPLATE
---
name: ${SKILL_NAME}
description: Reference for using the TODO CLI tool. Use when TODO, TODO, or TODO.
---

# TODO Tool CLI Reference

TODO tool is TODO - one-sentence description.

## Critical Notes

TODO - Any critical warnings or gotchas.

## Command Reference

### TODO-command -- TODO Brief description

\`\`\`bash
# Basic usage
TODO-tool TODO-command TODO-args

# With common flags
TODO-tool TODO-command --flag value TODO-args
\`\`\`

Key flags:
- \`--TODO\` -- TODO description

## Common Patterns

### TODO Pattern 1

\`\`\`bash
TODO - complete runnable example
\`\`\`

## Reference Files

| File | Purpose |
|------|---------|
| \`references/TODO.md\` | TODO |
TEMPLATE
}

generate_hybrid_body() {
  cat <<TEMPLATE
---
name: ${SKILL_NAME}
description: TODO - Complete description. Use when TODO, TODO, or TODO.
argument-hint: "[TODO - expected argument]"
---

# ${SKILL_TITLE}

TODO - One-sentence summary covering both knowledge and workflow aspects.

## When to Use

- TODO - Scenario 1 (knowledge aspect)
- TODO - Scenario 2 (workflow aspect)
- TODO - Scenario 3 (script aspect)

## Quick Reference

| Feature | Description |
|---------|-------------|
| TODO | TODO |
| TODO | TODO |

## Workflow

### Step 1: TODO

TODO - Description and instructions.

### Step 2: TODO

TODO - Description and instructions.

### Step 3: TODO

TODO - Description and instructions.

## Script Reference

| Script | Purpose | Output |
|--------|---------|--------|
| \`scripts/TODO.sh\` | TODO | TODO |

## Dependencies

- **TODO** - Why it's needed

## Reference Files

| File | Purpose |
|------|---------|
| \`references/TODO.md\` | TODO |
TEMPLATE
}

# --- Create structure ---

mkdir -p "$SKILL_DIR"
echo "Created: $SKILL_DIR/"

# Generate type-specific SKILL.md
case "$SKILL_TYPE" in
  knowledge) generate_knowledge_body > "$SKILL_DIR/SKILL.md" ;;
  workflow)  generate_workflow_body  > "$SKILL_DIR/SKILL.md" ;;
  tool)      generate_tool_body     > "$SKILL_DIR/SKILL.md" ;;
  hybrid)    generate_hybrid_body   > "$SKILL_DIR/SKILL.md" ;;
esac
echo "Created: SKILL.md (${SKILL_TYPE} template)"

if $WANT_REFERENCES; then
  mkdir -p "$SKILL_DIR/references"
  cat > "$SKILL_DIR/references/TODO-rename-me.md" <<EOF
# TODO - Reference Title

TODO - Reference content for ${SKILL_TITLE}.
EOF
  echo "Created: references/"
fi

if $WANT_SCRIPTS; then
  mkdir -p "$SKILL_DIR/scripts"
  cat > "$SKILL_DIR/scripts/TODO-rename-me.sh" <<EOF
#!/usr/bin/env bash
# TODO - Script for ${SKILL_NAME}
echo "TODO: implement ${SKILL_NAME} script"
EOF
  chmod +x "$SKILL_DIR/scripts/TODO-rename-me.sh"
  echo "Created: scripts/"
fi

if $WANT_ASSETS; then
  mkdir -p "$SKILL_DIR/assets"
  echo "Created: assets/"
fi

if $WANT_PROVENANCE; then
  TODAY=$(date -u +%Y-%m-%d)
  cat > "$SKILL_DIR/provenance.yml" <<EOF
schema_version: 1
skill: ${SKILL_NAME}
last_full_update: "${TODAY}"

# Upstream sources this skill was curated from.
# GitHub sources track commit SHAs for diffing. Web sources use WebFetch + comparison.
sources:
  # TODO: Add your upstream sources. Examples:
  #
  # official-docs:
  #   url: https://example.com/docs
  #   type: web
  #   last_checked: "${TODAY}"
  #
  # upstream-repo:
  #   url: https://github.com/org/repo
  #   type: github
  #   owner: org
  #   repo: repo
  #   path: path/to/relevant/dir
  #   last_checked_sha: "TODO"
  #   last_checked: "${TODAY}"

# Maps each curated file to its upstream source(s) and curation decision.
#
# Decision taxonomy:
#   kept        - Taken as-is (formatting changes only)
#   simplified  - Reduced complexity while preserving meaning
#   elided      - Intentionally excluded (rationale required)
#   altered     - Changed in meaning or approach from source
#   synthesized - Combined from multiple sources into something new
#   original    - Not derived from any upstream source
curation_decisions: {}
  # TODO: Add entries for each curated file. Example:
  #
  # references/api-reference.md:
  #   - source: upstream-repo
  #     sections: ["README.md"]
  #     decision: simplified
  #     rationale: Condensed API docs into quick-reference format.
EOF
  echo "Created: provenance.yml (upstream source tracking)"
fi

if $WANT_EVALS; then
  mkdir -p "$SKILL_DIR/evals"
  cat > "$SKILL_DIR/evals/craboodle.yaml" <<EOF
version: "1"
EOF
  cat > "$SKILL_DIR/evals/base.yaml" <<EOF
# Shared scuttlerun defaults for ${SKILL_NAME} evals
# model: — uncomment and pick a model. scuttlerun defaults to claude-haiku-4-5,
# which is cheap but under-represents Sonnet/Opus user workflows. See
# bootstrap-evals.md Step 5 "Model selection" for tradeoffs.
tools:
  # scuttlerun defaults — repeat them here because the array replaces, not extends
  - Read
  - Write
  - Edit
  - Bash
  - Glob
  - Grep
  - AskUserQuestion
  - Skill
  # Add skill-specific tools below (e.g., TodoWrite, Task, mcp__*); run 'scuttlerun -n' to verify names.
project:
  skills:
    - ~/.claude/skills/${SKILL_NAME}
EOF
  echo "Created: evals/ (craboodle.yaml, base.yaml) — add scenario directories via the bootstrap-evals workflow"
fi

echo ""
echo "Skill '${SKILL_NAME}' scaffolded at ${SKILL_DIR}"
echo "Type: ${SKILL_TYPE}"
echo ""
echo "Next steps:"
echo "  1. Edit SKILL.md to replace TODO placeholders"
echo "  2. Rename/customize files in references/, scripts/, assets/"
echo "  3. Run /skillcraft to validate"
