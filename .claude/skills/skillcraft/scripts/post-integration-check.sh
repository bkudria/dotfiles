#!/usr/bin/env bash
# post-integration-check.sh — Content quality checks for post-integration validation
#
# Usage:
#   post-integration-check.sh <skill-directory>
#
# Checks content quality issues that quick-validate.sh doesn't cover.
# Run after integrating content from an external source.
#
# Checks performed:
#   IC1: Cross-reference consistency (files referenced by name exist and are in SKILL.md)
#   IC2: Word count compliance (SKILL.md body against targets)
#   IC3: Terminology consistency (near-duplicate terms across headings)
#   IC4: Description pitfall check (workflow-summary words in frontmatter description)

set -euo pipefail
shopt -s nullglob

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

PASS=0
FAIL=0
WARN=0

pass() { echo -e "  ${GREEN}PASS${NC} $1: $2"; PASS=$((PASS + 1)); }
fail() { echo -e "  ${RED}FAIL${NC} $1: $2"; FAIL=$((FAIL + 1)); }
warn() { echo -e "  ${YELLOW}WARN${NC} $1: $2"; WARN=$((WARN + 1)); }

if [[ $# -lt 1 ]]; then
    echo "Usage: post-integration-check.sh <skill-directory>"
    exit 1
fi

skill_dir="${1/#\~/$HOME}"

if [[ ! -d "$skill_dir" ]]; then
    echo "Error: Directory not found: $skill_dir"
    exit 1
fi

skill_md="$skill_dir/SKILL.md"

if [[ ! -f "$skill_md" ]]; then
    echo "Error: SKILL.md not found in $skill_dir"
    exit 1
fi

dir_name=$(basename "$skill_dir")

echo ""
echo "Post-integration check: $dir_name"
echo "  Path: $skill_dir"
echo ""

# --- IC1: Cross-reference consistency ---
# For every file in references/ and workflows/, check if it's referenced from SKILL.md.
# Also check that if file A mentions file B by name, B exists.

ic1_missing_from_skillmd=0
ic1_broken_refs=0
ic1_checked=0

# Check all reference and workflow files are mentioned in SKILL.md
for dir in references workflows scripts; do
    if [[ -d "$skill_dir/$dir" ]]; then
        for file in "$skill_dir/$dir"/*; do
            [[ -f "$file" ]] || continue
            basename_file=$(basename "$file")
            ic1_checked=$((ic1_checked + 1))
            if ! grep -q "$basename_file" "$skill_md" 2>/dev/null; then
                warn "IC1" "File '$dir/$basename_file' not referenced from SKILL.md"
                ic1_missing_from_skillmd=$((ic1_missing_from_skillmd + 1))
            fi
        done
    fi
done

# Check cross-references between content files (skip references inside fenced code blocks)
for dir in references workflows; do
    if [[ -d "$skill_dir/$dir" ]]; then
        for file in "$skill_dir/$dir"/*.md; do
            [[ -f "$file" ]] || continue
            source_rel="$dir/$(basename "$file")"
            # Strip fenced code blocks before scanning for references
            stripped=$(awk '/^```/{skip=!skip; next} !skip{print}' "$file")
            refs=$(echo "$stripped" | grep -oE '`(references|workflows|scripts)/[a-zA-Z0-9._-]+`' 2>/dev/null \
                | tr -d '`' | sort -u || true)
            while IFS= read -r ref; do
                [[ -z "$ref" ]] && continue
                if [[ ! -f "$skill_dir/$ref" ]]; then
                    fail "IC1" "Broken cross-reference in $source_rel: $ref does not exist"
                    ic1_broken_refs=$((ic1_broken_refs + 1))
                fi
            done <<< "$refs"
        done
    fi
done

if [[ $ic1_checked -gt 0 && $ic1_missing_from_skillmd -eq 0 && $ic1_broken_refs -eq 0 ]]; then
    pass "IC1" "All $ic1_checked files referenced from SKILL.md, no broken cross-references"
elif [[ $ic1_broken_refs -eq 0 && $ic1_missing_from_skillmd -gt 0 ]]; then
    # Warnings were already emitted above; no separate pass needed
    true
fi

# --- IC2: Word count compliance ---
# Check SKILL.md body word count against targets from writing-style.md

# Extract body (everything after second ---)
body=$(awk 'BEGIN{c=0} /^---$/{c++; next} c>=2{print}' "$skill_md")
word_count=$(echo "$body" | wc -w | tr -d ' ')

# Check if description or name suggests frequently-loaded
frontmatter=$(awk 'NR==1{next} /^---$/{exit} {print}' "$skill_md")
desc=$(echo "$frontmatter" | grep -E '^description:' | sed 's/^description:[[:space:]]*//' || true)

# Determine target: frequently-loaded skills get 200, standard gets 500
# Heuristic: if description contains "MUST be loaded" or "always load", it's frequently-loaded
target=500
freq_label="standard"
if echo "$desc" | grep -qiE 'must be loaded|always load|every conversation'; then
    target=200
    freq_label="frequently-loaded"
fi

if [[ $word_count -le $target ]]; then
    pass "IC2" "SKILL.md body: $word_count words (target: <$target for $freq_label)"
else
    warn "IC2" "SKILL.md body: $word_count words exceeds <$target target for $freq_label skill"
fi

# --- IC3: Terminology consistency ---
# Extract multi-word terms from headings across all files, look for near-duplicates

declare -A terms_seen
ic3_issues=0

# Collect all headings from all markdown files
all_headings=""
for file in "$skill_md" "$skill_dir"/references/*.md "$skill_dir"/workflows/*.md; do
    [[ -f "$file" ]] || continue
    file_headings=$(grep -E '^#{1,4} ' "$file" 2>/dev/null | sed 's/^#* //' || true)
    all_headings="$all_headings"$'\n'"$file_headings"
done

# Normalize and look for near-duplicate terms (differing only in hyphens/spaces/case)
while IFS= read -r heading; do
    [[ -z "$heading" ]] && continue
    # Normalize: lowercase, replace hyphens with spaces, collapse whitespace
    normalized=$(echo "$heading" | tr '[:upper:]' '[:lower:]' | tr '-' ' ' | tr -s ' ')
    if [[ -n "${terms_seen[$normalized]+x}" ]]; then
        existing="${terms_seen[$normalized]}"
        if [[ "$existing" != "$heading" ]]; then
            warn "IC3" "Possible inconsistent terms: '$existing' vs '$heading'"
            ic3_issues=$((ic3_issues + 1))
        fi
    else
        terms_seen[$normalized]="$heading"
    fi
done <<< "$all_headings"

if [[ $ic3_issues -eq 0 ]]; then
    pass "IC3" "No inconsistent heading terms detected"
fi

# --- IC4: Description pitfall check ---
# Scan frontmatter description for workflow-summary words that suggest CSO anti-pattern #12
# (Workflow Summary Description)

desc_clean=$(echo "$desc" | tr -d '"' | tr -d "'")
ic4_issues=0

# Look for verb+noun patterns suggesting process description rather than trigger keywords
workflow_patterns=(
    'dispatches'
    'orchestrates'
    'coordinates'
    'manages the process'
    'handles the workflow'
    'runs through'
    'walks through'
    'steps through'
    'phase [0-9]'
    'step [0-9]'
)

for pattern in "${workflow_patterns[@]}"; do
    if echo "$desc_clean" | grep -qiE "$pattern"; then
        warn "IC4" "Description contains workflow-summary language: '$pattern' (CSO anti-pattern #12)"
        ic4_issues=$((ic4_issues + 1))
    fi
done

if [[ $ic4_issues -eq 0 ]]; then
    pass "IC4" "Description does not contain workflow-summary language"
fi

# --- Summary ---

echo ""
echo "  Score: $PASS passed, $FAIL failed, $WARN warnings"

[[ $FAIL -gt 0 ]] && exit 1
exit 0
