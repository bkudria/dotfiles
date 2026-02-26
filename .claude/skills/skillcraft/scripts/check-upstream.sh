#!/usr/bin/env bash
# check-upstream.sh — Check upstream sources for changes and optionally update metadata
#
# Usage:
#   check-upstream.sh <skill-directory>
#   check-upstream.sh <skill-directory> --update-metadata
#
# Reads provenance.yml and checks each upstream source for changes since last check.
# GitHub sources use gh api compare; web sources are flagged for manual review.
#
# With --update-metadata, also updates last_checked dates, last_checked_sha values,
# and last_full_update in provenance.yml.

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

CHANGED_COUNT=0
UNCHANGED_COUNT=0
MANUAL_COUNT=0
ERROR_COUNT=0
HAS_ERRORS=false
UPDATE_METADATA=false

# Associative arrays for tracking new SHAs
declare -A NEW_SHAS

usage() {
    cat <<'USAGE'
Usage: check-upstream.sh <skill-directory>
       check-upstream.sh <skill-directory> --update-metadata

Arguments:
  <skill-directory>     Path to the skill directory containing provenance.yml
  --update-metadata     Also update dates and SHAs in provenance.yml
USAGE
    exit 1
}

# --- Argument parsing ---

if [[ $# -lt 1 ]]; then
    usage
fi

SKILL_DIR="${1/#\~/$HOME}"

if [[ "${2:-}" == "--update-metadata" ]]; then
    UPDATE_METADATA=true
fi

if [[ ! -d "$SKILL_DIR" ]]; then
    echo "Error: Directory not found: $SKILL_DIR"
    exit 1
fi

PROV="$SKILL_DIR/provenance.yml"
if [[ ! -f "$PROV" ]]; then
    echo "Error: No provenance.yml found in $SKILL_DIR"
    exit 1
fi

# --- Prerequisites ---

if ! command -v yq &>/dev/null; then
    echo "Error: yq not installed (brew install yq)"
    exit 1
fi

if ! command -v gh &>/dev/null; then
    echo "Error: gh not installed (brew install gh)"
    exit 1
fi

if ! gh auth status &>/dev/null 2>&1; then
    echo "Error: gh not authenticated (run: gh auth login)"
    exit 1
fi

# --- Header ---

SKILL_NAME=$(basename "$SKILL_DIR")
LAST_UPDATE=$(yq '.last_full_update' "$PROV" | tr -d '"')

echo ""
echo -e "${BOLD}Checking upstream sources for: $SKILL_NAME${NC}"
echo "  provenance.yml: $PROV"
echo "  Last full update: $LAST_UPDATE"

# Staleness warning
LAST_EPOCH=$(date -j -f "%Y-%m-%d" "$LAST_UPDATE" +%s 2>/dev/null || date -d "$LAST_UPDATE" +%s 2>/dev/null || echo 0)
NOW_EPOCH=$(date +%s)
if [[ "$LAST_EPOCH" -gt 0 ]]; then
    DAYS_AGO=$(( (NOW_EPOCH - LAST_EPOCH) / 86400 ))
    if [[ $DAYS_AGO -gt 30 ]]; then
        echo -e "  ${YELLOW}Warning: last check was $DAYS_AGO days ago${NC}"
    fi
fi

echo ""

# --- Check each source ---

SOURCE_NAMES=$(yq '.sources | keys | .[]' "$PROV")

while IFS= read -r source_name; do
    [[ -z "$source_name" ]] && continue

    source_type=$(yq ".sources[\"$source_name\"].type" "$PROV" | tr -d '"')
    last_checked=$(yq ".sources[\"$source_name\"].last_checked" "$PROV" | tr -d '"')

    if [[ "$source_type" == "github" ]]; then
        owner=$(yq ".sources[\"$source_name\"].owner" "$PROV" | tr -d '"')
        repo=$(yq ".sources[\"$source_name\"].repo" "$PROV" | tr -d '"')
        path=$(yq ".sources[\"$source_name\"].path" "$PROV" | tr -d '"')
        stored_sha=$(yq ".sources[\"$source_name\"].last_checked_sha" "$PROV" | tr -d '"')

        # Compare stored SHA against HEAD
        compare_result=$(gh api "repos/$owner/$repo/compare/${stored_sha}...HEAD" \
            --jq "[.files[] | select(.filename | startswith(\"$path/\"))] | length" 2>&1) || {
            echo -e "  ${RED}ERROR${NC}    $source_name — gh api failed: $compare_result"
            ERROR_COUNT=$((ERROR_COUNT + 1))
            HAS_ERRORS=true
            continue
        }

        if [[ "$compare_result" == "0" ]]; then
            echo -e "  ${GREEN}UNCHANGED${NC}  $source_name (last checked: $last_checked)"
            UNCHANGED_COUNT=$((UNCHANGED_COUNT + 1))
        else
            # Get changed files
            changed_files=$(gh api "repos/$owner/$repo/compare/${stored_sha}...HEAD" \
                --jq ".files[] | select(.filename | startswith(\"$path/\")) | \"      \(.status | .[0:1] | ascii_upcase) \(.filename)\"" 2>/dev/null || true)

            # Get new HEAD SHA for this path
            new_sha=$(gh api "repos/$owner/$repo/commits?path=$path&per_page=1" \
                --jq '.[0].sha' 2>/dev/null || true)

            echo -e "  ${YELLOW}CHANGED${NC}    $source_name — $compare_result file(s) changed (last checked: $last_checked)"
            if [[ -n "$new_sha" ]]; then
                echo -e "    ${CYAN}→${NC} new HEAD: ${new_sha:0:12}"
                NEW_SHAS[$source_name]="$new_sha"
            fi
            if [[ -n "$changed_files" ]]; then
                echo -e "    ${CYAN}→${NC} Changed files:"
                echo "$changed_files"
            fi
            CHANGED_COUNT=$((CHANGED_COUNT + 1))
        fi

    elif [[ "$source_type" == "web" ]]; then
        url=$(yq ".sources[\"$source_name\"].url" "$PROV" | tr -d '"')
        echo -e "  ${CYAN}MANUAL${NC}     $source_name — web source, check manually (last checked: $last_checked)"
        echo -e "    ${CYAN}→${NC} URL: $url"
        MANUAL_COUNT=$((MANUAL_COUNT + 1))
    else
        echo -e "  ${RED}ERROR${NC}    $source_name — unknown source type: $source_type"
        ERROR_COUNT=$((ERROR_COUNT + 1))
        HAS_ERRORS=true
    fi
done <<< "$SOURCE_NAMES"

# --- Summary ---

echo ""
echo -e "${BOLD}Summary:${NC} $CHANGED_COUNT changed, $UNCHANGED_COUNT unchanged, $MANUAL_COUNT manual"
if [[ $ERROR_COUNT -gt 0 ]]; then
    echo -e "  ${RED}$ERROR_COUNT error(s)${NC}"
fi

# --- Update metadata ---

if $UPDATE_METADATA; then
    echo ""
    TODAY=$(date -u +%Y-%m-%d)
    echo -e "${BOLD}Updating provenance metadata (UTC date: $TODAY)${NC}"

    # Update last_full_update
    yq -i ".last_full_update = \"$TODAY\"" "$PROV"
    echo "  Updated last_full_update → $TODAY"

    # Update last_checked for all sources
    while IFS= read -r source_name; do
        [[ -z "$source_name" ]] && continue
        yq -i ".sources[\"$source_name\"].last_checked = \"$TODAY\"" "$PROV"
        echo "  Updated $source_name.last_checked → $TODAY"

        # Update SHA if we captured a new one
        if [[ -n "${NEW_SHAS[$source_name]:-}" ]]; then
            yq -i ".sources[\"$source_name\"].last_checked_sha = \"${NEW_SHAS[$source_name]}\"" "$PROV"
            echo "  Updated $source_name.last_checked_sha → ${NEW_SHAS[$source_name]:0:12}..."
        fi
    done <<< "$SOURCE_NAMES"

    echo ""
    echo -e "${GREEN}Metadata updated.${NC} Run this script again without --update-metadata to verify."
fi

# --- Exit ---

if $HAS_ERRORS; then
    exit 1
fi
exit 0
