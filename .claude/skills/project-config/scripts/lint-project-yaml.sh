#!/usr/bin/env bash
# Usage: lint-project-yaml.sh <project-yaml-path> [--fix] [--json]
#   Reports redundant fields in project.yaml when profiles are used.
#   A field is redundant if it has the same value as the profile default.
#   Checks both metadata (defaults:) and standards from profiles.
#   --fix: Rewrite project.yaml removing redundant fields (in-place).
#   --json: Machine-readable JSON output (mutually exclusive with --fix).
#   Exit code: 0 if clean (or after fix), 1 if redundancies found.
set -euo pipefail

# --- Colors ---
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
RESET='\033[0m'

# --- Args ---
FIX=false
JSON_OUTPUT=false
PROJECT_YAML=""

for arg in "$@"; do
  case "$arg" in
    --fix) FIX=true ;;
    --json) JSON_OUTPUT=true ;;
    -*) echo "Unknown flag: $arg" >&2; exit 1 ;;
    *) PROJECT_YAML="$arg" ;;
  esac
done

if $FIX && $JSON_OUTPUT; then
  echo "Error: --fix and --json are mutually exclusive" >&2
  exit 1
fi

if [[ -z "$PROJECT_YAML" ]]; then
  echo "Usage: lint-project-yaml.sh <project-yaml-path|project-root> [--fix] [--json]" >&2
  exit 1
fi

# Accept project root directory — normalize to project.yaml path
if [[ -d "$PROJECT_YAML" ]]; then
  PROJECT_YAML="${PROJECT_YAML%/}/project.yaml"
fi

if [[ ! -f "$PROJECT_YAML" ]]; then
  echo "Error: File not found: $PROJECT_YAML" >&2
  exit 1
fi

if ! command -v yq &>/dev/null; then
  echo "Error: yq is required (brew install yq)" >&2
  exit 1
fi

# --- Resolve profiles ---
SKILL_DIR="${CLAUDE_SKILL_DIR:-${HOME}/.claude/skills/project-config}"

PROFILE_NAMES=$(yq -r '(.profiles // []) | .[]' "$PROJECT_YAML" 2>/dev/null || true)

if [[ -z "$PROFILE_NAMES" ]]; then
  if $JSON_OUTPUT; then
    echo '{"results": [], "redundant_count": 0}'
  else
    echo "No profiles declared — nothing to lint."
  fi
  exit 0
fi

# Merge all profiles (later overrides earlier)
MERGED_PROFILE=$(mktemp)
trap "rm -f $MERGED_PROFILE" EXIT

echo "{}" > "$MERGED_PROFILE"
for PROFILE_NAME in $PROFILE_NAMES; do
  PROFILE_PATH="${SKILL_DIR}/references/profiles/${PROFILE_NAME}.yaml"
  if [[ ! -f "$PROFILE_PATH" ]]; then
    echo "Error: Profile not found: $PROFILE_PATH" >&2
    exit 1
  fi
  # Merge this profile into the accumulated result
  yq eval-all 'select(fileIndex == 0) * select(fileIndex == 1)' "$MERGED_PROFILE" "$PROFILE_PATH" > "${MERGED_PROFILE}.tmp"
  mv "${MERGED_PROFILE}.tmp" "$MERGED_PROFILE"
done

# --- Results collection ---
LINT_RESULTS=()
REDUNDANT_COUNT=0
REDUNDANT_PATHS=()

lint_emit() {
  local status="$1" field="$2" detail="$3"
  LINT_RESULTS+=("${status}	${field}	${detail}")
  if [[ "$status" == "REDUNDANT" ]]; then
    REDUNDANT_COUNT=$((REDUNDANT_COUNT + 1))
  fi
}

if ! $JSON_OUTPUT; then
  DISPLAY_PROFILES=$(echo "$PROFILE_NAMES" | tr '\n' ', ' | sed 's/, $//')
  echo -e "Linting: ${CYAN}$(basename "$PROJECT_YAML")${RESET} (profiles: ${CYAN}${DISPLAY_PROFILES}${RESET})"
  echo ""
fi

# --- Check metadata against profile defaults ---
DEFAULTS_KEYS=$(yq -r '.defaults | keys | .[]' "$MERGED_PROFILE" 2>/dev/null || true)

for KEY in $DEFAULTS_KEYS; do
  PROJ_VAL=$(yq -o=json ".\"$KEY\"" "$PROJECT_YAML" 2>/dev/null || echo "null")
  PROF_VAL=$(yq -o=json ".defaults.\"$KEY\"" "$MERGED_PROFILE")

  if [[ "$PROJ_VAL" == "null" ]]; then
    continue  # Not set in project — fine
  elif [[ "$PROJ_VAL" == "$PROF_VAL" ]]; then
    DISPLAY_VAL=$(yq -r ".\"$KEY\"" "$PROJECT_YAML")
    lint_emit "REDUNDANT" "$KEY" "${DISPLAY_VAL} (same as profile default)"
    REDUNDANT_PATHS+=("\"$KEY\"")
  else
    lint_emit "OK" "$KEY" "override (differs from profile default)"
  fi
done

# --- Check standards against profile ---
STANDARDS=$(yq -r '.standards | keys | .[]' "$PROJECT_YAML" 2>/dev/null || true)

if [[ -z "$STANDARDS" && -z "$DEFAULTS_KEYS" ]]; then
  if $JSON_OUTPUT; then
    echo '{"results": [], "redundant_count": 0}'
  else
    echo "  No standards or metadata to lint."
  fi
  exit 0
fi

for STD in $STANDARDS; do
  # Check if profile defines this standard
  PROFILE_HAS_STD=$(yq -r ".standards.\"$STD\" // \"null\"" "$MERGED_PROFILE")

  if [[ "$PROFILE_HAS_STD" == "null" ]]; then
    lint_emit "OK" "$STD" "addition (not in profile)"
    continue
  fi

  # Get fields in this standard from project.yaml
  FIELDS=$(yq -r ".standards.\"$STD\" | keys | .[]" "$PROJECT_YAML" 2>/dev/null || true)

  if [[ -z "$FIELDS" ]]; then
    continue
  fi

  STD_HAS_OVERRIDE=false
  STD_REDUNDANT_FIELDS=()

  for FIELD in $FIELDS; do
    # Get values as JSON for reliable comparison
    PROJ_VAL=$(yq -o=json ".standards.\"$STD\".\"$FIELD\"" "$PROJECT_YAML")
    PROF_VAL=$(yq -o=json ".standards.\"$STD\".\"$FIELD\"" "$MERGED_PROFILE" 2>/dev/null || echo "null")

    if [[ "$PROF_VAL" == "null" ]]; then
      # Field not in profile — it's an addition/override
      lint_emit "OK" "${STD}.${FIELD}" "override (not in profile)"
      STD_HAS_OVERRIDE=true
    elif [[ "$PROJ_VAL" == "$PROF_VAL" ]]; then
      # Same value — redundant
      DISPLAY_VAL=$(yq -r ".standards.\"$STD\".\"$FIELD\"" "$PROJECT_YAML")
      lint_emit "REDUNDANT" "${STD}.${FIELD}" "${DISPLAY_VAL} (same as profile)"
      STD_REDUNDANT_FIELDS+=("$FIELD")
      REDUNDANT_PATHS+=("standards.\"$STD\".\"$FIELD\"")
    else
      # Different value — legitimate override
      lint_emit "OK" "${STD}.${FIELD}" "override (differs from profile)"
      STD_HAS_OVERRIDE=true
    fi
  done

  # Flag fully redundant standard entries
  if [[ "$STD_HAS_OVERRIDE" == "false" && ${#STD_REDUNDANT_FIELDS[@]} -gt 0 ]]; then
    lint_emit "REDUNDANT" "$STD" "fully redundant — entire entry can be removed"
  fi
done

# --- Output ---

if $JSON_OUTPUT; then
  echo "{"
  echo "  \"results\": ["
  first=true
  for line in "${LINT_RESULTS[@]}"; do
    IFS=$'\t' read -r status field detail <<< "$line"
    $first || echo ","
    first=false
    printf '    {"status": "%s", "field": "%s", "detail": "%s"}' "$status" "$field" "$detail"
  done
  echo ""
  echo "  ],"
  echo "  \"redundant_count\": $REDUNDANT_COUNT"
  echo "}"

  if [[ $REDUNDANT_COUNT -gt 0 ]]; then
    exit 1
  fi
  exit 0
fi

# --- Colored text output ---
for line in "${LINT_RESULTS[@]}"; do
  IFS=$'\t' read -r status field detail <<< "$line"
  case "$status" in
    REDUNDANT) echo -e "  ${YELLOW}REDUNDANT${RESET}  ${field}: ${detail}" ;;
    OK) echo -e "  ${GREEN}OK${RESET}  ${field}: ${detail}" ;;
  esac
done

echo ""

if [[ $REDUNDANT_COUNT -eq 0 ]]; then
  echo -e "  ${GREEN}Clean${RESET}: no redundant fields."
  exit 0
fi

echo -e "  ${YELLOW}${REDUNDANT_COUNT} redundant field(s) found.${RESET}"

# --- Fix mode ---
if [[ "$FIX" == "true" ]]; then
  echo ""
  echo "Fixing..."

  for RPATH in "${REDUNDANT_PATHS[@]}"; do
    yq -i "del(.${RPATH})" "$PROJECT_YAML"
  done

  # Remove empty standard entries
  STANDARDS_AFTER=$(yq -r '.standards | keys | .[]' "$PROJECT_YAML" 2>/dev/null || true)
  for STD in $STANDARDS_AFTER; do
    FIELD_COUNT=$(yq -r ".standards.\"$STD\" | length" "$PROJECT_YAML" 2>/dev/null || echo "0")
    if [[ "$FIELD_COUNT" == "0" ]]; then
      echo "  Removing empty entry: ${STD}"
      yq -i "del(.standards.\"$STD\")" "$PROJECT_YAML"
    fi
  done

  # Remove empty standards block
  STD_COUNT=$(yq -r '.standards | length' "$PROJECT_YAML" 2>/dev/null || echo "0")
  if [[ "$STD_COUNT" == "0" ]]; then
    echo "  Removing empty standards block"
    yq -i 'del(.standards)' "$PROJECT_YAML"
  fi

  echo -e "  ${GREEN}Fixed${RESET}: removed ${REDUNDANT_COUNT} redundant field(s)."
  exit 0
else
  echo "  Run with --fix to remove them."
  exit 1
fi
