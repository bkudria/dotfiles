#!/usr/bin/env bash
set -euo pipefail

# check-standards.sh — Deterministic file-existence and config checks for project standards
#
# Usage: check-standards.sh <project-root> [--json]
#
# Reads project.yaml, resolves profile-inherited standards, and checks each one.
# Exit code: 0 if the script ran successfully (FAIL results are in the output),
#            1 if the script itself errored (missing deps, no project.yaml).

SKILL_DIR="${CLAUDE_SKILL_DIR:-${HOME}/.claude/skills/project-config}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
RESET='\033[0m'

# Parse args
JSON_OUTPUT=false
PROJECT_ROOT=""

for arg in "$@"; do
  case "$arg" in
    --json) JSON_OUTPUT=true ;;
    -*) echo "Unknown flag: $arg" >&2; echo "Usage: check-standards.sh <project-root> [--json]" >&2; exit 1 ;;
    *) PROJECT_ROOT="$arg" ;;
  esac
done

if [[ -z "$PROJECT_ROOT" ]]; then
  echo "Usage: check-standards.sh <project-root> [--json]" >&2
  exit 1
fi

PROJECT_ROOT="${PROJECT_ROOT%/}"

if [[ ! -f "$PROJECT_ROOT/project.yaml" ]]; then
  echo "Error: $PROJECT_ROOT/project.yaml not found" >&2
  exit 1
fi

# Check dependencies
for cmd in yq jq; do
  if ! command -v "$cmd" &>/dev/null; then
    echo "Error: $cmd is required (brew install $cmd)" >&2
    exit 1
  fi
done

HAS_MQ=false
if command -v mq &>/dev/null; then
  HAS_MQ=true
fi

# Collect results: each line is STATUS\tSTANDARD\tDETAIL
RESULTS=()

emit() {
  local status="$1" standard="$2" detail="$3"
  RESULTS+=("${status}	${standard}	${detail}")
}

# --- Resolve merged standards from profiles + project overrides ---

MERGED_YAML=$(mktemp)
trap "rm -f '$MERGED_YAML'" EXIT

# Get profiles list
PROFILES=$(yq -r '.profiles // [] | .[]' "$PROJECT_ROOT/project.yaml" 2>/dev/null || true)

# Start with empty standards, layer profiles, then project overrides
if [[ -n "$PROFILES" ]]; then
  echo "{}" > "$MERGED_YAML"
  for profile in $PROFILES; do
    pfile="$SKILL_DIR/references/profiles/${profile}.yaml"
    if [[ -f "$pfile" ]]; then
      yq eval-all 'select(fileIndex == 0) * select(fileIndex == 1).standards' \
        "$MERGED_YAML" "$pfile" > "${MERGED_YAML}.tmp" 2>/dev/null
      mv "${MERGED_YAML}.tmp" "$MERGED_YAML"
    fi
  done
  # Layer project.yaml standards on top
  yq eval-all 'select(fileIndex == 0) * (select(fileIndex == 1).standards // {})' \
    "$MERGED_YAML" "$PROJECT_ROOT/project.yaml" > "${MERGED_YAML}.tmp" 2>/dev/null
  mv "${MERGED_YAML}.tmp" "$MERGED_YAML"
else
  yq '.standards // {}' "$PROJECT_ROOT/project.yaml" > "$MERGED_YAML"
fi

# Read project metadata
LANGUAGE=$(yq -r '.language // ""' "$PROJECT_ROOT/project.yaml" 2>/dev/null)
VISIBILITY=$(yq -r '.visibility // ""' "$PROJECT_ROOT/project.yaml" 2>/dev/null)

# If visibility not set in project, check profile defaults
if [[ -z "$VISIBILITY" ]] && [[ -n "$PROFILES" ]]; then
  for profile in $PROFILES; do
    pfile="$SKILL_DIR/references/profiles/${profile}.yaml"
    if [[ -f "$pfile" ]]; then
      VISIBILITY=$(yq -r '.defaults.visibility // ""' "$pfile" 2>/dev/null)
    fi
  done
fi
VISIBILITY="${VISIBILITY:-private}"

# --- Helper: check if standard is active ---
is_active() {
  local std="$1"
  yq -e ".[\"$std\"]" "$MERGED_YAML" &>/dev/null
}

# --- Helper: get standard field ---
std_field() {
  local std="$1" field="$2" default="${3:-}"
  local val
  val=$(yq -r ".[\"${std}\"].${field} // \"\"" "$MERGED_YAML" 2>/dev/null)
  echo "${val:-$default}"
}

# --- Helper: get severity ---
severity() {
  local std="$1"
  local req rec
  req=$(yq -r ".[\"${std}\"].required // \"\"" "$MERGED_YAML" 2>/dev/null)
  rec=$(yq -r ".[\"${std}\"].recommended // \"\"" "$MERGED_YAML" 2>/dev/null)
  if [[ "$rec" == "true" ]]; then
    echo "recommended"
  else
    echo "required"  # default
  fi
}

fail_or_warn() {
  local std="$1"
  if [[ "$(severity "$std")" == "recommended" ]]; then
    echo "WARN"
  else
    echo "FAIL"
  fi
}

# --- Helper: first existing file from list ---
first_match() {
  local root="$1"
  shift
  for f in "$@"; do
    if [[ -e "$root/$f" ]]; then
      echo "$f"
      return 0
    fi
  done
  return 1
}

# --- Helper: first existing directory from list ---
first_dir() {
  local root="$1"
  shift
  for d in "$@"; do
    if [[ -d "$root/$d" ]]; then
      echo "$d"
      return 0
    fi
  done
  return 1
}

# --- Standard checks ---

check_readme() {
  local found
  if found=$(first_match "$PROJECT_ROOT" README.md README README.txt README.rdoc README.org); then
    # Non-empty check
    if [[ ! -s "$PROJECT_ROOT/$found" ]]; then
      emit "$(fail_or_warn readme)" "readme" "$found exists but is empty"
      return
    fi

    local detail="$found exists"

    # Heading check
    if ! grep -q '^#' "$PROJECT_ROOT/$found" 2>/dev/null; then
      emit "WARN" "readme" "$found exists but has no headings"
      return
    fi

    # Reference check
    local refs
    refs=$(yq -r '.["readme"].references // [] | .[]' "$MERGED_YAML" 2>/dev/null || true)
    if [[ -n "$refs" ]]; then
      local missing_refs=""
      for ref in $refs; do
        local ref_file=""
        case "$ref" in
          goals) ref_file="GOALS.md" ;;
          spec)  ref_file="SPEC.md" ;;
          docs)  ref_file="docs/" ;;
          *)     ref_file="$ref" ;;
        esac
        if ! grep -qi "$ref_file" "$PROJECT_ROOT/$found" 2>/dev/null; then
          missing_refs="${missing_refs:+$missing_refs, }$ref_file"
        fi
      done
      if [[ -n "$missing_refs" ]]; then
        emit "$(fail_or_warn readme)" "readme" "$found exists but missing references: $missing_refs"
        return
      fi
      detail="$detail, references present"
    fi

    # Heading section check (requires mq)
    local req_sections
    req_sections=$(yq -r '.["readme"].sections // [] | .[]' "$MERGED_YAML" 2>/dev/null || true)
    if [[ -n "$req_sections" ]] && $HAS_MQ; then
      local headings
      headings=$(mq -F text '.h2' "$PROJECT_ROOT/$found" 2>/dev/null | tr '[:upper:]' '[:lower:]')
      local missing_sects=""
      for sect in $req_sections; do
        local sect_lower
        sect_lower=$(echo "$sect" | tr '[:upper:]' '[:lower:]')
        if ! echo "$headings" | grep -qi "$sect_lower"; then
          missing_sects="${missing_sects:+$missing_sects, }$sect"
        fi
      done
      if [[ -n "$missing_sects" ]]; then
        emit "WARN" "readme" "$found exists but missing sections: $missing_sects"
        return
      fi
      detail="$detail, required sections present"
    fi

    emit "PASS" "readme" "$detail"
  else
    emit "$(fail_or_warn readme)" "readme" "No README file found"
  fi
}

check_gitignore() {
  if [[ -e "$PROJECT_ROOT/.gitignore" ]]; then
    emit "PASS" "gitignore" ".gitignore exists"
  else
    emit "$(fail_or_warn gitignore)" "gitignore" "No .gitignore found"
  fi
}

check_license() {
  local found
  if found=$(first_match "$PROJECT_ROOT" LICENSE LICENSE.md LICENSE.txt COPYING COPYING.md); then
    local spdx
    spdx=$(std_field license spdx "")

    if [[ -z "$spdx" ]]; then
      emit "PASS" "license" "$found exists"
      return
    fi

    local matched=false
    case "$spdx" in
      MIT)
        grep -qi "Permission is hereby granted" "$PROJECT_ROOT/$found" 2>/dev/null && matched=true
        ;;
      Apache-2.0)
        grep -qi "Apache License" "$PROJECT_ROOT/$found" 2>/dev/null && \
          grep -qi "Version 2.0" "$PROJECT_ROOT/$found" 2>/dev/null && matched=true
        ;;
      GPL-3.0-only)
        grep -qi "GNU GENERAL PUBLIC LICENSE" "$PROJECT_ROOT/$found" 2>/dev/null && \
          grep -qi "Version 3" "$PROJECT_ROOT/$found" 2>/dev/null && matched=true
        ;;
      *)
        emit "PASS" "license" "$found exists (SPDX $spdx — content not validated)"
        return
        ;;
    esac

    if $matched; then
      emit "PASS" "license" "$found exists, $spdx verified"
    else
      emit "WARN" "license" "$found exists but content doesn't match $spdx"
    fi
  else
    emit "$(fail_or_warn license)" "license" "No LICENSE file found"
  fi
}

check_tests() {
  local dir
  dir=$(std_field tests directory "")

  if [[ -z "$dir" ]]; then
    emit "SKIP" "tests" "No directory declared — verify manually"
    return
  fi

  if [[ ! -d "$PROJECT_ROOT/$dir" ]]; then
    emit "$(fail_or_warn tests)" "tests" "Configured directory $dir/ not found"
    return
  fi

  local count
  count=$(find "$PROJECT_ROOT/$dir" -maxdepth 2 -type f 2>/dev/null | wc -l | tr -d ' ')
  local detail="$dir/ exists ($count files)"

  local config
  config=$(std_field tests config "")
  if [[ -n "$config" ]]; then
    if [[ -e "$PROJECT_ROOT/$config" ]]; then
      detail="$detail, $config found"
    else
      emit "WARN" "tests" "$detail but declared config $config not found"
      return
    fi
  fi

  local framework
  framework=$(std_field tests framework "")
  if [[ -n "$framework" ]]; then
    detail="$detail, framework: $framework"
  fi

  emit "PASS" "tests" "$detail"
}

check_claude_md() {
  local found
  if found=$(first_match "$PROJECT_ROOT" CLAUDE.md AGENTS.md); then
    local detail="$found exists"

    # Section check (requires mq)
    local req_sections
    req_sections=$(yq -r '.["claude-md"].sections // [] | .[]' "$MERGED_YAML" 2>/dev/null || true)
    if [[ -n "$req_sections" ]] && $HAS_MQ; then
      local target="$PROJECT_ROOT/$found"
      # Follow symlink
      if [[ -L "$target" ]]; then
        target=$(readlink -f "$target")
      fi
      local headings
      headings=$(mq -F text '.h2' "$target" 2>/dev/null | tr '[:upper:]' '[:lower:]')
      local missing=""
      for sect in $req_sections; do
        local sect_lower
        sect_lower=$(echo "$sect" | tr '[:upper:]' '[:lower:]')
        if ! echo "$headings" | grep -qi "$sect_lower"; then
          missing="${missing:+$missing, }$sect"
        fi
      done
      if [[ -n "$missing" ]]; then
        emit "WARN" "claude-md" "$found exists but missing sections: $missing"
        return
      fi
      detail="$detail, required sections present"
    fi

    emit "PASS" "claude-md" "$detail"
  else
    emit "$(fail_or_warn claude-md)" "claude-md" "No CLAUDE.md or AGENTS.md found"
  fi
}

check_goals() {
  local found
  if found=$(first_match "$PROJECT_ROOT" GOALS.md goals.md goals.yaml); then
    emit "PASS" "goals" "$found exists"
  else
    emit "$(fail_or_warn goals)" "goals" "No GOALS.md found"
  fi
}

check_spec() {
  local candidates=("SPEC.md" "specification.md" "design.md")
  # Only check spec.md if no spec/ test directory exists
  if [[ ! -d "$PROJECT_ROOT/spec" ]]; then
    candidates+=("spec.md")
  fi

  local found
  for f in "${candidates[@]}"; do
    if [[ -e "$PROJECT_ROOT/$f" ]]; then
      emit "PASS" "spec" "$f exists"
      return
    fi
  done

  emit "$(fail_or_warn spec)" "spec" "No SPEC.md found"
}

check_linter() {
  local config
  config=$(std_field linter config "")

  if [[ -n "$config" ]]; then
    if [[ -e "$PROJECT_ROOT/$config" ]]; then
      META_LINTER_CFG="$config"
      local tool
      tool=$(std_field linter tool "")
      if [[ -n "$tool" ]]; then
        emit "PASS" "linter" "$tool config: $config"
      else
        emit "PASS" "linter" "Config: $config"
      fi
    else
      emit "$(fail_or_warn linter)" "linter" "Declared config $config not found"
    fi
  else
    emit "SKIP" "linter" "No config declared — verify manually"
  fi
}

check_coverage() {
  local config
  config=$(std_field coverage config "")

  if [[ -z "$config" ]]; then
    emit "SKIP" "coverage" "No config declared — verify manually"
    return
  fi

  if [[ ! -e "$PROJECT_ROOT/$config" ]]; then
    emit "$(fail_or_warn coverage)" "coverage" "Declared config $config not found"
    return
  fi

  # Config exists — check for ratchet
  local ratchet_pattern
  ratchet_pattern=$(std_field coverage ratchet_pattern "")

  if [[ -n "$ratchet_pattern" ]]; then
    if grep -qi "$ratchet_pattern" "$PROJECT_ROOT/$config" 2>/dev/null; then
      emit "PASS" "coverage" "Coverage configured in $config with ratchet"
    else
      emit "$(fail_or_warn coverage)" "coverage" "Coverage configured in $config but no ratchet/threshold set"
    fi
  else
    emit "SKIP" "coverage" "Coverage config $config exists — verify ratchet manually"
  fi
}

check_ci() {
  if [[ -d "$PROJECT_ROOT/.github/workflows" ]]; then
    local count
    count=$(find "$PROJECT_ROOT/.github/workflows" -maxdepth 1 -name '*.yml' -o -name '*.yaml' 2>/dev/null | wc -l | tr -d ' ')
    if [[ "$count" -gt 0 ]]; then
      emit "PASS" "ci" ".github/workflows/ has $count workflow files"
      return
    fi
  fi

  local found
  if found=$(first_match "$PROJECT_ROOT" .gitlab-ci.yml .travis.yml Jenkinsfile); then
    emit "PASS" "ci" "$found exists"
    return
  fi

  if found=$(first_dir "$PROJECT_ROOT" .circleci .buildkite); then
    emit "PASS" "ci" "$found/ exists"
    return
  fi

  emit "$(fail_or_warn ci)" "ci" "No CI configuration found"
}

check_changelog() {
  local found
  if found=$(first_match "$PROJECT_ROOT" CHANGELOG.md CHANGELOG HISTORY.md CHANGES.md); then
    if grep -q '## \[' "$PROJECT_ROOT/$found" 2>/dev/null; then
      emit "PASS" "changelog" "$found exists, Keep-a-Changelog format"
    else
      emit "WARN" "changelog" "$found exists but no version sections found"
    fi
  else
    emit "$(fail_or_warn changelog)" "changelog" "No CHANGELOG file found"
  fi
}

check_contributing() {
  if [[ "$VISIBILITY" != "public" ]]; then
    emit "SKIP" "contributing" "Not required (visibility: $VISIBILITY)"
    return
  fi

  local found
  if found=$(first_match "$PROJECT_ROOT" CONTRIBUTING.md CONTRIBUTING); then
    emit "PASS" "contributing" "$found exists"
  else
    emit "$(fail_or_warn contributing)" "contributing" "No CONTRIBUTING.md found (required for public projects)"
  fi
}

check_editorconfig() {
  if [[ -e "$PROJECT_ROOT/.editorconfig" ]]; then
    emit "PASS" "editorconfig" ".editorconfig exists"
  else
    emit "$(fail_or_warn editorconfig)" "editorconfig" "No .editorconfig found"
  fi
}

check_docs() {
  local found
  if found=$(first_dir "$PROJECT_ROOT" docs doc); then
    local count
    count=$(find "$PROJECT_ROOT/$found" -maxdepth 1 -type f 2>/dev/null | wc -l | tr -d ' ')
    if [[ "$count" -gt 0 ]]; then
      emit "PASS" "docs" "$found/ exists ($count files)"
    else
      emit "WARN" "docs" "$found/ exists but is empty"
    fi
  else
    emit "$(fail_or_warn docs)" "docs" "No docs/ directory found"
  fi
}

check_code_of_conduct() {
  local found
  if found=$(first_match "$PROJECT_ROOT" CODE_OF_CONDUCT.md CODE_OF_CONDUCT CODE_OF_CONDUCT.txt); then
    emit "PASS" "code-of-conduct" "$found exists"
  else
    emit "$(fail_or_warn code-of-conduct)" "code-of-conduct" "No CODE_OF_CONDUCT file found"
  fi
}

# --- Initialize metadata (populated by check functions) ---
META_LINTER_CFG=""

# --- Run checks for each active standard ---

ALL_STANDARDS=(readme gitignore license tests claude-md goals spec linter coverage ci changelog contributing editorconfig docs code-of-conduct)

for std in "${ALL_STANDARDS[@]}"; do
  if is_active "$std"; then
    # Call the check function (replace hyphens with underscores for function name)
    func_name="check_${std//-/_}"
    if declare -f "$func_name" &>/dev/null; then
      "$func_name"
    else
      emit "SKIP" "$std" "No check implemented"
    fi
  fi
done

# --- Output ---

if $JSON_OUTPUT; then
  # Collect metadata for Claude to use during runtime verification
  META_FRAMEWORK=$(std_field tests framework "")
  META_TESTDIR=$(std_field tests directory "")

  # Build JSON safely using jq to handle special characters in detail strings
  RESULTS_JSON="[]"
  for line in "${RESULTS[@]}"; do
    IFS=$'\t' read -r status standard detail <<< "$line"
    RESULTS_JSON=$(echo "$RESULTS_JSON" | jq --arg s "$status" --arg n "$standard" --arg d "$detail" \
      '. + [{"status": $s, "standard": $n, "detail": $d}]')
  done

  jq -n --argjson results "$RESULTS_JSON" \
    --arg lang "$LANGUAGE" --arg fw "$META_FRAMEWORK" \
    --arg td "$META_TESTDIR" --arg lc "$META_LINTER_CFG" \
    '{results: $results, metadata: {language: $lang, test_framework: $fw, test_directory: $td, linter_config: $lc}}'
else
  echo -e "${CYAN}Standards check: ${PROJECT_ROOT}${RESET}"
  echo ""
  for line in "${RESULTS[@]}"; do
    IFS=$'\t' read -r status standard detail <<< "$line"
    case "$status" in
      PASS) echo -e "  ${GREEN}PASS${RESET}  ${standard}  ${detail}" ;;
      FAIL) echo -e "  ${RED}FAIL${RESET}  ${standard}  ${detail}" ;;
      WARN) echo -e "  ${YELLOW}WARN${RESET}  ${standard}  ${detail}" ;;
      SKIP) echo -e "  ${CYAN}SKIP${RESET}  ${standard}  ${detail}" ;;
    esac
  done

  echo ""
  pass_count=0 fail_count=0 warn_count=0 skip_count=0
  for line in "${RESULTS[@]}"; do
    IFS=$'\t' read -r status _ _ <<< "$line"
    case "$status" in
      PASS) pass_count=$((pass_count + 1)) ;;
      FAIL) fail_count=$((fail_count + 1)) ;;
      WARN) warn_count=$((warn_count + 1)) ;;
      SKIP) skip_count=$((skip_count + 1)) ;;
    esac
  done
  echo -e "  ${GREEN}${pass_count} passed${RESET}, ${RED}${fail_count} failed${RESET}, ${YELLOW}${warn_count} warnings${RESET}, ${CYAN}${skip_count} skipped${RESET}"
fi

# FAIL results are communicated via output, not exit code.
# Exit 0 = script ran successfully. Exit 1 = script error (handled above).
