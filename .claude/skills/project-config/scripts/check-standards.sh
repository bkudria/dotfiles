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
          goals)            ref_file="GOALS.md" ;;
          spec)             ref_file="SPEC.md" ;;
          docs)             ref_file="docs/" ;;
          contributing)     ref_file="CONTRIBUTING.md" ;;
          code-of-conduct)  ref_file="CODE_OF_CONDUCT.md" ;;
          security-policy)  ref_file="SECURITY.md" ;;
          support)          ref_file="SUPPORT.md" ;;
          changelog)        ref_file="CHANGELOG.md" ;;
          *)                ref_file="$ref" ;;
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
        emit "$(fail_or_warn readme)" "readme" "$found exists but missing sections: $missing_sects"
        return
      fi
      detail="$detail, required sections present"
    fi

    # Recommended section check (requires mq) — always WARN, never FAIL
    local rec_sections
    rec_sections=$(yq -r '.["readme"].recommended_sections // [] | .[]' "$MERGED_YAML" 2>/dev/null || true)
    if [[ -n "$rec_sections" ]] && $HAS_MQ; then
      local headings
      headings=${headings:-$(mq -F text '.h2' "$PROJECT_ROOT/$found" 2>/dev/null | tr '[:upper:]' '[:lower:]')}
      local missing_rec=""
      for sect in $rec_sections; do
        local sect_lower
        sect_lower=$(echo "$sect" | tr '[:upper:]' '[:lower:]')
        if ! echo "$headings" | grep -qi "$sect_lower"; then
          missing_rec="${missing_rec:+$missing_rec, }$sect"
        fi
      done
      if [[ -n "$missing_rec" ]]; then
        emit "WARN" "readme" "$found exists but missing recommended sections: $missing_rec"
        return
      fi
      detail="$detail, recommended sections present"
    fi

    emit "PASS" "readme" "$detail"
  else
    emit "$(fail_or_warn readme)" "readme" "No README file found"
  fi
}

check_readme_badges() {
  local found
  if found=$(first_match "$PROJECT_ROOT" README.md README README.txt README.rdoc README.org); then
    if grep -qiE "img\.shields\.io|badge\.svg|codecov\.io|badgen\.net" "$PROJECT_ROOT/$found" 2>/dev/null; then
      emit "PASS" "readme-badges" "Badge(s) found in $found"
    else
      emit "$(fail_or_warn readme-badges)" "readme-badges" "No status badges found in $found"
    fi
  else
    emit "SKIP" "readme-badges" "No README file found"
  fi
}

check_gitignore() {
  if [[ -e "$PROJECT_ROOT/.gitignore" ]]; then
    # Public visibility: warn only about project-specific patterns whose
    # absence creates real risk (e.g., committed secrets via .env).
    # Personal/editor patterns (.vscode/, .idea/, etc.) are out of scope —
    # those belong in a developer's personal gitignore, not the project's.
    if [[ "$VISIBILITY" == "public" ]]; then
      local missing_patterns=""
      for pattern in ".env"; do
        if ! grep -q "$pattern" "$PROJECT_ROOT/.gitignore" 2>/dev/null; then
          missing_patterns="${missing_patterns:+$missing_patterns, }$pattern"
        fi
      done
      if [[ -n "$missing_patterns" ]]; then
        emit "WARN" "gitignore" ".gitignore exists but missing recommended patterns for public repo: $missing_patterns"
        return
      fi
    fi
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
      # Check current_year if configured
      local check_year
      check_year=$(std_field license current_year "")
      if [[ "$check_year" == "true" ]]; then
        local current_year
        current_year=$(date +%Y)
        if ! grep -q "$current_year" "$PROJECT_ROOT/$found" 2>/dev/null; then
          emit "WARN" "license" "$found exists, $spdx verified, but copyright year $current_year not found"
          return
        fi
      fi
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

    # Public visibility: remind to review for internal-only content
    if [[ "$VISIBILITY" == "public" ]]; then
      emit "SKIP" "claude-md.review" "Review content for internal-only references before public release"
    fi
  else
    emit "$(fail_or_warn claude-md)" "claude-md" "No CLAUDE.md or AGENTS.md found"
  fi
}

check_goals() {
  local found
  if found=$(first_match "$PROJECT_ROOT" \
    GOALS.md goals.md goals.yaml \
    docs/GOALS.md docs/goals.md docs/goals.yaml); then
    emit "PASS" "goals" "$found exists"
  else
    emit "$(fail_or_warn goals)" "goals" "No GOALS.md or docs/goals.md found"
  fi
}

check_spec() {
  local candidates=("SPEC.md" "specification.md" "design.md")
  # Only check spec.md at root if no spec/ test directory exists (could be confused with rspec dir)
  if [[ ! -d "$PROJECT_ROOT/spec" ]]; then
    candidates+=("spec.md")
  fi
  # docs/ paths — always safe since they can't be confused with a test directory
  candidates+=("docs/SPEC.md" "docs/spec.md" "docs/specification.md" "docs/design.md")

  local found
  for f in "${candidates[@]}"; do
    if [[ -e "$PROJECT_ROOT/$f" ]]; then
      emit "PASS" "spec" "$f exists"
      return
    fi
  done

  emit "$(fail_or_warn spec)" "spec" "No SPEC.md or docs/spec.md found"
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

check_formatter() {
  local config
  config=$(std_field formatter config "")

  if [[ -n "$config" ]]; then
    if [[ -e "$PROJECT_ROOT/$config" ]]; then
      META_FORMATTER_CFG="$config"
      local tool
      tool=$(std_field formatter tool "")
      if [[ -n "$tool" ]]; then
        emit "PASS" "formatter" "$tool config: $config"
      else
        emit "PASS" "formatter" "Config: $config"
      fi
    else
      emit "$(fail_or_warn formatter)" "formatter" "Declared config $config not found"
    fi
  else
    emit "SKIP" "formatter" "No config declared — verify manually"
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
      # Check for [Unreleased] section
      if grep -qi '## \[Unreleased\]' "$PROJECT_ROOT/$found" 2>/dev/null; then
        emit "PASS" "changelog" "$found exists, Keep-a-Changelog format with [Unreleased] section"
      else
        emit "WARN" "changelog" "$found exists, Keep-a-Changelog format but no [Unreleased] section"
      fi
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

check_security_policy() {
  local found
  if found=$(first_match "$PROJECT_ROOT" SECURITY.md SECURITY SECURITY.txt .github/SECURITY.md); then
    emit "PASS" "security-policy" "$found exists"
  else
    emit "$(fail_or_warn security-policy)" "security-policy" "No SECURITY.md file found"
  fi
}

check_issue_templates() {
  # Modern multi-template form: .github/ISSUE_TEMPLATE/ directory
  if [[ -d "$PROJECT_ROOT/.github/ISSUE_TEMPLATE" ]]; then
    local count
    count=$(find "$PROJECT_ROOT/.github/ISSUE_TEMPLATE" -maxdepth 1 -type f \
      \( -name '*.md' -o -name '*.yml' -o -name '*.yaml' \) \
      ! -name 'config.yml' ! -name 'config.yaml' 2>/dev/null | wc -l | tr -d ' ')
    if [[ "$count" -gt 0 ]]; then
      emit "PASS" "issue-templates" ".github/ISSUE_TEMPLATE/ has $count templates"
      return
    fi
  fi

  # GitLab equivalent
  if [[ -d "$PROJECT_ROOT/.gitlab/issue_templates" ]]; then
    local count
    count=$(find "$PROJECT_ROOT/.gitlab/issue_templates" -maxdepth 1 -type f -name '*.md' 2>/dev/null | wc -l | tr -d ' ')
    if [[ "$count" -gt 0 ]]; then
      emit "PASS" "issue-templates" ".gitlab/issue_templates/ has $count templates"
      return
    fi
  fi

  # Legacy single-file form — PASS but WARN
  if [[ -f "$PROJECT_ROOT/.github/ISSUE_TEMPLATE.md" ]]; then
    emit "WARN" "issue-templates" ".github/ISSUE_TEMPLATE.md exists (legacy single-file form — consider migrating to .github/ISSUE_TEMPLATE/ directory)"
    return
  fi

  emit "$(fail_or_warn issue-templates)" "issue-templates" "No issue templates found (.github/ISSUE_TEMPLATE/ or .gitlab/issue_templates/)"
}

check_pr_template() {
  # GitHub single-file form (canonical + case variants + root/docs locations)
  local found
  if found=$(first_match "$PROJECT_ROOT" \
    .github/PULL_REQUEST_TEMPLATE.md \
    .github/pull_request_template.md \
    PULL_REQUEST_TEMPLATE.md \
    docs/PULL_REQUEST_TEMPLATE.md); then
    emit "PASS" "pr-template" "$found exists"
    return
  fi

  # GitHub multi-template form: .github/PULL_REQUEST_TEMPLATE/ directory
  if [[ -d "$PROJECT_ROOT/.github/PULL_REQUEST_TEMPLATE" ]]; then
    local count
    count=$(find "$PROJECT_ROOT/.github/PULL_REQUEST_TEMPLATE" -maxdepth 1 -type f -name '*.md' 2>/dev/null | wc -l | tr -d ' ')
    if [[ "$count" -gt 0 ]]; then
      emit "PASS" "pr-template" ".github/PULL_REQUEST_TEMPLATE/ has $count templates"
      return
    fi
  fi

  # GitLab equivalent
  if [[ -d "$PROJECT_ROOT/.gitlab/merge_request_templates" ]]; then
    local count
    count=$(find "$PROJECT_ROOT/.gitlab/merge_request_templates" -maxdepth 1 -type f -name '*.md' 2>/dev/null | wc -l | tr -d ' ')
    if [[ "$count" -gt 0 ]]; then
      emit "PASS" "pr-template" ".gitlab/merge_request_templates/ has $count templates"
      return
    fi
  fi

  emit "$(fail_or_warn pr-template)" "pr-template" "No PR template found"
}

check_support() {
  local found
  if found=$(first_match "$PROJECT_ROOT" SUPPORT.md .github/SUPPORT.md docs/SUPPORT.md); then
    emit "PASS" "support" "$found exists"
  else
    emit "$(fail_or_warn support)" "support" "No SUPPORT.md found"
  fi
}

check_commit_convention() {
  # Commitlint config files
  local found
  if found=$(first_match "$PROJECT_ROOT" \
    commitlint.config.js commitlint.config.cjs commitlint.config.mjs commitlint.config.ts \
    .commitlintrc .commitlintrc.json .commitlintrc.yml .commitlintrc.yaml \
    .commitlintrc.js .commitlintrc.cjs .commitlintrc.ts); then
    emit "PASS" "commit-convention" "commitlint config: $found"
    return
  fi

  # Commitizen config
  if found=$(first_match "$PROJECT_ROOT" .czrc .cz.json); then
    emit "PASS" "commit-convention" "commitizen config: $found"
    return
  fi

  # Embedded commitlint config in package.json
  if [[ -e "$PROJECT_ROOT/package.json" ]] && command -v jq &>/dev/null; then
    if jq -e '.commitlint' "$PROJECT_ROOT/package.json" &>/dev/null; then
      emit "PASS" "commit-convention" "commitlint config in package.json"
      return
    fi
  fi

  # CONTRIBUTING.md content grep
  local contrib
  if contrib=$(first_match "$PROJECT_ROOT" CONTRIBUTING.md CONTRIBUTING); then
    if grep -qiE "conventional commit|commit message format|commit convention|angular commit" "$PROJECT_ROOT/$contrib" 2>/dev/null; then
      emit "PASS" "commit-convention" "Commit convention documented in $contrib"
      return
    fi
  fi

  emit "$(fail_or_warn commit-convention)" "commit-convention" "No commit convention config or documentation found"
}

check_release_automation() {
  # release-please
  local found
  if found=$(first_match "$PROJECT_ROOT" release-please-config.json .release-please-manifest.json); then
    emit "PASS" "release-automation" "release-please config: $found"
    return
  fi

  # semantic-release config files
  if found=$(first_match "$PROJECT_ROOT" \
    .releaserc .releaserc.json .releaserc.yml .releaserc.yaml \
    .releaserc.js .releaserc.cjs release.config.js release.config.cjs release.config.ts); then
    emit "PASS" "release-automation" "semantic-release config: $found"
    return
  fi

  # changesets
  if [[ -e "$PROJECT_ROOT/.changeset/config.json" ]]; then
    emit "PASS" "release-automation" ".changeset/config.json exists"
    return
  fi

  # GoReleaser
  if found=$(first_match "$PROJECT_ROOT" .goreleaser.yml .goreleaser.yaml goreleaser.yml goreleaser.yaml); then
    emit "PASS" "release-automation" "GoReleaser config: $found"
    return
  fi

  # semantic-release embedded in package.json
  if [[ -e "$PROJECT_ROOT/package.json" ]] && command -v jq &>/dev/null; then
    if jq -e '.release' "$PROJECT_ROOT/package.json" &>/dev/null; then
      emit "PASS" "release-automation" "semantic-release config in package.json"
      return
    fi
  fi

  emit "$(fail_or_warn release-automation)" "release-automation" "No release automation config found"
}

check_dependency_updates() {
  # Dependabot
  local found
  if found=$(first_match "$PROJECT_ROOT" .github/dependabot.yml .github/dependabot.yaml); then
    emit "PASS" "dependency-updates" "Dependabot config: $found"
    return
  fi

  # Renovate config files
  if found=$(first_match "$PROJECT_ROOT" \
    renovate.json renovate.json5 .renovaterc .renovaterc.json \
    .github/renovate.json .github/renovate.json5); then
    emit "PASS" "dependency-updates" "Renovate config: $found"
    return
  fi

  # Renovate embedded in package.json
  if [[ -e "$PROJECT_ROOT/package.json" ]] && command -v jq &>/dev/null; then
    if jq -e '.renovate' "$PROJECT_ROOT/package.json" &>/dev/null; then
      emit "PASS" "dependency-updates" "Renovate config in package.json"
      return
    fi
  fi

  emit "$(fail_or_warn dependency-updates)" "dependency-updates" "No Dependabot or Renovate config found"
}

check_lockfile() {
  case "$LANGUAGE" in
    typescript|javascript)
      local found
      if found=$(first_match "$PROJECT_ROOT" package-lock.json yarn.lock pnpm-lock.yaml bun.lockb bun.lock); then
        emit "PASS" "lockfile" "$found exists"
      else
        emit "$(fail_or_warn lockfile)" "lockfile" "No JS/TS lockfile found (package-lock.json, yarn.lock, pnpm-lock.yaml, bun.lockb)"
      fi
      ;;
    ruby)
      if [[ -e "$PROJECT_ROOT/Gemfile.lock" ]]; then
        emit "PASS" "lockfile" "Gemfile.lock exists"
      else
        emit "$(fail_or_warn lockfile)" "lockfile" "No Gemfile.lock found"
      fi
      ;;
    python)
      local found
      if found=$(first_match "$PROJECT_ROOT" poetry.lock Pipfile.lock uv.lock); then
        emit "PASS" "lockfile" "$found exists"
      else
        emit "$(fail_or_warn lockfile)" "lockfile" "No Python lockfile found (poetry.lock, Pipfile.lock, uv.lock)"
      fi
      ;;
    rust)
      if [[ -e "$PROJECT_ROOT/Cargo.lock" ]]; then
        emit "PASS" "lockfile" "Cargo.lock exists"
      else
        emit "$(fail_or_warn lockfile)" "lockfile" "No Cargo.lock found"
      fi
      ;;
    go)
      if [[ -e "$PROJECT_ROOT/go.sum" ]]; then
        emit "PASS" "lockfile" "go.sum exists"
      else
        emit "$(fail_or_warn lockfile)" "lockfile" "No go.sum found"
      fi
      ;;
    php)
      if [[ -e "$PROJECT_ROOT/composer.lock" ]]; then
        emit "PASS" "lockfile" "composer.lock exists"
      else
        emit "$(fail_or_warn lockfile)" "lockfile" "No composer.lock found"
      fi
      ;;
    elixir)
      if [[ -e "$PROJECT_ROOT/mix.lock" ]]; then
        emit "PASS" "lockfile" "mix.lock exists"
      else
        emit "$(fail_or_warn lockfile)" "lockfile" "No mix.lock found"
      fi
      ;;
    swift)
      if [[ -e "$PROJECT_ROOT/Package.resolved" ]]; then
        emit "PASS" "lockfile" "Package.resolved exists"
      else
        emit "$(fail_or_warn lockfile)" "lockfile" "No Package.resolved found"
      fi
      ;;
    *)
      emit "SKIP" "lockfile" "Unknown language ($LANGUAGE) — verify manually"
      ;;
  esac
}

check_runtime_version() {
  emit "SKIP" "runtime-version" "Verify manually: project should declare its required runtime/language version (manifest field, dotfile, version-manager config, or README)"
}

check_package_metadata() {
  local manifest
  manifest=$(std_field package-metadata manifest "")

  if [[ -z "$manifest" ]]; then
    emit "SKIP" "package-metadata" "No manifest declared — verify manually"
    return
  fi

  if [[ ! -e "$PROJECT_ROOT/$manifest" ]]; then
    emit "$(fail_or_warn package-metadata)" "package-metadata" "Declared manifest $manifest not found"
    return
  fi

  # Best-effort field validation based on manifest type
  local missing_fields=""
  case "$manifest" in
    package.json)
      if command -v jq &>/dev/null; then
        for field in name version license repository description; do
          local val
          val=$(jq -r ".$field // empty" "$PROJECT_ROOT/$manifest" 2>/dev/null)
          if [[ -z "$val" ]]; then
            missing_fields="${missing_fields:+$missing_fields, }$field"
          fi
        done
      fi
      ;;
    Cargo.toml)
      for field in name version license repository; do
        local val
        val=$(yq -r ".package.$field // \"\"" "$PROJECT_ROOT/$manifest" 2>/dev/null)
        if [[ -z "$val" ]]; then
          missing_fields="${missing_fields:+$missing_fields, }$field"
        fi
      done
      ;;
    pyproject.toml)
      for field in name version; do
        local val
        val=$(yq -r ".project.$field // \"\"" "$PROJECT_ROOT/$manifest" 2>/dev/null)
        if [[ -z "$val" ]]; then
          missing_fields="${missing_fields:+$missing_fields, }$field"
        fi
      done
      ;;
    *)
      # Unknown manifest type — just check existence
      emit "PASS" "package-metadata" "$manifest exists (field validation not supported for this format)"
      return
      ;;
  esac

  if [[ -n "$missing_fields" ]]; then
    emit "WARN" "package-metadata" "$manifest exists but missing fields: $missing_fields"
  else
    emit "PASS" "package-metadata" "$manifest exists, key fields present"
  fi
}

check_package_metadata_complete() {
  emit "SKIP" "package-metadata-complete" "Verify manually: manifest fills in discoverability/governance fields beyond the bare minimum (keywords/topics, author/maintainer, bugs URL, homepage URL, contributors, funding)"
}

check_metadata_quality() {
  emit "SKIP" "metadata-quality" "Verify manually: manifest description, repo description, README tagline, and keywords are accurate and informative (not terse placeholders), and consistent across surfaces"
}

check_security_automation() {
  emit "SKIP" "security-automation" "Verify manually: project has automated SAST (e.g., CodeQL, Semgrep), SCA (e.g., dependency-review-action, npm audit, pip-audit), and supply-chain hardening (e.g., OSSF Scorecard, signed releases, pinned action versions) running in CI"
}

check_privacy_posture() {
  emit "SKIP" "privacy-posture" "Verify manually: README and/or SECURITY.md state the project's privacy posture explicitly (e.g., what telemetry/data is or isn't collected, where prompts and outputs are sent, retention or sharing policy). A vague mention of 'privacy' is not a posture."
}

check_release_process() {
  emit "SKIP" "release-process" "Verify manually: contributor-facing docs (CONTRIBUTING.md, RELEASING.md, or a docs/ release page) explain how a release is cut end-to-end — commit-message conventions, who/what creates the release PR, how versions are bumped, how the artifact is published, and how CHANGELOG entries appear. A bare 'we use release-please' line is not enough."
}

check_shell_completion() {
  emit "SKIP" "shell-completion" "Verify manually: the CLI ships shell completion for at least bash and zsh — either as a built-in subcommand (e.g., '<cli> completion bash') that prints a script, or as static completion files installed by the package (e.g., share/bash-completion/, share/zsh/site-functions/). Document where users source or install the script."
}

check_visual_demo() {
  emit "SKIP" "visual-demo" "Verify manually: README features an embedded visual demo of the project (animated GIF, asciinema cast, short video, or screenshot) prominently, ideally near the top. For CLIs, prefer a reproducible source (e.g., charmbracelet/vhs .tape file, asciinema cast). For libraries or UIs, a screenshot or short clip is fine. Static text alone is not a visual demo."
}

check_comparison() {
  emit "SKIP" "comparison" "Verify manually: README discusses how this project compares to similar or alternative tools. Format is flexible — comparison table, bullet list of differences, or short prose section. Identify peer projects by name where possible, lead with what makes this project distinct (not feature parity), and acknowledge cases where alternatives are better. Public projects in crowded niches benefit most; novel projects can simply state that no direct alternatives exist."
}

check_publish_config() {
  case "$LANGUAGE" in
    typescript|javascript)
      if [[ -e "$PROJECT_ROOT/.npmignore" ]]; then
        emit "PASS" "publish-config" ".npmignore exists"
      elif command -v jq &>/dev/null && [[ -e "$PROJECT_ROOT/package.json" ]]; then
        local files_field
        files_field=$(jq -r '.files // empty' "$PROJECT_ROOT/package.json" 2>/dev/null)
        if [[ -n "$files_field" ]]; then
          emit "PASS" "publish-config" "package.json has \"files\" field"
        else
          emit "$(fail_or_warn publish-config)" "publish-config" "No .npmignore or package.json \"files\" field found"
        fi
      else
        emit "$(fail_or_warn publish-config)" "publish-config" "No .npmignore found"
      fi
      ;;
    ruby)
      local gemspec
      gemspec=$(find "$PROJECT_ROOT" -maxdepth 1 -name '*.gemspec' 2>/dev/null | head -1)
      if [[ -n "$gemspec" ]] && grep -q 'files' "$gemspec" 2>/dev/null; then
        emit "PASS" "publish-config" "$(basename "$gemspec") has files attribute"
      else
        emit "$(fail_or_warn publish-config)" "publish-config" "No gemspec with files attribute found"
      fi
      ;;
    rust)
      if [[ -e "$PROJECT_ROOT/Cargo.toml" ]]; then
        if grep -qE '(exclude|include)' "$PROJECT_ROOT/Cargo.toml" 2>/dev/null; then
          emit "PASS" "publish-config" "Cargo.toml has exclude/include"
        else
          emit "WARN" "publish-config" "Cargo.toml exists but no exclude/include (Cargo has safe defaults)"
        fi
      else
        emit "$(fail_or_warn publish-config)" "publish-config" "No Cargo.toml found"
      fi
      ;;
    python)
      if [[ -e "$PROJECT_ROOT/MANIFEST.in" ]]; then
        emit "PASS" "publish-config" "MANIFEST.in exists"
      elif [[ -e "$PROJECT_ROOT/pyproject.toml" ]] && grep -q 'packages' "$PROJECT_ROOT/pyproject.toml" 2>/dev/null; then
        emit "PASS" "publish-config" "pyproject.toml has packages config"
      else
        emit "$(fail_or_warn publish-config)" "publish-config" "No MANIFEST.in or pyproject.toml packages config found"
      fi
      ;;
    *)
      emit "SKIP" "publish-config" "Unknown language ($LANGUAGE) — verify manually"
      ;;
  esac
}

# --- Initialize metadata (populated by check functions) ---
META_LINTER_CFG=""
META_FORMATTER_CFG=""

# --- Run checks for each active standard ---

ALL_STANDARDS=(readme readme-badges gitignore license tests claude-md goals spec linter formatter coverage ci changelog contributing editorconfig docs code-of-conduct security-policy security-automation privacy-posture issue-templates pr-template support commit-convention release-automation release-process dependency-updates lockfile runtime-version package-metadata package-metadata-complete metadata-quality publish-config shell-completion visual-demo comparison)

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
    --arg fc "$META_FORMATTER_CFG" \
    '{results: $results, metadata: {language: $lang, test_framework: $fw, test_directory: $td, linter_config: $lc, formatter_config: $fc}}'
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
