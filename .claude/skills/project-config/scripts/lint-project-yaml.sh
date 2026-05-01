#!/usr/bin/env bash
# lint-project-yaml.sh — Validate project.yaml and standard YAMLs against the
# project-config skill schema.
#
# Usage:
#   lint-project-yaml.sh <project-yaml>     Validate the given project.yaml
#                                            and the standard YAMLs in the
#                                            profiles it selects.
#   lint-project-yaml.sh --skill            Validate every standard YAML in
#                                            <skill>/profiles/*/*.yaml.
set -euo pipefail

SKILL_DIR="${CLAUDE_SKILL_DIR:-${HOME}/.claude/skills/project-config}"

usage() {
  echo "Usage:" >&2
  echo "  lint-project-yaml.sh <project-yaml>" >&2
  echo "  lint-project-yaml.sh --skill" >&2
  exit 1
}

require_cmd() {
  for cmd in "$@"; do
    command -v "$cmd" >/dev/null 2>&1 || {
      echo "Error: $cmd is required" >&2
      exit 1
    }
  done
}
require_cmd yq

ERRORS=0
err() {
  echo "Error: $*" >&2
  ERRORS=$((ERRORS + 1))
}

# Validate one standard YAML file.
# $1 = filesystem path to standard YAML
# $2 = id (e.g., "base/readme") used in error messages
validate_standard() {
  local file="$1" id="$2"

  if ! yq -e '.' "$file" >/dev/null 2>&1; then
    err "$id: failed to parse YAML"
    return
  fi

  local extras
  extras=$(yq -r 'keys | .[] | select(. != "required" and . != "description" and . != "check" and . != "notes")' \
           "$file" 2>/dev/null || true)
  if [[ -n "$extras" ]]; then
    while IFS= read -r k; do
      [[ -n "$k" ]] && err "$id: unexpected top-level key: $k"
    done <<<"$extras"
  fi

  local required
  required=$(yq -r '.required' "$file" 2>/dev/null)
  case "$required" in
    true|false) ;;
    null|"")    err "$id: missing required field 'required: true|false'" ;;
    *)          err "$id: 'required' must be a boolean, got: $required" ;;
  esac

  local description
  description=$(yq -r '.description // ""' "$file" 2>/dev/null)
  [[ -n "$description" ]] || err "$id: missing or empty 'description'"

  local has_check has_script has_prompt
  has_check=$(yq -r 'has("check")' "$file" 2>/dev/null)
  if [[ "$has_check" != "true" ]]; then
    err "$id: missing 'check' block"
    return
  fi

  has_script=$(yq -r '.check | has("script")' "$file" 2>/dev/null)
  has_prompt=$(yq -r '.check | has("prompt")' "$file" 2>/dev/null)

  if [[ "$has_script" == "true" && "$has_prompt" == "true" ]]; then
    err "$id: 'check' must have exactly one of script or prompt, found both"
  elif [[ "$has_script" != "true" && "$has_prompt" != "true" ]]; then
    err "$id: 'check' must have exactly one of script or prompt, found neither"
  fi

  if [[ "$has_script" == "true" ]]; then
    local script_body
    script_body=$(yq -r '.check.script' "$file" 2>/dev/null)
    [[ -n "$script_body" && "$script_body" != "null" ]] || err "$id: 'check.script' is empty"
  fi
  if [[ "$has_prompt" == "true" ]]; then
    local prompt_body
    prompt_body=$(yq -r '.check.prompt' "$file" 2>/dev/null)
    [[ -n "$prompt_body" && "$prompt_body" != "null" ]] || err "$id: 'check.prompt' is empty"
  fi

  local has_notes
  has_notes=$(yq -r 'has("notes")' "$file" 2>/dev/null)
  if [[ "$has_notes" == "true" ]]; then
    local notes
    notes=$(yq -r '.notes // ""' "$file" 2>/dev/null)
    [[ -n "$notes" ]] || err "$id: 'notes' is present but empty"
  fi
}

lint_skill() {
  local sdir="$SKILL_DIR/profiles"
  [[ -d "$sdir" ]] || { echo "Error: $sdir not found" >&2; exit 1; }

  while IFS= read -r f; do
    [[ -n "$f" ]] || continue
    local rel id
    rel="${f#$sdir/}"
    id="${rel%.yaml}"
    validate_standard "$f" "$id"
  done < <(find "$sdir" -mindepth 2 -maxdepth 2 -type f -name '*.yaml' | sort)

  if [[ "$ERRORS" -gt 0 ]]; then
    echo "$ERRORS error(s)" >&2
    exit 1
  fi
  echo "OK"
}

lint_project_yaml() {
  local pyaml="$1"
  [[ -f "$pyaml" ]] || { echo "Error: $pyaml not found" >&2; exit 1; }

  if ! yq -e '.' "$pyaml" >/dev/null 2>&1; then
    err "project.yaml failed to parse"
    echo "$ERRORS error(s)" >&2
    exit 1
  fi

  local extras
  extras=$(yq -r 'keys | .[] | select(. != "profiles" and . != "disabled" and . != "required")' "$pyaml" 2>/dev/null || true)
  if [[ -n "$extras" ]]; then
    while IFS= read -r k; do
      [[ -n "$k" ]] && err "project.yaml has unexpected top-level key: $k"
    done <<<"$extras"
  fi

  local has_profiles
  has_profiles=$(yq -r 'has("profiles")' "$pyaml" 2>/dev/null)
  if [[ "$has_profiles" != "true" ]]; then
    err "project.yaml is missing 'profiles' (a non-empty list)"
  fi

  local profile_count
  profile_count=$(yq -r '.profiles | length' "$pyaml" 2>/dev/null)
  if [[ -z "$profile_count" || "$profile_count" == "null" || "$profile_count" -eq 0 ]]; then
    err "project.yaml 'profiles' must be a non-empty list"
  fi

  local profiles=()
  while IFS= read -r p; do
    [[ -n "$p" ]] && profiles+=("$p")
  done < <(yq -r '.profiles[]?' "$pyaml" 2>/dev/null || true)

  local valid_profiles=()
  for profile in "${profiles[@]}"; do
    if [[ -d "$SKILL_DIR/profiles/$profile" ]]; then
      valid_profiles+=("$profile")
    else
      err "profile not found: $profile (looked in $SKILL_DIR/profiles/$profile)"
    fi
  done

  while IFS= read -r key; do
    [[ -n "$key" ]] || continue
    local profile_part basename_part
    profile_part="${key%%/*}"
    basename_part="${key##*/}"
    if [[ "$profile_part" == "$key" || -z "$basename_part" ]]; then
      err "disabled key '$key' must be of the form '<profile>/<standard>'"
      continue
    fi

    local in_selected=0
    for vp in "${valid_profiles[@]}"; do
      [[ "$vp" == "$profile_part" ]] && in_selected=1 && break
    done
    if [[ "$in_selected" -eq 0 ]]; then
      err "disabled key '$key' references profile '$profile_part' which is not in the selected profiles list"
      continue
    fi

    if [[ ! -f "$SKILL_DIR/profiles/$profile_part/$basename_part.yaml" ]]; then
      err "disabled key '$key' references a standard that does not exist: $SKILL_DIR/profiles/$profile_part/$basename_part.yaml"
      continue
    fi

    local reason
    reason=$(yq -r ".disabled.\"$key\" // \"\"" "$pyaml" 2>/dev/null)
    [[ -n "$reason" ]] || err "disabled key '$key' has empty reason (a non-empty string is required)"
  done < <(yq -r '.disabled // {} | keys | .[]?' "$pyaml" 2>/dev/null || true)

  local has_required required_kind
  has_required=$(yq -r 'has("required")' "$pyaml" 2>/dev/null)
  if [[ "$has_required" == "true" ]]; then
    required_kind=$(yq -r '.required | type' "$pyaml" 2>/dev/null)
    if [[ "$required_kind" != "!!seq" ]]; then
      err "project.yaml 'required' must be a list of '<profile>/<standard>' strings"
    else
      while IFS= read -r entry; do
        [[ -n "$entry" ]] || continue
        local profile_part basename_part
        profile_part="${entry%%/*}"
        basename_part="${entry##*/}"
        if [[ "$profile_part" == "$entry" || -z "$basename_part" ]]; then
          err "required entry '$entry' must be of the form '<profile>/<standard>'"
          continue
        fi

        local in_selected=0
        for vp in "${valid_profiles[@]}"; do
          [[ "$vp" == "$profile_part" ]] && in_selected=1 && break
        done
        if [[ "$in_selected" -eq 0 ]]; then
          err "required entry '$entry' references profile '$profile_part' which is not in the selected profiles list"
          continue
        fi

        local std_path="$SKILL_DIR/profiles/$profile_part/$basename_part.yaml"
        if [[ ! -f "$std_path" ]]; then
          err "required entry '$entry' references a standard that does not exist: $std_path"
          continue
        fi

        local std_required
        std_required=$(yq -r '.required' "$std_path" 2>/dev/null)
        if [[ "$std_required" == "true" ]]; then
          err "required entry '$entry' is a no-op: standard is already 'required: true' in its YAML"
        fi

        local in_disabled
        in_disabled=$(yq -r ".disabled.\"$entry\" // \"\"" "$pyaml" 2>/dev/null)
        if [[ -n "$in_disabled" ]]; then
          err "id '$entry' appears in both 'required:' and 'disabled:' (mutually exclusive)"
        fi
      done < <(yq -r '.required[]?' "$pyaml" 2>/dev/null || true)
    fi
  fi

  for profile in "${valid_profiles[@]}"; do
    while IFS= read -r f; do
      [[ -n "$f" ]] || continue
      local id="$profile/$(basename "$f" .yaml)"
      validate_standard "$f" "$id"
    done < <(find "$SKILL_DIR/profiles/$profile" -maxdepth 1 -type f -name '*.yaml' | sort)
  done

  if [[ "$ERRORS" -gt 0 ]]; then
    echo "$ERRORS error(s)" >&2
    exit 1
  fi
  echo "OK"
}

# ───── dispatch ─────────────────────────────────────────────────────────────

[[ $# -ge 1 ]] || usage
case "$1" in
  --skill) lint_skill ;;
  -h|--help) usage ;;
  -*) echo "Unknown flag: $1" >&2; usage ;;
  *) lint_project_yaml "$1" ;;
esac
