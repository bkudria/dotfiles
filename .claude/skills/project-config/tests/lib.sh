#!/usr/bin/env bash
set -euo pipefail

PASS_COUNT=0
FAIL_COUNT=0
FAIL_NAMES=()

assert_eq() {
  local name="$1" expected="$2" actual="$3"
  if [[ "$expected" == "$actual" ]]; then
    PASS_COUNT=$((PASS_COUNT+1))
    printf '  \033[0;32mok\033[0m   %s\n' "$name"
  else
    FAIL_COUNT=$((FAIL_COUNT+1))
    FAIL_NAMES+=("$name")
    printf '  \033[0;31mFAIL\033[0m %s\n' "$name"
    printf '       expected: %q\n' "$expected"
    printf '       actual:   %q\n' "$actual"
  fi
}

assert_contains() {
  local name="$1" needle="$2" haystack="$3"
  if [[ "$haystack" == *"$needle"* ]]; then
    PASS_COUNT=$((PASS_COUNT+1))
    printf '  \033[0;32mok\033[0m   %s\n' "$name"
  else
    FAIL_COUNT=$((FAIL_COUNT+1))
    FAIL_NAMES+=("$name")
    printf '  \033[0;31mFAIL\033[0m %s\n' "$name"
    printf '       expected to contain: %q\n' "$needle"
    printf '       actual: %q\n' "$haystack"
  fi
}

assert_not_contains() {
  local name="$1" needle="$2" haystack="$3"
  if [[ "$haystack" != *"$needle"* ]]; then
    PASS_COUNT=$((PASS_COUNT+1))
    printf '  \033[0;32mok\033[0m   %s\n' "$name"
  else
    FAIL_COUNT=$((FAIL_COUNT+1))
    FAIL_NAMES+=("$name")
    printf '  \033[0;31mFAIL\033[0m %s\n' "$name"
    printf '       expected NOT to contain: %q\n' "$needle"
    printf '       actual: %q\n' "$haystack"
  fi
}

assert_exit_code() {
  local name="$1" expected="$2" actual="$3"
  if [[ "$expected" == "$actual" ]]; then
    PASS_COUNT=$((PASS_COUNT+1))
    printf '  \033[0;32mok\033[0m   %s\n' "$name"
  else
    FAIL_COUNT=$((FAIL_COUNT+1))
    FAIL_NAMES+=("$name")
    printf '  \033[0;31mFAIL\033[0m %s\n' "$name"
    printf '       expected exit: %s, got: %s\n' "$expected" "$actual"
  fi
}

summary() {
  local total=$((PASS_COUNT+FAIL_COUNT))
  echo
  if [[ $FAIL_COUNT -eq 0 ]]; then
    printf '\033[0;32m%d/%d passed\033[0m\n' "$PASS_COUNT" "$total"
    exit 0
  else
    printf '\033[0;31m%d/%d passed (%d failed)\033[0m\n' "$PASS_COUNT" "$total" "$FAIL_COUNT"
    for n in "${FAIL_NAMES[@]}"; do
      printf '  - %s\n' "$n"
    done
    exit 1
  fi
}

setup_fake_skill() {
  local dir
  dir=$(mktemp -d)
  mkdir -p "$dir/profiles" "$dir/scripts"
  ln -s "$REAL_SKILL_DIR/scripts/run-audit.sh" "$dir/scripts/run-audit.sh" 2>/dev/null || true
  ln -s "$REAL_SKILL_DIR/scripts/lint-project-yaml.sh" "$dir/scripts/lint-project-yaml.sh" 2>/dev/null || true
  echo "$dir"
}
