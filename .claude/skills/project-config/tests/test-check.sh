#!/usr/bin/env bash
# Tests for `run-audit.sh --check <state-dir>`
#
# `--check` is the CI pass/fail signal, separated from `--render` so that the
# render output (the audit table) is no longer entangled with an exit-code
# signal that the bash harness presents as a tool ERROR in interactive use.
set -euo pipefail

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REAL_SKILL_DIR="$(cd "$TEST_DIR/.." && pwd)"
RUNNER="$REAL_SKILL_DIR/scripts/run-audit.sh"

# shellcheck source=lib.sh
source "$TEST_DIR/lib.sh"

echo "test-check.sh"

check_state() {
  local state; state=$(mktemp -d)
  printf '%s' "$1" > "$state/merged.json"
  local rc=0
  "$RUNNER" --check "$state" || rc=$?
  rm -rf "$state"
  return $rc
}

# --- Test 1: PASS-only results exit 0 ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",  "status": "PASS", "detail": "ok", "description": "."},
    {"id": "base/license", "status": "PASS", "detail": "ok", "description": "."}
  ],
  "disabled_count": 0
}
EOF
)
set +e
check_state "$results" >/dev/null 2>&1
rc=$?
set -e
assert_exit_code "check exits 0 with PASS-only results" "0" "$rc"

# --- Test 2: any FAIL row exits 1 ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",  "status": "PASS", "detail": "ok", "description": "."},
    {"id": "base/license", "status": "FAIL", "detail": "missing", "description": "."}
  ],
  "disabled_count": 0
}
EOF
)
set +e
check_state "$results" >/dev/null 2>&1
rc=$?
set -e
assert_exit_code "check exits 1 with at least one FAIL" "1" "$rc"

# --- Test 3: SUGG without FAIL exits 0 (SUGG is non-blocking) ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme", "status": "PASS", "detail": "ok",     "description": "."},
    {"id": "base/linter", "status": "SUGG", "detail": "no cfg", "description": "."}
  ],
  "disabled_count": 0
}
EOF
)
set +e
check_state "$results" >/dev/null 2>&1
rc=$?
set -e
assert_exit_code "check exits 0 when SUGG present but no FAIL" "0" "$rc"

# --- Test 4: missing state-dir merged.json exits >=2 (operational error,
#             distinct from "audit had FAILs") ---
empty_dir=$(mktemp -d)
trap 'rm -rf "$empty_dir"' EXIT
set +e
"$RUNNER" --check "$empty_dir" >/dev/null 2>&1
rc=$?
set -e
assert_eq "check exits >=2 with missing merged.json (operational error)" \
  "true" "$([[ $rc -ge 2 ]] && echo true || echo false)"

# --- Test 5: state-dir form reads merged.json ---
state_dir=$(mktemp -d)
trap 'rm -rf "$empty_dir" "$state_dir"' EXIT
cat > "$state_dir/merged.json" <<'EOF'
{
  "resolved": [
    {"id": "base/license", "status": "FAIL", "detail": ".", "description": "."}
  ],
  "disabled_count": 0
}
EOF
set +e
"$RUNNER" --check "$state_dir" >/dev/null 2>&1
rc=$?
set -e
assert_exit_code "check reads merged.json from state-dir" "1" "$rc"

# --- Test 7: unresolved pending entries exit >=2 (operational error) ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme", "status": "PASS", "detail": ".", "description": "."}
  ],
  "pending": [
    {"id": "base/linter", "required": true, "description": ".", "rendered_prompt": "..."}
  ],
  "disabled_count": 0
}
EOF
)
set +e
check_state "$results" >/dev/null 2>&1
rc=$?
set -e
assert_eq "check exits >=2 with unresolved pending (operational error)" \
  "true" "$([[ $rc -ge 2 ]] && echo true || echo false)"

summary
