#!/usr/bin/env bash
# Tests for `run-audit.sh --gate <state-dir>`.
#
# `--gate` is the mid-flow checkpoint between round 1 (required) and round 2
# (suggested). It reads merged.json and exits:
#   0  — every effective-required entry has status PASS (run round 2)
#   1  — at least one effective-required entry has status FAIL (skip round 2)
#   2  — operational error (missing file, malformed JSON)
set -euo pipefail

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REAL_SKILL_DIR="$(cd "$TEST_DIR/.." && pwd)"
RUNNER="$REAL_SKILL_DIR/scripts/run-audit.sh"

# shellcheck source=lib.sh
source "$TEST_DIR/lib.sh"

echo "test-gate.sh"

gate_state() {
  local state; state=$(mktemp -d)
  printf '%s' "$1" > "$state/merged.json"
  local rc=0
  "$RUNNER" --gate "$state" || rc=$?
  rm -rf "$state"
  return $rc
}

# --- Test 1: missing merged.json → exit 2 (operational error) ---
empty_dir=$(mktemp -d)
trap 'rm -rf "$empty_dir"' EXIT
set +e
"$RUNNER" --gate "$empty_dir" >/dev/null 2>&1
rc=$?
set -e
assert_eq "gate exits >=2 with missing merged.json" \
  "true" "$([[ $rc -ge 2 ]] && echo true || echo false)"

# --- Test 2: every effective-required PASS → exit 0 ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",  "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": true},
    {"id": "base/license", "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": true}
  ],
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
set +e
gate_state "$results" >/dev/null 2>&1
rc=$?
set -e
assert_exit_code "gate exits 0 when all effective-required PASS" "0" "$rc"

# --- Test 3: any effective-required FAIL → exit 1 ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",  "status": "PASS", "detail": "ok",     "description": ".", "intrinsic_required": true},
    {"id": "base/license", "status": "FAIL", "detail": "missing","description": ".", "intrinsic_required": true}
  ],
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
set +e
gate_state "$results" >/dev/null 2>&1
rc=$?
set -e
assert_exit_code "gate exits 1 when any effective-required FAIL" "1" "$rc"

# --- Test 4: SUGG entries do NOT trigger the gate (gate is required-only) ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",  "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": true},
    {"id": "base/linter",  "status": "SUGG", "detail": ".",  "description": ".", "intrinsic_required": false}
  ],
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
set +e
gate_state "$results" >/dev/null 2>&1
rc=$?
set -e
assert_exit_code "gate ignores SUGG entries" "0" "$rc"

# --- Test 5: a FAIL with intrinsic_required=false that IS in required_overrides → exit 1 ---
# This standard's YAML declared `required: false`, but the project upgraded it
# via `required:` overrides. The gate must respect the override.
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",   "status": "PASS", "detail": "ok",     "description": ".", "intrinsic_required": true},
    {"id": "base/lockfile", "status": "FAIL", "detail": "missing","description": ".", "intrinsic_required": false}
  ],
  "required_overrides": ["base/lockfile"],
  "disabled_count": 0
}
EOF
)
set +e
gate_state "$results" >/dev/null 2>&1
rc=$?
set -e
assert_exit_code "gate respects required_overrides for non-intrinsic-required FAILs" "1" "$rc"

# --- Test 6: a FAIL with intrinsic_required=false NOT in required_overrides → exit 0 ---
# An intrinsic-suggested standard that fails as SUGG should never trigger the
# gate even when status=FAIL (which can happen if the standard is genuinely
# required by intrinsic + override; but here we are testing the inverse).
# In practice merge would resolve this to SUGG, but we test gate semantics
# defensively.
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",   "status": "PASS", "detail": "ok",  "description": ".", "intrinsic_required": true},
    {"id": "base/optional", "status": "FAIL", "detail": "n/a", "description": ".", "intrinsic_required": false}
  ],
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
set +e
gate_state "$results" >/dev/null 2>&1
rc=$?
set -e
assert_exit_code "gate ignores FAIL on intrinsic-suggested-and-not-overridden entries" "0" "$rc"

# --- Test 7: malformed JSON → exit 2 (operational error) ---
state=$(mktemp -d)
printf '%s' '{not valid json' > "$state/merged.json"
set +e
"$RUNNER" --gate "$state" >/dev/null 2>&1
rc=$?
set -e
rm -rf "$state"
assert_eq "gate exits >=2 with malformed JSON" \
  "true" "$([[ $rc -ge 2 ]] && echo true || echo false)"

# --- Test 8: empty resolved array → exit 0 (no required entries, no FAILs) ---
results=$(cat <<'EOF'
{
  "resolved": [],
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
set +e
gate_state "$results" >/dev/null 2>&1
rc=$?
set -e
assert_exit_code "gate exits 0 with empty resolved array" "0" "$rc"

# --- Test 9: gate with no args prints usage and exits 1 ---
set +e
err=$("$RUNNER" --gate 2>&1 >/dev/null)
rc=$?
set -e
assert_exit_code "gate with no args exits 1 (usage)" "1" "$rc"
assert_contains "usage mentions --gate" "--gate" "$err"

summary
