#!/usr/bin/env bash
# Tests for `run-audit.sh --render <results-json>`
set -euo pipefail

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REAL_SKILL_DIR="$(cd "$TEST_DIR/.." && pwd)"
RUNNER="$REAL_SKILL_DIR/scripts/run-audit.sh"

# shellcheck source=lib.sh
source "$TEST_DIR/lib.sh"

echo "test-render.sh"

# --- Test 1: minimal table renders with PASS row ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme", "status": "PASS", "detail": "README.md found", "description": "A README file exists."}
  ],
  "disabled_count": 0
}
EOF
)
out=$(echo "$results" | "$RUNNER" --render -)
assert_contains "table header columns" "| Standard | Status | Detail |" "$out"
assert_contains "PASS row id" "base/readme" "$out"
assert_contains "PASS row status" "PASS" "$out"
assert_contains "PASS row detail" "README.md found" "$out"

# --- Test 2: per-status counts ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme", "status": "PASS", "detail": "ok", "description": "."},
    {"id": "base/license", "status": "FAIL", "detail": "missing", "description": "."},
    {"id": "base/tests",   "status": "FAIL", "detail": "no spec", "description": "."},
    {"id": "base/linter",  "status": "SUGG", "detail": "no config", "description": "."}
  ],
  "disabled_count": 0
}
EOF
)
out=$(echo "$results" | "$RUNNER" --render - || true)
assert_contains "counts line includes PASS" "1 PASS" "$out"
assert_contains "counts line includes FAIL" "2 FAIL" "$out"
assert_contains "counts line includes SUGG" "1 SUGG" "$out"

# --- Test 3: status-first sort, FAIL → SUGG → PASS ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/zeta-pass",  "status": "PASS", "detail": ".", "description": "."},
    {"id": "base/alpha-fail", "status": "FAIL", "detail": ".", "description": "."},
    {"id": "base/mike-sugg",  "status": "SUGG", "detail": ".", "description": "."}
  ],
  "disabled_count": 0
}
EOF
)
out=$(echo "$results" | "$RUNNER" --render - || true)
table_section=$(printf '%s' "$out" | awk '/^\| Standard/{flag=1; next} /^$/{if(flag){flag=0}} flag')
fail_pos=$(printf '%s\n' "$table_section" | grep -n alpha-fail | head -1 | cut -d: -f1)
sugg_pos=$(printf '%s\n' "$table_section" | grep -n mike-sugg | head -1 | cut -d: -f1)
pass_pos=$(printf '%s\n' "$table_section" | grep -n zeta-pass | head -1 | cut -d: -f1)
assert_eq "FAIL appears before SUGG" "true" "$([[ -n "$fail_pos" && -n "$sugg_pos" && $fail_pos -lt $sugg_pos ]] && echo true || echo false)"
assert_eq "SUGG appears before PASS" "true" "$([[ -n "$sugg_pos" && -n "$pass_pos" && $sugg_pos -lt $pass_pos ]] && echo true || echo false)"

# --- Test 4: alphabetical within each bucket ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/zeta",  "status": "FAIL", "detail": ".", "description": "."},
    {"id": "base/alpha", "status": "FAIL", "detail": ".", "description": "."},
    {"id": "base/mike",  "status": "FAIL", "detail": ".", "description": "."}
  ],
  "disabled_count": 0
}
EOF
)
out=$(echo "$results" | "$RUNNER" --render - || true)
order=$(printf '%s' "$out" | awk -F'|' '/^\| base\/(alpha|mike|zeta) /{gsub(/ /,"",$2); print $2}' | tr '\n' ' ')
assert_eq "alphabetical within FAIL bucket" "base/alpha base/mike base/zeta " "$order"

# --- Test 5: remediation list lists FAIL and SUGG, not PASS ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",  "status": "PASS", "detail": "ok",      "description": "README exists"},
    {"id": "base/license", "status": "FAIL", "detail": "missing", "description": "LICENSE file"},
    {"id": "base/linter",  "status": "SUGG", "detail": "no cfg",  "description": "Linter configured"}
  ],
  "disabled_count": 0
}
EOF
)
out=$(echo "$results" | "$RUNNER" --render - || true)
remediation=$(printf '%s' "$out" | awk '/^## Remediation/{flag=1; next} flag')
assert_contains "remediation has FAIL entry" "base/license" "$remediation"
assert_contains "remediation has SUGG entry" "base/linter" "$remediation"
assert_not_contains "remediation omits PASS entry" "base/readme" "$remediation"
assert_contains "remediation includes description" "LICENSE file" "$remediation"

# --- Test 6: disabled_count > 0 surfaces a one-line note ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme", "status": "PASS", "detail": ".", "description": "."}
  ],
  "disabled_count": 3
}
EOF
)
out=$(echo "$results" | "$RUNNER" --render -)
assert_contains "disabled note when count > 0" "3 standards disabled in project.yaml" "$out"

# --- Test 7: disabled_count == 0 omits the note ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme", "status": "PASS", "detail": ".", "description": "."}
  ],
  "disabled_count": 0
}
EOF
)
out=$(echo "$results" | "$RUNNER" --render -)
assert_not_contains "no disabled note when count == 0" "standards disabled" "$out"

# --- Test 8: render exits 1 when FAIL rows are present ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/license", "status": "FAIL", "detail": ".", "description": "."}
  ],
  "disabled_count": 0
}
EOF
)
set +e
echo "$results" | "$RUNNER" --render - >/dev/null 2>&1
rc=$?
set -e
assert_exit_code "render exits 1 with FAIL rows" "1" "$rc"

# --- Test 9: render reads from a file path argument ---
tmpfile=$(mktemp)
trap 'rm -f "$tmpfile"' EXIT
cat > "$tmpfile" <<'EOF'
{
  "resolved": [
    {"id": "base/readme", "status": "PASS", "detail": "fp", "description": "."}
  ],
  "disabled_count": 0
}
EOF
out=$("$RUNNER" --render "$tmpfile")
assert_contains "render reads from file path" "base/readme" "$out"

# --- Test 10: render rejects input with unresolved pending entries ---
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
err=$(echo "$results" | "$RUNNER" --render - 2>&1 >/dev/null)
rc=$?
set -e
assert_exit_code "render rejects unresolved pending" "1" "$rc"
assert_contains "error mentions pending id" "base/linter" "$err"

# --- Test 11: render exits 0 when only PASS/SUGG rows (no FAIL) ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",  "status": "PASS", "detail": ".", "description": "."},
    {"id": "base/linter",  "status": "SUGG", "detail": ".", "description": "."}
  ],
  "disabled_count": 0
}
EOF
)
set +e
echo "$results" | "$RUNNER" --render - >/dev/null 2>&1
rc=$?
set -e
assert_exit_code "render exits 0 with no FAIL rows (PASS+SUGG)" "0" "$rc"

summary
