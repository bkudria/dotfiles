#!/usr/bin/env bash
# Tests for `run-audit.sh --render <results-json>`
set -euo pipefail

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REAL_SKILL_DIR="$(cd "$TEST_DIR/.." && pwd)"
RUNNER="$REAL_SKILL_DIR/scripts/run-audit.sh"

# shellcheck source=lib.sh
source "$TEST_DIR/lib.sh"

echo "test-render.sh"

render_state() {
  local state; state=$(mktemp -d)
  printf '%s' "$1" > "$state/merged.json"
  local rc=0
  "$RUNNER" --render "$state" || rc=$?
  rm -rf "$state"
  return $rc
}

# --- Test 1: minimal output renders header + count line; PASS row omitted from table ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme", "status": "PASS", "detail": "README.md found", "description": "A README file exists."}
  ],
  "disabled_count": 0
}
EOF
)
out=$(render_state "$results")
assert_contains "table header columns" "| Standard | Status | Detail |" "$out"
assert_contains "count line surfaces PASS total" "1 PASS, 0 FAIL, 0 SUGG" "$out"
table_section=$(printf '%s' "$out" | awk '/^\| Standard/{flag=1; next} /^$/{if(flag){flag=0}} flag')
assert_not_contains "PASS row omitted from table" "base/readme" "$table_section"
assert_not_contains "PASS detail omitted from table" "README.md found" "$table_section"

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
out=$(render_state "$results" || true)
assert_contains "counts line includes PASS" "1 PASS" "$out"
assert_contains "counts line includes FAIL" "2 FAIL" "$out"
assert_contains "counts line includes SUGG" "1 SUGG" "$out"

# --- Test 3: status-first sort, FAIL → SUGG (PASS rows are omitted from the table) ---
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
out=$(render_state "$results" || true)
table_section=$(printf '%s' "$out" | awk '/^\| Standard/{flag=1; next} /^$/{if(flag){flag=0}} flag')
fail_pos=$(printf '%s\n' "$table_section" | grep -n alpha-fail | head -1 | cut -d: -f1 || true)
sugg_pos=$(printf '%s\n' "$table_section" | grep -n mike-sugg  | head -1 | cut -d: -f1 || true)
pass_pos=$(printf '%s\n' "$table_section" | grep -n zeta-pass  | head -1 | cut -d: -f1 || true)
assert_eq "FAIL appears before SUGG" "true" "$([[ -n "$fail_pos" && -n "$sugg_pos" && $fail_pos -lt $sugg_pos ]] && echo true || echo false)"
assert_eq "PASS row omitted from table" "" "$pass_pos"

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
out=$(render_state "$results" || true)
order=$(printf '%s' "$out" | awk -F'|' '/^\| base\/(alpha|mike|zeta) /{gsub(/ /,"",$2); print $2}' | tr '\n' ' ')
assert_eq "alphabetical within FAIL bucket" "base/alpha base/mike base/zeta " "$order"

# --- Test 5: render output has no `## Remediation` section (table is the artifact) ---
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
out=$(render_state "$results" || true)
assert_not_contains "no remediation header"      "## Remediation"      "$out"
assert_not_contains "no remediation FAIL bullet" "[FAIL] base/license" "$out"
assert_not_contains "no remediation SUGG bullet" "[SUGG] base/linter"  "$out"
table_section=$(printf '%s' "$out" | awk '/^\| Standard/{flag=1; next} /^$/{if(flag){flag=0}} flag')
assert_contains "table has FAIL row" "base/license" "$table_section"
assert_contains "table has SUGG row" "base/linter"  "$table_section"
assert_not_contains "table omits PASS row" "base/readme" "$table_section"

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
out=$(render_state "$results")
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
out=$(render_state "$results")
assert_not_contains "no disabled note when count == 0" "standards disabled" "$out"

# --- Test 8: render exits 0 even when FAIL rows are present ---
# The exit-1-on-FAIL signal lives in --check, not --render. Render is a pure
# formatter so its stdout (the audit table) is not reframed as a tool ERROR
# in interactive use. CI uses --check for the pass/fail signal.
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
out=$(render_state "$results" 2>&1)
rc=$?
set -e
assert_exit_code "render exits 0 with FAIL rows (signal moved to --check)" "0" "$rc"
assert_contains "render still emits FAIL row in table" "base/license" "$out"

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
err=$(render_state "$results" 2>&1 >/dev/null)
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
render_state "$results" >/dev/null 2>&1
rc=$?
set -e
assert_exit_code "render exits 0 with no FAIL rows (PASS+SUGG)" "0" "$rc"

# --- Test S1: all-pass + eligible SUGG-style PASSing standards → suggestion block emitted ---
# An "eligible" PASSing standard is one whose underlying YAML had `required: false`
# AND is not already in the project's `required:` list. When the audit is
# completely passing (zero FAIL, zero SUGG) AND at least one such eligible
# standard exists, render emits a copy-pasteable YAML suggestion below the
# count line so the user can lock the current behaviour in.
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",   "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": true},
    {"id": "base/lockfile", "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": false}
  ],
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
out=$(render_state "$results")
assert_contains "all-pass: suggestion heading present"          "to enforce them going forward" "$out"
assert_contains "all-pass: suggestion uses YAML key 'required:'" $'\nrequired:\n'              "$out"
assert_contains "all-pass: suggestion lists eligible id"        "  - base/lockfile"            "$out"
assert_not_contains "all-pass: required-by-YAML not suggested"  "  - base/readme"              "$out"

# --- Test S2: SUGG row present → suggestion suppressed (audit not "completely passing") ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",   "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": true},
    {"id": "base/lockfile", "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": false},
    {"id": "base/linter",   "status": "SUGG", "detail": ".",  "description": ".", "intrinsic_required": false}
  ],
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
out=$(render_state "$results")
assert_not_contains "SUGG row present: no suggestion heading"  "to enforce them going forward" "$out"

# --- Test S3: FAIL row present → suggestion suppressed ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",   "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": true},
    {"id": "base/lockfile", "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": false},
    {"id": "base/license",  "status": "FAIL", "detail": ".",  "description": ".", "intrinsic_required": true}
  ],
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
out=$(render_state "$results")
assert_not_contains "FAIL row present: no suggestion heading"  "to enforce them going forward" "$out"

# --- Test S4: zero eligible PASS (all intrinsic_required=true) → suggestion suppressed ---
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
out=$(render_state "$results")
assert_not_contains "no eligible SUGG-style PASS: no suggestion" "to enforce them going forward" "$out"

# --- Test S5: every eligible PASS already in required_overrides → suggestion suppressed ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",   "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": true},
    {"id": "base/lockfile", "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": false}
  ],
  "required_overrides": ["base/lockfile"],
  "disabled_count": 0
}
EOF
)
out=$(render_state "$results")
assert_not_contains "all eligibles already in required: → no suggestion" "to enforce them going forward" "$out"

# --- Test S6: multiple eligibles → listed alphabetically ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "public/zeta",  "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": false},
    {"id": "base/lockfile","status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": false},
    {"id": "base/alpha",   "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": false}
  ],
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
out=$(render_state "$results")
sugg_section=$(printf '%s\n' "$out" | awk '/^required:/{flag=1; next} flag')
order=$(printf '%s\n' "$sugg_section" | awk '/^  - /{sub(/^  - /,""); print}' | tr '\n' ' ')
assert_eq "suggestion block lists eligibles alphabetically" "base/alpha base/lockfile public/zeta " "$order"

# ===== Two-pass render tests: skipped-suggested line + lock-in guard =====

# --- Test SK1: scopes_collected==["required"] AND any FAIL AND suggested_total > 0
#               → emit "N suggested standards skipped" line ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",  "status": "FAIL", "detail": "missing", "description": ".", "intrinsic_required": true}
  ],
  "scopes_collected": ["required"],
  "suggested_total": 5,
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
out=$(render_state "$results")
assert_contains "skipped-line: surfaces suggested_total" "5 suggested standards skipped" "$out"
assert_contains "skipped-line: mentions required failures" "required failures" "$out"

# --- Test SK2: scopes_collected==["required"] AND suggested_total == 0 → no skipped line ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",  "status": "FAIL", "detail": "missing", "description": ".", "intrinsic_required": true}
  ],
  "scopes_collected": ["required"],
  "suggested_total": 0,
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
out=$(render_state "$results")
assert_not_contains "no skipped line when suggested_total=0" "suggested standards skipped" "$out"

# --- Test SK3: scopes_collected==["required","suggested"] → no skipped line regardless ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",  "status": "FAIL", "detail": "missing", "description": ".", "intrinsic_required": true},
    {"id": "base/linter",  "status": "SUGG", "detail": ".",       "description": ".", "intrinsic_required": false}
  ],
  "scopes_collected": ["required","suggested"],
  "suggested_total": 1,
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
out=$(render_state "$results")
assert_not_contains "no skipped line when suggested scope ran" "suggested standards skipped" "$out"

# --- Test SK4: lock-in suggestion suppressed when scopes_collected lacks "suggested" ---
# Even with all required PASS and an eligible PASS-style standard, the lock-in
# suggestion can't fire because the suggested round never ran — there's no way
# to know which SUGG-style standards would have PASSed.
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",   "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": true},
    {"id": "base/lockfile", "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": false}
  ],
  "scopes_collected": ["required"],
  "suggested_total": 0,
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
out=$(render_state "$results")
assert_not_contains "lock-in suppressed when suggested scope absent" "to enforce them going forward" "$out"

# --- Test SK5: skipped line appears AFTER the count line and (if present) disabled line ---
results=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",  "status": "FAIL", "detail": "missing", "description": ".", "intrinsic_required": true}
  ],
  "scopes_collected": ["required"],
  "suggested_total": 2,
  "required_overrides": [],
  "disabled_count": 1
}
EOF
)
out=$(render_state "$results")
count_pos=$(printf '%s\n' "$out" | grep -n "PASS, 1 FAIL" | head -1 | cut -d: -f1)
skip_pos=$(printf '%s\n' "$out" | grep -n "suggested standards skipped" | head -1 | cut -d: -f1)
disabled_pos=$(printf '%s\n' "$out" | grep -n "standards disabled" | head -1 | cut -d: -f1)
assert_eq "skipped line appears after count line" "true" "$([[ -n "$skip_pos" && -n "$count_pos" && $skip_pos -gt $count_pos ]] && echo true || echo false)"
assert_eq "skipped line appears after disabled line" "true" "$([[ -n "$skip_pos" && -n "$disabled_pos" && $skip_pos -gt $disabled_pos ]] && echo true || echo false)"

summary
