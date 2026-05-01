#!/usr/bin/env bash
# Tests for `run-audit.sh --merge <collect-file> <responses-dir>`
set -euo pipefail

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REAL_SKILL_DIR="$(cd "$TEST_DIR/.." && pwd)"
RUNNER="$REAL_SKILL_DIR/scripts/run-audit.sh"

# shellcheck source=lib.sh
source "$TEST_DIR/lib.sh"

echo "test-merge.sh"

TMPDIRS=()
cleanup() {
  for d in "${TMPDIRS[@]:-}"; do
    if [[ -n "$d" && -d "$d" ]]; then
      rm -rf "$d"
    fi
  done
  return 0
}
trap cleanup EXIT

write_response() {
  local dir="$1" id="$2" content="$3"
  local path="$dir/$id.txt"
  mkdir -p "$(dirname "$path")"
  printf '%s' "$content" > "$path"
}

# --- Test 1: --merge with no args prints usage and exits 1 ---
set +e
err=$("$RUNNER" --merge 2>&1 >/dev/null)
rc=$?
set -e
assert_exit_code "merge with no args exits 1" "1" "$rc"
assert_contains "usage mentions --merge" "--merge" "$err"

# --- Test 2: happy path PASS ---
collect=$(cat <<'EOF'
{
  "resolved": [],
  "pending": [
    {"id": "base/coverage-run", "required": true, "description": "Coverage runs", "rendered_prompt": "..."}
  ],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect" > "$dir/collect.json"
mkdir -p "$dir/r"
write_response "$dir/r" "base/coverage-run" $'```json\n{"met":true,"detail":"ok"}\n```\n'
out=$("$RUNNER" --merge "$dir/collect.json" "$dir/r")
status=$(echo "$out" | jq -r '.resolved[0].status')
detail=$(echo "$out" | jq -r '.resolved[0].detail')
pending_len=$(echo "$out" | jq '.pending | length')
desc=$(echo "$out" | jq -r '.resolved[0].description')
id=$(echo "$out" | jq -r '.resolved[0].id')
assert_eq "happy path status=PASS" "PASS" "$status"
assert_eq "happy path detail" "ok" "$detail"
assert_eq "happy path pending empty" "0" "$pending_len"
assert_eq "happy path description preserved" "Coverage runs" "$desc"
assert_eq "happy path id preserved" "base/coverage-run" "$id"

# --- Test 3: met=false, required=true → FAIL ---
collect=$(cat <<'EOF'
{
  "resolved": [],
  "pending": [
    {"id": "base/x", "required": true, "description": "x", "rendered_prompt": "..."}
  ],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect" > "$dir/collect.json"
mkdir -p "$dir/r"
write_response "$dir/r" "base/x" $'```json\n{"met":false,"detail":"missing"}\n```\n'
out=$("$RUNNER" --merge "$dir/collect.json" "$dir/r")
status=$(echo "$out" | jq -r '.resolved[0].status')
assert_eq "met=false required=true → FAIL" "FAIL" "$status"

# --- Test 4: met=false, required=false → SUGG ---
collect=$(cat <<'EOF'
{
  "resolved": [],
  "pending": [
    {"id": "base/y", "required": false, "description": "y", "rendered_prompt": "..."}
  ],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect" > "$dir/collect.json"
mkdir -p "$dir/r"
write_response "$dir/r" "base/y" $'```json\n{"met":false,"detail":"absent"}\n```\n'
out=$("$RUNNER" --merge "$dir/collect.json" "$dir/r")
status=$(echo "$out" | jq -r '.resolved[0].status')
assert_eq "met=false required=false → SUGG" "SUGG" "$status"

# --- Test 5: existing resolved entries from collect are preserved ---
collect=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme", "status": "PASS", "detail": "found", "description": "README"}
  ],
  "pending": [
    {"id": "base/x", "required": true, "description": "x", "rendered_prompt": "..."}
  ],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect" > "$dir/collect.json"
mkdir -p "$dir/r"
write_response "$dir/r" "base/x" $'```json\n{"met":true,"detail":"ok"}\n```\n'
out=$("$RUNNER" --merge "$dir/collect.json" "$dir/r")
total=$(echo "$out" | jq '.resolved | length')
existing_status=$(echo "$out" | jq -r '.resolved[] | select(.id=="base/readme") | .status')
existing_detail=$(echo "$out" | jq -r '.resolved[] | select(.id=="base/readme") | .detail')
assert_eq "merged resolved length 2" "2" "$total"
assert_eq "existing PASS preserved" "PASS" "$existing_status"
assert_eq "existing detail preserved" "found" "$existing_detail"

# --- Test 6: disabled_count is preserved ---
collect=$(cat <<'EOF'
{
  "resolved": [],
  "pending": [
    {"id": "base/x", "required": true, "description": "x", "rendered_prompt": "..."}
  ],
  "disabled_count": 7
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect" > "$dir/collect.json"
mkdir -p "$dir/r"
write_response "$dir/r" "base/x" $'```json\n{"met":true,"detail":"ok"}\n```\n'
out=$("$RUNNER" --merge "$dir/collect.json" "$dir/r")
dc=$(echo "$out" | jq -r '.disabled_count')
assert_eq "disabled_count preserved" "7" "$dc"

# --- Test 7: missing response file → FAIL with detail mentioning the missing path ---
collect=$(cat <<'EOF'
{
  "resolved": [],
  "pending": [
    {"id": "base/missing", "required": true, "description": "x", "rendered_prompt": "..."}
  ],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect" > "$dir/collect.json"
mkdir -p "$dir/r"
out=$("$RUNNER" --merge "$dir/collect.json" "$dir/r")
status=$(echo "$out" | jq -r '.resolved[0].status')
detail=$(echo "$out" | jq -r '.resolved[0].detail')
assert_eq "missing response → FAIL" "FAIL" "$status"
assert_contains "detail mentions no response" "no response" "$detail"

# --- Test 8: response with no JSON block → FAIL with detail "no fenced JSON block" ---
collect=$(cat <<'EOF'
{
  "resolved": [],
  "pending": [
    {"id": "base/x", "required": true, "description": "x", "rendered_prompt": "..."}
  ],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect" > "$dir/collect.json"
mkdir -p "$dir/r"
write_response "$dir/r" "base/x" "Just some prose, no JSON block here at all."
out=$("$RUNNER" --merge "$dir/collect.json" "$dir/r")
status=$(echo "$out" | jq -r '.resolved[0].status')
detail=$(echo "$out" | jq -r '.resolved[0].detail')
assert_eq "no JSON block → FAIL" "FAIL" "$status"
assert_contains "detail mentions no JSON block" "no fenced JSON block" "$detail"

# --- Test 9: malformed JSON inside fence → FAIL with detail mentioning JSON ---
collect=$(cat <<'EOF'
{
  "resolved": [],
  "pending": [
    {"id": "base/x", "required": true, "description": "x", "rendered_prompt": "..."}
  ],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect" > "$dir/collect.json"
mkdir -p "$dir/r"
write_response "$dir/r" "base/x" $'```json\n{"met":\n```\n'
out=$("$RUNNER" --merge "$dir/collect.json" "$dir/r")
status=$(echo "$out" | jq -r '.resolved[0].status')
detail=$(echo "$out" | jq -r '.resolved[0].detail')
assert_eq "malformed JSON → FAIL" "FAIL" "$status"
assert_contains "detail mentions JSON" "JSON" "$detail"

# --- Test 10: valid JSON but met is missing or non-bool → FAIL ---
collect=$(cat <<'EOF'
{
  "resolved": [],
  "pending": [
    {"id": "base/x", "required": true, "description": "x", "rendered_prompt": "..."}
  ],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect" > "$dir/collect.json"
mkdir -p "$dir/r"
write_response "$dir/r" "base/x" $'```json\n{"met":"yes","detail":"ok"}\n```\n'
out=$("$RUNNER" --merge "$dir/collect.json" "$dir/r")
status=$(echo "$out" | jq -r '.resolved[0].status')
detail=$(echo "$out" | jq -r '.resolved[0].detail')
assert_eq "non-bool met → FAIL" "FAIL" "$status"
assert_contains "detail mentions met" "met" "$detail"

# --- Test 11: prose before JSON block → still extracts ---
collect=$(cat <<'EOF'
{
  "resolved": [],
  "pending": [
    {"id": "base/x", "required": true, "description": "x", "rendered_prompt": "..."}
  ],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect" > "$dir/collect.json"
mkdir -p "$dir/r"
write_response "$dir/r" "base/x" $'A long prose explanation.\n\nMultiple paragraphs.\n\n```json\n{"met":true,"detail":"clean"}\n```\n'
out=$("$RUNNER" --merge "$dir/collect.json" "$dir/r")
status=$(echo "$out" | jq -r '.resolved[0].status')
detail=$(echo "$out" | jq -r '.resolved[0].detail')
assert_eq "prose-before block → PASS" "PASS" "$status"
assert_eq "prose-before block → detail clean" "clean" "$detail"

# --- Test 12: runtime trailer after JSON block → still extracts ---
collect=$(cat <<'EOF'
{
  "resolved": [],
  "pending": [
    {"id": "base/x", "required": true, "description": "x", "rendered_prompt": "..."}
  ],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect" > "$dir/collect.json"
mkdir -p "$dir/r"
write_response "$dir/r" "base/x" $'```json\n{"met":true,"detail":"ok"}\n```\nagentId: a2132711ed27 (use SendMessage with to: \'a2132711\' to continue this agent)\n<usage>total_tokens: 1234 tool_uses: 2 duration_ms: 5000</usage>\n'
out=$("$RUNNER" --merge "$dir/collect.json" "$dir/r")
status=$(echo "$out" | jq -r '.resolved[0].status')
detail=$(echo "$out" | jq -r '.resolved[0].detail')
assert_eq "runtime trailer after block → PASS" "PASS" "$status"
assert_eq "runtime trailer detail" "ok" "$detail"

# --- Test 13: multiple JSON blocks → uses LAST ---
collect=$(cat <<'EOF'
{
  "resolved": [],
  "pending": [
    {"id": "base/x", "required": true, "description": "x", "rendered_prompt": "..."}
  ],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect" > "$dir/collect.json"
mkdir -p "$dir/r"
write_response "$dir/r" "base/x" $'```json\n{"met":false,"detail":"first"}\n```\nMore prose.\n```json\n{"met":true,"detail":"last"}\n```\n'
out=$("$RUNNER" --merge "$dir/collect.json" "$dir/r")
status=$(echo "$out" | jq -r '.resolved[0].status')
detail=$(echo "$out" | jq -r '.resolved[0].detail')
assert_eq "multiple blocks → uses LAST status" "PASS" "$status"
assert_eq "multiple blocks → uses LAST detail" "last" "$detail"

# --- Test 14: collect-file path doesn't exist → exit 1 ---
dir=$(mktemp -d); TMPDIRS+=("$dir")
mkdir -p "$dir/r"
set +e
err=$("$RUNNER" --merge "$dir/no-such.json" "$dir/r" 2>&1 >/dev/null)
rc=$?
set -e
assert_exit_code "missing collect file → exit 1" "1" "$rc"
assert_contains "error mentions collect" "collect" "$err"

# --- Test 15: responses-dir doesn't exist → exit 1 ---
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo '{"resolved":[],"pending":[],"disabled_count":0}' > "$dir/collect.json"
set +e
err=$("$RUNNER" --merge "$dir/collect.json" "$dir/no-such-dir" 2>&1 >/dev/null)
rc=$?
set -e
assert_exit_code "missing responses dir → exit 1" "1" "$rc"
assert_contains "error mentions responses" "responses" "$err"

# --- Test 16: collect-file accepts - (stdin) ---
collect=$(cat <<'EOF'
{
  "resolved": [],
  "pending": [
    {"id": "base/x", "required": true, "description": "x", "rendered_prompt": "..."}
  ],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
mkdir -p "$dir/r"
write_response "$dir/r" "base/x" $'```json\n{"met":true,"detail":"ok"}\n```\n'
out=$(echo "$collect" | "$RUNNER" --merge - "$dir/r")
status=$(echo "$out" | jq -r '.resolved[0].status')
assert_eq "stdin collect → PASS resolved" "PASS" "$status"

# --- Test 17: empty pending array (no merge needed) ---
collect=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme", "status": "PASS", "detail": "found", "description": "README"}
  ],
  "pending": [],
  "disabled_count": 2
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect" > "$dir/collect.json"
mkdir -p "$dir/r"
out=$("$RUNNER" --merge "$dir/collect.json" "$dir/r")
total=$(echo "$out" | jq '.resolved | length')
pending_len=$(echo "$out" | jq '.pending | length')
dc=$(echo "$out" | jq -r '.disabled_count')
assert_eq "empty-pending preserves resolved length" "1" "$total"
assert_eq "empty-pending output pending=[]" "0" "$pending_len"
assert_eq "empty-pending preserves disabled_count" "2" "$dc"

# --- Test 18: output JSON shape invariants ---
collect=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/a", "status": "PASS", "detail": ".", "description": "."},
    {"id": "base/b", "status": "FAIL", "detail": ".", "description": "."}
  ],
  "pending": [
    {"id": "base/c", "required": true, "description": "c", "rendered_prompt": "..."},
    {"id": "base/d", "required": false, "description": "d", "rendered_prompt": "..."}
  ],
  "disabled_count": 1
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect" > "$dir/collect.json"
mkdir -p "$dir/r"
write_response "$dir/r" "base/c" $'```json\n{"met":true,"detail":"c-ok"}\n```\n'
write_response "$dir/r" "base/d" $'```json\n{"met":false,"detail":"d-absent"}\n```\n'
out=$("$RUNNER" --merge "$dir/collect.json" "$dir/r")
total=$(echo "$out" | jq '.resolved | length')
pending_len=$(echo "$out" | jq '.pending | length')
dc=$(echo "$out" | jq -r '.disabled_count')
has_a=$(echo "$out" | jq '[.resolved[] | select(.id=="base/a")] | length')
has_c=$(echo "$out" | jq '[.resolved[] | select(.id=="base/c")] | length')
status_d=$(echo "$out" | jq -r '.resolved[] | select(.id=="base/d") | .status')
assert_eq "shape: resolved length = collect.resolved + pending" "4" "$total"
assert_eq "shape: pending []" "0" "$pending_len"
assert_eq "shape: disabled_count preserved" "1" "$dc"
assert_eq "shape: original resolved present" "1" "$has_a"
assert_eq "shape: pending-derived resolved present" "1" "$has_c"
assert_eq "shape: SUGG status for d (met=false, required=false)" "SUGG" "$status_d"

summary
