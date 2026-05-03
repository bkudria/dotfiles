#!/usr/bin/env bash
# Tests for `run-audit.sh --merge <state-dir>`
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
mkdir -p "$dir/responses"
write_response "$dir/responses" "base/coverage-run" '{"met":true,"detail":"ok"}'
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
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
mkdir -p "$dir/responses"
write_response "$dir/responses" "base/x" '{"met":false,"detail":"missing"}'
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
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
mkdir -p "$dir/responses"
write_response "$dir/responses" "base/y" '{"met":false,"detail":"absent"}'
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
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
mkdir -p "$dir/responses"
write_response "$dir/responses" "base/x" '{"met":true,"detail":"ok"}'
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
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
mkdir -p "$dir/responses"
write_response "$dir/responses" "base/x" '{"met":true,"detail":"ok"}'
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
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
mkdir -p "$dir/responses"
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
status=$(echo "$out" | jq -r '.resolved[0].status')
detail=$(echo "$out" | jq -r '.resolved[0].detail')
assert_eq "missing response → FAIL" "FAIL" "$status"
assert_contains "detail mentions no response" "no response" "$detail"

# --- Test 8: response with no JSON payload (neither fenced block nor raw JSON) → FAIL ---
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
mkdir -p "$dir/responses"
write_response "$dir/responses" "base/x" "Just some prose, no JSON block here at all."
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
status=$(echo "$out" | jq -r '.resolved[0].status')
detail=$(echo "$out" | jq -r '.resolved[0].detail')
assert_eq "no JSON payload → FAIL" "FAIL" "$status"
assert_contains "detail mentions no JSON payload" "no JSON payload" "$detail"

# --- Test 9: malformed raw JSON → FAIL with detail mentioning JSON ---
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
mkdir -p "$dir/responses"
write_response "$dir/responses" "base/x" '{"met":'
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
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
mkdir -p "$dir/responses"
write_response "$dir/responses" "base/x" '{"met":"yes","detail":"ok"}'
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
status=$(echo "$out" | jq -r '.resolved[0].status')
detail=$(echo "$out" | jq -r '.resolved[0].detail')
assert_eq "non-bool met → FAIL" "FAIL" "$status"
assert_contains "detail mentions met" "met" "$detail"

# --- Test 11: prose around fenced JSON → FAIL (directive forbids prose / fence) ---
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
mkdir -p "$dir/responses"
write_response "$dir/responses" "base/x" $'A long prose explanation.\n\nMultiple paragraphs.\n\n```json\n{"met":true,"detail":"clean"}\n```\n'
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
status=$(echo "$out" | jq -r '.resolved[0].status')
detail=$(echo "$out" | jq -r '.resolved[0].detail')
assert_eq "prose-before-fence → FAIL" "FAIL" "$status"
assert_contains "prose-before-fence detail mentions JSON" "JSON" "$detail"

# --- Test 11b: contract-compliant raw JSON (no fence, no prose) → PASS ---
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
mkdir -p "$dir/responses"
write_response "$dir/responses" "base/x" '{"met":true,"detail":"raw"}'
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
status=$(echo "$out" | jq -r '.resolved[0].status')
detail=$(echo "$out" | jq -r '.resolved[0].detail')
assert_eq "raw JSON (no fence) → PASS" "PASS" "$status"
assert_eq "raw JSON detail" "raw" "$detail"

# --- Test 12: fenced JSON response → FAIL (directive forbids fenced code block) ---
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
mkdir -p "$dir/responses"
write_response "$dir/responses" "base/x" $'```json\n{"met":true,"detail":"ok"}\n```\n'
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
status=$(echo "$out" | jq -r '.resolved[0].status')
detail=$(echo "$out" | jq -r '.resolved[0].detail')
assert_eq "fenced JSON → FAIL" "FAIL" "$status"
assert_contains "fenced JSON detail mentions JSON" "JSON" "$detail"

# --- Test 13: multiple fenced JSON blocks → FAIL (directive forbids fenced code block) ---
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
mkdir -p "$dir/responses"
write_response "$dir/responses" "base/x" $'```json\n{"met":false,"detail":"first"}\n```\nMore prose.\n```json\n{"met":true,"detail":"last"}\n```\n'
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
status=$(echo "$out" | jq -r '.resolved[0].status')
detail=$(echo "$out" | jq -r '.resolved[0].detail')
assert_eq "multiple fenced blocks → FAIL" "FAIL" "$status"
assert_contains "multiple-fenced detail mentions JSON" "JSON" "$detail"

# --- Test 14: state-dir missing collect.json → exit 1 ---
dir=$(mktemp -d); TMPDIRS+=("$dir")
mkdir -p "$dir/responses"
set +e
err=$("$RUNNER" --merge "$dir" 2>&1 >/dev/null)
rc=$?
set -e
assert_exit_code "missing collect.json → exit 1" "1" "$rc"
assert_contains "error mentions collect" "collect" "$err"

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
mkdir -p "$dir/responses"
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
total=$(echo "$out" | jq '.resolved | length')
pending_len=$(echo "$out" | jq '.pending | length')
dc=$(echo "$out" | jq -r '.disabled_count')
assert_eq "empty-pending preserves resolved length" "1" "$total"
assert_eq "empty-pending output pending=[]" "0" "$pending_len"
assert_eq "empty-pending preserves disabled_count" "2" "$dc"

# --- Test S1: merge propagates required_overrides from collect to merged ---
# The render layer needs the project's `required:` list to filter out
# already-listed entries from the suggestion block, so merge must not drop it.
collect=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme", "status": "PASS", "detail": ".", "description": ".", "intrinsic_required": true}
  ],
  "pending": [],
  "required_overrides": ["base/lockfile"],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect" > "$dir/collect.json"
mkdir -p "$dir/responses"
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
overrides_len=$(echo "$out" | jq '.required_overrides | length')
overrides_first=$(echo "$out" | jq -r '.required_overrides[0]')
assert_eq "merge propagates required_overrides length" "1" "$overrides_len"
assert_eq "merge propagates required_overrides entry" "base/lockfile" "$overrides_first"

# --- Test S2: merge propagates intrinsic_required for collect-resolved entries ---
collect=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme",   "status": "PASS", "detail": ".", "description": ".", "intrinsic_required": true},
    {"id": "base/lockfile", "status": "PASS", "detail": ".", "description": ".", "intrinsic_required": false}
  ],
  "pending": [],
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect" > "$dir/collect.json"
mkdir -p "$dir/responses"
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
readme_intrinsic=$(echo "$out" | jq -r '.resolved[] | select(.id=="base/readme") | .intrinsic_required')
lock_intrinsic=$(echo "$out" | jq -r '.resolved[] | select(.id=="base/lockfile") | .intrinsic_required')
assert_eq "intrinsic_required preserved for resolved (true)"  "true"  "$readme_intrinsic"
assert_eq "intrinsic_required preserved for resolved (false)" "false" "$lock_intrinsic"

# --- Test S3: merge carries intrinsic_required from pending entries ---
# When a pending entry resolves through merge, the resulting resolved row must
# also expose intrinsic_required so render can apply the SUGG-style filter
# uniformly regardless of check kind.
collect=$(cat <<'EOF'
{
  "resolved": [],
  "pending": [
    {"id": "base/p1", "required": false, "intrinsic_required": false, "description": ".", "rendered_prompt": "..."},
    {"id": "base/p2", "required": true,  "intrinsic_required": true,  "description": ".", "rendered_prompt": "..."}
  ],
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect" > "$dir/collect.json"
mkdir -p "$dir/responses"
write_response "$dir/responses" "base/p1" '{"met":true,"detail":"ok"}'
write_response "$dir/responses" "base/p2" '{"met":true,"detail":"ok"}'
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
p1_intrinsic=$(echo "$out" | jq -r '.resolved[] | select(.id=="base/p1") | .intrinsic_required')
p2_intrinsic=$(echo "$out" | jq -r '.resolved[] | select(.id=="base/p2") | .intrinsic_required')
assert_eq "pending→resolved intrinsic_required preserved (false)" "false" "$p1_intrinsic"
assert_eq "pending→resolved intrinsic_required preserved (true)"  "true"  "$p2_intrinsic"

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
mkdir -p "$dir/responses"
write_response "$dir/responses" "base/c" '{"met":true,"detail":"c-ok"}'
write_response "$dir/responses" "base/d" '{"met":false,"detail":"d-absent"}'
out=$("$RUNNER" --merge "$dir" && cat "$dir/merged.json")
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

# ===== Two-pass merge tests =====

# --- Test M1: --merge with only collect-required.json sets scopes_collected=["required"] ---
collect_req=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme", "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": true}
  ],
  "pending": [],
  "required_overrides": [],
  "disabled_count": 0,
  "suggested_total": 3
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect_req" > "$dir/collect-required.json"
mkdir -p "$dir/responses"
"$RUNNER" --merge "$dir" >/dev/null
sc=$(jq -c '.scopes_collected' "$dir/merged.json")
assert_eq "merge with only collect-required.json sets scopes_collected" '["required"]' "$sc"
st=$(jq -r '.suggested_total' "$dir/merged.json")
assert_eq "merge propagates suggested_total from collect-required.json" "3" "$st"
n=$(jq '.resolved | length' "$dir/merged.json")
assert_eq "merge with required only emits one resolved entry" "1" "$n"

# --- Test M2: --merge with both collect files sets scopes_collected=["required","suggested"] ---
collect_req=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/readme", "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": true}
  ],
  "pending": [],
  "required_overrides": [],
  "disabled_count": 0,
  "suggested_total": 1
}
EOF
)
collect_sugg=$(cat <<'EOF'
{
  "resolved": [
    {"id": "base/lockfile", "status": "PASS", "detail": "ok", "description": ".", "intrinsic_required": false}
  ],
  "pending": [],
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect_req"  > "$dir/collect-required.json"
echo "$collect_sugg" > "$dir/collect-suggested.json"
mkdir -p "$dir/responses"
"$RUNNER" --merge "$dir" >/dev/null
sc=$(jq -c '.scopes_collected' "$dir/merged.json")
assert_eq "merge with both collect files records both scopes" '["required","suggested"]' "$sc"
n=$(jq '.resolved | length' "$dir/merged.json")
assert_eq "merge with both collects unions resolved arrays" "2" "$n"
has_lockfile=$(jq '[.resolved[] | select(.id=="base/lockfile")] | length' "$dir/merged.json")
assert_eq "merge includes suggested-scope entry" "1" "$has_lockfile"

# --- Test M3: re-running --merge after adding collect-suggested.json overwrites
#              the prior round-1 merged.json with the union ---
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect_req" > "$dir/collect-required.json"
mkdir -p "$dir/responses"
"$RUNNER" --merge "$dir" >/dev/null
n_round1=$(jq '.resolved | length' "$dir/merged.json")
assert_eq "round-1 merge: one entry" "1" "$n_round1"
# Now add collect-suggested.json and re-merge
echo "$collect_sugg" > "$dir/collect-suggested.json"
"$RUNNER" --merge "$dir" >/dev/null
n_round2=$(jq '.resolved | length' "$dir/merged.json")
sc_round2=$(jq -c '.scopes_collected' "$dir/merged.json")
assert_eq "round-2 merge overwrites with union: two entries" "2" "$n_round2"
assert_eq "round-2 merge updates scopes_collected" '["required","suggested"]' "$sc_round2"

# --- Test M4: suggested_total defaults to 0 in merged.json when omitted ---
collect_legacy=$(cat <<'EOF'
{
  "resolved": [],
  "pending": [],
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect_legacy" > "$dir/collect.json"
mkdir -p "$dir/responses"
"$RUNNER" --merge "$dir" >/dev/null
st=$(jq -r '.suggested_total' "$dir/merged.json")
assert_eq "merge defaults suggested_total to 0 when collect lacks it" "0" "$st"

# --- Test M5: pending entries from BOTH collect files are dispatched in merge ---
# Verifies that pending arrays union correctly and sub-agent responses for
# either scope are honored.
collect_req=$(cat <<'EOF'
{
  "resolved": [],
  "pending": [
    {"id": "base/p-req", "required": true, "intrinsic_required": true, "description": ".", "rendered_prompt": "..."}
  ],
  "required_overrides": [],
  "disabled_count": 0,
  "suggested_total": 1
}
EOF
)
collect_sugg=$(cat <<'EOF'
{
  "resolved": [],
  "pending": [
    {"id": "base/p-sugg", "required": false, "intrinsic_required": false, "description": ".", "rendered_prompt": "..."}
  ],
  "required_overrides": [],
  "disabled_count": 0
}
EOF
)
dir=$(mktemp -d); TMPDIRS+=("$dir")
echo "$collect_req"  > "$dir/collect-required.json"
echo "$collect_sugg" > "$dir/collect-suggested.json"
mkdir -p "$dir/responses/base"
write_response "$dir/responses" "base/p-req"  '{"met":true,"detail":"ok"}'
write_response "$dir/responses" "base/p-sugg" '{"met":false,"detail":"absent"}'
"$RUNNER" --merge "$dir" >/dev/null
status_req=$(jq -r '.resolved[] | select(.id=="base/p-req") | .status' "$dir/merged.json")
status_sugg=$(jq -r '.resolved[] | select(.id=="base/p-sugg") | .status' "$dir/merged.json")
assert_eq "merge resolves required pending across both collect files" "PASS" "$status_req"
assert_eq "merge resolves suggested pending across both collect files" "SUGG" "$status_sugg"

summary
