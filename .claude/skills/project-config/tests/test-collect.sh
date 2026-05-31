#!/usr/bin/env bash
# Tests for `run-audit.sh --collect <project-root> <state-dir> --scope <scope>`
set -euo pipefail

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REAL_SKILL_DIR="$(cd "$TEST_DIR/.." && pwd)"
RUNNER="$REAL_SKILL_DIR/scripts/run-audit.sh"

# shellcheck source=lib.sh
source "$TEST_DIR/lib.sh"

echo "test-collect.sh"

# --- Build a fake skill root with a tiny test profile --
SKILL_TMP=$(mktemp -d)
trap 'rm -rf "$SKILL_TMP"' EXIT
mkdir -p "$SKILL_TMP/profiles/testfx"
cat > "$SKILL_TMP/profiles/testfx/marker.yaml" <<'EOF'
required: true
description: "A .marker file exists at the project root."
check:
  script: |
    cd "$PROJECT_ROOT"
    if [[ -f .marker ]]; then
      echo ".marker present"
      exit 0
    fi
    echo ".marker missing"
    exit 1
EOF

cat > "$SKILL_TMP/profiles/testfx/optional.yaml" <<'EOF'
required: false
description: "An optional .opt file exists."
check:
  script: |
    cd "$PROJECT_ROOT"
    if [[ -f .opt ]]; then
      echo ".opt present"
      exit 0
    fi
    echo ".opt missing"
    exit 1
EOF

cat > "$SKILL_TMP/profiles/testfx/manual.yaml" <<'EOF'
required: true
description: "A manual standard verified by prompt."
check:
  prompt: |
    Verify thingy at $PROJECT_ROOT.
EOF

run_collect_scope() {
  local project_root="$1" scope="$2"
  local state; state=$(mktemp -d)
  CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$project_root" "$state" --scope "$scope" >/dev/null
  cat "$state/collect-$scope.json"
  rm -rf "$state"
}

# --- Test A1: --collect without --scope errors with a clear message ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker"
state=$(mktemp -d)
set +e
err=$(CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$proj" "$state" 2>&1 >/dev/null)
rc=$?
set -e
assert_eq "--collect without --scope exits non-zero" "1" "$rc"
assert_contains "error mentions --scope requirement" "scope" "$err"
rm -rf "$proj" "$state"

# --- Test A2: --collect with invalid --scope errors ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker"
state=$(mktemp -d)
set +e
err=$(CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$proj" "$state" --scope foo 2>&1 >/dev/null)
rc=$?
set -e
assert_eq "--collect with invalid --scope exits non-zero" "1" "$rc"
assert_contains "error mentions invalid scope value" "scope" "$err"
rm -rf "$proj" "$state"

# --- Test A3: --scope required writes collect-required.json (not collect.json) ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker"
state=$(mktemp -d)
CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$proj" "$state" --scope required >/dev/null
assert_eq "--scope required writes collect-required.json" "true" "$([[ -f "$state/collect-required.json" ]] && echo true || echo false)"
assert_eq "--scope required does NOT write collect.json" "false" "$([[ -f "$state/collect.json" ]] && echo true || echo false)"
assert_eq "--scope required does NOT write collect-suggested.json" "false" "$([[ -f "$state/collect-suggested.json" ]] && echo true || echo false)"
rm -rf "$proj" "$state"

# --- Test A4: --scope suggested writes collect-suggested.json ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker"
state=$(mktemp -d)
CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$proj" "$state" --scope suggested >/dev/null
assert_eq "--scope suggested writes collect-suggested.json" "true" "$([[ -f "$state/collect-suggested.json" ]] && echo true || echo false)"
assert_eq "--scope suggested does NOT write collect.json" "false" "$([[ -f "$state/collect.json" ]] && echo true || echo false)"
rm -rf "$proj" "$state"

# --- Test 1: deterministic PASS for required, met (--scope required) ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker" "$proj/.opt"
out=$(run_collect_scope "$proj" required)
status=$(printf '%s' "$out" | jq -r '.resolved[] | select(.id=="testfx/marker") | .status')
detail=$(printf '%s' "$out" | jq -r '.resolved[] | select(.id=="testfx/marker") | .detail')
desc=$(printf '%s' "$out" | jq -r '.resolved[] | select(.id=="testfx/marker") | .description')
assert_eq "marker resolves PASS when present" "PASS" "$status"
assert_eq "marker detail comes from script stdout" ".marker present" "$detail"
assert_eq "description carried into resolved" "A .marker file exists at the project root." "$desc"
out_sugg=$(run_collect_scope "$proj" suggested)
opt_status=$(printf '%s' "$out_sugg" | jq -r '.resolved[] | select(.id=="testfx/optional") | .status')
assert_eq "optional resolves PASS when met" "PASS" "$opt_status"
rm -rf "$proj"

# --- Test 2: deterministic FAIL for required, unmet (--scope required) ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.opt"
out=$(run_collect_scope "$proj" required)
status=$(printf '%s' "$out" | jq -r '.resolved[] | select(.id=="testfx/marker") | .status')
detail=$(printf '%s' "$out" | jq -r '.resolved[] | select(.id=="testfx/marker") | .detail')
assert_eq "marker resolves FAIL when missing & required" "FAIL" "$status"
assert_eq "FAIL detail comes from script stdout" ".marker missing" "$detail"
rm -rf "$proj"

# --- Test 3: deterministic SUGG for not-required, unmet (--scope suggested) ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker"
out=$(run_collect_scope "$proj" suggested)
opt_status=$(printf '%s' "$out" | jq -r '.resolved[] | select(.id=="testfx/optional") | .status')
assert_eq "optional resolves SUGG when missing & !required" "SUGG" "$opt_status"
rm -rf "$proj"

# --- Test 4: prompt-based standard goes to pending in --scope required ---
# Prompt content lives in a per-entry file (state-dir/prompts/<id>.txt); the
# pending entry carries prompt_path, NOT rendered_prompt.
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker" "$proj/.opt"
state=$(mktemp -d)
CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$proj" "$state" --scope required >/dev/null
out=$(cat "$state/collect-required.json")
pending_count=$(printf '%s' "$out" | jq '.pending | length')
pending_id=$(printf '%s' "$out" | jq -r '.pending[0].id')
pending_required=$(printf '%s' "$out" | jq -r '.pending[0].required')
pending_prompt_path=$(printf '%s' "$out" | jq -r '.pending[0].prompt_path')
pending_has_prompt_path=$(printf '%s' "$out" | jq '.pending[0] | has("prompt_path")')
pending_has_rendered_prompt=$(printf '%s' "$out" | jq '.pending[0] | has("rendered_prompt")')
manual_in_resolved=$(printf '%s' "$out" | jq '[.resolved[] | select(.id=="testfx/manual")] | length')
assert_eq "exactly one pending entry" "1" "$pending_count"
assert_eq "pending id is correct" "testfx/manual" "$pending_id"
assert_eq "pending required flag carried" "true" "$pending_required"
assert_eq "pending entry has prompt_path field" "true" "$pending_has_prompt_path"
assert_eq "pending entry does NOT have rendered_prompt field" "false" "$pending_has_rendered_prompt"
assert_contains "prompt_path points under prompts/ in state-dir" "/prompts/" "$pending_prompt_path"
prompt_contents=$(cat "$pending_prompt_path" 2>/dev/null || echo "")
assert_contains "prompt file has \$PROJECT_ROOT substituted" "$proj" "$prompt_contents"
assert_eq "manual standard NOT in resolved" "0" "$manual_in_resolved"
has_response_path=$(printf '%s' "$out" | jq '.pending[0] | has("response_path")')
assert_eq "state-dir collect includes response_path" "true" "$has_response_path"
rm -rf "$proj" "$state"

# --- Test 5: disabled standards omitted + counted (regardless of scope) ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<EOF
profiles: [testfx]
disabled:
  testfx/optional: "Not relevant for this project"
EOF
touch "$proj/.marker"
out=$(run_collect_scope "$proj" required)
disabled_count=$(printf '%s' "$out" | jq -r '.disabled_count')
optional_in_resolved=$(printf '%s' "$out" | jq '[.resolved[] | select(.id=="testfx/optional")] | length')
optional_in_pending=$(printf '%s' "$out" | jq '[.pending[] | select(.id=="testfx/optional")] | length')
assert_eq "disabled_count == 1 (--scope required)" "1" "$disabled_count"
assert_eq "disabled standard absent from required-resolved" "0" "$optional_in_resolved"
assert_eq "disabled standard absent from required-pending" "0" "$optional_in_pending"
out_sugg=$(run_collect_scope "$proj" suggested)
optional_in_sugg=$(printf '%s' "$out_sugg" | jq '[.resolved[] | select(.id=="testfx/optional")] | length')
optional_in_sugg_pending=$(printf '%s' "$out_sugg" | jq '[.pending[] | select(.id=="testfx/optional")] | length')
assert_eq "disabled standard absent from suggested-resolved" "0" "$optional_in_sugg"
assert_eq "disabled standard absent from suggested-pending" "0" "$optional_in_sugg_pending"
rm -rf "$proj"

# --- Test 6: malformed standard (neither script nor prompt) is a runner error ---
mkdir -p "$SKILL_TMP/profiles/badfx"
cat > "$SKILL_TMP/profiles/badfx/empty.yaml" <<'EOF'
required: true
description: "Has neither script nor prompt."
check: {}
EOF
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [badfx]
EOF
state=$(mktemp -d)
set +e
err=$(CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$proj" "$state" --scope required 2>&1 >/dev/null)
rc=$?
set -e
assert_eq "malformed standard exits non-zero" "1" "$rc"
assert_contains "malformed standard error mentions id" "badfx/empty" "$err"
rm -rf "$proj" "$state"
rm -rf "$SKILL_TMP/profiles/badfx"

# --- Test R1: SUGG standard listed in required: + unmet → resolved as FAIL,
#              and routed into --scope required (not suggested) ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
required:
  - testfx/optional
EOF
touch "$proj/.marker"
out=$(run_collect_scope "$proj" required)
opt_status=$(printf '%s' "$out" | jq -r '.resolved[] | select(.id=="testfx/optional") | .status')
assert_eq "optional upgraded to FAIL via required: list" "FAIL" "$opt_status"
out_sugg=$(run_collect_scope "$proj" suggested)
opt_in_sugg=$(printf '%s' "$out_sugg" | jq '[.resolved[] | select(.id=="testfx/optional")] | length')
assert_eq "overridden SUGG-intrinsic standard NOT in suggested scope" "0" "$opt_in_sugg"
rm -rf "$proj"

# --- Test R2: SUGG standard NOT in required: still resolves SUGG when unmet ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker"
out=$(run_collect_scope "$proj" suggested)
opt_status=$(printf '%s' "$out" | jq -r '.resolved[] | select(.id=="testfx/optional") | .status')
assert_eq "optional stays SUGG without required: override" "SUGG" "$opt_status"
rm -rf "$proj"

# --- Test R3: prompt SUGG standard listed in required: → pending entry has required:true ---
mkdir -p "$SKILL_TMP/profiles/promptfx"
cat > "$SKILL_TMP/profiles/promptfx/sugg-prompt.yaml" <<'EOF'
required: false
description: "A suggestion verified by prompt."
check:
  prompt: |
    Verify thingy at $PROJECT_ROOT.
EOF
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [promptfx]
required:
  - promptfx/sugg-prompt
EOF
out=$(run_collect_scope "$proj" required)
pending_required=$(printf '%s' "$out" | jq -r '.pending[] | select(.id=="promptfx/sugg-prompt") | .required')
assert_eq "prompt SUGG promoted to required:true in pending" "true" "$pending_required"
rm -rf "$proj"
rm -rf "$SKILL_TMP/profiles/promptfx"

# --- Test S1: collect-required.json carries intrinsic_required on each resolved entry ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker" "$proj/.opt"
out=$(run_collect_scope "$proj" required)
out_sugg=$(run_collect_scope "$proj" suggested)
marker_intrinsic=$(printf '%s' "$out" | jq -r '.resolved[] | select(.id=="testfx/marker") | .intrinsic_required')
opt_intrinsic=$(printf '%s' "$out_sugg" | jq -r '.resolved[] | select(.id=="testfx/optional") | .intrinsic_required')
assert_eq "intrinsic_required reflects standard YAML (required: true)"  "true"  "$marker_intrinsic"
assert_eq "intrinsic_required reflects standard YAML (required: false)" "false" "$opt_intrinsic"
rm -rf "$proj"

# --- Test S2: pending entries carry intrinsic_required even when overridden ---
mkdir -p "$SKILL_TMP/profiles/promptfx2"
cat > "$SKILL_TMP/profiles/promptfx2/sugg-prompt.yaml" <<'EOF'
required: false
description: "A suggestion verified by prompt."
check:
  prompt: |
    Verify thingy at $PROJECT_ROOT.
EOF
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [promptfx2]
required:
  - promptfx2/sugg-prompt
EOF
out=$(run_collect_scope "$proj" required)
pending_required=$(printf '%s' "$out" | jq -r '.pending[] | select(.id=="promptfx2/sugg-prompt") | .required')
pending_intrinsic=$(printf '%s' "$out" | jq -r '.pending[] | select(.id=="promptfx2/sugg-prompt") | .intrinsic_required')
assert_eq "pending effective required reflects override (true)"   "true"  "$pending_required"
assert_eq "pending intrinsic_required reflects YAML (false)"      "false" "$pending_intrinsic"
rm -rf "$proj"
rm -rf "$SKILL_TMP/profiles/promptfx2"

# --- Test S3: collect-required.json carries required_overrides at top level ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
required:
  - testfx/optional
EOF
touch "$proj/.marker" "$proj/.opt"
out=$(run_collect_scope "$proj" required)
overrides_len=$(printf '%s' "$out" | jq '.required_overrides | length')
overrides_first=$(printf '%s' "$out" | jq -r '.required_overrides[0]')
assert_eq "required_overrides length matches project.yaml" "1" "$overrides_len"
assert_eq "required_overrides entry is the listed id" "testfx/optional" "$overrides_first"
rm -rf "$proj"

# --- Test S4: required_overrides is [] when project.yaml omits required: ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker"
out=$(run_collect_scope "$proj" required)
overrides_len=$(printf '%s' "$out" | jq '.required_overrides | length')
overrides_type=$(printf '%s' "$out" | jq -r '.required_overrides | type')
assert_eq "required_overrides is empty when omitted" "0"     "$overrides_len"
assert_eq "required_overrides is array when omitted" "array" "$overrides_type"
rm -rf "$proj"

# --- Test 7: project.yaml with unknown top-level keys → fail-fast via lint ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
name: example
description: example project
language: ruby
standards:
  testing: rspec
EOF
touch "$proj/.marker" "$proj/.opt"
state=$(mktemp -d)
set +e
err=$(CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$proj" "$state" --scope required 2>&1 >/dev/null)
rc=$?
set -e
assert_eq "schema-invalid project.yaml exits non-zero" "1" "$rc"
assert_contains "lint error mentions unexpected key" "unexpected" "$err"
rm -rf "$proj" "$state"

# ===== New tests for two-pass behavior =====

# --- Test B1: --scope required walks ONLY effective-required standards ---
# testfx has marker (required), optional (suggested), manual (required).
# --scope required should include marker + manual; optional must be absent.
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker"
out=$(run_collect_scope "$proj" required)
has_marker=$(printf '%s' "$out" | jq '[.resolved[] | select(.id=="testfx/marker")] | length')
has_manual=$(printf '%s' "$out" | jq '[.pending[]  | select(.id=="testfx/manual")] | length')
has_optional_resolved=$(printf '%s' "$out" | jq '[.resolved[] | select(.id=="testfx/optional")] | length')
has_optional_pending=$(printf '%s' "$out" | jq '[.pending[]  | select(.id=="testfx/optional")] | length')
assert_eq "required scope includes marker"   "1" "$has_marker"
assert_eq "required scope includes manual"   "1" "$has_manual"
assert_eq "required scope EXCLUDES optional from resolved" "0" "$has_optional_resolved"
assert_eq "required scope EXCLUDES optional from pending"  "0" "$has_optional_pending"
rm -rf "$proj"

# --- Test B2: --scope suggested walks ONLY effective-suggested standards ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker" "$proj/.opt"
out=$(run_collect_scope "$proj" suggested)
has_optional=$(printf '%s' "$out" | jq '[.resolved[] | select(.id=="testfx/optional")] | length')
has_marker=$(printf '%s' "$out" | jq '[.resolved[] | select(.id=="testfx/marker")] | length')
has_manual=$(printf '%s' "$out" | jq '[.pending[]  | select(.id=="testfx/manual")] | length')
assert_eq "suggested scope includes optional" "1" "$has_optional"
assert_eq "suggested scope EXCLUDES marker"   "0" "$has_marker"
assert_eq "suggested scope EXCLUDES manual"   "0" "$has_manual"
rm -rf "$proj"

# --- Test C1: collect-required.json includes suggested_total counting effective-suggesteds ---
# testfx has marker (req) + optional (sugg) + manual (req).
# Without overrides: suggested_total should be 1 (optional only).
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker"
out=$(run_collect_scope "$proj" required)
suggested_total=$(printf '%s' "$out" | jq -r '.suggested_total')
assert_eq "suggested_total counts effective-suggested standards" "1" "$suggested_total"
rm -rf "$proj"

# --- Test C2: suggested_total is 0 when all standards are required (intrinsic or override) ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
required:
  - testfx/optional
EOF
touch "$proj/.marker"
out=$(run_collect_scope "$proj" required)
suggested_total=$(printf '%s' "$out" | jq -r '.suggested_total')
assert_eq "suggested_total=0 when all standards effective-required" "0" "$suggested_total"
rm -rf "$proj"

# --- Test C3: disabled standards do NOT count in suggested_total ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<EOF
profiles: [testfx]
disabled:
  testfx/optional: "not relevant"
EOF
touch "$proj/.marker"
out=$(run_collect_scope "$proj" required)
suggested_total=$(printf '%s' "$out" | jq -r '.suggested_total')
assert_eq "disabled effective-suggested standards excluded from suggested_total" "0" "$suggested_total"
rm -rf "$proj"

# --- Test SZ1: collect-<scope>.json size is bounded by metadata, not prompt body.
#               With 50 prompt-based standards each carrying a ~1500-char prompt
#               body, the index file must remain well under the Read tool's 25k
#               token cap (~100k bytes for ASCII; we assert < 60k for headroom). ---
mkdir -p "$SKILL_TMP/profiles/big"
big_body=""
# Build a ~1500-char body using a deterministic filler.
for _ in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
  big_body+="Verify standard at \$PROJECT_ROOT and confirm it meets the bar. "
done
for i in $(seq 1 50); do
  cat > "$SKILL_TMP/profiles/big/std-$i.yaml" <<EOF
required: false
description: "Big test standard $i."
check:
  prompt: |
    $big_body
EOF
done
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [big]
EOF
state=$(mktemp -d)
CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$proj" "$state" --scope suggested >/dev/null
size=$(wc -c < "$state/collect-suggested.json" | tr -d ' ')
[[ $size -lt 60000 ]] && under_limit=true || under_limit=false
assert_eq "collect-suggested.json under 60k bytes for 50 prompt standards" "true" "$under_limit"
rm -rf "$proj" "$state"
rm -rf "$SKILL_TMP/profiles/big"

# --- Test W1: workflows/audit.md delegates per-standard verification to the
#              verify.js Workflow in both rounds, and no longer drives the
#              fan-out via single-message dispatch GATEs in the main thread. ---
WORKFLOW="$REAL_SKILL_DIR/workflows/audit.md"

verify_ref_count=$(grep -c "verify.js" "$WORKFLOW" || true)
[[ $verify_ref_count -ge 2 ]] && delegates_both_rounds=true || delegates_both_rounds=false
assert_eq "workflows/audit.md dispatches verify.js in both rounds" "true" "$delegates_both_rounds"

workflow_ref_count=$(grep -c "Workflow" "$WORKFLOW" || true)
[[ $workflow_ref_count -ge 2 ]] && uses_workflow_tool=true || uses_workflow_tool=false
assert_eq "workflows/audit.md dispatches via the Workflow tool" "true" "$uses_workflow_tool"

old_dispatch_gate=$(grep -c "Single-message dispatch" "$WORKFLOW" || true)
assert_eq "workflows/audit.md has dropped the single-message dispatch GATEs" "0" "$old_dispatch_gate"

old_count_gate=$(grep -c "count the Agent tool_use blocks" "$WORKFLOW" || true)
assert_eq "workflows/audit.md no longer asks Claude to count Agent tool_use blocks" "0" "$old_count_gate"

prompt_path_count=$(grep -c "prompt_path" "$WORKFLOW" || true)
[[ $prompt_path_count -ge 2 ]] && enough_prompt_path=true || enough_prompt_path=false
assert_eq "workflows/audit.md still references the per-entry prompt_path file handshake" "true" "$enough_prompt_path"

summary
