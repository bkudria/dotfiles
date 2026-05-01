#!/usr/bin/env bash
# Tests for `run-audit.sh --collect <project-root>`
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

run_collect() {
  local project_root="$1"
  CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$project_root"
}

# --- Test 1: deterministic PASS for required, met ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker" "$proj/.opt"
out=$(run_collect "$proj")
status=$(printf '%s' "$out" | jq -r '.resolved[] | select(.id=="testfx/marker") | .status')
detail=$(printf '%s' "$out" | jq -r '.resolved[] | select(.id=="testfx/marker") | .detail')
desc=$(printf '%s' "$out" | jq -r '.resolved[] | select(.id=="testfx/marker") | .description')
assert_eq "marker resolves PASS when present" "PASS" "$status"
assert_eq "marker detail comes from script stdout" ".marker present" "$detail"
assert_eq "description carried into resolved" "A .marker file exists at the project root." "$desc"
opt_status=$(printf '%s' "$out" | jq -r '.resolved[] | select(.id=="testfx/optional") | .status')
assert_eq "optional resolves PASS when met" "PASS" "$opt_status"
rm -rf "$proj"

# --- Test 2: deterministic FAIL for required, unmet ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.opt"
out=$(run_collect "$proj")
status=$(printf '%s' "$out" | jq -r '.resolved[] | select(.id=="testfx/marker") | .status')
detail=$(printf '%s' "$out" | jq -r '.resolved[] | select(.id=="testfx/marker") | .detail')
assert_eq "marker resolves FAIL when missing & required" "FAIL" "$status"
assert_eq "FAIL detail comes from script stdout" ".marker missing" "$detail"
rm -rf "$proj"

# --- Test 3: deterministic SUGG for not-required, unmet ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker"
out=$(run_collect "$proj")
opt_status=$(printf '%s' "$out" | jq -r '.resolved[] | select(.id=="testfx/optional") | .status')
assert_eq "optional resolves SUGG when missing & !required" "SUGG" "$opt_status"
rm -rf "$proj"

# --- Test 4: prompt-based standard goes to pending, not resolved ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker" "$proj/.opt"
out=$(run_collect "$proj")
pending_count=$(printf '%s' "$out" | jq '.pending | length')
pending_id=$(printf '%s' "$out" | jq -r '.pending[0].id')
pending_required=$(printf '%s' "$out" | jq -r '.pending[0].required')
pending_prompt=$(printf '%s' "$out" | jq -r '.pending[0].rendered_prompt')
manual_in_resolved=$(printf '%s' "$out" | jq '[.resolved[] | select(.id=="testfx/manual")] | length')
assert_eq "exactly one pending entry" "1" "$pending_count"
assert_eq "pending id is correct" "testfx/manual" "$pending_id"
assert_eq "pending required flag carried" "true" "$pending_required"
assert_contains "rendered_prompt has \$PROJECT_ROOT substituted" "$proj" "$pending_prompt"
assert_eq "manual standard NOT in resolved" "0" "$manual_in_resolved"
rm -rf "$proj"

# --- Test 5: disabled standards omitted from both arrays + counted ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<EOF
profiles: [testfx]
disabled:
  testfx/optional: "Not relevant for this project"
EOF
touch "$proj/.marker"
out=$(run_collect "$proj")
disabled_count=$(printf '%s' "$out" | jq -r '.disabled_count')
optional_in_resolved=$(printf '%s' "$out" | jq '[.resolved[] | select(.id=="testfx/optional")] | length')
optional_in_pending=$(printf '%s' "$out" | jq '[.pending[] | select(.id=="testfx/optional")] | length')
assert_eq "disabled_count == 1" "1" "$disabled_count"
assert_eq "disabled standard absent from resolved" "0" "$optional_in_resolved"
assert_eq "disabled standard absent from pending" "0" "$optional_in_pending"
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
set +e
err=$(CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$proj" 2>&1 >/dev/null)
rc=$?
set -e
assert_eq "malformed standard exits non-zero" "1" "$rc"
assert_contains "malformed standard error mentions id" "badfx/empty" "$err"
rm -rf "$proj"
rm -rf "$SKILL_TMP/profiles/badfx"

summary
