#!/usr/bin/env bash
# Tests for project-context detection during --collect.
#
# The runner detects language / runtime / package manager once from manifest
# files in the project root and prepends a "Detected project context" block to
# every prompt-based standard's rendered_prompt. It also surfaces the detected
# context as a top-level `project_context` field in collect.json.
set -euo pipefail

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REAL_SKILL_DIR="$(cd "$TEST_DIR/.." && pwd)"
RUNNER="$REAL_SKILL_DIR/scripts/run-audit.sh"

# shellcheck source=lib.sh
source "$TEST_DIR/lib.sh"

echo "test-context-detect.sh"

# A fake skill root with a prompt-based standard, so we can inspect what the
# runner injects into rendered_prompt without bringing in any real standards.
SKILL_TMP=$(mktemp -d)
trap 'rm -rf "$SKILL_TMP"' EXIT
mkdir -p "$SKILL_TMP/profiles/probe"
cat > "$SKILL_TMP/profiles/probe/manual.yaml" <<'EOF'
required: true
description: "Probe standard verified by prompt."
check:
  prompt: |
    Verify thingy at $PROJECT_ROOT.
EOF
cat > "$SKILL_TMP/profiles/probe/script.yaml" <<'EOF'
required: true
description: "Script-based probe."
check:
  script: |
    exit 0
EOF

run_collect() {
  local project_root="$1"
  local state; state=$(mktemp -d)
  CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$project_root" "$state" >/dev/null
  cat "$state/collect.json"
  rm -rf "$state"
}

# --- Test 1: Node.js project (package.json) detected and surfaced ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [probe]
EOF
cat > "$proj/package.json" <<'EOF'
{"name": "x", "version": "0.0.0"}
EOF
out=$(run_collect "$proj")
context=$(printf '%s' "$out" | jq -r '.project_context // ""')
prompt=$(printf '%s' "$out" | jq -r '.pending[0].rendered_prompt')
assert_contains "project_context names Node.js" "Node.js" "$context"
assert_contains "rendered_prompt carries detected-context block" "Detected project context" "$prompt"
assert_contains "rendered_prompt names Node.js"          "Node.js"                     "$prompt"
assert_contains "rendered_prompt names primary manifest" "package.json"                "$prompt"
rm -rf "$proj"

# --- Test 2: Node + pnpm-lock.yaml → package manager identified ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [probe]
EOF
cat > "$proj/package.json" <<'EOF'
{"name": "x", "version": "0.0.0"}
EOF
touch "$proj/pnpm-lock.yaml"
out=$(run_collect "$proj")
prompt=$(printf '%s' "$out" | jq -r '.pending[0].rendered_prompt')
assert_contains "rendered_prompt names pnpm package manager" "pnpm" "$prompt"
rm -rf "$proj"

# --- Test 3: Ruby project (Gemfile) ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [probe]
EOF
echo "source 'https://rubygems.org'" > "$proj/Gemfile"
out=$(run_collect "$proj")
context=$(printf '%s' "$out" | jq -r '.project_context // ""')
assert_contains "project_context names Ruby" "Ruby" "$context"
rm -rf "$proj"

# --- Test 4: Python project (pyproject.toml) ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [probe]
EOF
echo "[project]" > "$proj/pyproject.toml"
out=$(run_collect "$proj")
context=$(printf '%s' "$out" | jq -r '.project_context // ""')
assert_contains "project_context names Python" "Python" "$context"
rm -rf "$proj"

# --- Test 5: Rust project (Cargo.toml) ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [probe]
EOF
echo "[package]" > "$proj/Cargo.toml"
out=$(run_collect "$proj")
context=$(printf '%s' "$out" | jq -r '.project_context // ""')
assert_contains "project_context names Rust" "Rust" "$context"
rm -rf "$proj"

# --- Test 6: Go project (go.mod) ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [probe]
EOF
echo "module example.com/x" > "$proj/go.mod"
out=$(run_collect "$proj")
context=$(printf '%s' "$out" | jq -r '.project_context // ""')
assert_contains "project_context names Go" "Go" "$context"
rm -rf "$proj"

# --- Test 7: Project with no recognised manifest → context_block is empty,
#             rendered_prompt has no "Detected project context" header ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [probe]
EOF
out=$(run_collect "$proj")
context=$(printf '%s' "$out" | jq -r '.project_context // ""')
prompt=$(printf '%s' "$out" | jq -r '.pending[0].rendered_prompt')
assert_eq "project_context is empty when no manifest" "" "$context"
assert_not_contains "rendered_prompt has no context header" "Detected project context" "$prompt"
rm -rf "$proj"

# --- Test 8: Detection happens once; same context appears in collect.json
#             top-level field and in every pending rendered_prompt ---
mkdir -p "$SKILL_TMP/profiles/probe2"
cat > "$SKILL_TMP/profiles/probe2/manual2.yaml" <<'EOF'
required: false
description: "Second probe verified by prompt."
check:
  prompt: |
    Second verification at $PROJECT_ROOT.
EOF
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [probe, probe2]
EOF
cat > "$proj/package.json" <<'EOF'
{"name": "x"}
EOF
out=$(run_collect "$proj")
pending_count=$(printf '%s' "$out" | jq '.pending | length')
all_have_context=$(printf '%s' "$out" \
  | jq '[.pending[] | select(.rendered_prompt | contains("Detected project context"))] | length')
assert_eq "two prompt-based pending entries"               "2" "$pending_count"
assert_eq "both pending prompts include the context block" "2" "$all_have_context"
rm -rf "$proj"
rm -rf "$SKILL_TMP/profiles/probe2"

# --- Test 9: Script-based standards are unaffected (no rendered_prompt is
#             generated for them; resolved entries are unchanged) ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [probe]
EOF
cat > "$proj/package.json" <<'EOF'
{"name": "x"}
EOF
out=$(run_collect "$proj")
script_status=$(printf '%s' "$out" | jq -r '.resolved[] | select(.id=="probe/script") | .status')
assert_eq "script-based standard resolves normally" "PASS" "$script_status"
rm -rf "$proj"

summary
