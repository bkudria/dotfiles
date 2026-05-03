#!/usr/bin/env bash
# Tests for the state-dir signatures of run-audit.sh:
#   run-audit.sh --init
#   run-audit.sh --collect <project-root> <state-dir> --scope <required|suggested>
#   run-audit.sh --merge  <state-dir>
#   run-audit.sh --render <state-dir>
set -euo pipefail

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REAL_SKILL_DIR="$(cd "$TEST_DIR/.." && pwd)"
RUNNER="$REAL_SKILL_DIR/scripts/run-audit.sh"

# shellcheck source=lib.sh
source "$TEST_DIR/lib.sh"

echo "test-state-dir.sh"

# --- Build a fake skill root with a tiny test profile (mirrors test-collect.sh) --
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

cat > "$SKILL_TMP/profiles/testfx/manual.yaml" <<'EOF'
required: true
description: "A manual standard verified by prompt."
notes: |
  Edge case to remember: the thingy can be in either /etc/thingy or
  ~/.config/thingy; check ~/.config first (XDG precedence).
check:
  prompt: |
    Verify thingy at $PROJECT_ROOT.
EOF

# --- Test 1: --init prints a fresh, empty, existing directory path ---
state1=$("$RUNNER" --init)
state2=$("$RUNNER" --init)
[[ -d "$state1" ]] && d1_exists=1 || d1_exists=0
[[ -d "$state2" ]] && d2_exists=1 || d2_exists=0
assert_eq "--init dir 1 exists" "1" "$d1_exists"
assert_eq "--init dir 2 exists" "1" "$d2_exists"
[[ "$state1" != "$state2" ]] && unique=1 || unique=0
assert_eq "--init returns unique paths across invocations" "1" "$unique"
# Directory should be empty
empty1_count=$(find "$state1" -mindepth 1 -maxdepth 1 | wc -l | tr -d ' ')
assert_eq "--init dir is initially empty" "0" "$empty1_count"
rm -rf "$state1" "$state2"

# --- Test 2: --collect <project-root> <state-dir> --scope required writes
#             collect-required.json there ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker"
state=$("$RUNNER" --init)
CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$proj" "$state" --scope required >/dev/null
[[ -f "$state/collect-required.json" ]] && cj_exists=1 || cj_exists=0
assert_eq "--collect writes collect-required.json" "1" "$cj_exists"
status=$(jq -r '.resolved[] | select(.id=="testfx/marker") | .status' "$state/collect-required.json")
assert_eq "collect-required.json has correct PASS for marker" "PASS" "$status"
pending_id=$(jq -r '.pending[0].id' "$state/collect-required.json")
assert_eq "collect-required.json has manual standard pending" "testfx/manual" "$pending_id"
rm -rf "$proj" "$state"

# --- Test 3: --collect with state-dir is idempotent (re-running overwrites cleanly) ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker"
state=$("$RUNNER" --init)
CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$proj" "$state" --scope required >/dev/null
# Second invocation — must succeed, must not error on noclobber, must overwrite
set +e
err=$(CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$proj" "$state" --scope required 2>&1 >/dev/null)
rc=$?
set -e
assert_exit_code "--collect rerun against same state-dir succeeds" "0" "$rc"
assert_eq "--collect rerun does not print noclobber error" "" "$err"
rm -rf "$proj" "$state"

# --- Test 4: --merge <state-dir> reads collect-required.json + responses/, writes merged.json ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker"
state=$("$RUNNER" --init)
CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$proj" "$state" --scope required >/dev/null
mkdir -p "$state/responses/testfx"
printf '%s' '{"met":true,"detail":"verified"}' > "$state/responses/testfx/manual.txt"
"$RUNNER" --merge "$state" >/dev/null
[[ -f "$state/merged.json" ]] && mj_exists=1 || mj_exists=0
assert_eq "--merge <state-dir> writes <state-dir>/merged.json" "1" "$mj_exists"
manual_status=$(jq -r '.resolved[] | select(.id=="testfx/manual") | .status' "$state/merged.json")
assert_eq "merged.json has manual=PASS" "PASS" "$manual_status"
rm -rf "$proj" "$state"

# --- Test 5: --render <state-dir> reads merged.json and prints table ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker"
state=$("$RUNNER" --init)
CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$proj" "$state" --scope required >/dev/null
mkdir -p "$state/responses/testfx"
printf '%s' '{"met":true,"detail":"verified"}' > "$state/responses/testfx/manual.txt"
"$RUNNER" --merge "$state" >/dev/null
out=$("$RUNNER" --render "$state")
assert_contains "render output has table header" "| Standard | Status | Detail |" "$out"
assert_contains "render output has count line" "PASS" "$out"
rm -rf "$proj" "$state"

# --- Test 6: --merge fails clearly when state-dir lacks any collect file ---
state=$("$RUNNER" --init)
mkdir -p "$state/responses"
set +e
err=$("$RUNNER" --merge "$state" 2>&1 >/dev/null)
rc=$?
set -e
assert_exit_code "--merge missing collect file exits 1" "1" "$rc"
assert_contains "--merge error mentions collect" "collect" "$err"
rm -rf "$state"

# --- Test 7: --render fails clearly when state-dir lacks merged.json ---
state=$("$RUNNER" --init)
set +e
err=$("$RUNNER" --render "$state" 2>&1 >/dev/null)
rc=$?
set -e
assert_exit_code "--render missing merged.json exits 1" "1" "$rc"
assert_contains "--render error mentions merged" "merged" "$err"
rm -rf "$state"

# --- Test 8: --init path has no literal "XXXXXX" placeholder (BSD mktemp -t portability) ---
state=$("$RUNNER" --init)
[[ "$state" != *XXXXXX* ]] && no_literal_x=1 || no_literal_x=0
assert_eq "--init path does not contain literal XXXXXX" "1" "$no_literal_x"
rm -rf "$state"

# --- Test 9: --collect stamps response_path and prompt_path on each pending
#             entry; the prompt file at prompt_path embeds the directive
#             (mentions response_path and the Write tool). ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker"
state=$("$RUNNER" --init)
CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$proj" "$state" --scope required >/dev/null
response_path=$(jq -r '.pending[] | select(.id=="testfx/manual") | .response_path' "$state/collect-required.json")
assert_eq "response_path equals state-dir/responses/<id>.txt" "$state/responses/testfx/manual.txt" "$response_path"
prompt_path=$(jq -r '.pending[] | select(.id=="testfx/manual") | .prompt_path' "$state/collect-required.json")
assert_eq "prompt_path equals state-dir/prompts/<id>.txt" "$state/prompts/testfx/manual.txt" "$prompt_path"
[[ -f "$prompt_path" ]] && prompt_exists=true || prompt_exists=false
assert_eq "prompt file exists at prompt_path" "true" "$prompt_exists"
prompt_contents=$(cat "$prompt_path" 2>/dev/null || echo "")
assert_contains "prompt file mentions response_path" "$state/responses/testfx/manual.txt" "$prompt_contents"
assert_contains "prompt file instructs Write tool use" "Write" "$prompt_contents"
rm -rf "$proj" "$state"

# --- Test 10: --merge accepts response file containing only raw JSON (no fences) ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker"
state=$("$RUNNER" --init)
CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$proj" "$state" --scope required >/dev/null
mkdir -p "$state/responses/testfx"
printf '%s' '{"met":true,"detail":"raw-json verified"}' > "$state/responses/testfx/manual.txt"
"$RUNNER" --merge "$state" >/dev/null
manual_status=$(jq -r '.resolved[] | select(.id=="testfx/manual") | .status' "$state/merged.json")
manual_detail=$(jq -r '.resolved[] | select(.id=="testfx/manual") | .detail' "$state/merged.json")
assert_eq "raw-JSON response resolves to PASS" "PASS" "$manual_status"
assert_eq "raw-JSON detail carried through" "raw-json verified" "$manual_detail"
rm -rf "$proj" "$state"

# --- Test 11: prompt rendered for a YAML with notes embeds the notes
#              under a labeled background section, between project_context
#              and the check.prompt body. ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
touch "$proj/.marker"
state=$("$RUNNER" --init)
CLAUDE_SKILL_DIR="$SKILL_TMP" "$RUNNER" --collect "$proj" "$state" --scope required >/dev/null
prompt_path=$(jq -r '.pending[] | select(.id=="testfx/manual") | .prompt_path' "$state/collect-required.json")
prompt_contents=$(cat "$prompt_path" 2>/dev/null || echo "")
assert_contains "prompt embeds the notes label"      "Maintainer notes for this standard" "$prompt_contents"
assert_contains "prompt embeds the notes content"    "XDG precedence"                     "$prompt_contents"
assert_contains "prompt still embeds the check body" "Verify thingy at"                   "$prompt_contents"
notes_pos=$(printf '%s' "$prompt_contents" | awk '/Maintainer notes/ {print NR; exit}')
check_pos=$(printf '%s' "$prompt_contents" | awk '/Verify thingy at/ {print NR; exit}')
[[ -n "$notes_pos" && -n "$check_pos" && "$notes_pos" -lt "$check_pos" ]] && order_ok=1 || order_ok=0
assert_eq "notes block appears before check body in rendered prompt" "1" "$order_ok"
rm -rf "$proj" "$state"

summary
