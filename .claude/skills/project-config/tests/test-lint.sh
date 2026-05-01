#!/usr/bin/env bash
# Tests for `lint-project-yaml.sh`
set -euo pipefail

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REAL_SKILL_DIR="$(cd "$TEST_DIR/.." && pwd)"
LINTER="$REAL_SKILL_DIR/scripts/lint-project-yaml.sh"

# shellcheck source=lib.sh
source "$TEST_DIR/lib.sh"

echo "test-lint.sh"

# --- Build a fake skill root with a tiny test profile --
SKILL_TMP=$(mktemp -d)
trap 'rm -rf "$SKILL_TMP"' EXIT
mkdir -p "$SKILL_TMP/profiles/testfx" "$SKILL_TMP/profiles/extra"
cat > "$SKILL_TMP/profiles/testfx/marker.yaml" <<'EOF'
required: true
description: "A .marker file exists at the project root."
check:
  script: |
    exit 0
EOF
cat > "$SKILL_TMP/profiles/testfx/optional.yaml" <<'EOF'
required: false
description: "An optional .opt file exists."
check:
  prompt: |
    Verify $PROJECT_ROOT.
EOF
cat > "$SKILL_TMP/profiles/extra/extra.yaml" <<'EOF'
required: true
description: "Extra standard."
check:
  script: |
    exit 0
EOF

run_lint() {
  CLAUDE_SKILL_DIR="$SKILL_TMP" "$LINTER" "$@"
}

# --- Test 1: well-formed project.yaml passes ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
EOF
set +e
out=$(run_lint "$proj/project.yaml" 2>&1)
rc=$?
set -e
assert_exit_code "well-formed project.yaml exits 0" "0" "$rc"
rm -rf "$proj"

# --- Test 2: rejects unknown top-level keys ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
language: ruby
EOF
set +e
err=$(run_lint "$proj/project.yaml" 2>&1)
rc=$?
set -e
assert_exit_code "unknown top-level key fails" "1" "$rc"
assert_contains "error names the unknown key" "language" "$err"
rm -rf "$proj"

# --- Test 3: rejects non-existent profile ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [nonexistent]
EOF
set +e
err=$(run_lint "$proj/project.yaml" 2>&1)
rc=$?
set -e
assert_exit_code "non-existent profile fails" "1" "$rc"
assert_contains "error mentions profile name" "nonexistent" "$err"
rm -rf "$proj"

# --- Test 4: rejects empty disabled reason ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
disabled:
  testfx/optional: ""
EOF
set +e
err=$(run_lint "$proj/project.yaml" 2>&1)
rc=$?
set -e
assert_exit_code "empty disabled reason fails" "1" "$rc"
assert_contains "error mentions empty reason" "testfx/optional" "$err"
rm -rf "$proj"

# --- Test 5: rejects disabled key referring to a profile not in profiles list ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
disabled:
  extra/extra: "Some reason"
EOF
set +e
err=$(run_lint "$proj/project.yaml" 2>&1)
rc=$?
set -e
assert_exit_code "stale disabled (profile not selected) fails" "1" "$rc"
assert_contains "error mentions stale id" "extra/extra" "$err"
rm -rf "$proj"

# --- Test 6: rejects disabled key referring to a non-existent standard in selected profile ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
disabled:
  testfx/nonexistent: "Some reason"
EOF
set +e
err=$(run_lint "$proj/project.yaml" 2>&1)
rc=$?
set -e
assert_exit_code "stale disabled (standard missing) fails" "1" "$rc"
assert_contains "error mentions missing id" "testfx/nonexistent" "$err"
rm -rf "$proj"

# --- Test 7: --skill mode validates standard YAMLs ---
mkdir -p "$SKILL_TMP/profiles/badfx"
cat > "$SKILL_TMP/profiles/badfx/missing-required.yaml" <<'EOF'
description: "Missing required field"
check:
  script: |
    exit 0
EOF
set +e
err=$(run_lint --skill 2>&1)
rc=$?
set -e
assert_exit_code "--skill catches malformed standard" "1" "$rc"
assert_contains "--skill error names the bad file" "badfx/missing-required" "$err"
rm -rf "$SKILL_TMP/profiles/badfx"

# --- Test 8: --skill mode rejects standard with both script and prompt ---
mkdir -p "$SKILL_TMP/profiles/badfx2"
cat > "$SKILL_TMP/profiles/badfx2/both.yaml" <<'EOF'
required: true
description: "Has both script and prompt"
check:
  script: |
    exit 0
  prompt: |
    Verify $PROJECT_ROOT.
EOF
set +e
err=$(run_lint --skill 2>&1)
rc=$?
set -e
assert_exit_code "--skill rejects both check.script and check.prompt" "1" "$rc"
assert_contains "--skill error mentions both" "badfx2/both" "$err"
rm -rf "$SKILL_TMP/profiles/badfx2"

# --- Test 9: --skill mode rejects standard with neither script nor prompt ---
mkdir -p "$SKILL_TMP/profiles/badfx3"
cat > "$SKILL_TMP/profiles/badfx3/neither.yaml" <<'EOF'
required: true
description: "Has neither"
check: {}
EOF
set +e
err=$(run_lint --skill 2>&1)
rc=$?
set -e
assert_exit_code "--skill rejects neither check.script nor check.prompt" "1" "$rc"
assert_contains "--skill error mentions neither" "badfx3/neither" "$err"
rm -rf "$SKILL_TMP/profiles/badfx3"

# --- Test 10: --skill mode rejects extra top-level keys in standard YAML ---
mkdir -p "$SKILL_TMP/profiles/badfx4"
cat > "$SKILL_TMP/profiles/badfx4/extra-key.yaml" <<'EOF'
required: true
description: "Has stray top-level key"
unexpected: "value"
check:
  script: |
    exit 0
EOF
set +e
err=$(run_lint --skill 2>&1)
rc=$?
set -e
assert_exit_code "--skill rejects extra top-level keys" "1" "$rc"
assert_contains "--skill error names the bad key" "unexpected" "$err"
rm -rf "$SKILL_TMP/profiles/badfx4"

# --- Test R1: well-formed required: list passes ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
required:
  - testfx/optional
EOF
set +e
out=$(run_lint "$proj/project.yaml" 2>&1)
rc=$?
set -e
assert_exit_code "well-formed required: list exits 0" "0" "$rc"
rm -rf "$proj"

# --- Test R2: required: as a map (not a list) fails ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
required:
  testfx/optional: yes
EOF
set +e
err=$(run_lint "$proj/project.yaml" 2>&1)
rc=$?
set -e
assert_exit_code "required: as map fails" "1" "$rc"
assert_contains "error mentions required must be list" "required" "$err"
rm -rf "$proj"

# --- Test R3: malformed required entry (no slash) fails ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
required:
  - notvalid
EOF
set +e
err=$(run_lint "$proj/project.yaml" 2>&1)
rc=$?
set -e
assert_exit_code "malformed required entry fails" "1" "$rc"
assert_contains "error mentions malformed entry" "notvalid" "$err"
rm -rf "$proj"

# --- Test R4: required entry referring to a profile not in profiles list ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
required:
  - extra/extra
EOF
set +e
err=$(run_lint "$proj/project.yaml" 2>&1)
rc=$?
set -e
assert_exit_code "stale required (profile not selected) fails" "1" "$rc"
assert_contains "error mentions stale id" "extra/extra" "$err"
rm -rf "$proj"

# --- Test R5: required entry referring to a non-existent standard ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
required:
  - testfx/nonexistent
EOF
set +e
err=$(run_lint "$proj/project.yaml" 2>&1)
rc=$?
set -e
assert_exit_code "stale required (standard missing) fails" "1" "$rc"
assert_contains "error mentions missing id" "testfx/nonexistent" "$err"
rm -rf "$proj"

# --- Test R6: required entry whose standard is already required: true (no-op) fails ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
required:
  - testfx/marker
EOF
set +e
err=$(run_lint "$proj/project.yaml" 2>&1)
rc=$?
set -e
assert_exit_code "no-op required (already required: true) fails" "1" "$rc"
assert_contains "error mentions already-required id" "testfx/marker" "$err"
rm -rf "$proj"

# --- Test R7: same id in both required: and disabled: fails ---
proj=$(mktemp -d)
cat > "$proj/project.yaml" <<'EOF'
profiles: [testfx]
required:
  - testfx/optional
disabled:
  testfx/optional: "Conflicting"
EOF
set +e
err=$(run_lint "$proj/project.yaml" 2>&1)
rc=$?
set -e
assert_exit_code "id in both required and disabled fails" "1" "$rc"
assert_contains "error mentions conflict" "testfx/optional" "$err"
rm -rf "$proj"

# --- Test 11: --skill mode allows optional notes ---
mkdir -p "$SKILL_TMP/profiles/okfx"
cat > "$SKILL_TMP/profiles/okfx/with-notes.yaml" <<'EOF'
required: true
description: "Has notes."
notes: |
  Some maintainer-facing context.
check:
  script: |
    exit 0
EOF
set +e
out=$(run_lint --skill 2>&1)
rc=$?
set -e
assert_exit_code "--skill accepts optional notes" "0" "$rc"
rm -rf "$SKILL_TMP/profiles/okfx"

summary
