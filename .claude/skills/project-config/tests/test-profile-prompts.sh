#!/usr/bin/env bash
# Tests that profile YAMLs' verifier prompts are observation-only.
#
# A verifier prompt's "Report met ... or unmet ..." tail describes what to
# write into `detail`. That tail must describe what was found, not prescribe
# what to do. Banned phrases below are the prescriptive markers observed
# leaking into SUGG-row detail strings during real audits.
set -uo pipefail

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REAL_SKILL_DIR="$(cd "$TEST_DIR/.." && pwd)"

# shellcheck source=lib.sh
source "$TEST_DIR/lib.sh"

echo "test-profile-prompts.sh"

# Banned prescriptive phrases, scoped to the verifier prompt body only
# (the `check.prompt` field). Each entry is a substring match against the
# prompt body; case-sensitive.
banned=(
  "concrete next step"
  "migration suggestion"
  "suggesting the appropriate"
  "suggestion of which"
  "recommending the"
  "should accept flags"
  "should use"
  "fix by"
)

shopt -s nullglob
for f in "$REAL_SKILL_DIR"/profiles/*/*.yaml; do
  rel="${f#$REAL_SKILL_DIR/}"
  prompt=$(yq -r '.check.prompt // ""' "$f" 2>/dev/null || echo "")
  [[ -z "$prompt" ]] && continue
  # Collapse all whitespace runs to single spaces so banned phrases match
  # even when YAML literal blocks split them across lines.
  prompt_normalized=$(printf '%s' "$prompt" | tr '\n' ' ' | tr -s ' ')
  for phrase in "${banned[@]}"; do
    if [[ "$prompt_normalized" == *"$phrase"* ]]; then
      assert_not_contains "$rel verifier prompt is observation-only ($phrase)" "$phrase" "$prompt_normalized"
    fi
  done
done

# Sanity check: the file enumerator must have actually scanned prompts.
# Guards against the YAML parser silently returning empty for every file.
scanned=0
for f in "$REAL_SKILL_DIR"/profiles/*/*.yaml; do
  prompt=$(yq -r '.check.prompt // ""' "$f" 2>/dev/null || echo "")
  [[ -n "$prompt" ]] && scanned=$((scanned+1))
done
[[ $scanned -gt 5 ]] && many_scanned=1 || many_scanned=0
assert_eq "scanned a meaningful number of prompt-based YAMLs" "1" "$many_scanned"

summary
