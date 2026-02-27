#!/usr/bin/env bash
# aggregate-results.sh — Aggregate grading results into benchmark.json
#
# Usage:
#   aggregate-results.sh <skill-dir> <iteration-number>
#
# Reads grading.json files from each scenario in the specified iteration,
# computes pass rates and deltas, and writes benchmark.json.
#
# Requires: jq (brew install jq), yq (brew install yq)

set -euo pipefail

# --- Colors ---
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BOLD='\033[1m'
NC='\033[0m'

# --- Usage ---
usage() {
  cat <<'USAGE'
Usage:
  aggregate-results.sh <skill-dir> <iteration-number>

Reads grading.json files from evals/iteration-N/ and produces benchmark.json.
USAGE
  exit 1
}

[[ $# -lt 2 ]] && usage

SKILL_DIR="${1/#\~/$HOME}"
ITERATION="$2"
ITER_DIR="$SKILL_DIR/evals/iteration-${ITERATION}"

# --- Validation ---

if [[ ! -d "$ITER_DIR" ]]; then
  echo -e "${RED}Iteration directory not found: $ITER_DIR${NC}"
  exit 1
fi

# Check for jq
if ! command -v jq >/dev/null 2>&1; then
  echo -e "${RED}jq is required. Install with: brew install jq${NC}"
  exit 1
fi

# Extract skill name
SKILL_NAME="unknown"
if [[ -f "$SKILL_DIR/SKILL.md" ]]; then
  SKILL_NAME=$(yq --front-matter=extract '.name' "$SKILL_DIR/SKILL.md" 2>/dev/null || echo "unknown")
fi

# --- Collect grading results ---

SCENARIOS_JSON="[]"
TOTAL_WITH_PASSED=0
TOTAL_WITH_TOTAL=0
TOTAL_WITHOUT_PASSED=0
TOTAL_WITHOUT_TOTAL=0
TOTAL_DISCRIMINATING=0
SCENARIO_COUNT=0
SCENARIOS_PASSING=0

for scenario_dir in "$ITER_DIR"/*/; do
  [[ -d "$scenario_dir" ]] || continue
  scenario_id=$(basename "$scenario_dir")

  grading_file="$scenario_dir/grading.json"
  if [[ ! -f "$grading_file" ]]; then
    echo -e "${YELLOW}Warning: No grading.json in $scenario_id — skipping${NC}"
    continue
  fi

  SCENARIO_COUNT=$((SCENARIO_COUNT + 1))

  # Extract per-scenario metrics from grading.json
  scenario_json=$(jq -r --arg id "$scenario_id" '
    # Count assertions
    (.assertions | length) as $total |
    ([.assertions[] | select(.with_skill == true)] | length) as $with_passed |
    ([.assertions[] | select(.without_skill == true)] | length) as $without_passed |
    ([.assertions[] | select(.discriminates == true)] | length) as $discriminating |

    # Compute pass rates
    (if $total > 0 then ($with_passed / $total) else 0 end) as $with_rate |
    (if $total > 0 then ($without_passed / $total) else 0 end) as $without_rate |

    {
      id: $id,
      name: (.scenario_id // $id),
      with_skill: {
        assertions_passed: $with_passed,
        assertions_total: $total,
        pass_rate: ($with_rate * 100 | round / 100),
        rubric_scores: (.rubric_scores.with_skill // {})
      },
      without_skill: {
        assertions_passed: $without_passed,
        assertions_total: $total,
        pass_rate: ($without_rate * 100 | round / 100),
        rubric_scores: (.rubric_scores.without_skill // {})
      },
      delta: (($with_rate - $without_rate) * 100 | round / 100),
      discriminating_assertions: $discriminating,
      improvement_suggestions: (.improvement_suggestions // [])
    }
  ' "$grading_file")

  # Accumulate totals
  with_passed=$(echo "$scenario_json" | jq '.with_skill.assertions_passed')
  with_total=$(echo "$scenario_json" | jq '.with_skill.assertions_total')
  without_passed=$(echo "$scenario_json" | jq '.without_skill.assertions_passed')
  disc=$(echo "$scenario_json" | jq '.discriminating_assertions')
  with_rate=$(echo "$scenario_json" | jq '.with_skill.pass_rate')

  TOTAL_WITH_PASSED=$((TOTAL_WITH_PASSED + with_passed))
  TOTAL_WITH_TOTAL=$((TOTAL_WITH_TOTAL + with_total))
  TOTAL_WITHOUT_PASSED=$((TOTAL_WITHOUT_PASSED + without_passed))
  TOTAL_DISCRIMINATING=$((TOTAL_DISCRIMINATING + disc))

  # Count scenarios where with-skill pass rate >= 0.8
  if (( $(echo "$with_rate >= 0.8" | bc -l) )); then
    SCENARIOS_PASSING=$((SCENARIOS_PASSING + 1))
  fi

  # Add scenario name from evals.yml if available
  if [[ -f "$SKILL_DIR/evals/evals.yml" ]]; then
    scenario_name=$(yq -r ".scenarios[] | select(.id == \"$scenario_id\") | .name" "$SKILL_DIR/evals/evals.yml" 2>/dev/null || echo "$scenario_id")
    scenario_json=$(echo "$scenario_json" | jq --arg name "$scenario_name" '.name = $name')
  fi

  SCENARIOS_JSON=$(echo "$SCENARIOS_JSON" | jq --argjson s "$scenario_json" '. += [$s]')
done

# --- Compute summary ---

if [[ $TOTAL_WITH_TOTAL -gt 0 ]]; then
  WITH_RATE=$(echo "scale=2; $TOTAL_WITH_PASSED / $TOTAL_WITH_TOTAL" | bc)
  WITHOUT_RATE=$(echo "scale=2; $TOTAL_WITHOUT_PASSED / $TOTAL_WITH_TOTAL" | bc)
  MEAN_DELTA=$(echo "scale=2; $WITH_RATE - $WITHOUT_RATE" | bc)
  DISC_RATIO=$(echo "scale=2; $TOTAL_DISCRIMINATING / $TOTAL_WITH_TOTAL" | bc)
else
  WITH_RATE="0"
  WITHOUT_RATE="0"
  MEAN_DELTA="0"
  DISC_RATIO="0"
fi

# --- Write benchmark.json ---

TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
BENCHMARK_FILE="$ITER_DIR/benchmark.json"

jq -n \
  --arg skill "$SKILL_NAME" \
  --argjson iteration "$ITERATION" \
  --arg timestamp "$TIMESTAMP" \
  --argjson scenarios "$SCENARIOS_JSON" \
  --arg with_rate "$WITH_RATE" \
  --arg without_rate "$WITHOUT_RATE" \
  --arg mean_delta "$MEAN_DELTA" \
  --arg disc_ratio "$DISC_RATIO" \
  --arg passing "${SCENARIOS_PASSING}/${SCENARIO_COUNT}" \
  '{
    skill: $skill,
    iteration: $iteration,
    timestamp: $timestamp,
    scenarios: $scenarios,
    summary: {
      with_skill_pass_rate: ($with_rate | tonumber),
      without_skill_pass_rate: ($without_rate | tonumber),
      mean_delta: ($mean_delta | tonumber),
      discriminating_ratio: ($disc_ratio | tonumber),
      scenarios_passing: $passing
    }
  }' > "$BENCHMARK_FILE"

echo -e "${GREEN}Wrote: $BENCHMARK_FILE${NC}"
echo ""

# --- Display summary ---

echo -e "${BOLD}Benchmark Summary — ${SKILL_NAME} (iteration ${ITERATION})${NC}"
echo ""
echo -e "  With skill pass rate:    ${BOLD}${WITH_RATE}${NC}"
echo -e "  Without skill pass rate: ${WITHOUT_RATE}"
echo -e "  Mean delta:              ${BOLD}${MEAN_DELTA}${NC}"
echo -e "  Discriminating ratio:    ${DISC_RATIO}"
echo -e "  Scenarios passing:       ${SCENARIOS_PASSING}/${SCENARIO_COUNT}"
echo ""

# Per-scenario summary
printf "  %-30s %8s %8s %8s\n" "Scenario" "With" "Without" "Delta"
printf "  %-30s %8s %8s %8s\n" "--------" "----" "-------" "-----"

echo "$SCENARIOS_JSON" | jq -r '.[] | "\(.name)\t\(.with_skill.pass_rate)\t\(.without_skill.pass_rate)\t\(.delta)"' | while IFS=$'\t' read -r name with without delta; do
  printf "  %-30s %8s %8s %8s\n" "${name:0:30}" "$with" "$without" "$delta"
done

# --- Verdict ---

echo ""
if (( $(echo "$MEAN_DELTA >= 0.2" | bc -l) )) && (( $(echo "$WITH_RATE >= 0.8" | bc -l) )); then
  echo -e "  ${GREEN}${BOLD}PASS${NC} — Skill is effective (delta ≥ 0.2, pass rate ≥ 0.8)"
elif (( $(echo "$MEAN_DELTA < 0" | bc -l) )); then
  echo -e "  ${RED}${BOLD}REGRESSION${NC} — Skill performs worse than baseline"
elif (( $(echo "$MEAN_DELTA < 0.2" | bc -l) )); then
  echo -e "  ${YELLOW}${BOLD}WEAK${NC} — Low delta (< 0.2). Revise skill or assertions."
else
  echo -e "  ${YELLOW}${BOLD}PARTIAL${NC} — Good delta but low pass rate. Revise skill content."
fi
