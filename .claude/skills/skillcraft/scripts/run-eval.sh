#!/usr/bin/env bash
# run-eval.sh — Eval pipeline orchestration for Claude Code skills
#
# Usage:
#   run-eval.sh init <skill-dir>                    Create evals/ directory with template evals.yml
#   run-eval.sh status <skill-dir>                  Show current eval state
#   run-eval.sh new-iteration <skill-dir>           Create next iteration directory structure
#   run-eval.sh show <skill-dir> [iteration]        Display benchmark results as formatted table
#   run-eval.sh scenarios <skill-dir>               List scenario IDs and prompts from evals.yml
#
# Requires: yq (brew install yq), jq (brew install jq)
#
# Note: This script handles structural operations (directory creation, YAML parsing,
# results display). Actual subagent spawning for eval runs happens in the Phase 6
# workflow document — only Claude can invoke the Task tool.

set -euo pipefail

# --- Colors ---
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

# --- Dependency check ---
check_deps() {
  local missing=()
  command -v yq >/dev/null 2>&1 || missing+=("yq")
  command -v jq >/dev/null 2>&1 || missing+=("jq")
  if [[ ${#missing[@]} -gt 0 ]]; then
    echo -e "${RED}Missing dependencies: ${missing[*]}${NC}"
    echo "Install with: brew install ${missing[*]}"
    exit 1
  fi
}

# --- Usage ---
usage() {
  cat <<'USAGE'
Usage:
  run-eval.sh init <skill-dir>                    Create evals/ with template evals.yml
  run-eval.sh status <skill-dir>                  Show current eval state
  run-eval.sh new-iteration <skill-dir>           Create next iteration directory structure
  run-eval.sh show <skill-dir> [iteration]        Display benchmark results
  run-eval.sh scenarios <skill-dir>               List scenario IDs from evals.yml
USAGE
  exit 1
}

# --- Subcommands ---

cmd_init() {
  local skill_dir="$1"
  local evals_dir="$skill_dir/evals"
  local skill_name

  if [[ -d "$evals_dir" ]]; then
    echo -e "${YELLOW}evals/ directory already exists at $evals_dir${NC}"
    exit 1
  fi

  # Extract skill name from SKILL.md frontmatter
  if [[ -f "$skill_dir/SKILL.md" ]]; then
    skill_name=$(yq --front-matter=extract '.name' "$skill_dir/SKILL.md" 2>/dev/null || echo "unknown")
  else
    skill_name="unknown"
  fi

  mkdir -p "$evals_dir"
  cat > "$evals_dir/evals.yml" <<EOF
# Eval scenarios for ${skill_name}
# See references/eval-guide.md in the skillcraft skill for schema details.
#
# Each scenario defines:
#   id:         Unique kebab-case identifier (used as directory name)
#   name:       Human-readable description for reports
#   prompt:     The exact task given to both with-skill and without-skill subagents
#   assertions: Objectively verifiable pass/fail checks (3-5 recommended)
#   rubric:     Qualitative grading criteria for the grader agent

skill: ${skill_name}
scenarios:
  - id: scenario-1
    name: "TODO - Descriptive name for this scenario"
    prompt: |
      TODO - Write the exact task/prompt to test.
      This prompt is given to both a subagent with the skill loaded
      and a baseline subagent without it.
    assertions:
      - "TODO - Objectively verifiable assertion 1"
      - "TODO - Another verifiable assertion"
      - "TODO - A third assertion"
    rubric: |
      1. Did the output follow the skill's primary instruction?
      2. Was the output format correct?
      3. Were common pitfalls avoided?
EOF

  echo -e "${GREEN}Created: $evals_dir/${NC}"
  echo -e "${GREEN}Created: $evals_dir/evals.yml${NC}"
  echo ""
  echo "Next steps:"
  echo "  1. Edit evals/evals.yml to define 3+ real scenarios"
  echo "  2. Run Phase 6 workflow to execute eval"
}

cmd_status() {
  local skill_dir="$1"
  local evals_dir="$skill_dir/evals"
  local skill_name

  if [[ ! -d "$evals_dir" ]]; then
    echo -e "${YELLOW}No evals/ directory found at $skill_dir${NC}"
    echo "Run: run-eval.sh init $skill_dir"
    exit 0
  fi

  # Extract skill name
  if [[ -f "$skill_dir/SKILL.md" ]]; then
    skill_name=$(yq --front-matter=extract '.name' "$skill_dir/SKILL.md" 2>/dev/null || echo "unknown")
  else
    skill_name=$(basename "$skill_dir")
  fi

  echo -e "${BOLD}Eval Status: ${skill_name}${NC}"
  echo ""

  # Check evals.yml
  if [[ -f "$evals_dir/evals.yml" ]]; then
    local scenario_count
    scenario_count=$(yq '.scenarios | length' "$evals_dir/evals.yml" 2>/dev/null || echo "0")
    echo -e "  Scenarios defined: ${BOLD}${scenario_count}${NC}"

    # List scenario IDs
    if [[ "$scenario_count" -gt 0 ]]; then
      echo "  Scenario IDs:"
      yq -r '.scenarios[].id' "$evals_dir/evals.yml" 2>/dev/null | while read -r id; do
        echo "    - $id"
      done
    fi
  else
    echo -e "  ${RED}evals.yml not found${NC}"
  fi

  echo ""

  # Count iterations
  local iteration_count=0
  local latest_benchmark=""
  for iter_dir in "$evals_dir"/iteration-*/; do
    [[ -d "$iter_dir" ]] || continue
    iteration_count=$((iteration_count + 1))
    if [[ -f "$iter_dir/benchmark.json" ]]; then
      latest_benchmark="$iter_dir/benchmark.json"
    fi
  done

  echo -e "  Iterations: ${BOLD}${iteration_count}${NC}"

  if [[ -n "$latest_benchmark" ]]; then
    echo ""
    echo -e "  ${BOLD}Latest benchmark:${NC}"
    local with_rate without_rate delta
    with_rate=$(jq -r '.summary.with_skill_pass_rate // "N/A"' "$latest_benchmark")
    without_rate=$(jq -r '.summary.without_skill_pass_rate // "N/A"' "$latest_benchmark")
    delta=$(jq -r '.summary.mean_delta // "N/A"' "$latest_benchmark")
    echo "    With skill:    $with_rate"
    echo "    Without skill: $without_rate"
    echo "    Delta:         $delta"
  fi
}

cmd_new_iteration() {
  local skill_dir="$1"
  local evals_dir="$skill_dir/evals"

  if [[ ! -f "$evals_dir/evals.yml" ]]; then
    echo -e "${RED}No evals.yml found. Run: run-eval.sh init $skill_dir${NC}"
    exit 1
  fi

  # Determine next iteration number
  local next_iter=1
  for iter_dir in "$evals_dir"/iteration-*/; do
    [[ -d "$iter_dir" ]] || continue
    local num
    num=$(basename "$iter_dir" | sed 's/iteration-//')
    if [[ "$num" -ge "$next_iter" ]]; then
      next_iter=$((num + 1))
    fi
  done

  local iter_dir="$evals_dir/iteration-${next_iter}"

  # Create directory structure for each scenario
  local scenario_ids
  scenario_ids=$(yq -r '.scenarios[].id' "$evals_dir/evals.yml" 2>/dev/null)

  if [[ -z "$scenario_ids" ]]; then
    echo -e "${RED}No scenarios found in evals.yml${NC}"
    exit 1
  fi

  while read -r scenario_id; do
    mkdir -p "$iter_dir/$scenario_id/with_skill"
    mkdir -p "$iter_dir/$scenario_id/without_skill"
  done <<< "$scenario_ids"

  echo -e "${GREEN}Created iteration directory: $iter_dir${NC}"
  echo ""
  echo "Directory structure:"
  find "$iter_dir" -type d | sort | while read -r dir; do
    local indent
    indent=$(echo "$dir" | sed "s|$iter_dir||" | sed 's|[^/]||g' | sed 's|/|  |g')
    echo "  ${indent}$(basename "$dir")/"
  done
  echo ""
  echo "Next steps:"
  echo "  1. Run with-skill and without-skill subagents for each scenario"
  echo "  2. Save outputs to <scenario>/with_skill/output.md and <scenario>/without_skill/output.md"
  echo "  3. Run grader for each scenario"
  echo "  4. Run: aggregate-results.sh $skill_dir $next_iter"
}

cmd_show() {
  local skill_dir="$1"
  local iteration="${2:-}"
  local evals_dir="$skill_dir/evals"

  # Find the right benchmark.json
  local benchmark_file=""

  if [[ -n "$iteration" ]]; then
    benchmark_file="$evals_dir/iteration-${iteration}/benchmark.json"
  else
    # Find latest
    for iter_dir in "$evals_dir"/iteration-*/; do
      [[ -d "$iter_dir" ]] || continue
      if [[ -f "$iter_dir/benchmark.json" ]]; then
        benchmark_file="$iter_dir/benchmark.json"
      fi
    done
  fi

  if [[ -z "$benchmark_file" || ! -f "$benchmark_file" ]]; then
    echo -e "${YELLOW}No benchmark.json found${NC}"
    if [[ -n "$iteration" ]]; then
      echo "Expected at: $evals_dir/iteration-${iteration}/benchmark.json"
    else
      echo "Run the eval pipeline first, then aggregate results."
    fi
    exit 0
  fi

  echo -e "${BOLD}Benchmark Results${NC}"
  echo -e "File: $benchmark_file"
  echo ""

  # Summary
  echo -e "${BOLD}Summary${NC}"
  jq -r '
    .summary |
    "  With skill pass rate:    \(.with_skill_pass_rate)",
    "  Without skill pass rate: \(.without_skill_pass_rate)",
    "  Mean delta:              \(.mean_delta)",
    "  Discriminating ratio:    \(.discriminating_ratio)",
    "  Scenarios passing:       \(.scenarios_passing)"
  ' "$benchmark_file"

  echo ""

  # Per-scenario table
  echo -e "${BOLD}Per-Scenario Results${NC}"
  echo ""
  printf "  %-30s %10s %10s %8s\n" "Scenario" "With" "Without" "Delta"
  printf "  %-30s %10s %10s %8s\n" "--------" "----" "-------" "-----"

  jq -r '
    .scenarios[] |
    "\(.name)\t\(.with_skill.pass_rate)\t\(.without_skill.pass_rate)\t\(.delta)"
  ' "$benchmark_file" | while IFS=$'\t' read -r name with without delta; do
    # Truncate name to 30 chars
    local display_name="${name:0:30}"
    printf "  %-30s %10s %10s %8s\n" "$display_name" "$with" "$without" "$delta"
  done

  echo ""

  # Discrimination summary
  local disc_count
  disc_count=$(jq '[.scenarios[].discriminating_assertions] | add // 0' "$benchmark_file")
  local total_assertions
  total_assertions=$(jq '[.scenarios[] | .with_skill.assertions_total] | add // 0' "$benchmark_file")
  echo -e "  Discriminating assertions: $disc_count / $total_assertions"
}

cmd_scenarios() {
  local skill_dir="$1"
  local evals_file="$skill_dir/evals/evals.yml"

  if [[ ! -f "$evals_file" ]]; then
    echo -e "${RED}No evals.yml found at $evals_file${NC}"
    exit 1
  fi

  echo -e "${BOLD}Eval Scenarios${NC}"
  echo ""

  yq -r '.scenarios[] | "\(.id)\t\(.name)"' "$evals_file" | while IFS=$'\t' read -r id name; do
    echo -e "  ${BOLD}$id${NC}: $name"
    # Show assertion count
    local assertion_count
    assertion_count=$(yq ".scenarios[] | select(.id == \"$id\") | .assertions | length" "$evals_file")
    echo "    Assertions: $assertion_count"
    echo ""
  done
}

# --- Main ---

check_deps

[[ $# -lt 2 ]] && usage

COMMAND="$1"
SKILL_DIR="${2/#\~/$HOME}"

# Validate skill directory
if [[ ! -d "$SKILL_DIR" ]]; then
  echo -e "${RED}Skill directory not found: $SKILL_DIR${NC}"
  exit 1
fi

case "$COMMAND" in
  init)          cmd_init "$SKILL_DIR" ;;
  status)        cmd_status "$SKILL_DIR" ;;
  new-iteration) cmd_new_iteration "$SKILL_DIR" ;;
  show)          cmd_show "$SKILL_DIR" "${3:-}" ;;
  scenarios)     cmd_scenarios "$SKILL_DIR" ;;
  *)             echo "Unknown command: $COMMAND"; usage ;;
esac
