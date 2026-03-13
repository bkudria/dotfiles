#!/usr/bin/env bash
# run-eval.sh — Eval pipeline for Claude Code skills
#
# Usage:
#   run-eval.sh run <skill-dir> [options]              Run full eval pipeline
#   run-eval.sh init <skill-dir>                       Create evals/ directory with template evals.yml
#   run-eval.sh status <skill-dir>                     Show current eval state
#   run-eval.sh new-iteration <skill-dir>              Create next iteration directory structure
#   run-eval.sh show <skill-dir> [iteration]           Display benchmark results as formatted table
#   run-eval.sh scenarios <skill-dir>                  List scenario IDs and prompts from evals.yml
#
# Run options:
#   --iteration N     Reuse existing iteration directory (default: create new)
#   --model MODEL     Model to use for eval runs (default: system default)
#   --sequential      Run scenarios sequentially (default: parallel batches)
#   --skip-grading    Skip grading step (useful for debugging scenario runs)
#   --skip-aggregate  Skip aggregation step
#
# Requires: yq, jq, scuttlerun, pincenez (for 'run' subcommand)

set -euo pipefail

# --- Constants ---
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SKILLCRAFT_DIR="$(dirname "$SCRIPT_DIR")"

# --- Colors ---
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

# --- Progress tracking ---
STEP=0
TOTAL_STEPS=0
PIPELINE_START=0

format_duration() {
  local secs="$1"
  if [[ "$secs" -ge 60 ]]; then
    printf '%dm %ds' $((secs / 60)) $((secs % 60))
  else
    printf '%ds' "$secs"
  fi
}

phase_banner() {
  local phase_num="$1"
  local phase_total="$2"
  local label="$3"
  printf '\n%b── %s (%d/%d) %b' "$BOLD" "$label" "$phase_num" "$phase_total" "$NC" >&2
  printf '%.0s─' {1..40} >&2
  printf '\n\n' >&2
}

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

check_scuttlerun() {
  if ! command -v scuttlerun >/dev/null 2>&1; then
    echo -e "${RED}Missing dependency: scuttlerun CLI${NC}"
    exit 1
  fi
}

check_pincenez() {
  if ! command -v pincenez >/dev/null 2>&1; then
    echo -e "${RED}Missing dependency: pincenez CLI${NC}"
    exit 1
  fi
}

# --- Usage ---
usage() {
  cat <<'USAGE'
Usage:
  run-eval.sh run <skill-dir> [options]              Run full eval pipeline
  run-eval.sh init <skill-dir>                       Create evals/ with template evals.yml
  run-eval.sh status <skill-dir>                     Show current eval state
  run-eval.sh new-iteration <skill-dir>              Create next iteration directory
  run-eval.sh show <skill-dir> [iteration]           Display benchmark results
  run-eval.sh scenarios <skill-dir>                  List scenario IDs from evals.yml

Run options:
  --iteration N     Reuse existing iteration directory
  --model MODEL     Model for eval runs (default: system default)
  --sequential      Run scenarios sequentially (default: parallel batches)
  --skip-grading    Skip grading step
  --skip-aggregate  Skip aggregation step

Examples:
  run-eval.sh run ~/.claude/skills/haiku-writer
  run-eval.sh run ~/.claude/skills/jq --model claude-haiku-4-5 --sequential
  run-eval.sh show ~/.claude/skills/haiku-writer
USAGE
  exit "${1:-1}"
}

# --- Extract text from scuttlerun YAML output ---
extract_scuttlerun_text() {
  local yaml_file="$1"
  yq -r '[.conversation[] | select(has("assistant")) | .assistant] | join("\n\n")' "$yaml_file"
}

# --- Generate pincenez rubric from evals.yml scenario ---
generate_rubric() {
  local evals_file="$1"
  local scenario_id="$2"
  local rubric_file="$3"

  id="$scenario_id" yq '
    .scenarios[] | select(.id == strenv(id)) |
    {
      "context": .prompt,
      "assertions": [.assertions[] | {"check": .}]
    }
  ' "$evals_file" > "$rubric_file"
}

# --- Merge pincenez gradings into grading.json ---
merge_gradings() {
  local scenario_id="$1"
  local with_grading="$2"
  local without_grading="$3"
  local output_file="$4"

  # Convert pincenez YAML outputs to JSON, match by id, compute discrimination
  local with_json without_json
  with_json=$(yq -o=json '.assertions' "$with_grading")
  without_json=$(yq -o=json '.assertions' "$without_grading")

  jq -n \
    --arg scenario_id "$scenario_id" \
    --argjson with_assertions "$with_json" \
    --argjson without_assertions "$without_json" '
    # Index without_skill assertions by id
    ($without_assertions | map({(.id): .}) | add // {}) as $without_map |
    {
      scenario_id: $scenario_id,
      assertions: [
        $with_assertions[] |
        . as $w |
        ($without_map[$w.id] // {}) as $wo |
        ($w.pass // false) as $wp |
        ($wo.pass // false) as $wop |
        {
          text: $w.check,
          with_skill: $wp,
          without_skill: $wop,
          evidence_with: ($w.evidence // null),
          evidence_without: ($wo.evidence // null),
          discriminates: ($wp == true and $wop != true)
        }
      ]
    }
  ' > "$output_file"
}

# --- Generate scuttlerun config YAML ---
generate_scuttlerun_config() {
  local prompt="$1"
  local variant="$2"
  local skill_dir="$3"
  local config_file
  config_file=$(mktemp /tmp/eval-scuttlerun-config-XXXXXX)

  if [[ "$variant" == "with_skill" ]]; then
    cat >| "$config_file" <<WARRENEOF
prompt: |
$(echo "$prompt" | sed 's/^/  /')
project:
  claude_md: |
    Use relative paths. Do not use absolute paths.
  skills:
    - $skill_dir
tools:
  - Read
  - Write
  - Bash
  - Glob
  - Grep
  - Skill
user:
  turn_policy: single
WARRENEOF
  else
    cat >| "$config_file" <<WARRENEOF
prompt: |
$(echo "$prompt" | sed 's/^/  /')
project:
  claude_md: |
    Use relative paths. Do not use absolute paths.
tools:
  - Read
  - Write
  - Bash
  - Glob
  - Grep
  - Skill
user:
  turn_policy: single
WARRENEOF
  fi

  echo "$config_file"
}

# --- Wait for a batch of background pids ---
wait_for_batch() {
  local failed=0
  for pid in "$@"; do
    wait "$pid" || failed=$((failed + 1))
  done
  return $failed
}

# --- Print result from a result file (used after parallel batch) ---
print_batch_results() {
  local total_steps="$1"
  shift
  # Each result file contains: step_num|variant|scenario_id|status|elapsed
  # Sort by step number for ordered output
  local results=()
  for f in "$@"; do
    [[ -f "$f" ]] && results+=("$f")
  done

  for f in "${results[@]}"; do
    IFS='|' read -r step_num variant scenario_id status elapsed < "$f"
    case "$status" in
      done)
        printf '%b[%d/%d]%b %s for %s... %bdone%b (%s)\n' \
          "$BOLD" "$step_num" "$total_steps" "$NC" "$variant" "$scenario_id" "$GREEN" "$NC" "$(format_duration "$elapsed")" >&2
        ;;
      failed)
        printf '%b[%d/%d]%b %s for %s... %bfailed%b (%s)\n' \
          "$BOLD" "$step_num" "$total_steps" "$NC" "$variant" "$scenario_id" "$RED" "$NC" "$(format_duration "$elapsed")" >&2
        ;;
      skipped)
        printf '%b[%d/%d]%b %s for %s... %bskipped%b (output exists)\n' \
          "$BOLD" "$step_num" "$total_steps" "$NC" "$variant" "$scenario_id" "$YELLOW" "$NC" >&2
        ;;
    esac
  done
}

# --- Print grading result from a result file ---
print_grading_results() {
  local total_steps="$1"
  shift
  for f in "$@"; do
    [[ -f "$f" ]] || continue
    IFS='|' read -r step_num scenario_id status elapsed < "$f"
    case "$status" in
      done)
        printf '%b[%d/%d]%b grading %s... %bdone%b (%s)\n' \
          "$BOLD" "$step_num" "$total_steps" "$NC" "$scenario_id" "$GREEN" "$NC" "$(format_duration "$elapsed")" >&2
        ;;
      extracting)
        printf '%b[%d/%d]%b grading %s... %bextracting%b (%s)\n' \
          "$BOLD" "$step_num" "$total_steps" "$NC" "$scenario_id" "$YELLOW" "$NC" "$(format_duration "$elapsed")" >&2
        ;;
      failed)
        printf '%b[%d/%d]%b grading %s... %bfailed%b (%s)\n' \
          "$BOLD" "$step_num" "$total_steps" "$NC" "$scenario_id" "$RED" "$NC" "$(format_duration "$elapsed")" >&2
        ;;
      skipped)
        printf '%b[%d/%d]%b grading %s... %bskipped%b (exists)\n' \
          "$BOLD" "$step_num" "$total_steps" "$NC" "$scenario_id" "$YELLOW" "$NC" >&2
        ;;
    esac
  done
}

# --- Run a single scenario variant ---
# When result_file is provided (parallel mode), writes status there instead of printing
run_scenario_variant() {
  local scenario_id="$1"
  local prompt="$2"
  local variant="$3"  # "with_skill" or "without_skill"
  local output_dir="$4"
  local model="$5"
  local skill_dir="$6"
  local step_num="${7:-}"
  local total_steps="${8:-}"
  local result_file="${9:-}"
  local output_file="$output_dir/output.md"

  # Skip if output already exists
  if [[ -f "$output_file" && -s "$output_file" ]]; then
    if [[ -n "$result_file" ]]; then
      echo "${step_num}|${variant}|${scenario_id}|skipped|0" > "$result_file"
    else
      STEP=$((STEP + 1))
      printf '%b[%d/%d]%b %s for %s... %bskipped%b (output exists)\n' \
        "$BOLD" "$STEP" "$TOTAL_STEPS" "$NC" "$variant" "$scenario_id" "$YELLOW" "$NC" >&2
    fi
    return 0
  fi

  # Generate scuttlerun config
  local scuttlerun_config
  scuttlerun_config=$(generate_scuttlerun_config "$prompt" "$variant" "$skill_dir")
  local scuttlerun_output
  scuttlerun_output=$(mktemp /tmp/eval-scuttlerun-output-XXXXXX)

  # Build scuttlerun command
  local cmd=(scuttlerun run "$scuttlerun_config")
  if [[ -n "$model" ]]; then
    cmd+=(--model "$model")
  fi

  if [[ -z "$result_file" ]]; then
    # Sequential mode: print directly
    STEP=$((STEP + 1))
    printf '%b[%d/%d]%b %s for %s... ' "$BOLD" "$STEP" "$TOTAL_STEPS" "$NC" "$variant" "$scenario_id" >&2
  fi
  local step_start=$SECONDS

  if "${cmd[@]}" >| "$scuttlerun_output" 2>"${scuttlerun_output}.err"; then
    {
      echo "# ${variant} output for: ${scenario_id}"
      echo ""
      extract_scuttlerun_text "$scuttlerun_output"
    } > "$output_file"
    local elapsed=$((SECONDS - step_start))
    # Clean up scuttlerun project dir if present
    local project_dir
    project_dir=$(yq -r '.project // ""' "$scuttlerun_output" 2>/dev/null)
    if [[ -n "$project_dir" && -d "$project_dir" ]]; then
      rm -rf "$project_dir"
    fi
    if [[ -n "$result_file" ]]; then
      echo "${step_num}|${variant}|${scenario_id}|done|${elapsed}" > "$result_file"
    else
      echo -e "${GREEN}done${NC} ($(format_duration $elapsed))" >&2
    fi
  else
    local elapsed=$((SECONDS - step_start))
    echo "# ERROR: scuttlerun failed for ${variant}" > "$output_file"
    if [[ -n "$result_file" ]]; then
      echo "${step_num}|${variant}|${scenario_id}|failed|${elapsed}" > "$result_file"
    else
      echo -e "${RED}failed${NC} ($(format_duration $elapsed))" >&2
    fi
  fi

  rm -f "$scuttlerun_config" "$scuttlerun_output" "${scuttlerun_output}.err"
}

# --- Run grader for a scenario ---
# When result_file is provided (parallel mode), writes status there instead of printing
run_grader() {
  local scenario_id="$1"
  local evals_file="$2"
  local iter_dir="$3"
  local model="$4"
  local step_num="${5:-}"
  local total_steps="${6:-}"
  local result_file="${7:-}"
  local scenario_dir="$iter_dir/$scenario_id"
  local grading_file="$scenario_dir/grading.json"

  # Skip if grading already exists
  if [[ -f "$grading_file" && -s "$grading_file" ]]; then
    if [[ -n "$result_file" ]]; then
      echo "${step_num}|${scenario_id}|skipped|0" > "$result_file"
    else
      STEP=$((STEP + 1))
      printf '%b[%d/%d]%b grading %s... %bskipped%b (exists)\n' \
        "$BOLD" "$STEP" "$TOTAL_STEPS" "$NC" "$scenario_id" "$YELLOW" "$NC" >&2
    fi
    return 0
  fi

  local with_output="$scenario_dir/with_skill/output.md"
  local without_output="$scenario_dir/without_skill/output.md"

  if [[ ! -f "$with_output" || ! -f "$without_output" ]]; then
    if [[ -n "$result_file" ]]; then
      echo "${step_num}|${scenario_id}|failed|0" > "$result_file"
    else
      echo -e "  ${RED}Missing output files for grading${NC}" >&2
    fi
    return 1
  fi

  if [[ -z "$result_file" ]]; then
    STEP=$((STEP + 1))
    printf '%b[%d/%d]%b grading %s... ' "$BOLD" "$STEP" "$TOTAL_STEPS" "$NC" "$scenario_id" >&2
  fi
  local step_start=$SECONDS

  # Generate rubric
  local rubric_file="$scenario_dir/rubric.yml"
  generate_rubric "$evals_file" "$scenario_id" "$rubric_file"

  # Run pincenez for both variants
  local with_grading="$scenario_dir/with_skill/grading.yml"
  local without_grading="$scenario_dir/without_skill/grading.yml"

  local pincenez_args=()
  if [[ -n "$model" ]]; then
    pincenez_args+=(--model "$model")
  fi

  local ok=true
  if ! pincenez "${pincenez_args[@]}" "$rubric_file" "$with_output" > "$with_grading" 2>/dev/null; then
    ok=false
  fi
  if ! pincenez "${pincenez_args[@]}" "$rubric_file" "$without_output" > "$without_grading" 2>/dev/null; then
    ok=false
  fi

  if $ok && [[ -s "$with_grading" && -s "$without_grading" ]]; then
    merge_gradings "$scenario_id" "$with_grading" "$without_grading" "$grading_file"
    local elapsed=$((SECONDS - step_start))
    if [[ -n "$result_file" ]]; then
      echo "${step_num}|${scenario_id}|done|${elapsed}" > "$result_file"
    else
      echo -e "${GREEN}done${NC} ($(format_duration $elapsed))" >&2
    fi
  else
    local elapsed=$((SECONDS - step_start))
    if [[ -n "$result_file" ]]; then
      echo "${step_num}|${scenario_id}|failed|${elapsed}" > "$result_file"
    else
      echo -e "${RED}failed${NC} ($(format_duration $elapsed))" >&2
    fi
  fi
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
#   prompt:     The exact task for both with-skill and without-skill eval runs
#   assertions: Objectively verifiable pass/fail checks (3-5 recommended)

skill: ${skill_name}
scenarios:
  - id: scenario-1
    name: "TODO - Descriptive name for this scenario"
    prompt: |
      TODO - Write the exact task/prompt to test.
      This prompt is given to both a with-skill and without-skill
      eval run via run-eval.sh run.
    assertions:
      - "TODO - Objectively verifiable assertion 1"
      - "TODO - Another verifiable assertion"
      - "TODO - A third assertion"
EOF

  echo -e "${GREEN}Created: $evals_dir/${NC}"
  echo -e "${GREEN}Created: $evals_dir/evals.yml${NC}"
  echo ""
  echo "Next steps:"
  echo "  1. Edit evals/evals.yml to define 3+ real scenarios"
  echo "  2. Run: run-eval.sh run $skill_dir"
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

  if [[ -f "$skill_dir/SKILL.md" ]]; then
    skill_name=$(yq --front-matter=extract '.name' "$skill_dir/SKILL.md" 2>/dev/null || echo "unknown")
  else
    skill_name=$(basename "$skill_dir")
  fi

  echo -e "${BOLD}Eval Status: ${skill_name}${NC}"
  echo ""

  if [[ -f "$evals_dir/evals.yml" ]]; then
    local scenario_count
    scenario_count=$(yq '.scenarios | length' "$evals_dir/evals.yml" 2>/dev/null || echo "0")
    echo -e "  Scenarios defined: ${BOLD}${scenario_count}${NC}"

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
}

cmd_show() {
  local skill_dir="$1"
  local iteration="${2:-}"
  local evals_dir="$skill_dir/evals"

  local benchmark_file=""

  if [[ -n "$iteration" ]]; then
    benchmark_file="$evals_dir/iteration-${iteration}/benchmark.json"
  else
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
      echo "Run the eval pipeline first: run-eval.sh run $skill_dir"
    fi
    exit 0
  fi

  echo -e "${BOLD}Benchmark Results${NC}"
  echo -e "File: $benchmark_file"
  echo ""

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

  echo -e "${BOLD}Per-Scenario Results${NC}"
  echo ""
  printf "  %-30s %10s %10s %8s\n" "Scenario" "With" "Without" "Delta"
  printf "  %-30s %10s %10s %8s\n" "--------" "----" "-------" "-----"

  jq -r '
    .scenarios[] |
    "\(.name)\t\(.with_skill.pass_rate)\t\(.without_skill.pass_rate)\t\(.delta)"
  ' "$benchmark_file" | while IFS=$'\t' read -r name with without delta; do
    local display_name="${name:0:30}"
    printf "  %-30s %10s %10s %8s\n" "$display_name" "$with" "$without" "$delta"
  done

  echo ""

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
    local assertion_count
    assertion_count=$(yq ".scenarios[] | select(.id == \"$id\") | .assertions | length" "$evals_file")
    echo "    Assertions: $assertion_count"
    echo ""
  done
}

cmd_run() {
  local skill_dir="$1"
  shift

  # Parse run-specific options
  local iteration="" model="" sequential=false skip_grading=false skip_aggregate=false

  while [[ $# -gt 0 ]]; do
    case "$1" in
      --iteration)      iteration="$2"; shift 2 ;;
      --model)          model="$2"; shift 2 ;;
      --sequential)     sequential=true; shift ;;
      --parallel)       shift ;;  # no-op, parallel is now the default
      --skip-grading)   skip_grading=true; shift ;;
      --skip-aggregate) skip_aggregate=true; shift ;;
      -h|--help)        usage 0 ;;
      *)                echo -e "${RED}Unknown option: $1${NC}"; usage ;;
    esac
  done

  check_scuttlerun
  check_pincenez

  local evals_file="$skill_dir/evals/evals.yml"
  if [[ ! -f "$evals_file" ]]; then
    echo -e "${RED}No evals.yml found at $evals_file${NC}"
    echo "Run: run-eval.sh init $skill_dir"
    exit 1
  fi

  local skill_name
  skill_name=$(yq --front-matter=extract '.name' "$skill_dir/SKILL.md" 2>/dev/null || basename "$skill_dir")

  echo -e "${BOLD}Eval Runner — ${skill_name}${NC}"
  echo ""

  # --- Create or reuse iteration directory ---
  local iter_dir iter_num
  if [[ -n "$iteration" ]]; then
    iter_dir="$skill_dir/evals/iteration-${iteration}"
    if [[ ! -d "$iter_dir" ]]; then
      echo -e "${RED}Iteration directory not found: $iter_dir${NC}"
      exit 1
    fi
    iter_num="$iteration"
    echo -e "  Reusing iteration: ${BOLD}${iter_num}${NC}"
  else
    cmd_new_iteration "$skill_dir"
    # Find the iteration that was just created
    local latest=0
    for d in "$skill_dir/evals"/iteration-*/; do
      [[ -d "$d" ]] || continue
      local n
      n=$(basename "$d" | sed 's/iteration-//')
      if [[ "$n" -gt "$latest" ]]; then
        latest="$n"
      fi
    done
    iter_num="$latest"
    iter_dir="$skill_dir/evals/iteration-${iter_num}"
    echo -e "  Created iteration: ${BOLD}${iter_num}${NC}"
  fi

  echo -e "  Directory: ${iter_dir}"
  echo ""

  # --- Get scenario list ---
  local scenario_count
  scenario_count=$(yq '.scenarios | length' "$evals_file")

  # --- Compute total steps and phases ---
  local run_steps=$((scenario_count * 2))
  local grading_steps=0
  local phase_count
  if $sequential; then
    phase_count=1
  else
    phase_count=2  # with_skill + without_skill batches
  fi
  if ! $skip_grading; then
    grading_steps=$scenario_count
    phase_count=$((phase_count + 1))
    if ! $skip_aggregate; then
      phase_count=$((phase_count + 1))
    fi
  fi
  TOTAL_STEPS=$((run_steps + grading_steps))
  STEP=0
  PIPELINE_START=$SECONDS

  # Pipeline plan
  local plan_parts="${scenario_count} scenarios x 2 variants = ${run_steps} runs"
  if [[ $grading_steps -gt 0 ]]; then
    plan_parts="${plan_parts} + ${grading_steps} grading = ${TOTAL_STEPS} steps"
  else
    plan_parts="${plan_parts} = ${TOTAL_STEPS} steps"
  fi
  echo -e "  Plan: ${plan_parts}"
  echo ""

  local current_phase=0

  # --- Pre-read scenario data ---
  local scenario_ids=() scenario_names=() scenario_prompts=()
  for i in $(seq 0 $((scenario_count - 1))); do
    scenario_ids+=($(yq -r ".scenarios[$i].id" "$evals_file"))
    scenario_names+=("$(yq -r ".scenarios[$i].name" "$evals_file")")
    scenario_prompts+=("$(yq -r ".scenarios[$i].prompt" "$evals_file")")
  done

  # Ensure directories exist
  for scenario_id in "${scenario_ids[@]}"; do
    mkdir -p "$iter_dir/$scenario_id/with_skill" "$iter_dir/$scenario_id/without_skill"
  done

  if $sequential; then
    # --- Sequential mode: original behavior ---
    current_phase=$((current_phase + 1))
    phase_banner "$current_phase" "$phase_count" "Scenarios"

    for i in $(seq 0 $((scenario_count - 1))); do
      echo -e "  ${BOLD}${scenario_names[$i]}${NC} (${scenario_ids[$i]})"

      run_scenario_variant "${scenario_ids[$i]}" "${scenario_prompts[$i]}" "with_skill" \
        "$iter_dir/${scenario_ids[$i]}/with_skill" "$model" "$skill_dir"

      run_scenario_variant "${scenario_ids[$i]}" "${scenario_prompts[$i]}" "without_skill" \
        "$iter_dir/${scenario_ids[$i]}/without_skill" "$model" "$skill_dir"

      echo ""
    done

    # --- Grade (sequential) ---
    if ! $skip_grading; then
      current_phase=$((current_phase + 1))
      phase_banner "$current_phase" "$phase_count" "Grading"

      for i in $(seq 0 $((scenario_count - 1))); do
        run_grader "${scenario_ids[$i]}" "$evals_file" "$iter_dir" "$model"
      done
    fi
  else
    # --- Parallel mode: batch by variant ---
    local batch_tmp
    batch_tmp=$(mktemp -d /tmp/eval-batch-XXXXXX)
    local pids=() result_files=()

    # Batch 1: ALL with_skill runs
    current_phase=$((current_phase + 1))
    phase_banner "$current_phase" "$phase_count" "with_skill runs"
    printf '  Running %d with_skill scenarios in parallel...\n' "$scenario_count" >&2

    pids=()
    result_files=()
    for i in $(seq 0 $((scenario_count - 1))); do
      local step_num=$((i + 1))
      local rf="$batch_tmp/with_${scenario_ids[$i]}.result"
      result_files+=("$rf")
      run_scenario_variant "${scenario_ids[$i]}" "${scenario_prompts[$i]}" "with_skill" \
        "$iter_dir/${scenario_ids[$i]}/with_skill" "$model" "$skill_dir" \
        "$step_num" "$TOTAL_STEPS" "$rf" &
      pids+=($!)
    done
    wait_for_batch "${pids[@]}" || true
    print_batch_results "$TOTAL_STEPS" "${result_files[@]}"
    echo "" >&2

    # Batch 2: ALL without_skill runs
    current_phase=$((current_phase + 1))
    phase_banner "$current_phase" "$phase_count" "without_skill runs"
    printf '  Running %d without_skill scenarios in parallel...\n' "$scenario_count" >&2

    pids=()
    result_files=()
    for i in $(seq 0 $((scenario_count - 1))); do
      local step_num=$((scenario_count + i + 1))
      local rf="$batch_tmp/without_${scenario_ids[$i]}.result"
      result_files+=("$rf")
      run_scenario_variant "${scenario_ids[$i]}" "${scenario_prompts[$i]}" "without_skill" \
        "$iter_dir/${scenario_ids[$i]}/without_skill" "$model" "$skill_dir" \
        "$step_num" "$TOTAL_STEPS" "$rf" &
      pids+=($!)
    done
    wait_for_batch "${pids[@]}" || true

    print_batch_results "$TOTAL_STEPS" "${result_files[@]}"
    echo "" >&2

    # Batch 3: ALL graders in parallel
    if ! $skip_grading; then
      current_phase=$((current_phase + 1))
      phase_banner "$current_phase" "$phase_count" "Grading"
      printf '  Running %d graders in parallel...\n' "$scenario_count" >&2

      pids=()
      result_files=()
      for i in $(seq 0 $((scenario_count - 1))); do
        local step_num=$((scenario_count * 2 + i + 1))
        local rf="$batch_tmp/grade_${scenario_ids[$i]}.result"
        result_files+=("$rf")
        run_grader "${scenario_ids[$i]}" "$evals_file" "$iter_dir" "$model" \
          "$step_num" "$TOTAL_STEPS" "$rf" &
        pids+=($!)
      done
      wait_for_batch "${pids[@]}" || true
      print_grading_results "$TOTAL_STEPS" "${result_files[@]}"
    fi

    rm -rf "$batch_tmp"
  fi

  # --- Aggregate ---
  if ! $skip_grading && ! $skip_aggregate; then
    current_phase=$((current_phase + 1))
    phase_banner "$current_phase" "$phase_count" "Aggregating"
    "$SCRIPT_DIR/aggregate-results.sh" "$skill_dir" "$iter_num"
  fi

  local total_elapsed=$((SECONDS - PIPELINE_START))
  echo ""
  echo -e "${GREEN}${BOLD}Eval complete.${NC} ${TOTAL_STEPS} steps in $(format_duration $total_elapsed)."
}

# --- Main ---

check_deps

[[ $# -lt 1 ]] && usage
[[ "$1" == "-h" || "$1" == "--help" ]] && usage 0

COMMAND="$1"
shift

case "$COMMAND" in
  run)
    [[ $# -lt 1 ]] && usage
    SKILL_DIR="${1/#\~/$HOME}"
    shift
    [[ ! -d "$SKILL_DIR" ]] && { echo -e "${RED}Skill directory not found: $SKILL_DIR${NC}"; exit 1; }
    cmd_run "$SKILL_DIR" "$@"
    ;;
  init|status|new-iteration|show|scenarios)
    [[ $# -lt 1 ]] && usage
    SKILL_DIR="${1/#\~/$HOME}"
    [[ ! -d "$SKILL_DIR" ]] && { echo -e "${RED}Skill directory not found: $SKILL_DIR${NC}"; exit 1; }
    case "$COMMAND" in
      init)          cmd_init "$SKILL_DIR" ;;
      status)        cmd_status "$SKILL_DIR" ;;
      new-iteration) cmd_new_iteration "$SKILL_DIR" ;;
      show)          cmd_show "$SKILL_DIR" "${2:-}" ;;
      scenarios)     cmd_scenarios "$SKILL_DIR" ;;
    esac
    ;;
  *)
    echo -e "${RED}Unknown command: $COMMAND${NC}"
    usage
    ;;
esac
