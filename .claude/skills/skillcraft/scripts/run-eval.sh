#!/usr/bin/env bash
# run-eval.sh — Eval pipeline for Claude Code skills
#
# Usage:
#   run-eval.sh run <skill-dir> [options]              Run full eval pipeline
#   run-eval.sh init <skill-dir>                       Create evals/ with template scenario
#   run-eval.sh status <skill-dir>                     Show current eval state
#   run-eval.sh new-iteration <skill-dir>              Create next iteration directory structure
#   run-eval.sh show <skill-dir> [iteration]           Display benchmark results as formatted table
#   run-eval.sh scenarios <skill-dir>                  List scenario IDs and prompts
#
# Run options:
#   --iteration N          Reuse existing iteration directory (default: create new)
#   --agent-model MODEL    Model for agent sessions (default: claude-sonnet-4-6)
#   --grader-model MODEL   Model for grading assertions (default: claude-haiku-4-5)
#   --model MODEL          Shorthand: sets both agent and grader model
#   --repeats N            Run each scenario N times (default: 3)
#   --sequential           Run scenarios sequentially (default: parallel batches)
#   --skip-grading         Skip grading step (useful for debugging scenario runs)
#   --skip-aggregate       Skip aggregation step
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

# --- Signal handling ---
CHILD_PIDS=()
cleanup_on_signal() {
  echo -e "\n${RED}${BOLD}Interrupted.${NC} Cleaning up..." >&2
  # Kill all child processes
  for pid in "${CHILD_PIDS[@]}"; do
    kill "$pid" 2>/dev/null || true
  done
  # Also kill any remaining background jobs from this shell
  jobs -p 2>/dev/null | xargs -r kill 2>/dev/null || true
  # Clean up temp files
  rm -f /tmp/eval-scuttlerun-config-* /tmp/eval-scuttlerun-output-*
  rm -rf /tmp/eval-batch-*
  echo -e "${YELLOW}Background processes stopped. Temp files cleaned.${NC}" >&2
  exit 130
}
trap cleanup_on_signal SIGINT SIGTERM

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

# --- Scenario discovery helpers ---

# List scenario IDs by globbing */scenario.yml in the evals directory.
# Output: one scenario ID per line (sorted).
get_scenario_ids() {
  local evals_dir="$1"
  for f in "$evals_dir"/*/scenario.yml; do
    [[ -f "$f" ]] && basename "$(dirname "$f")"
  done | sort
}

# Determine the next iteration number from existing benchmark-N.json files.
get_next_iteration() {
  local evals_dir="$1"
  local max=0
  for f in "$evals_dir"/benchmark-*.json; do
    [[ -f "$f" ]] || continue
    local n="${f##*benchmark-}"; n="${n%.json}"
    [[ "$n" -gt "$max" ]] && max="$n"
  done
  echo $((max + 1))
}

# --- Usage ---
usage() {
  cat <<'USAGE'
Usage:
  run-eval.sh run <skill-dir> [options]              Run full eval pipeline
  run-eval.sh init <skill-dir>                       Create evals/ with template scenario
  run-eval.sh status <skill-dir>                     Show current eval state
  run-eval.sh new-iteration <skill-dir>              Create next iteration directory
  run-eval.sh show <skill-dir> [iteration]           Display benchmark results
  run-eval.sh scenarios <skill-dir>                  List scenario IDs and names

Run options:
  --iteration N          Reuse existing iteration directory
  --agent-model MODEL    Model for agent sessions (default: claude-sonnet-4-6)
  --grader-model MODEL   Model for grading assertions (default: claude-haiku-4-5)
  --model MODEL          Shorthand: sets both agent and grader model
  --repeats N            Run each scenario N times (default: 3)
  --sequential           Run scenarios sequentially (default: parallel batches)
  --skip-grading         Skip grading step
  --skip-aggregate       Skip aggregation step

Examples:
  run-eval.sh run ~/.claude/skills/haiku-writer
  run-eval.sh run ~/.claude/skills/jq --agent-model claude-haiku-4-5 --sequential
  run-eval.sh show ~/.claude/skills/haiku-writer
USAGE
  exit "${1:-1}"
}

# --- Extract text and tool calls from scuttlerun YAML output ---
extract_scuttlerun_text() {
  local yaml_file="$1"
  yq -r '
    [.conversation[] |
      if has("assistant") then .assistant
      elif has("tool") then "[Tool: " + .tool + (if has("path") then " " + .path elif has("pattern") then " " + .pattern else "" end) + "]"
      else empty
      end
    ] | join("\n\n")
  ' "$yaml_file"
}

# --- Generate pincenez rubric from per-scenario scenario.yml ---
generate_rubric() {
  local evals_dir="$1"
  local scenario_id="$2"
  local rubric_file="$3"
  local scenario_file="$evals_dir/$scenario_id/scenario.yml"

  yq '{
    "context": .prompt,
    "assertions": .assertions
  }' "$scenario_file" > "$rubric_file"
}

# --- Merge pincenez gradings across reps into grading.json ---
# Aggregates N reps per variant using majority vote (pass_rate >= 0.5)
merge_gradings_multi_rep() {
  local scenario_id="$1"
  local scenario_dir="$2"
  local repeats="$3"
  local output_file="$4"

  # Collect all rep gradings into JSON arrays
  local with_all="[]"
  local without_all="[]"
  for rep in $(seq 1 "$repeats"); do
    local wg="$scenario_dir/with_skill/rep-${rep}/grading.yml"
    local wog="$scenario_dir/without_skill/rep-${rep}/grading.yml"
    if [[ -f "$wg" ]]; then
      with_all=$(echo "$with_all" | jq --argjson g "$(yq -o=json '.assertions' "$wg")" '. += [$g]')
    fi
    if [[ -f "$wog" ]]; then
      without_all=$(echo "$without_all" | jq --argjson g "$(yq -o=json '.assertions' "$wog")" '. += [$g]')
    fi
  done

  # Aggregate: majority vote per assertion across reps
  jq -n \
    --arg scenario_id "$scenario_id" \
    --argjson with_reps "$with_all" \
    --argjson without_reps "$without_all" \
    --argjson repeats "$repeats" '
    ($with_reps[0] | length) as $num_assertions |
    {
      scenario_id: $scenario_id,
      assertions: [
        range($num_assertions) | . as $i |
        ([($with_reps[][$i].pass // false)] | map(select(. == true)) | length) as $with_pass_count |
        ([($without_reps[][$i].pass // false)] | map(select(. == true)) | length) as $without_pass_count |
        ($with_pass_count / $repeats) as $with_rate |
        ($without_pass_count / $repeats) as $without_rate |
        ($with_rate >= 0.5) as $wp |
        ($without_rate >= 0.5) as $wop |
        {
          text: $with_reps[0][$i].check,
          with_skill: $wp,
          without_skill: $wop,
          with_skill_pass_rate: ($with_rate * 100 | round / 100),
          without_skill_pass_rate: ($without_rate * 100 | round / 100),
          repeats: $repeats,
          evidence_with: ([$with_reps[][$i] | select(.pass == $wp) | .evidence] | first // null),
          evidence_without: ([$without_reps[][$i] | select(.pass == $wop) | .evidence] | first // null),
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
  local files_json="${4:-}"
  local config_file
  config_file=$(mktemp /tmp/eval-scuttlerun-config-XXXXXX)

  # Base config — yq uses env()/strenv() for variable injection
  PROMPT="$prompt" yq -n '
    .prompt = strenv(PROMPT) |
    .project.claude_md = "Use relative paths. Do not use absolute paths." |
    .tools = ["Read", "Write", "Bash", "Glob", "Grep", "Skill"] |
    .user.turn_policy = "single"
  ' > "$config_file"

  # Add skill for with_skill variant
  if [[ "$variant" == "with_skill" ]]; then
    SKILL_DIR="$skill_dir" yq -i '.project.skills = [strenv(SKILL_DIR)]' "$config_file"
  fi

  # Merge project.files if provided
  if [[ -n "$files_json" && "$files_json" != "{}" && "$files_json" != "null" ]]; then
    yq -i eval-all 'select(fi == 0) * select(fi == 1)' \
      "$config_file" <(echo "$files_json" | yq -P '{"project": {"files": .}}')
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
  local files_json="${10:-}"
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
  scuttlerun_config=$(generate_scuttlerun_config "$prompt" "$variant" "$skill_dir" "$files_json")
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

# --- Run grader for a scenario (grades all reps, merges into grading.json) ---
# When result_file is provided (parallel mode), writes status there instead of printing
run_grader() {
  local scenario_id="$1"
  local evals_dir="$2"
  local iter_num="$3"
  local model="$4"
  local step_num="${5:-}"
  local total_steps="${6:-}"
  local result_file="${7:-}"
  local repeats="${8:-3}"
  local scenario_dir="$evals_dir/$scenario_id/iteration-$iter_num"
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

  # Check that all rep outputs exist
  local missing=false
  for rep in $(seq 1 "$repeats"); do
    if [[ ! -f "$scenario_dir/with_skill/rep-${rep}/output.md" ]] || \
       [[ ! -f "$scenario_dir/without_skill/rep-${rep}/output.md" ]]; then
      missing=true
      break
    fi
  done

  if $missing; then
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

  # Generate rubric (once per scenario)
  local rubric_file="$scenario_dir/rubric.yml"
  generate_rubric "$evals_dir" "$scenario_id" "$rubric_file"

  local pincenez_args=()
  if [[ -n "$model" ]]; then
    pincenez_args+=(--model "$model")
  fi

  # Run pincenez on each rep's output (all in parallel)
  local grade_pids=()
  for rep in $(seq 1 "$repeats"); do
    local with_output="$scenario_dir/with_skill/rep-${rep}/output.md"
    local without_output="$scenario_dir/without_skill/rep-${rep}/output.md"
    local with_grading="$scenario_dir/with_skill/rep-${rep}/grading.yml"
    local without_grading="$scenario_dir/without_skill/rep-${rep}/grading.yml"

    if [[ ! -f "$with_grading" || ! -s "$with_grading" ]]; then
      pincenez "${pincenez_args[@]}" "$rubric_file" "$with_output" > "$with_grading" 2>/dev/null &
      grade_pids+=($!)
    fi
    if [[ ! -f "$without_grading" || ! -s "$without_grading" ]]; then
      pincenez "${pincenez_args[@]}" "$rubric_file" "$without_output" > "$without_grading" 2>/dev/null &
      grade_pids+=($!)
    fi
  done

  local ok=true
  for pid in "${grade_pids[@]}"; do
    wait "$pid" || ok=false
  done

  if $ok; then
    merge_gradings_multi_rep "$scenario_id" "$scenario_dir" "$repeats" "$grading_file"
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

  if [[ -d "$evals_dir" ]]; then
    echo -e "${YELLOW}evals/ directory already exists at $evals_dir${NC}"
    exit 1
  fi

  mkdir -p "$evals_dir/scenario-1"
  cat > "$evals_dir/scenario-1/scenario.yml" <<'EOF'
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
  echo -e "${GREEN}Created: $evals_dir/scenario-1/scenario.yml${NC}"
  echo ""
  echo "Next steps:"
  echo "  1. Rename scenario-1/ and edit its scenario.yml"
  echo "  2. Create 2+ more scenario directories with scenario.yml files"
  echo "  3. Run: run-eval.sh run $skill_dir"
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

  local scenario_ids
  scenario_ids=$(get_scenario_ids "$evals_dir")
  local scenario_count=0
  if [[ -n "$scenario_ids" ]]; then
    scenario_count=$(echo "$scenario_ids" | wc -l | tr -d ' ')
  fi

  echo -e "  Scenarios defined: ${BOLD}${scenario_count}${NC}"

  if [[ "$scenario_count" -gt 0 ]]; then
    echo "  Scenario IDs:"
    echo "$scenario_ids" | while read -r id; do
      echo "    - $id"
    done
  fi

  echo ""

  local iteration_count=0
  local latest_benchmark=""
  for f in "$evals_dir"/benchmark-*.json; do
    [[ -f "$f" ]] || continue
    iteration_count=$((iteration_count + 1))
    latest_benchmark="$f"
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

  local scenario_ids
  scenario_ids=$(get_scenario_ids "$evals_dir")

  if [[ -z "$scenario_ids" ]]; then
    echo -e "${RED}No scenarios found (no */scenario.yml in $evals_dir). Run: run-eval.sh init $skill_dir${NC}"
    exit 1
  fi

  local next_iter
  next_iter=$(get_next_iteration "$evals_dir")

  while read -r scenario_id; do
    mkdir -p "$evals_dir/$scenario_id/iteration-${next_iter}/with_skill"
    mkdir -p "$evals_dir/$scenario_id/iteration-${next_iter}/without_skill"
  done <<< "$scenario_ids"

  echo -e "${GREEN}Created iteration ${next_iter} directories${NC}"
}

cmd_show() {
  local skill_dir="$1"
  local iteration="${2:-}"
  local evals_dir="$skill_dir/evals"

  local benchmark_file=""

  if [[ -n "$iteration" ]]; then
    benchmark_file="$evals_dir/benchmark-${iteration}.json"
  else
    # Find the latest benchmark file by version sort
    for f in "$evals_dir"/benchmark-*.json; do
      [[ -f "$f" ]] && benchmark_file="$f"
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
  local evals_dir="$skill_dir/evals"

  local scenario_ids
  scenario_ids=$(get_scenario_ids "$evals_dir")

  if [[ -z "$scenario_ids" ]]; then
    echo -e "${RED}No scenarios found in $evals_dir${NC}"
    exit 1
  fi

  echo -e "${BOLD}Eval Scenarios${NC}"
  echo ""

  echo "$scenario_ids" | while read -r id; do
    local scenario_file="$evals_dir/$id/scenario.yml"
    local name
    name=$(yq -r '.name' "$scenario_file")
    local assertion_count
    assertion_count=$(yq '.assertions | length' "$scenario_file")
    echo -e "  ${BOLD}$id${NC}: $name"
    echo "    Assertions: $assertion_count"
    echo ""
  done
}

cmd_run() {
  local skill_dir="$1"
  shift

  # Parse run-specific options
  local iteration="" agent_model="claude-sonnet-4-6" grader_model="claude-haiku-4-5"
  local sequential=false skip_grading=false skip_aggregate=false repeats=3

  while [[ $# -gt 0 ]]; do
    case "$1" in
      --iteration)      iteration="$2"; shift 2 ;;
      --agent-model)    agent_model="$2"; shift 2 ;;
      --grader-model)   grader_model="$2"; shift 2 ;;
      --model)          agent_model="$2"; grader_model="$2"; shift 2 ;;
      --repeats)        repeats="$2"; shift 2 ;;
      --sequential)     sequential=true; shift ;;
      --parallel)       shift ;;  # no-op, parallel is now the default
      --skip-grading)   skip_grading=true; shift ;;
      --skip-aggregate) skip_aggregate=true; shift ;;
      -h|--help)        usage 0 ;;
      *)                echo -e "${RED}Unknown option: $1${NC}"; usage ;;
    esac
  done

  if ! [[ "$repeats" =~ ^[1-9][0-9]*$ ]]; then
    echo -e "${RED}--repeats must be a positive integer, got: $repeats${NC}"
    exit 1
  fi

  check_scuttlerun
  check_pincenez

  local evals_dir="$skill_dir/evals"

  local scenario_ids_raw
  scenario_ids_raw=$(get_scenario_ids "$evals_dir")
  if [[ -z "$scenario_ids_raw" ]]; then
    echo -e "${RED}No scenarios found (no */scenario.yml in $evals_dir)${NC}"
    echo "Run: run-eval.sh init $skill_dir"
    exit 1
  fi

  local skill_name
  skill_name=$(yq --front-matter=extract '.name' "$skill_dir/SKILL.md" 2>/dev/null || basename "$skill_dir")

  echo -e "${BOLD}Eval Runner — ${skill_name}${NC}"
  echo ""

  # --- Create or reuse iteration ---
  local iter_num
  if [[ -n "$iteration" ]]; then
    # Verify at least one scenario has this iteration dir
    local found=false
    echo "$scenario_ids_raw" | while read -r sid; do
      [[ -d "$evals_dir/$sid/iteration-$iteration" ]] && { found=true; break; }
    done
    iter_num="$iteration"
    echo -e "  Reusing iteration: ${BOLD}${iter_num}${NC}"
  else
    cmd_new_iteration "$skill_dir"
    iter_num=$(get_next_iteration "$evals_dir")
    iter_num=$((iter_num - 1))  # get_next_iteration returns N+1, we just created N
    echo -e "  Created iteration: ${BOLD}${iter_num}${NC}"
  fi

  echo ""

  # --- Get scenario list ---
  local scenario_count
  scenario_count=$(echo "$scenario_ids_raw" | wc -l | tr -d ' ')

  # --- Compute total steps and phases ---
  local run_steps=$((scenario_count * 2 * repeats))
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
  local plan_parts="${scenario_count} scenarios x 2 variants x ${repeats} reps = ${run_steps} runs"
  if [[ $grading_steps -gt 0 ]]; then
    plan_parts="${plan_parts} + ${grading_steps} grading = ${TOTAL_STEPS} steps"
  else
    plan_parts="${plan_parts} = ${TOTAL_STEPS} steps"
  fi
  echo -e "  Plan: ${plan_parts}"
  echo ""

  local current_phase=0

  # --- Pre-read scenario data from per-scenario files ---
  local scenario_ids=() scenario_names=() scenario_prompts=() scenario_files=()
  while read -r sid; do
    local scenario_file="$evals_dir/$sid/scenario.yml"
    scenario_ids+=("$sid")
    scenario_names+=("$(yq -r '.name' "$scenario_file")")
    scenario_prompts+=("$(yq -r '.prompt' "$scenario_file")")
    scenario_files+=("$(yq -o=json '.files // {}' "$scenario_file")")
  done <<< "$scenario_ids_raw"

  # Ensure iteration directories exist (always use rep subdirs)
  for scenario_id in "${scenario_ids[@]}"; do
    for rep in $(seq 1 "$repeats"); do
      mkdir -p "$evals_dir/$scenario_id/iteration-${iter_num}/with_skill/rep-${rep}" \
               "$evals_dir/$scenario_id/iteration-${iter_num}/without_skill/rep-${rep}"
    done
  done

  if $sequential; then
    # --- Sequential mode: original behavior ---
    current_phase=$((current_phase + 1))
    phase_banner "$current_phase" "$phase_count" "Scenarios"

    for i in $(seq 0 $((scenario_count - 1))); do
      echo -e "  ${BOLD}${scenario_names[$i]}${NC} (${scenario_ids[$i]})"

      for rep in $(seq 1 "$repeats"); do
        run_scenario_variant "${scenario_ids[$i]}" "${scenario_prompts[$i]}" "with_skill rep-${rep}" \
          "$evals_dir/${scenario_ids[$i]}/iteration-${iter_num}/with_skill/rep-${rep}" "$agent_model" "$skill_dir" \
          "" "" "" "${scenario_files[$i]}"

        run_scenario_variant "${scenario_ids[$i]}" "${scenario_prompts[$i]}" "without_skill rep-${rep}" \
          "$evals_dir/${scenario_ids[$i]}/iteration-${iter_num}/without_skill/rep-${rep}" "$agent_model" "$skill_dir" \
          "" "" "" "${scenario_files[$i]}"
      done

      echo ""
    done

    # --- Grade (sequential) ---
    if ! $skip_grading; then
      current_phase=$((current_phase + 1))
      phase_banner "$current_phase" "$phase_count" "Grading"

      for i in $(seq 0 $((scenario_count - 1))); do
        run_grader "${scenario_ids[$i]}" "$evals_dir" "$iter_num" "$grader_model" "" "" "" "$repeats"
      done
    fi
  else
    # --- Parallel mode: batch by variant ---
    local batch_tmp
    batch_tmp=$(mktemp -d /tmp/eval-batch-XXXXXX)
    local pids=() result_files=()

    # Batch 1: ALL with_skill runs (all reps)
    current_phase=$((current_phase + 1))
    phase_banner "$current_phase" "$phase_count" "with_skill runs"
    local with_skill_jobs=$((scenario_count * repeats))
    printf '  Running %d with_skill jobs in parallel (%d scenarios x %d reps)...\n' "$with_skill_jobs" "$scenario_count" "$repeats" >&2

    pids=()
    result_files=()
    local step_counter=0
    for i in $(seq 0 $((scenario_count - 1))); do
      for rep in $(seq 1 "$repeats"); do
        step_counter=$((step_counter + 1))
        local rf="$batch_tmp/with_${scenario_ids[$i]}_rep${rep}.result"
        result_files+=("$rf")
        run_scenario_variant "${scenario_ids[$i]}" "${scenario_prompts[$i]}" "with_skill rep-${rep}" \
          "$evals_dir/${scenario_ids[$i]}/iteration-${iter_num}/with_skill/rep-${rep}" "$agent_model" "$skill_dir" \
          "$step_counter" "$TOTAL_STEPS" "$rf" "${scenario_files[$i]}" &
        pids+=($!)
      done
    done
    wait_for_batch "${pids[@]}" || true
    print_batch_results "$TOTAL_STEPS" "${result_files[@]}"
    echo "" >&2

    # Batch 2: ALL without_skill runs (all reps)
    current_phase=$((current_phase + 1))
    phase_banner "$current_phase" "$phase_count" "without_skill runs"
    local without_skill_jobs=$((scenario_count * repeats))
    printf '  Running %d without_skill jobs in parallel (%d scenarios x %d reps)...\n' "$without_skill_jobs" "$scenario_count" "$repeats" >&2

    pids=()
    result_files=()
    step_counter=$((scenario_count * repeats))
    for i in $(seq 0 $((scenario_count - 1))); do
      for rep in $(seq 1 "$repeats"); do
        step_counter=$((step_counter + 1))
        local rf="$batch_tmp/without_${scenario_ids[$i]}_rep${rep}.result"
        result_files+=("$rf")
        run_scenario_variant "${scenario_ids[$i]}" "${scenario_prompts[$i]}" "without_skill rep-${rep}" \
          "$evals_dir/${scenario_ids[$i]}/iteration-${iter_num}/without_skill/rep-${rep}" "$agent_model" "$skill_dir" \
          "$step_counter" "$TOTAL_STEPS" "$rf" "${scenario_files[$i]}" &
        pids+=($!)
      done
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
        local step_num=$((scenario_count * 2 * repeats + i + 1))
        local rf="$batch_tmp/grade_${scenario_ids[$i]}.result"
        result_files+=("$rf")
        run_grader "${scenario_ids[$i]}" "$evals_dir" "$iter_num" "$grader_model" \
          "$step_num" "$TOTAL_STEPS" "$rf" "$repeats" &
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
    "$SCRIPT_DIR/aggregate-results.sh" "$skill_dir" "$iter_num" "$evals_dir"
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
