#!/bin/bash
# ask-form.sh - Multi-question form (supports >4 questions) with review step
#
# Usage: ask-form.sh form.json
#    OR: ask-form.sh --inline '{"questions":[...]}'
#    OR: ask-form.sh --no-review ...  (skip review step)
#
# JSON format:
# {
#   "questions": [
#     {"question": "Name?", "type": "input", "key": "name"},
#     {"question": "Role?", "type": "choose", "options": ["dev", "pm"], "key": "role"},
#     {"question": "Bio?", "type": "write", "key": "bio"},
#     {"question": "Config?", "type": "file", "key": "config"},
#     {"question": "Features?", "type": "multi", "options": ["a", "b", "c"], "key": "features"},
#     {"question": "Proceed?", "type": "confirm", "key": "proceed"}
#   ]
# }
#
# Supported types: input, choose, multi, write, file, filter, confirm
#
# For choose/multi types, options can include descriptions using "label|description" format:
#   "options": ["Python|Great for scripting", "Go|Fast and compiled"]
# And support these optional flags:
#   "descriptions": true    - Enable the "label|description" parsing
#   "other": true           - Add "Other..." option for custom input
#   "skippable": true       - Add "Skip" option
#   "chattable": true       - Add "Chat about this" option (exits form with code 2)
#
# Returns: JSON object with all answers to stdout
# Exit: 0 on completion, 1 on cancel/error

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INTERACTION_SCRIPTS="$HOME/.claude/skills/interactive-tmux/scripts"
RUN_INTERACTIVE="$INTERACTION_SCRIPTS/run-interactive.sh"

# Check for jq
if ! command -v jq &> /dev/null; then
    echo "Error: jq is required for ask-form.sh" >&2
    exit 1
fi

# Cleanup function to ensure interaction is closed on exit
interaction_id=""
cleanup() {
    if [[ -n "$interaction_id" ]]; then
        "$INTERACTION_SCRIPTS/end-interaction.sh" "$interaction_id" 2>/dev/null || true
    fi
}
trap cleanup EXIT

# Parse arguments
json_file=""
inline_json=""
skip_review=false

while [[ $# -gt 0 ]]; do
    case "$1" in
        --inline)
            inline_json="$2"
            shift 2
            ;;
        --no-review)
            skip_review=true
            shift
            ;;
        -*)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
        *)
            json_file="$1"
            shift
            ;;
    esac
done

# Get JSON content
if [[ -n "$inline_json" ]]; then
    json_content="$inline_json"
elif [[ -n "$json_file" ]]; then
    if [[ ! -f "$json_file" ]]; then
        echo "Error: File not found: $json_file" >&2
        exit 1
    fi
    json_content=$(cat "$json_file")
else
    echo "Usage: ask-form.sh form.json" >&2
    echo "   OR: ask-form.sh --inline '{\"questions\":[...]}'" >&2
    exit 1
fi

# Validate JSON structure
if ! echo "$json_content" | jq -e '.questions' > /dev/null 2>&1; then
    echo "Error: Invalid JSON - must contain 'questions' array" >&2
    exit 1
fi

num_questions=$(echo "$json_content" | jq '.questions | length')

# Arrays to store answers (indexed by question number)
declare -a answers
declare -a answer_types

# Function to ask a single question by index
ask_question() {
    local i=$1
    local question_obj=$(echo "$json_content" | jq -c ".questions[$i]")

    local question=$(echo "$question_obj" | jq -r '.question')
    local type=$(echo "$question_obj" | jq -r '.type')
    local key=$(echo "$question_obj" | jq -r '.key')

    # Get optional fields
    local placeholder=$(echo "$question_obj" | jq -r '.placeholder // empty')
    local options_json=$(echo "$question_obj" | jq -c '.options // []')
    local default_value=$(echo "$question_obj" | jq -r '.default // empty')

    local answer=""
    local exit_code=0

    case "$type" in
        input)
            cmd=("$SCRIPT_DIR/ask-input.sh" --header "$question")
            [[ -n "$placeholder" ]] && cmd+=(--placeholder "$placeholder")
            [[ -n "$default_value" ]] && cmd+=(--value "$default_value")
            answer=$("${cmd[@]}") || exit_code=$?
            ;;

        choose)
            cmd=("$SCRIPT_DIR/ask-choose.sh" --header "$question")
            # Check for optional flags
            [[ $(echo "$question_obj" | jq -r '.descriptions // false') == "true" ]] && cmd+=(--descriptions)
            [[ $(echo "$question_obj" | jq -r '.other // false') == "true" ]] && cmd+=(--other)
            [[ $(echo "$question_obj" | jq -r '.skippable // false') == "true" ]] && cmd+=(--skippable)
            [[ $(echo "$question_obj" | jq -r '.chattable // false') == "true" ]] && cmd+=(--chattable)
            while IFS= read -r opt; do
                cmd+=("$opt")
            done < <(echo "$options_json" | jq -r '.[]')
            answer=$("${cmd[@]}") || exit_code=$?
            # Handle chat exit (code 2) - propagate to form level
            if [[ $exit_code -eq 2 ]]; then
                echo "$answer"
                exit 2
            fi
            ;;

        multi)
            cmd=("$SCRIPT_DIR/ask-multi.sh" --header "$question")
            # Check for optional flags
            [[ $(echo "$question_obj" | jq -r '.descriptions // false') == "true" ]] && cmd+=(--descriptions)
            [[ $(echo "$question_obj" | jq -r '.other // false') == "true" ]] && cmd+=(--other)
            [[ $(echo "$question_obj" | jq -r '.skippable // false') == "true" ]] && cmd+=(--skippable)
            [[ $(echo "$question_obj" | jq -r '.chattable // false') == "true" ]] && cmd+=(--chattable)
            # Check for limit
            local limit_val=$(echo "$question_obj" | jq -r '.limit // empty')
            [[ -n "$limit_val" ]] && cmd+=(--limit "$limit_val")
            while IFS= read -r opt; do
                cmd+=("$opt")
            done < <(echo "$options_json" | jq -r '.[]')
            answer=$("${cmd[@]}") || exit_code=$?
            # Handle chat exit (code 2) - propagate to form level
            if [[ $exit_code -eq 2 ]]; then
                echo "$answer"
                exit 2
            fi
            ;;

        write)
            cmd=("$SCRIPT_DIR/ask-write.sh" --header "$question")
            [[ -n "$placeholder" ]] && cmd+=(--placeholder "$placeholder")
            [[ -n "$default_value" ]] && cmd+=(--value "$default_value")
            answer=$("${cmd[@]}") || exit_code=$?
            ;;

        file)
            cmd=("$SCRIPT_DIR/ask-file.sh")
            [[ -n "$default_value" ]] && cmd+=("$default_value")
            answer=$("${cmd[@]}") || exit_code=$?
            ;;

        filter)
            cmd=("$SCRIPT_DIR/ask-filter.sh" --header "$question")
            while IFS= read -r opt; do
                cmd+=("$opt")
            done < <(echo "$options_json" | jq -r '.[]')
            answer=$("${cmd[@]}") || exit_code=$?
            ;;

        confirm)
            cmd=("$SCRIPT_DIR/ask-confirm.sh")
            local yes_label=$(echo "$question_obj" | jq -r '.yes // empty')
            local no_label=$(echo "$question_obj" | jq -r '.no // empty')
            [[ -n "$yes_label" ]] && cmd+=(--yes "$yes_label")
            [[ -n "$no_label" ]] && cmd+=(--no "$no_label")
            cmd+=("$question")
            if "${cmd[@]}"; then
                answer="true"
            else
                answer="false"
            fi
            ;;

        *)
            echo "Error: Unknown question type: $type" >&2
            return 1
            ;;
    esac

    # Check if user cancelled (non-zero exit for non-confirm types)
    if [[ "$type" != "confirm" && $exit_code -ne 0 ]]; then
        echo "Error: User cancelled on question: $question" >&2
        return 1
    fi

    # Store answer
    answers[$i]="$answer"
    answer_types[$i]="$type"
}

# Function to format an answer for display
format_answer() {
    local i=$1
    local answer="${answers[$i]}"
    local type="${answer_types[$i]}"

    case "$type" in
        confirm)
            if [[ "$answer" == "true" ]]; then
                echo "Yes"
            else
                echo "No"
            fi
            ;;
        multi)
            # Show as comma-separated
            echo "$answer" | tr '\n' ',' | sed 's/,$//' | sed 's/,/, /g'
            ;;
        write)
            # Truncate long multi-line text
            local first_line=$(echo "$answer" | head -1)
            local line_count=$(echo "$answer" | wc -l | tr -d ' ')
            if [[ $line_count -gt 1 ]]; then
                echo "${first_line:0:40}... (+$((line_count-1)) lines)"
            elif [[ ${#first_line} -gt 50 ]]; then
                echo "${first_line:0:50}..."
            else
                echo "$first_line"
            fi
            ;;
        *)
            # Truncate if too long
            if [[ ${#answer} -gt 50 ]]; then
                echo "${answer:0:50}..."
            else
                echo "$answer"
            fi
            ;;
    esac
}

# Function to build results JSON
build_results() {
    local results="{}"

    for ((i=0; i<num_questions; i++)); do
        local question_obj=$(echo "$json_content" | jq -c ".questions[$i]")
        local key=$(echo "$question_obj" | jq -r '.key')
        local type="${answer_types[$i]}"
        local answer="${answers[$i]}"

        if [[ "$type" == "confirm" ]]; then
            results=$(echo "$results" | jq --arg key "$key" --argjson val "$answer" '. + {($key): $val}')
        elif [[ "$type" == "multi" ]]; then
            local answer_array=$(echo "$answer" | jq -R -s 'split("\n") | map(select(length > 0))')
            results=$(echo "$results" | jq --arg key "$key" --argjson val "$answer_array" '. + {($key): $val}')
        else
            results=$(echo "$results" | jq --arg key "$key" --arg val "$answer" '. + {($key): $val}')
        fi
    done

    echo "$results"
}

# Start an interaction so all questions use the same pane
interaction_id=$("$INTERACTION_SCRIPTS/start-interaction.sh")

# Phase 1: Ask all questions
for ((i=0; i<num_questions; i++)); do
    ask_question "$i" || exit 1
done

# Phase 2: Review (unless skipped)
if [[ "$skip_review" == false && $num_questions -gt 1 ]]; then
    while true; do
        # Build review options
        review_options=()
        for ((i=0; i<num_questions; i++)); do
            question_text=$(echo "$json_content" | jq -r ".questions[$i].question")
            formatted_answer=$(format_answer "$i")
            # Format: "1. Question? → Answer"
            review_options+=("$((i+1)). ${question_text} → ${formatted_answer}")
        done

        # Add submit option at the end
        review_options+=("✓ Submit")

        # Show review with gum choose
        selection=$("$RUN_INTERACTIVE" gum choose \
            --header "Review your answers (select to edit, or Submit):" \
            "${review_options[@]}") || {
            echo "Error: Review cancelled" >&2
            exit 1
        }

        # Check if user selected Submit
        if [[ "$selection" == "✓ Submit" ]]; then
            break
        fi

        # Extract question number from selection (e.g., "1. Question? → Answer" -> 0)
        question_num=$(echo "$selection" | grep -o '^[0-9]*' | head -1)
        if [[ -n "$question_num" ]]; then
            # Re-ask that question (convert to 0-indexed)
            ask_question "$((question_num - 1))" || exit 1
        fi
    done
fi

# Output final results
build_results | jq .
