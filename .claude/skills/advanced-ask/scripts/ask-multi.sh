#!/bin/bash
# ask-multi.sh - Select multiple options from a list
#
# Usage: ask-multi.sh [options] option1 option2 ...
#
# Options:
#   --header "text"    Header text shown above choices
#   --limit N          Maximum number of selections (default: unlimited)
#   --descriptions     Enable descriptions: options are "label|description" format
#   --other            Add "Other..." option for custom input
#   --skippable        Add "Skip" option to allow skipping (same as selecting nothing)
#   --chattable        Add "Chat about this" option (exits with code 2)
#
# Examples:
#   ask-multi.sh "Tests" "CI" "Docker"
#   ask-multi.sh --descriptions "Tests|Unit and integration tests" "CI|GitHub Actions"
#   ask-multi.sh --other --skippable "Option1" "Option2"
#
# Returns: newline-separated selections to stdout (may include custom "Other" input)
# Exit codes:
#   0 - Normal selection (including empty selection, Other, Skip)
#   1 - Cancelled/error
#   2 - "Chat about this" selected (header/question in stdout)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUN_INTERACTIVE="$HOME/.claude/skills/interactive-tmux/scripts/run-interactive.sh"

# Parse arguments
header=""
limit=""
with_descriptions=false
with_other=false
skippable=false
chattable=false
options=()

while [[ $# -gt 0 ]]; do
    case "$1" in
        --header)
            header="$2"
            shift 2
            ;;
        --limit)
            limit="$2"
            shift 2
            ;;
        --descriptions)
            with_descriptions=true
            shift
            ;;
        --other)
            with_other=true
            shift
            ;;
        --skippable)
            skippable=true
            shift
            ;;
        --chattable)
            chattable=true
            shift
            ;;
        *)
            options+=("$1")
            shift
            ;;
    esac
done

if [[ ${#options[@]} -eq 0 ]]; then
    echo "Usage: ask-multi.sh [--header \"text\"] [--limit N] [--descriptions] [--other] [--skippable] [--chattable] option1 option2 ..." >&2
    exit 1
fi

# Special option markers - use visible separator so it's clear it's not selectable
SEPARATOR="────────────"
OTHER_OPTION="✎ Other..."
SKIP_OPTION="↩ Skip"
CHAT_OPTION="💬 Chat about this"

# Function to ask the question (may be called multiple times if separator selected)
ask_once() {
    local cmd=(gum choose)
    [[ -n "$header" ]] && cmd+=(--header "$header")

    if [[ -n "$limit" ]]; then
        cmd+=(--limit "$limit")
    else
        cmd+=(--no-limit)
    fi

    local formatted_options=()

    if [[ "$with_descriptions" == true ]]; then
        cmd+=(--label-delimiter="|")
        for opt in "${options[@]}"; do
            if [[ "$opt" == *"|"* ]]; then
                local label="${opt%%|*}"
                local desc="${opt#*|}"
                formatted_options+=("$(printf '%s\n    %s|%s' "$label" "$desc" "$label")")
            else
                formatted_options+=("${opt}|${opt}")
            fi
        done
    else
        formatted_options=("${options[@]}")
    fi

    # Add special options with separator
    if [[ "$with_other" == true || "$skippable" == true || "$chattable" == true ]]; then
        if [[ "$with_descriptions" == true ]]; then
            formatted_options+=("$SEPARATOR|$SEPARATOR")
            [[ "$with_other" == true ]] && formatted_options+=("$OTHER_OPTION|$OTHER_OPTION")
            [[ "$skippable" == true ]] && formatted_options+=("$SKIP_OPTION|$SKIP_OPTION")
            [[ "$chattable" == true ]] && formatted_options+=("$CHAT_OPTION|$CHAT_OPTION")
        else
            formatted_options+=("$SEPARATOR")
            [[ "$with_other" == true ]] && formatted_options+=("$OTHER_OPTION")
            [[ "$skippable" == true ]] && formatted_options+=("$SKIP_OPTION")
            [[ "$chattable" == true ]] && formatted_options+=("$CHAT_OPTION")
        fi
    fi

    cmd+=("--")
    cmd+=("${formatted_options[@]}")
    "$RUN_INTERACTIVE" "${cmd[@]}"
}

# Main loop - re-ask if only separator is selected
while true; do
    choices=$(ask_once) || exit 1

    # Check for special selections in the output
    has_separator=false
    has_other=false
    has_skip=false
    has_chat=false
    normal_choices=()

    while IFS= read -r line; do
        case "$line" in
            "$SEPARATOR")
                has_separator=true
                ;;
            "$OTHER_OPTION")
                has_other=true
                ;;
            "$SKIP_OPTION")
                has_skip=true
                ;;
            "$CHAT_OPTION")
                has_chat=true
                ;;
            "")
                # Empty line, ignore
                ;;
            *)
                normal_choices+=("$line")
                ;;
        esac
    done <<< "$choices"

    # If only separator selected, re-ask
    if [[ "$has_separator" == true && ${#normal_choices[@]} -eq 0 && "$has_other" == false && "$has_skip" == false && "$has_chat" == false ]]; then
        continue
    fi

    # Handle chat - takes priority
    if [[ "$has_chat" == true ]]; then
        echo "${header:-Question}"
        exit 2
    fi

    # Handle other - prompt for custom input and add to choices
    if [[ "$has_other" == true ]]; then
        # Build context header showing the question and what was already selected
        other_header="${header:-Enter your choice}"
        if [[ ${#normal_choices[@]} -gt 0 ]]; then
            selected_list=$(printf '%s, ' ${normal_choices[@]+"${normal_choices[@]}"})
            selected_list="${selected_list%, }"  # Remove trailing comma
            other_header="$other_header"$'\n'"Already selected: $selected_list"
        fi
        custom=$("$RUN_INTERACTIVE" gum input --header "$other_header" --placeholder "Type additional choice...") || exit 1
        if [[ -n "$custom" ]]; then
            normal_choices+=("$custom")
        fi
    fi

    # Output all normal choices (skip just means no special handling needed)
    for choice in ${normal_choices[@]+"${normal_choices[@]}"}; do
        echo "$choice"
    done
    exit 0
done
