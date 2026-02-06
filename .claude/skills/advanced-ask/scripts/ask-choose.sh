#!/bin/bash
# ask-choose.sh - Select a single option from a list (supports >4 options)
#
# Usage: ask-choose.sh [options] option1 option2 ...
#
# Options:
#   --header "text"    Header text shown above choices
#   --descriptions     Enable descriptions: options are "label|description" format
#   --other            Add "Other..." option for custom input
#   --skippable        Add "Skip" option to allow skipping
#   --chattable        Add "Chat about this" option (exits with code 2)
#
# Examples:
#   ask-choose.sh "Red" "Green" "Blue"
#   ask-choose.sh --descriptions "Red|A warm color" "Green|Nature's color"
#   ask-choose.sh --other --skippable "Option1" "Option2"
#
# Returns: selected label to stdout
# Exit codes:
#   0 - Normal selection (including Other and Skip)
#   1 - Cancelled/error
#   2 - "Chat about this" selected (header/question in stdout)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUN_INTERACTIVE="$HOME/.claude/skills/interactive-tmux/scripts/run-interactive.sh"

# Parse arguments
header=""
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
    echo "Usage: ask-choose.sh [--header \"text\"] [--descriptions] [--other] [--skippable] [--chattable] option1 option2 ..." >&2
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

    cmd+=("${formatted_options[@]}")
    "$RUN_INTERACTIVE" "${cmd[@]}"
}

# Main loop - re-ask if separator is selected
while true; do
    choice=$(ask_once) || exit 1

    # Handle special selections
    case "$choice" in
        "$SEPARATOR")
            # Separator selected by accident - re-ask
            continue
            ;;
        "$OTHER_OPTION")
            # Prompt for custom input, showing the original question as context
            custom=$("$RUN_INTERACTIVE" gum input --header "${header:-Enter your choice}" --placeholder "Type your answer...") || exit 1
            echo "$custom"
            exit 0
            ;;
        "$SKIP_OPTION")
            # Return empty for skip
            echo ""
            exit 0
            ;;
        "$CHAT_OPTION")
            # Return the question/header and exit with code 2
            echo "${header:-Question}"
            exit 2
            ;;
        *)
            # Normal selection
            echo "$choice"
            exit 0
            ;;
    esac
done
