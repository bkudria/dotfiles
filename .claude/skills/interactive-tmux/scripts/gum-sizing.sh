#!/bin/bash
# gum-sizing.sh - Shared sizing logic for gum commands
#
# Source this file to get: calculate_gum_sizing, inject_gum_height
#
# Usage:
#   source "$(dirname "${BASH_SOURCE[0]}")/gum-sizing.sh"
#   if calculate_gum_sizing "$@"; then
#       echo "pane_lines=$pane_lines"
#       [[ -n "$gum_height" ]] && echo "gum_height=$gum_height"
#   fi

# Calculate pane sizing for any gum subcommand.
# Sets: pane_lines (always), gum_item_count + gum_height (choose/filter only)
# Returns 0 if recognized gum subcommand, 1 otherwise.
calculate_gum_sizing() {
    local args=("$@")
    local cmd="${args[0]}"
    local subcmd="${args[1]:-}"
    local window_height
    window_height=$(tmux display-message -p '#{window_height}')

    if [[ "$(basename "$cmd")" != "gum" ]]; then
        return 1
    fi

    # Reset outputs
    gum_height=""
    gum_item_count=0

    case "$subcmd" in
        choose|filter)
            _calculate_choose_filter_sizing "$window_height" "${args[@]}"
            return $?
            ;;
        confirm)
            # question + Yes/No + help line + margins
            pane_lines=5
            ;;
        input)
            # prompt + input field + margins
            pane_lines=5
            ;;
        write)
            # header + ~10 line editor + help line + margins
            pane_lines=15
            ;;
        file)
            # header + file list + help line + margins
            local max_lines=$((window_height * 50 / 100))
            pane_lines=20
            if [[ $pane_lines -gt $max_lines ]]; then
                pane_lines=$max_lines
            fi
            ;;
        table)
            # header row + data rows + margins
            local max_lines=$((window_height * 50 / 100))
            pane_lines=20
            if [[ $pane_lines -gt $max_lines ]]; then
                pane_lines=$max_lines
            fi
            ;;
        spin)
            # spinner + message
            pane_lines=3
            ;;
        *)
            return 1
            ;;
    esac

    return 0
}

# Internal: sizing for choose/filter (dynamic based on option count).
_calculate_choose_filter_sizing() {
    local window_height="$1"
    shift
    local args=("$@")

    # Count non-flag arguments after "gum choose/filter"
    local item_count=0
    local skip_next=false
    local i
    for (( i=2; i<${#args[@]}; i++ )); do
        if [[ "$skip_next" == true ]]; then
            skip_next=false
            continue
        fi
        case "${args[$i]}" in
            --header|--cursor|--cursor-prefix|--selected-prefix|--unselected-prefix|--height|--limit|--timeout|--cursor.foreground|--header.foreground|--item.foreground|--selected.foreground|--label-delimiter)
                skip_next=true  # next arg is the flag's value
                ;;
            --*)
                ;;  # boolean flag, skip
            *)
                item_count=$((item_count + 1))
                ;;
        esac
    done

    if [[ $item_count -eq 0 ]]; then
        return 1
    fi

    gum_item_count=$item_count

    # Count actual visual lines (multi-line items like descriptions take >1 line each)
    local visual_lines=0
    local skip_next_v=false
    for (( i=2; i<${#args[@]}; i++ )); do
        if [[ "$skip_next_v" == true ]]; then
            skip_next_v=false
            continue
        fi
        case "${args[$i]}" in
            --header|--cursor|--cursor-prefix|--selected-prefix|--unselected-prefix|--height|--limit|--timeout|--cursor.foreground|--header.foreground|--item.foreground|--selected.foreground|--label-delimiter)
                skip_next_v=true
                ;;
            --*)
                ;;
            *)
                # Count newlines in this option string + 1 for the line itself
                local newline_count
                newline_count=$(printf '%s' "${args[$i]}" | wc -l | tr -d ' ')
                visual_lines=$((visual_lines + newline_count + 1))
                ;;
        esac
    done

    # Pane needs room for: items + header(1) + help line(1) + prompt(1) + margins(3)
    local padding=6
    local desired=$((visual_lines + padding))

    local min_lines=5
    local max_lines=$((window_height * 80 / 100))

    # Clamp
    if [[ $desired -lt $min_lines ]]; then
        pane_lines=$min_lines
    elif [[ $desired -gt $max_lines ]]; then
        pane_lines=$max_lines
    else
        pane_lines=$desired
    fi

    # gum --height = number of visible ITEMS (not terminal lines).
    # Convert available terminal lines to item count based on lines-per-item.
    local available_lines=$((pane_lines - padding))
    if [[ $visual_lines -gt 0 && $visual_lines -gt $item_count ]]; then
        # Multi-line items: scale down to number of items that fit
        gum_height=$((available_lines * item_count / visual_lines))
    else
        # Single-line items: 1 item = 1 line
        gum_height=$available_lines
    fi
    if [[ $gum_height -lt 1 ]]; then
        gum_height=1
    fi

    return 0
}

# Build a new argument list with --height injected after "gum choose/filter".
# Sets: injected_args array
inject_gum_height() {
    local height="$1"
    shift
    injected_args=()

    # Check if --height is already set
    for arg in "$@"; do
        if [[ "$arg" == "--height" || "$arg" == --height=* ]]; then
            injected_args=("$@")
            return
        fi
    done

    # Inject --height after "gum choose/filter" (position 2)
    local i=0
    for arg in "$@"; do
        injected_args+=("$arg")
        i=$((i + 1))
        if [[ $i -eq 2 ]]; then
            injected_args+=("--height" "$height")
        fi
    done
}
