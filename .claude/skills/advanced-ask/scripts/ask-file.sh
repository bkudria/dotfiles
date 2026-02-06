#!/bin/bash
# ask-file.sh - Interactive file/directory picker using fzf
#
# Usage: ask-file.sh [options] [start-path]
#
# Options:
#   --header "text"    Header text shown above the picker
#   --directory        Only show directories
#   --all              Include hidden files
#   --preview          Show file preview (for files, not directories)
#   --glob "pattern"   Filter by glob pattern (e.g., "*.md", "SKILL.md", "**/*.ts")
#   --name "pattern"   Filter by filename pattern (e.g., "*.md")
#   --ext "ext"        Filter by extension (e.g., "md", "ts", "py")
#
# Returns: selected path to stdout
# Exit: 0 on selection, 1 on cancel/error
#
# Note: Uses fzf instead of gum file due to gum file display bugs
# See: https://github.com/charmbracelet/gum/issues/977

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUN_INTERACTIVE="$HOME/.claude/skills/interactive-tmux/scripts/run-interactive.sh"

# Check for required tools
if ! command -v fzf &> /dev/null; then
    echo "Error: fzf is required for ask-file.sh (brew install fzf)" >&2
    exit 1
fi

# Parse arguments
header=""
directory_only=false
show_all=false
show_preview=false
glob_pattern=""
name_pattern=""
extension=""
start_path="."

while [[ $# -gt 0 ]]; do
    case "$1" in
        --header)
            header="$2"
            shift 2
            ;;
        --directory)
            directory_only=true
            shift
            ;;
        --all)
            show_all=true
            shift
            ;;
        --preview)
            show_preview=true
            shift
            ;;
        --glob)
            glob_pattern="$2"
            shift 2
            ;;
        --name)
            name_pattern="$2"
            shift 2
            ;;
        --ext)
            extension="$2"
            shift 2
            ;;
        -*)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
        *)
            start_path="$1"
            shift
            ;;
    esac
done

# Resolve start path to absolute
start_path=$(cd "$start_path" 2>/dev/null && pwd || echo "$start_path")

# Build the command to run interactively
# We'll create a small inline script that does the find | fzf pipeline

# Determine find command
if command -v fd &> /dev/null; then
    # Prefer fd if available (faster, respects .gitignore by default)
    find_part="fd"

    # Type filter
    if [[ "$directory_only" == true ]]; then
        find_part+=" --type d"
    else
        find_part+=" --type f"
    fi

    # Hidden files
    [[ "$show_all" == true ]] && find_part+=" --hidden"

    # Glob/name/extension filters (fd supports these natively)
    if [[ -n "$glob_pattern" ]]; then
        find_part+=" --glob '$glob_pattern'"
    elif [[ -n "$name_pattern" ]]; then
        find_part+=" --glob '$name_pattern'"
    elif [[ -n "$extension" ]]; then
        find_part+=" --extension '$extension'"
    fi

    find_part+=" '$start_path'"
else
    # Fall back to find
    find_part="find '$start_path'"

    # Hidden files filter
    [[ "$show_all" != true ]] && find_part+=" -not -path '*/.*'"

    # Type filter
    [[ "$directory_only" == true ]] && find_part+=" -type d" || find_part+=" -type f"

    # Name/glob/extension filters
    if [[ -n "$glob_pattern" ]]; then
        find_part+=" -name '$glob_pattern'"
    elif [[ -n "$name_pattern" ]]; then
        find_part+=" -name '$name_pattern'"
    elif [[ -n "$extension" ]]; then
        find_part+=" -name '*.$extension'"
    fi
fi

# Build fzf command
fzf_part="fzf"
[[ -n "$header" ]] && fzf_part+=" --header '$header'"

# Add preview for files (not directories)
if [[ "$show_preview" == true && "$directory_only" != true ]]; then
    fzf_part+=" --preview 'head -100 {}' --preview-window 'right:50%:wrap'"
fi

# Combine into single command
full_cmd="$find_part 2>/dev/null | $fzf_part"

# Run interactively via tmux wrapper
"$RUN_INTERACTIVE" bash -c "$full_cmd"
