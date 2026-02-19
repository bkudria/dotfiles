#!/bin/bash
# ask-filter.sh - Fuzzy filter through a list of items
#
# Usage: ask-filter.sh [--header "text"] [--limit N] [--placeholder "hint"] [--options-file FILE] item1 item2 ...
#    OR: echo -e "item1\nitem2" | ask-filter.sh [--header "text"] [--limit N]
# Returns: selected item(s) to stdout
# Exit: 0 on selection, 1 on cancel/error

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUN_INTERACTIVE="$HOME/.claude/skills/interactive-tmux/scripts/run-interactive.sh"

# Parse arguments
header=""
limit=""
placeholder=""
options_file=""
items=()

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
        --placeholder)
            placeholder="$2"
            shift 2
            ;;
        --options-file)
            options_file="$2"
            shift 2
            ;;
        *)
            items+=("$1")
            shift
            ;;
    esac
done

# Load items from file if specified
if [[ -n "$options_file" ]]; then
    if [[ ! -f "$options_file" ]]; then
        echo "Error: Options file not found: $options_file" >&2
        exit 1
    fi
    while IFS= read -r line || [[ -n "$line" ]]; do
        [[ -n "$line" ]] && items+=("$line")
    done < "$options_file"
fi

# Build gum command
cmd=(gum filter)
[[ -n "$header" ]] && cmd+=(--header "$header")
[[ -n "$placeholder" ]] && cmd+=(--placeholder "$placeholder")
if [[ -n "$limit" ]]; then
    cmd+=(--limit "$limit")
fi

# If items provided as arguments, pass them
if [[ ${#items[@]} -gt 0 ]]; then
    cmd+=("${items[@]}")
fi

# Run interactively (stdin will be passed through if no items given as args)
if [[ ${#items[@]} -eq 0 ]] && ! [[ -t 0 ]]; then
    # Reading from stdin - need to handle differently
    # Create temp file with stdin content and a wrapper script
    tmpfile=$(mktemp)
    wrapperfile=$(mktemp)
    cat > "$tmpfile"

    # Build properly quoted command in wrapper script
    {
        echo '#!/bin/bash'
        printf 'cat %q | ' "$tmpfile"
        printf '%q ' "${cmd[@]}"
        echo ''
    } > "$wrapperfile"
    chmod +x "$wrapperfile"

    "$RUN_INTERACTIVE" "$wrapperfile"
    rm -f "$tmpfile" "$wrapperfile"
else
    "$RUN_INTERACTIVE" "${cmd[@]}"
fi
