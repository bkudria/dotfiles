#!/bin/bash
# ask-confirm.sh - Yes/no confirmation with custom button labels
#
# Usage: ask-confirm.sh [--header "text"] [--yes "label"] [--no "label"] [--default] "question"
# Returns: exit code 0 = yes, 1 = no
# Note: No stdout output; use exit code to determine answer

set -uo pipefail  # Note: not -e, we want to capture gum confirm's exit code

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUN_INTERACTIVE="$HOME/.claude/skills/interactive-tmux/scripts/run-interactive.sh"

# Parse arguments
header=""
yes_label=""
no_label=""
default=false
question=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --header)
            header="$2"
            shift 2
            ;;
        --yes)
            yes_label="$2"
            shift 2
            ;;
        --no)
            no_label="$2"
            shift 2
            ;;
        --default)
            default=true
            shift
            ;;
        -*)
            echo "Unknown option: $1" >&2
            exit 2
            ;;
        *)
            question="$1"
            shift
            ;;
    esac
done

if [[ -z "$question" ]]; then
    echo "Usage: ask-confirm.sh [options] \"question\"" >&2
    exit 2
fi

# Build gum command
cmd=(gum confirm)
[[ "$default" == true ]] && cmd+=(--default)
[[ -n "$yes_label" ]] && cmd+=(--affirmative "$yes_label")
[[ -n "$no_label" ]] && cmd+=(--negative "$no_label")

# Note: gum confirm doesn't support --header, so we include it in the question
if [[ -n "$header" ]]; then
    question="[$header] $question"
fi

cmd+=("$question")

# Run interactively and preserve exit code
"$RUN_INTERACTIVE" "${cmd[@]}"
