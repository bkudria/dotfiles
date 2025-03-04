#!/usr/bin/env bash

set -euo pipefail

COMMIT_MSG_FILE="$1"
COMMIT_SOURCE="${2:-}"

case "$COMMIT_SOURCE" in
  commit|merge|template|squash)
    exit 0
    ;;
esac

if ! command -v llm >/dev/null 2>&1; then
  echo "Error: 'llm' command not found." >&2
  exit 1
fi

if [ "$COMMIT_SOURCE" = "message" ] && [ -f "$COMMIT_MSG_FILE" ] && ! grep Model "$COMMIT_MSG_FILE"; then
    MSG=$(<"$COMMIT_MSG_FILE")
else
    MSG=""
fi

echo "Generating commit message using llm..."

DIFF_CMD="git diff --staged -U5"

# If lines added == lines removed, that means only words were changed
if [ -z "$(git diff --numstat | awk '{adds+=$1; dels+=$2} END {print adds - dels}')" ]; then
  DIFF_CMD="git diff --staged --word-diff=plain"
fi

PROMPT_TEMPLATE="$(llm templates path)/git-prepare-commit-message.yaml"
PROMPT_COMMIT_SHA_CMD=("git" "-C" "$HOME" "log" "-1" "--pretty=format:%h" "--" "$PROMPT_TEMPLATE")
echo "${PROMPT_COMMIT_SHA_CMD[@]}"
if ! PROMPT_COMMIT_SHA="$("${PROMPT_COMMIT_SHA_CMD[@]}")"; then
  echo "Error: Failed to get commit SHA." >&2
  exit 1
fi

eval "$DIFF_CMD" | \
    llm \
        -t git-prepare-commit-message \
        -p prompt_commit_sha "$PROMPT_COMMIT_SHA" \
        -p previous_commits "$(git log --pretty -n 5 --relative-date | grep -Ev 'commit .{40}|Author')" \
        -p branch "$()" \
        -p msg "$MSG" \
        -p diff_cmd "$DIFF_CMD" \
        -o prefill '```' \
        -o hide_prefill true \
        -o stop_sequences '```' \
        > "$COMMIT_MSG_FILE"
