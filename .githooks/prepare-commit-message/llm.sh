#!/usr/bin/env bash

set -euo pipefail

COMMIT_MSG_FILE="$1"
COMMIT_SOURCE="${2:-}"

case "$COMMIT_SOURCE" in
  commit|merge|template|squash)
    exit 0
    ;;
esac

if ! command -v llm &> /dev/null; then
  echo "Error: 'llm' command not found."
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

eval "$DIFF_CMD" | \
    llm \
        -t git-prepare-commit-message \
        -p previous_commits "$(git log --pretty="%ar: %s" -n 10 --relative-date)" \
        -p branch "$(git rev-parse --abbrev-ref HEAD)" \
        -p msg "$MSG" \
        -p diff_cmd "$DIFF_CMD" \
        > "$COMMIT_MSG_FILE"
