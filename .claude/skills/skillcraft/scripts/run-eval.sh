#!/usr/bin/env bash
set -euo pipefail
# run-eval.sh — Run skill evals via craboodle
#
# Usage: run-eval.sh <skill-dir> [craboodle-options...]
# Example: run-eval.sh ~/.claude/skills/haiku-writer --repeats 5 --sequential
#
# Expects <skill-dir>/evals/ to exist with a committed base.yml and scenario dirs.

skill_dir="${1:?Usage: run-eval.sh <skill-dir> [craboodle-options...]}"
skill_dir="${skill_dir/#\~/$HOME}"
shift

evals_dir="$skill_dir/evals"
[[ -d "$evals_dir" ]] || { echo "No evals/ directory in $skill_dir" >&2; exit 1; }

exec craboodle run "$evals_dir" "$@"
