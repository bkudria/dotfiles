#!/usr/bin/env bash
# Aggregate test runner for Phase A scripts.
set -uo pipefail

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

failures=0
for t in "$TEST_DIR"/test-*.sh; do
  [[ -f "$t" ]] || continue
  echo
  echo "── ${t##*/} ──"
  if bash "$t"; then
    :
  else
    failures=$((failures+1))
  fi
done

echo
if [[ $failures -eq 0 ]]; then
  echo "all suites passed"
  exit 0
else
  echo "$failures suite(s) failed"
  exit 1
fi
