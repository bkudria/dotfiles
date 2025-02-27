#!/bin/bash

set -euo pipefail

cd "$( dirname "${BASH_SOURCE[0]}" )"

# Create a temp directory for processed files
TEMP_DIR=$(mktemp -d)
trap 'rm -rf "$TEMP_DIR"' EXIT

# Compile Yuescript specs directly to temp directory
if ! yue -r -t "$TEMP_DIR" spec; then
    echo "Error: Failed to compile Yuescript specs"
    exit 1
fi

# Add busted runner to the beginning of each file
for spec_file in "$TEMP_DIR"/*.lua; do
    if [ -f "$spec_file" ]; then
        # Add busted runner to the beginning of each file
        sed -i '.bak' '1s/^/require("busted.runner")()/g' "$spec_file"
    fi
done

# Check if we have any test files
if [ ! "$(ls -A "$TEMP_DIR")" ]; then
    echo "No test files found in spec/ directory"
    exit 1
fi

# Run all tests
for spec_file in "$TEMP_DIR"/*.lua; do
    if [ -f "$spec_file" ]; then
        unbuffer lua "$spec_file" | sed 's/\.lua/\.yue/g'
    fi
done
