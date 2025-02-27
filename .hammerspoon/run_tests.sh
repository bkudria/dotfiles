#!/bin/bash

set -euo pipefail

# Create a temp directory for processed files
TEMP_DIR=$(mktemp -d)
trap 'rm -rf "$TEMP_DIR"' EXIT

# Compile Yuescript specs
if ! yue -r spec; then
    echo "Error: Failed to compile Yuescript specs"
    exit 1
fi

# Copy lua files to temp directory to avoid modifying originals
for spec_file in spec/*.lua; do
    if [ -f "$spec_file" ]; then
        base_name=$(basename "$spec_file")
        cp "$spec_file" "$TEMP_DIR/$base_name"
        # Add busted runner to the beginning of each file
        sed -i '.bak' '1s/^/require("busted.runner")()/g' "$TEMP_DIR/$base_name"
    fi
done

# Check if we have any test files
if [ ! "$(ls -A "$TEMP_DIR")" ]; then
    echo "No test files found in spec/ directory"
    exit 1
fi

# Run all tests
unbuffer lua "$TEMP_DIR"/*.lua | sed 's/\.lua/\.yue/g'
