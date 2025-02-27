#!/bin/bash

set -euo pipefail

# Get the directory where the script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

# Create a temp directory for processed files
TEMP_DIR=$(mktemp -d)
trap 'rm -rf "$TEMP_DIR"' EXIT

echo "Compiling Yuescript files..."

# Compile all Yuescript files in the current directory and subdirectories
output=$(yue -r -t "$TEMP_DIR" . 2>&1) || {
    echo "Error compiling Yuescript files:"
    echo "$output"
    exit 1
}

# Ensure helper.yue was compiled
if [ ! -f "$TEMP_DIR/spec/helper.lua" ]; then
    echo "Missing helper.lua - compilation may have failed"
    exit 1
fi

# Run busted on the compiled spec files
cd "$TEMP_DIR"
echo "Running tests..."
unbuffer busted --helper=spec/helper.lua spec/ | sed 's/\.lua/\.yue/g'
