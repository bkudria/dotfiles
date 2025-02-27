#!/bin/bash

set -euo pipefail

# Get the directory where the script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

# Create a temp directory for processed files
TEMP_DIR=$(mktemp -d)
trap 'rm -rf "$TEMP_DIR"' EXIT

echo "Compiling Yuescript files..."

# Find all spec files to determine which modules to compile
SPEC_FILES=()
for spec_file in spec/*_spec.yue; do
    if [ -f "$spec_file" ]; then
        SPEC_FILES+=("$spec_file")
    fi
done

# Extract module names from spec files
MODULES=()
for spec_file in "${SPEC_FILES[@]}"; do
    # Extract module name from spec file (e.g., "drive" from "drive_spec.yue")
    module_name=$(basename "$spec_file" _spec.yue)
    if [ -f "${module_name}.yue" ]; then
        MODULES+=("${module_name}")
    fi
done

# Compile only the needed modules
for module in "${MODULES[@]}"; do
    if [ -f "${module}.yue" ]; then
        # Capture output but only display it if there's an error
        output=$(yue -r -t "$TEMP_DIR" "${module}.yue" 2>&1) || {
            echo "Error compiling ${module}.yue:"
            echo "$output"
            exit 1
        }
    fi
done

# Compile spec files (capture output but only display it if there's an error)
mkdir -p "$TEMP_DIR/spec"
output=$(yue -r -t "$TEMP_DIR/spec" spec 2>&1) || {
    echo "Error compiling Yuescript specs:"
    echo "$output"
    exit 1
}

# Ensure the mocks directory exists
mkdir -p "$TEMP_DIR/spec/mocks"

# Compile the mock hs module
if [ -f "spec/mocks/hs.yue" ]; then
    output=$(yue -r -t "$TEMP_DIR/spec/mocks" spec/mocks/hs.yue 2>&1) || {
        echo "Error compiling mock hs module:"
        echo "$output"
        exit 1
    }
fi

# Compile helper.yue into the temp spec directory
if [ -f "spec/helper.yue" ]; then
    output=$(yue -r -t "$TEMP_DIR/spec" spec/helper.yue 2>&1) || {
        echo "Error compiling helper.yue:"
        echo "$output"
        exit 1
    }
else
    echo "Missing helper"
    exit 1
fi

# Run busted on the compiled spec files
cd "$TEMP_DIR"
echo "Running tests..."
unbuffer busted --helper=spec/helper.lua spec/ | sed 's/\.lua/\.yue/g'
