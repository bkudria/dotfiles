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

# Create a helper script that will be loaded before tests
cat > "$TEMP_DIR/spec/helper.lua" << 'EOF'
-- Load mocks first
package.path = "./spec/mocks/?.lua;" .. package.path
_G.hs = require('spec.mocks.hs')

-- Make modules available globally for specs
local function capitalize(str)
    return str:gsub("^%l", string.upper)
end

-- Load only modules that have corresponding spec files
local function loadModules()
    local loaded = {}
    local files = io.popen('ls spec/*_spec.lua 2>/dev/null'):lines()
    
    for spec_file in files do
        local module_name = spec_file:match("spec/(.-)_spec%.lua$")
        if module_name and not loaded[module_name] then
            local module_file = module_name .. ".lua"
            if io.open(module_file, "r") then
                local success, module = pcall(require, module_name)
                if success then
                    local globalName = capitalize(module_name)
                    _G[globalName] = module
                    loaded[module_name] = true
                end
            end
        end
    end
end

loadModules()
EOF

# Run busted on the compiled spec files
cd "$TEMP_DIR"
echo "Running tests..."
unbuffer busted --helper=spec/helper.lua spec/ | sed 's/\.lua/\.yue/g'
