#!/bin/bash

set -euo pipefail

# Get the directory where the script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

# Create a temp directory for processed files
TEMP_DIR=$(mktemp -d)
trap 'rm -rf "$TEMP_DIR"' EXIT

echo "Compiling Yuescript files..."

# Compile all Yuescript files to the temp directory
for yue_file in *.yue; do
    if [ -f "$yue_file" ]; then
        yue -r -t "$TEMP_DIR" "$yue_file"
        echo "Compiled $yue_file"
    fi
done

# Compile spec files
if ! yue -r -t "$TEMP_DIR/spec" spec; then
    echo "Error: Failed to compile Yuescript specs"
    exit 1
fi

# Create a helper script that will be loaded before tests
cat > "$TEMP_DIR/spec/helper.lua" << 'EOF'
-- Make modules available globally for specs
local function capitalize(str)
    return str:gsub("^%l", string.upper)
end

-- Load all modules and make them available globally with capitalized names
local function loadModules()
    local modules = {}
    local files = io.popen('ls *.lua 2>/dev/null'):lines()

    for file in files do
        if not file:match("_spec%.lua$") and file ~= "helper.lua" then
            local moduleName = file:gsub("%.lua$", "")
            local success, module = pcall(require, moduleName)
            if success then
                local globalName = capitalize(moduleName)
                _G[globalName] = module
                print("Loaded module " .. moduleName .. " as global " .. globalName)
            end
        end
    end
end

loadModules()
EOF

# Run busted on the compiled spec files
cd "$TEMP_DIR"
echo "Running tests..."
busted --helper=spec/helper.lua spec/ | sed 's/\.lua/\.yue/g'
