#!/bin/bash
cd "$(dirname "$0")"

# Add LuaRocks paths to environment
eval "$(luarocks path)"

# Ensure that LUA_PATH and LUA_CPATH are exported
export LUA_PATH
export LUA_CPATH

# Get the LuaRocks bin directory and ensure it's in PATH
LUAROCKS_BIN_DIR=$(luarocks config bin_dir)
export PATH="$LUAROCKS_BIN_DIR:$PATH"

# Debug: Print paths to verify
echo "LUA_PATH: $LUA_PATH"
echo "LUA_CPATH: $LUA_CPATH"
echo "PATH: $PATH"

# Set LUA_PATH to include current directory and ensure Lua can find 'busted.modules.files.yue'
export LUA_PATH="$PWD/?.lua;$PWD/?/init.lua;$LUA_PATH"

# Debug: Print final paths
echo "Final LUA_PATH: $LUA_PATH"
echo "Final LUA_CPATH: $LUA_CPATH"

# Confirm which busted is being used
echo "Using busted from: $(which busted)"

# Run busted
busted --pattern='.*_spec.yue' --helper=spec/helper.lua --loaders=yue,lua spec
