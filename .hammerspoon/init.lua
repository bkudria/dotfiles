-- -*- dash-at-point-docset: "hammerspoon,lua" -*-

local lVer = _VERSION:match("Lua (.+)$")
-- specify luarockt path yourself if this doesn't find it in the normal places
local luarocks = "/opt/homebrew/bin/luarocks"
if #luarocks > 0 then
    package.path = package.path .. ";" .. hs.execute(
            luarocks .. " --lua-version " .. lVer .. " path --lr-path"
        ):gsub("\n", "")
    package.cpath = package.cpath .. ";" .. hs.execute(
            luarocks .. " --lua-version " .. lVer .. " path --lr-cpath"
        ):gsub("\n", "")
end

require("moonscript_traceback").add()
require 'moonscript'
require 'index'

