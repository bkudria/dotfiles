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
