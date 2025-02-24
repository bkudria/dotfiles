local path = require 'pl.path'
local ok, yue = pcall(require, 'yue')

local ret = {}

ret.match = function(busted, filename)
  return ok and path.extension(filename) == '.yue'
end

ret.load = function(busted, filename)
  local file, err = yue.loadfile(filename)
  if not file then
    busted.publish({ 'error', 'file' }, { descriptor = 'file', name = filename }, nil, err, {})
  end
  return file
end

return {
  match = ret.match,
  load = ret.load
}
