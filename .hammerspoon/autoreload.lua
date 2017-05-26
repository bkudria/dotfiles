function basename(str)
  local name = string.gsub(str, "(.*/)(.*)", "%2")
  return name
end

local function reloadConfig(files)
  local doReload = false
  for _, file in pairs(files) do
    if basename(file):match('^[^#][a-z%-]+%.lua') then
      doReload = true
    end
  end
  if doReload then
    hs.reload()
  end
end


watcher = hs.pathwatcher.new(os.getenv('HOME')..'/.hammerspoon/', reloadConfig):start()
