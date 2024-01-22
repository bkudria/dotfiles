basename = (str) -> string.gsub(str, "(.*/)(.*)", "%2")
endswith = (ext) -> "^[^#][a-z%-]+%.#{ext}"

maybeReload = (paths, flags) ->
  reloadConditions = for i, path in pairs paths
    filename = basename(path)

    continue unless filename\match(endswith('lua')) or filename\match(endswith('moon'))
    continue if filename\match("^%.#.*")
    -- hs.alert.show("✅", 0.2)
    -- hs.timer.doAfter(0.3, hs.reload)
    hs.reload!

hs.pathwatcher.new("#{os.getenv('HOME')}/.hammerspoon/", maybeReload)\start!
