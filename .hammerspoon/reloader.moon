basename = (str) -> string.gsub(str, "(.*/)(.*)", "%2")
endswith = (ext) -> "^[^#][a-z%-]+%.#{ext}"

maybeReload = (paths, flags) ->
  reloadConditions = for i, path in pairs paths
    filename = basename(path)

    continue unless filename\match(endswith('lua')) or filename\match(endswith('moon'))
    continue if filename\match("^%.#.*")

    hs.reload!

hs.pathwatcher.new("#{os.getenv('HOME')}/.hammerspoon/", maybeReload)\start!
