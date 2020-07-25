(spoonConfig) ->
  hs.loadSpoon('SpoonInstall')
  for name, arg in pairs spoonConfig
    spoon.SpoonInstall\andUse(name, arg)
