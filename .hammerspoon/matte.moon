-- https://bugs.chromium.org/p/chromium/issues/detail?id=854609
class Matte
  new: =>
    @lastApp = nil
    @watcher = nil

  wrap: (app) =>
    @app = app
    @watcher = hs.application.watcher.new (name, event) ->
      if event == hs.application.watcher.activated
        if name == 'Google Chrome' and @lastApp == 'Reeder'
          hs.application.launchOrFocus(@lastApp)
        else
          @lastApp = name

    @

  handle: =>
    @lastApp = nil
    @app\handle!

  start: =>
    @watcher\start!
