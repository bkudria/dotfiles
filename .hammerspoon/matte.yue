class Matte
  new: =>
    @logger = hs.logger.new("matte", "info")

    @lastApp = nil
    @appWatcher = nil

    @filters = {}
    @coaslescingLayoutTimers = {}

    @tabs = {}
    -- @chooser\queryChangedCallback(() -> @searchTabs!)

  -- searchTabs: () =>
  --   query = @chooser\query!
  --   results = @clipper\search(query)
  --   choices = hs.fnutils.imap(results, (item) ->
  --     { text: item.clip, subText: os.date("%b %d, %H:%M:%S", item.ts) })
  --   if #query > 1 and #results > 1
  --     choices = hs.fnutils.concat(
  --       choices, {{
  --         text: table.concat(hs.fnutils.imap(results, (result) -> result.clip), "\n"),
  --           subText: "Paste all #{#results} items"
  --       }}
  --     )
  --   @chooser\choices(choices)

  chooseTab: (callback) =>
    @tabChooser = hs.chooser.new(callback)
    @tabChooser\choices -> @tabs
    @tabs = {}
    hs.task.new("/Users/bkudria/bin/chrome-tabs", nil, (_task, stdOut, _stdErr)->
      @parseTabs(stdOut)
      @tabChooser\refreshChoicesCallback(true)
      true
    )\start!
    @chooserCallback = callback
    @tabChooser\query(nil)
    @tabChooser\show!


  parseTabs: (stdOut) =>
    return true if #stdOut == 0
    lines = hs.fnutils.split(stdOut, '\n', true)
    items = hs.fnutils.imap(lines, (line) ->
      return nil if #line == 0
      item = hs.fnutils.split(line, " ", 1, true)
      url = item[1]
      title = item[2] or "[untitled]"
      {text: title, subText: url, title: title, url: url}
    )

    hs.fnutils.concat(
      @tabs,
      items
    )

  wrap: (app) =>
    @app = app

    -- https://bugs.chromium.org/p/chromium/issues/detail?id=854609
    @appWatcher = hs.application.watcher.new (name, event) ->
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
    @coaslescingLayoutTimers = [@makeCoaslescingLayoutTimerFor(screen) for screen in *hs.screen.allScreens!]
    @appWatcher\start!

  makeCoaslescingLayoutTimerFor: (screen) =>
    screenHint = screen\getUUID!
    filter = hs.window.filter.new({
      "Google Chrome": {
        visible: true,
        fullscreen: false,
        hasTitlebar: true,
        allowScreens: screenHint,
        allowRoles: {'AXStandardWindow'}
      }
    })

    phi =  ((1 + math.sqrt(5)) / 2)
    ratio = 1/phi

    screenAspect = screen\frame!.w / screen\frame!.h

    layout1 = hs.window.layout.new({
      {filter, "move 1 foc [0,0,100,100] #{screenHint}"}
    })

    layout2 = if screenAspect > 1 then
      hs.window.layout.new({
        {filter, "move 1 foc 0,0,#{ratio},1 #{screenHint} | move 1 foc #{ratio},0,1,1 #{screenHint}"}
      })
    else
      hs.window.layout.new({
        {filter, "move 1 foc 0,0,1,#{ratio} #{screenHint} | move 1 foc 0,#{ratio},1,1 #{screenHint}"}
      })

    layoutN = if screenAspect > 1 then
      hs.window.layout.new({
        {filter, "move 1 foc 0,0,#{ratio},1 #{screenHint} | move 1 foc #{ratio},0,1,#{ratio} #{screenHint} | tile all foc #{ratio},#{ratio},1,1 #{screenHint}"}
      })
    else
      hs.window.layout.new({
        {filter, "move 1 foc 0,0,1,#{ratio} #{screenHint} | move 1 foc 0,#{ratio},#{ratio},1 #{screenHint} | tile all foc #{ratio},#{ratio},1,1 #{screenHint}"}
      })

    timer = hs.timer.delayed.new(5, () ->
      filter\pause!
      numWindows = #filter\getWindows!
      @logger.i("=== layout #{numWindows} ===")
      layout1\apply! if numWindows == 1
      layout2\apply! if numWindows == 2
      layoutN\apply! if numWindows > 2
      filter\resume!
    )

    @logger.i("init")
    applyLayoutDebounced = (window, name, event) ->
      @logger.i("window event: #{event}, title: #{window\title!}")
      timer\start(0.1)
    filter\subscribe(
      {
        hs.window.filter.windowOnScreen,
        hs.window.filter.windowNotOnScreen,
        hs.window.filter.windowMoved,
        hs.window.filter.windowFocused,
        hs.window.filter.windowUnfocused
      },
      applyLayoutDebounced,
      true
    )

    timer
