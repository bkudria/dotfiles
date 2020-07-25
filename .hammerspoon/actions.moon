flipScreens = ->
  for _, screen in ipairs hs.screen.allScreens!
    currentRotation = screen\rotate!
    if currentRotation == 90
      screen\rotate(270)
    else
      screen\rotate(90)

editConfig = -> hs.execute '/usr/local/bin/emacsclient -n ~/.hammerspoon/index.moon'

parkMouse = -> hs.mouse.setAbsolutePosition{x: 2^13, y: 2^13}

toOtherScreen = ->
  with hs.window.focusedWindow!
    frame = \frame!\toUnitRect(\screen!\frame!)
    mvFn = -> \move({0, 0, frame.w, frame.h}, \screen!\next!)

    if \isFullscreen!
      hs.timer.waitUntil(
        -> not \isFullscreen!,
        -> mvFn! and \setFullScreen(true) and \focus!)
      \setFullScreen(false)
    else
      mvFn!
      \focus!


toggleFullScreen = ->
  with hs.window.focusedWindow!
    isFullScreen = \isFullscreen!
    \setFullScreen(not isFullScreen)

halfScreen = (direction) ->
  with window = hs.window.focusedWindow!
    \setFullScreen(false)

    with hs.grid
      .setGrid("1x2")
      .setMargins("0x0")
      .set(window, "0, #{direction == "north" and 0 or 1}, 1x1")

toggleHazeOver = ->
  _, isEnabled = hs.osascript.applescript 'tell application "HazeOver" to get enabled'
  print isEnabled
  hs.osascript.applescript "tell application \"HazeOver\" to set enabled to #{not isEnabled}"

{:flipScreens, :editConfig, :parkMouse, :toOtherScreen, :toggleFullScreen, :halfScreen, :toggleHazeOver}
