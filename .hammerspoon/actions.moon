flipScreens = ->
  for _, screen in ipairs hs.screen.allScreens!
    currentRotation = screen\rotate!
    if currentRotation == 90
      screen\rotate(270)
    else
      screen\rotate(90)

rotateSecondaryScreen = ->
  screens = hs.screen.allScreens!
  table.sort(screens, (s1, s2) -> s1\id! < s2\id!)
  {primary, secondary} = screens

  primaryRotation = primary\rotate!
  secondaryRotation = secondary\rotate!

  if secondaryRotation == 0
    if primaryRotation == 90
      secondary\rotate(270)
    else
      secondary\rotate(90)
  else
    secondary\rotate(0)

editConfig = -> hs.execute '~/bin/emacsclient -n ~/.hammerspoon/index.moon'

parkMouse = -> hs.mouse.setAbsolutePosition{x: 2^13, y: 2^13}

swapScreen = (window) ->
  with window
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

maximize = ->
  with window = hs.window.focusedWindow!
    with hs.grid
      .setGrid("1x1")
      .setMargins("0x0")
      .set(window, "0, 0, 1x1")

toggleHazeOver = ->
  _, isEnabled = hs.osascript.applescript 'tell application "HazeOver" to get enabled'
  hs.osascript.applescript "tell application \"HazeOver\" to set enabled to #{not isEnabled}"


setHazeOver = (intensity) ->
  hs.osascript.applescript "tell application \"HazeOver\" to set enabled to true"
  hs.osascript.applescript "tell application \"HazeOver\" to set intensity to #{intensity}"

-- https://forum.justgetflux.com/topic/1928/how-to-create-keyboard-shortcuts/10
toggleMovieMode = ->
  applescript = '
    -- Settings --

    property mainItem : "Color Effects"
    -- set to "Preferences...", "Color Effects", "Disable", etc.,
    -- make sure to use quote marks.

    property subItem : "Movie mode"
    -- set to submenu item, if there is one. Use "for this app" with Disable,
    -- to toggle disable for the current application.

    -- end of Settings --

    if mainItem is "Disable" and subItem is "for this app" then set subItem to 3

    tell application "System Events"
      tell application process "Flux"
        tell menu bar 2
          tell menu bar item 1
            try
              with timeout of 0.1 seconds
                perform action "AXPress"
              end timeout
            end try
          end tell
        end tell
      end tell
    end tell
    do shell script "killall \'System Events\'"
    tell application "System Events"
      tell application process "Flux"
        tell menu bar 2
          tell menu bar item 1
            tell menu 1
              tell menu item mainItem
                perform action "AXPress"
              end tell
              if menu 1 of menu item mainItem exists then
                tell menu 1 of menu item mainItem
                  tell menu item subItem
                    perform action "AXPress"
                  end tell
                end tell
              end if
            end tell
          end tell
        end tell
        if mainItem is "Preferences..." then set frontmost to true
      end tell
    end tell
  '
  hs.eventtap.keyStroke({"ctrl"}, "F2")
  hs.timer.doAfter 0.2, ->
    hs.osascript.applescript applescript

{
:flipScreens, :rotateSecondaryScreen, :editConfig, :parkMouse, :swapScreen,
:toggleFullScreen, :halfScreen, :maximize, :toggleHazeOver, :setHazeOver,
:toggleMovieMode
}
