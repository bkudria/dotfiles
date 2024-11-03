allScreensRect = hs.fnutils.reduce(hs.screen.allScreens!, (screen1, screen2) ->
  screen1\fullFrame!\union(screen2\fullFrame!)
)

(args) ->
  watcher = nil
  timer = nil
  pos = hs.mouse.getAbsolutePosition!

  watcher = hs.eventtap.new {hs.eventtap.event.types.mouseMoved}, (event) ->
    watcher\stop!
    timer\start!

  timer = hs.timer.doEvery args.delay, ->
    newPos = hs.mouse.getAbsolutePosition!
    if newPos.x != pos.x or newPos.y != pos.y
      pos = newPos
    else
      hs.mouse.setAbsolutePosition(allScreensRect.center)
      watcher\start!
      timer\stop!
