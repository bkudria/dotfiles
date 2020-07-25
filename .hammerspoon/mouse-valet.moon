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
      -- hs.eventtap.keyStroke({'fn'}, 'F8') -- Hide Cursor
      hs.mouse.setAbsolutePosition{x:2160, y:1920}
      watcher\start!
      timer\stop!
