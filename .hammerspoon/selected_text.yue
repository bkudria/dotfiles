() ->
  element = hs.uielement.focusedElement!
  selection = nil
  if element
    selection = element\selectedText!

  if (not selection) or (selection == "")
    hs.eventtap.keyStroke({"cmd"}, "c")
    hs.timer.usleep(20000)
    selection = hs.pasteboard.getContents!

  selection or ""
