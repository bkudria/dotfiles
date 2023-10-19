utf8 = require 'utf8'
keyMap = require"hs.keycodes".map

symbols = ")!@#$%^&*("
letters = "abcdefghijklmnopqrstuvwxyz"

symbols_unshifted = { "#{symbols\sub(i,i)}", "#{i - 1}" for i = 1,10 }
letters_unshifted = { "#{letters\sub(i,i)\upper!}", "#{letters\sub(i,i)}" for i = 1,#letters }

shift_map = {}
for k, v in pairs symbols_unshifted do shift_map[k] = v
for k, v in pairs letters_unshifted do shift_map[k] = v

char_map = {
  [" "]: "space"
}

-- https://github.com/Hammerspoon/hammerspoon/issues/1881
keyPress = (key, mods = {}) ->
  { hs.eventtap.event.newKeyEvent(mods, key, true), hs.eventtap.event.newKeyEvent(mods, key, false) }


charPress = (char) ->
  mods, key = if shift_map[char]
    {"shift"}, shift_map[char]
  else
    {}, (char_map[char] or char)

  keyPress(key, mods)

charPresses = (string) -> hs.fnutils.mapCat([char for char in string\gmatch(".")], charPress)
deletePresses = (count) -> hs.fnutils.mapCat([i for i = 1, count], -> keyPress("delete"))

resolveAction = (action, word) ->
  switch type(action)
    when 'function'
      status, result = pcall(action, word)
      if status
        result
      else
        "error"
    when 'table' then action
    else
      print "unknown action:"
      print(hs.inspect(action))
      "error"

class Typer
  new: (snippets) =>
    @word = ""
    @snippets = snippets
    @tap = hs.eventtap.new({ hs.eventtap.event.types.keyUp }, (event) -> @keyHandler(event))

  start: =>
    @tap\start!

  keyHandler: (event) =>
    key = keyMap[event\getKeyCode!]

    newEvents = { event }

    -- build @word
    switch key
      when "delete"
        if #@word > 0
          -- remove the last char from a string with support to utf8 characters
          t = [chars for _, chars in utf8.codes(@word)]
          table.remove(t, #t)
          @word = utf8.char(table.unpack(t))
      when "space", "return", "up", "down", "left", "right"
        @word = ""
      else
        flags = event\getFlags!
        if flags\containExactly({}) or flags\containExactly({"shift"})
          @word = @word .. event\getCharacters!
        else
          @word = ""

    if action = @snippets[@word]
      hs.fnutils.concat(newEvents, deletePresses(#@word))
      result = resolveAction(action, @word)
      @word = ""
      for line in *result
        hs.fnutils.concat(newEvents, charPresses(line))
        hs.fnutils.concat(newEvents, keyPress("return")) if #result > 1

    true, newEvents
