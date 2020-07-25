require 'hs.ipc'

handleTurnTouch = (action) ->
  switch type(action)
    when 'function' then action!
    when 'string'
      if #action == 1
        hs.eventtap.keyStroke({}, action)
      else
        hs.eventtap.keyStrokes(action)
    when 'table'
      for app, subaction in pairs action
        handleTurnTouch(subaction) if app\isFocused!
    else print "unknown action:" and print(hs.inspect(action))

(turntouchConfig) ->
  (major, minor) -> handleTurnTouch(turntouchConfig[major][minor])
