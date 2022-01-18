handle = (action) ->
  switch type(action)
    when 'function' then action!
    when 'string'
      if #action == 1
        hs.eventtap.keyStroke({}, action)
      else
        hs.eventtap.keyStrokes(action)
    when 'table'
      for app, subaction in pairs action
        handle(subaction) if app\isFocused!
    else print "unknown action:" and print(hs.inspect(action))

(message, config) -> handle(config[message])
