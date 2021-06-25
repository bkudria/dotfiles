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
  turntouchServer = hs.httpserver.new!
  turntouchServer\setInterface('localhost')
  turntouchServer\setPort('5555')
  turntouchServer\setCallback (method, path, headers, body) ->
    major, minor = path\match('^/(%l)(%l)$')
    handleTurnTouch(turntouchConfig[major][minor])
    '', 200, {}

  turntouchServer\start!
  turntouchServer
