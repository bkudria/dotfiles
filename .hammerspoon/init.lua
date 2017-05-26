require 'autoreload'

hyper = hs.hotkey.modal.new()

launch = function(appname)
  hs.application.launchOrFocus(appname)
end

launchOrCall = function(arg)
  if type(arg) == 'function' then
    arg()
  else
    launch(arg)
  end
  hyper.triggered = true
end

whenFocused = function(app, func)
  hs.timer.waitUntil(
    function()
      title = hs.window:focusedWindow():application():title()
      print(title)
      return title == app
    end,
    func,
    0.1
  )
end

slackAndCmdK = function()
  launch('Slack')
  whenFocused('Slack', function()
                hs.eventtap.keyStroke({'cmd'}, 'k')
                hs.eventtap.keyStroke({}, 'return')
  end)
end

apps = {
  {'b', 'Google Chrome'},
  {'e', 'Emacs'},
  {'t', 'iTerm'},
  {'l', 'Slack'},
  {'k', slackAndCmdK},
}

for i, app in ipairs(apps) do
  hyper:bind({}, app[1], function() launchOrCall(app[2]); hyper:exit(); end)
end

-- -- Sequential keybindings, e.g. Hyper-a,f for Finder
-- a = hs.hotkey.modal.new({}, "F16")
-- apps = {
--   {'d', 'Twitter'},
--   {'f', 'Finder'},
--   {'s', 'Skype'},
-- }
-- for i, app in ipairs(apps) do
--   a:bind({}, app[1], function() launch(app[2]); a:exit(); end)
-- end

-- pressedA = function() a:enter() end
-- releasedA = function() end
-- hyper:bind({}, 'a', nil, pressedA, releasedA)

-- Shortcut to reload config


ofun = function()
  hs.reload()
  hs.alert.show("Config loaded")
  hyper.triggered = true
end
hyper:bind({}, 'o', nil, ofun)

-- Enter Hyper Mode when F18 (Hyper/Capslock) is pressed
pressedF18 = function()
  hyper.triggered = false
  hyper:enter()
end

-- Leave Hyper Mode when F18 (Hyper/Capslock) is pressed,
--   send ESCAPE if no other keys are pressed.
releasedF18 = function()
  hyper:exit()
  if not hyper.triggered then
    hs.eventtap.keyStroke({}, 'SPACE')
  end
end

-- Bind the Hyper key
f18 = hs.hotkey.bind({}, 'F18', pressedF18, releasedF18)
