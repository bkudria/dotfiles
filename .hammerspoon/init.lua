-- -*- dash-at-point-docset: "hammerspoon,lua" -*-
require 'autoreload'

hyper = hs.hotkey.modal.new()

focusedApp = function()
  return hs.window:focusedWindow():application():title()
end

lastApp = false
launch = function(appName, toggle)
  toggle = toggle == nil and true or false
  currentApp = focusedApp()

  if toggle then
    if appName == currentApp then
      target = lastApp
      lastApp = currentApp
    else
      lastApp = currentApp
      target = appName
    end
  else
    target = appName
  end

  hs.application.launchOrFocus(target)
end

triggerHyper = function(key)
  hs.eventtap.keyStroke({"cmd","alt","shift","ctrl"}, key)
end

handle = function(key, action)
  if type(action) == 'function' then
    action()
  elseif type(action) == 'string' then
    launch(action)
  else
    triggerHyper(key)
  end
  hyper.triggered = true
end

whenFocused = function(app, func)
  hs.timer.waitUntil(function() return focusedApp() == app end, func, 0.1)
end

slackAndCmdK = function()
  launch('Slack', false)
  whenFocused('Slack', function()
                hs.eventtap.keyStroke({'cmd'}, 'k')
  end)
end

chromeAndFocusPage = function()
  launch('Google Chrome', false)
  whenFocused('Google Chrome', function()
                hs.eventtap.keyStroke({'cmd'}, 'f')
                hs.eventtap.keyStroke({}, 'escape')
  end)
end

editHSConfig = function() hs.execute('/usr/local/bin/emacsclient -n ~/.hammerspoon/init.lua') end

apps = {
  {',', editHSConfig},
  {'b', chromeAndFocusPage},
  {'c', nil},
  {'d', nil},
  {'e', 'Emacs'},
  {'f', 'Caprine'},
  {'h', function() triggerHyper('g') end},
  {'k', slackAndCmdK},
  {'l', slackAndCmdK},
  {'r', 'Reeder'},
  {'t', 'iTerm'},
  {'w', nil},
  {'x', nil}
}

for i, app in ipairs(apps) do
  hyper:bind({}, app[1], function() handle(app[1], app[2]) end)
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
hs.hotkey.bind({}, 'F18', pressedF18, releasedF18)
hs.hotkey.bind({'shift'}, 'F18', pressedF18, releasedF18)
hs.alert("!")
