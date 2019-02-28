-- -*- dash-at-point-docset: "hammerspoon,lua" -*-
require 'autoreload'

hyper = hs.hotkey.modal.new()

focusedApp = function()
  return hs.application:frontmostApplication():title()
end

launch = function(appName)
  hs.application.launchOrFocus(appName)
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
  hs.timer.waitUntil(function() return focusedApp() == app end, func, 0.001)
end

slackAndCmdK = function()
  if focusedApp() == 'Slack' then
    hs.eventtap.keyStroke({'cmd'}, 't')
  else
    slackApp()
    whenFocused('Slack', function()
                  hs.eventtap.keyStroke({'cmd'}, 't')
    end)
  end
end

chromeAndFocusPage = function()
  launch('Google Chrome')
  whenFocused('Google Chrome', function()
                hs.eventtap.keyStroke({'shift'}, 'f6')
  end)
end

slackApp = function()
  launch('/Applications/Slack.app')
end

editHSConfig = function() hs.execute('/usr/local/bin/emacsclient -n ~/.hammerspoon/init.lua') end

apps = {
  {',', editHSConfig},
  {'b', chromeAndFocusPage},
  {'c', nil},
  {'d', nil},
  {'e', '/Applications/Emacs.app'},
  {'f', 'Caprine'},
  {'g', nil},
  {'h', function() triggerHyper('g') end},
  {'k', slackAndCmdK},
  {'l', slackApp},
  {'m', 'SoundMate'},
  {'r', 'Reeder'},
  {'s', hs.caffeinate.startScreensaver },
  {'t', 'iTerm'},
  {'w', nil},
  {'x', nil},
  {'y', nil}
}

for i, app in ipairs(apps) do
  hyper:bind({}, app[1], function() handle(app[1], app[2]) end)
end

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
