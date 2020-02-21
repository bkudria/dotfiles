-- -*- dash-at-point-docset: "hammerspoon,lua" -*-
require 'autoreload'
-- require 'hyperlauncher'

flipScreens = function()
   for i, screen in ipairs(hs.screen.allScreens()) do
      print(screen)
      currentRotation = screen:rotate()
      if currentRotation == 90 then
         screen:rotate(270)
      else
         screen:rotate(90)
      end
   end
end

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
  hs.timer.waitUntil(function() return focusedapp() == app end, func, 0.001)
end

slackAndCmdK = function()
  if focusedApp() == 'Slack' then
    hs.eventtap.keyStroke({'cmd'}, 't')
  else
    launch('/Applications/Slack.app')
  end
end

chromeAndChooseTab = function()
  if focusedApp() == 'Google Chrome' then
    hs.eventtap.keyStroke({'shift'}, 't')
  else
    launch('Google Chrome')
  end
end

iTermAndChoose = function()
  if focusedApp() == 'iTerm2' then
    hs.eventtap.keyStroke({'cmd', 'shift'}, 'o')
    hs.eventtap.keyStrokes('/f ')
  else
    launch('iTerm')
  end
end

menuBarTrailerApp = function()
  hs.eventtap.keyStroke({'ctrl'}, 'F15')
  hs.eventtap.keyStroke({}, 'tab')
  hs.eventtap.keyStroke({'ctrl', 'cmd'}, 'g')
end

editHSConfig = function() hs.execute('/usr/local/bin/emacsclient -n ~/.hammerspoon/init.lua') end

apps = {
  {'\\', nil}, -- 1Password
  {',', editHSConfig},
  {'-', flipScreens},
  {'b', chromeAndChooseTab},
  {'c', nil}, -- Fantastical
  {'d', nil}, -- Dash
  {'e', '/Applications/Emacs.app'},
  {'f', 'Caprine'},
  {'g', menuBarTrailerApp},
  {'h', nil}, -- Trailer.app
  {'k', slackAndCmdK},
  {'l', slackAndCmdK},
  {'m', 'SoundMate'},
  {'r', 'Reeder'},
  {'s', hs.caffeinate.startScreensaver },
  {'t', iTermAndChoose},
  {'v', nil}, -- alfred clipboard
  {'w', nil}, -- Moom
  {'tab', function() triggerHyper('tab') end},
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



-- hs.hotkey.bind({'shift'}, 'F18', pressedF18, releasedF18)
hs.alert("!")
for i, app in ipairs(apps) do
  hyper:bind({}, app[1], function() handle(app[1], app[2]) end)
end
