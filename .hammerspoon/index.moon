require 'reloader'
require 'hammerspoon'

spoons = require 'spoons'
withTurnTouchConfig = require 'turntouch'
{App, apps} = require 'apps'
actions = require 'actions'
mouseValet = require 'mouse-valet'
Hyper = require 'hyper'
Matte = require 'matte'
Clock = require 'clock'

matte = Matte!\wrap(apps.Chrome)
clock = Clock!

spoons{
  Emojis: {
    fn: (emojis) ->
      emojis.chooser\rows(20)
      emojis.toggle = ->
        emojis.chooser\isVisible! and emojis.chooser\hide! or emojis.chooser\show!
  }
  Seal: {
    start: true,
    hotkeys: {toggle: {{'cmd'}, 'SPACE'}},
    fn: (seal) ->
      seal\loadPlugins{'apps', 'useractions', 'calc'}
      seal.plugins.useractions.actions = {Sleep: {fn: -> hs.caffeinate.systemSleep!}}
  }
  TextClipboardHistory: {start: true, config: {hist_size: 2^16, paste_on_select: true, show_in_menubar: false}}
}

Hyper!\space{
  'down': -> clock\show!
  'left': -> App.current!\prev!
  'right': -> App.current!\next!
  '-': apps.OnePassword
  ';': -> spoon.Emojis.toggle!
  '\'': -> hs.eventtap.keyStroke( {'cmd', 'shift'} , '/' )
  'b': matte
  'd': nil -- Dash
  'e': apps.Emacs
  'k': apps.Slack
  'r': apps.Reeder
  's': Hyper(name: 'System', afterAction: (hyper) -> hyper.modal\exit!)\space{
    'l': hs.caffeinate.startScreensaver
    's': hs.caffeinate.systemSleep
    'r': actions.rotateSecondaryScreen
    '-': actions.flipScreens
    ',': actions.editConfig
  }
  't': apps.iTerm
  'v': -> spoon.TextClipboardHistory\toggleClipboard!
  'w': Hyper(name: 'Window', afterAction: (hyper) -> hyper.modal\exit!)\space{
    '0': ->
      hs.eventtap.keyStroke({"ctrl"}, "1")
      hs.eventtap.keyStroke({"ctrl"}, "2")
    '3': -> actions.toggleHazeOver!
    'space': -> actions.swapScreen(hs.window.focusedWindow!)
    'w': -> actions.toggleFullScreen!
    'j': -> actions.halfScreen('south')
    'k': -> actions.halfScreen('north')
    'm': -> actions.maximize!
    '`': -> hs.hints.windowHints(hs.window.filter.default\getWindows!)
    'escape': (hyper) -> hyper.modal\exit!
  }
  'z': apps.Zoom
}

export turntouch = withTurnTouchConfig{
  n: {
    n: {
      [apps.Reeder]: 'k',
      [apps.Chrome]: '\\ttn'
    }
    s: {
      [apps.Chrome]: '\\tts',
      [apps.Reeder]: 'j',
      [apps.Slack]: apps.Slack.switchNextUnread
      [apps.Zoom]: -> hs.eventtap.keyStroke {'cmd', 'shift'}, 'a',
    }
    e: {
      [apps.Reeder]: 'b',
      [apps.Chrome]: '\\tte'
    }
    w: {
      [apps.Reeder]: -> hs.eventtap.keyStroke {}, 'SPACE',
      [apps.Chrome]: '\\ttw'
    }
  }
  s: {n: nil, s: nil, e: nil, w: nil}
  e: {n: nil, s: nil, e: nil, w: nil}
  w: {n: nil, s: nil, e: nil, w: nil}
}

mouseValet{delay: 10}

matte\start!
clock\start!

hs.notify.show("Hammerspoon", "Configuration", "Configuration reload successfully!")
