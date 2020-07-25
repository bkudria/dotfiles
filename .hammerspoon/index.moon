require 'reloader'
require 'hammerspoon'

spoons = require 'spoons'
withTurnTouchConfig = require 'turntouch'
apps = require 'apps'
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
  '\\': apps.OnePassword
  ';': -> spoon.Emojis.toggle!
  '2': -> clock\show!
  '5': -> hs.eventtap.keyStroke( {'cmd', 'shift'} , '/' )
  'b': matte
  'd': nil -- Dash
  'e': apps.Emacs
  'r': apps.Reeder
  's': Hyper(name: 'System', afterAction: (hyper) -> hyper.modal\exit!)\space{
    'l': hs.caffeinate.startScreensaver
    's': hs.caffeinate.systemSleep
    '-': actions.flipScreens
    ',': actions.editConfig
  }
  't': apps.iTerm
  'v': -> spoon.TextClipboardHistory\toggleClipboard!
  'w': Hyper(name: 'Window', afterAction: (hyper) -> hyper.modal\exit!)\space{
    '3': -> actions.toggleHazeOver!
    'space': -> actions.toOtherScreen!
    'w': -> actions.toggleFullScreen!
    'j': -> actions.halfScreen('south')
    'k': -> actions.halfScreen('north')
    '`': -> hs.hints.windowHints(hs.window.filter.default\getWindows!)
    'escape': (hyper) -> hyper.modal\exit!
  }
  'z': apps.Zoom
}

export turntouch = withTurnTouchConfig{
  n: {
    n: 'k'
    s: {
      [apps.Reeder]: 'j',
      [apps.Chrome]: '\\tts'
    }
    w: -> hs.eventtap.keyStroke {}, 'SPACE'
    e: {
      [apps.Reeder]: 'b',
      [apps.Chrome]: '\\tte'
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
