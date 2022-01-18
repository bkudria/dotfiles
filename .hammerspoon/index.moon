require 'reloader'
require 'hammerspoon'

spoons = require 'spoons'
dispatch = require 'dispatch'
{App, apps} = require 'apps'
actions = require 'actions'
-- mouseValet = require 'mouse-valet'
Hyper = require 'hyper'
Matte = require 'matte'
Clock = require 'clock'

matte = Matte!\wrap(apps.Chrome)
clock = Clock!

spoons{
  Seal: {
    start: true,
    hotkeys: {toggle: {{'cmd'}, 'SPACE'}},
    fn: (seal) ->
      seal.show = (self, query) ->
        frame = hs.application.frontmostApplication!\focusedWindow!\screen!\frame!
        center = frame.center
        self.chooser\query('')
        self.chooser\show(center\move({frame.w * -0.2, frame.h * -0.2}))
        self

      seal\loadPlugins{'apps', 'useractions', 'calc'}
      seal.plugins.useractions.actions = {Sleep: {fn: -> hs.caffeinate.systemSleep!}}
      seal.plugins.apps.appSearchPaths = {
        "/Applications", "/System/Applications",
        "/System/Library/PreferencePanes", "~/Applications"
      }
      seal.plugins.apps\restart!
  }
  TextClipboardHistory: {start: true, config: {hist_size: 2^16, paste_on_select: true, show_in_menubar: false}}
}

Hyper!\space{
  '`': -> clock\show!
  '1': -> clock\showDate!
  'left': -> App.current!\left!
  'right': -> App.current!\right!
  'up': -> App.current!\up!
  'down': -> App.current!\down!
  ']': apps.OnePassword
  ';': -> hs.eventtap.keyStroke( {'cmd', 'ctrl'} , 'SPACE' )
  '[': -> hs.eventtap.keyStroke( {'cmd', 'shift'} , '/' )
  '-': nil -- Notification Center
  'b': matte
  'c': nil -- Fantastical
  'd': nil -- Dash
  'e': apps.Emacs
  'k': apps.Slack
  'r': apps.Reeder
  's': Hyper(name: 'System', afterAction: (hyper) -> hyper.modal\exit!)\space{
    'delete': hs.caffeinate.systemSleep
    'l': hs.caffeinate.startScreensaver
    'r': actions.rotateSecondaryScreen
    'f': actions.flipScreens
    ',': actions.editConfig
    'escape': (hyper) -> hyper.modal\exit!
  }
  't': apps.iTerm
  'v': -> spoon.TextClipboardHistory\toggleClipboard!
  'w': Hyper(name: 'Window', afterAction: (hyper) -> hyper.modal\exit!)\space{
    '0': ->
      hs.eventtap.keyStroke({"ctrl"}, "1")
      hs.eventtap.keyStroke({"ctrl"}, "2")
    '2': -> actions.toggleMovieMode!
    '3': -> actions.toggleHazeOver!
    '4': -> actions.setHazeOver(30)
    '5': -> actions.setHazeOver(90)
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

export rButton = (message) -> dispatch message, {
  tapup:    App.current!\rUp
  tapdown:  App.current!\rDown
  tapleft:  App.current!\rLeft
  tapright: App.current!\rRight

  back: App.current!\rBack
  screen: App.current!\rScreen

  playpause: App.current!\rPlayPause
  mute: App.current!\rMute

  plus: App.current!\rPlus
  minus: App.current!\rMinus

  microphone: App.current!\toggleAudio
  power: App.current!\toggleVideo
}

-- mouseValet{delay: 10}

matte\start!
clock\start!

hs.notify.show("Hammerspoon", "Configuration", "Configuration reload successfully!")
