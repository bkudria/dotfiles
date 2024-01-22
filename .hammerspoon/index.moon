lastTime = hs.timer.absoluteTime!
startTime = lastTime
initLogger = hs.logger.new("init", "debug")
tprint = (lbl) ->
  initLogger.d("⏰ #{(hs.timer.absoluteTime! - lastTime) / 1000000}ms #{lbl}")
  lastTime = hs.timer.absoluteTime!

require 'console'
require 'reloader'
require 'hammerspoon'
require 'hs.ipc'

tprint "require"

spoons = require 'spoons'
dispatch = require 'dispatch'
{App, apps} = require 'apps'
actions = require 'actions'
-- mouseValet = require 'mouse-valet'
Hyper = require 'hyper'
Matte = require 'matte'
Clock = require 'clock'
Flux = require 'flux'
Clipper = require 'clipper'
Typer = require 'typer'
Paster = require 'paster'
yasnippets = require 'yasnippets'
selectedText = require 'selected_text'

tprint "load"

state = hs.watchable.new("state", true)
logger = hs.logger.new("state", "info")
hs.watchable.watch("state.*", (watcher, path, key, old, new) -> logger.i("#{path}.#{key}: #{old} → #{new}"))

logger.i(state["mode"])

tprint "init state"

clock = Clock!
flux = Flux!

tprint "init clock/flux"

matte = Matte!\wrap(apps.Chrome)

tprint "init matte"

typer = Typer(yasnippets(apps.Emacs))
clipper = Clipper!
paster = Paster(clipper, typer)

tprint "init"

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
}

tprint "spoons"

Hyper!\space{
  '`': -> clock\show!
  'tab': typer\autocomplete_word
  '2': -> clock\showDate!
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
  'g': Hyper(name: 'Selected Text', afterAction: (hyper) -> hyper.modal\exit!)\space{
    'u': -> hs.eventtap.keyStrokes(string.upper(selectedText!))
    'l': Hyper(name: 'Link', afterAction: (hyper) -> hyper.modal\exit!)\space{
      'b': -> hs.eventtap.keyStrokes("[#{selectedText!}](#{apps.Chrome.currentURL!})")
      't': -> matte\chooseTab((tab) ->
        hs.eventtap.keyStrokes("[#{selectedText!}](#{tab["url"]})") if tab
      )
      'v': -> hs.eventtap.keyStrokes("[#{selectedText!}](#{hs.pasteboard.getContents!})")
     }
  }
  'i': Hyper(name: "Insert", afterAction: (hyper) -> hyper.modal\exit!)\space{
    'b': Hyper(name: 'Browser Tab', afterAction: (hyper) -> hyper.modal\exit!)\space{
      'b': -> hs.eventtap.keyStrokes(apps.Chrome.currentURL!)
      'l': -> matte\chooseTab((tab) -> hs.eventtap.keyStrokes("[#{tab['title']}](#{tab['url']})") if tab)
      'u': -> matte\chooseTab((tab) -> hs.eventtap.keyStrokes(tab['url']) if tab)
      't': -> matte\chooseTab((tab) -> hs.eventtap.keyStrokes(tab['title']) if tab)
     }
  }
  'l': apps.Island
  'k': apps.Slack
  'r': apps.Reeder
  's': Hyper(name: 'System', afterAction: (hyper) -> hyper.modal\exit!)\space{
    'delete': hs.caffeinate.systemSleep
    'c': hs.toggleConsole
    'l': hs.caffeinate.startScreensaver
    'r': =>
      @clips\stop!
      hs.reload!
    's': actions.normalMode
    'w': actions.mediaMode
    'f': actions.toggleMovieMode
    ',': actions.editConfig
    'escape': (hyper) -> hyper.modal\exit!
  }
  't': apps.iTerm
  'v': ->
      paster\select!
  'w': Hyper(name: 'Window', afterAction: (hyper) -> hyper.modal\exit!)\space{
    '1': -> actions.screenFraction(1, 3)
    '2': -> actions.screenFraction(2, 3)
    '3': -> actions.screenFraction(3, 3)
    '4': -> actions.setHazeOver(30)
    '5': -> actions.setHazeOver(95)
    'space': -> actions.swapScreen(hs.window.focusedWindow!)
    'w': -> actions.maximize!
    'j': -> actions.screenFraction(2, 2)
    'k': -> actions.screenFraction(1, 2)
    'm': -> actions.maximize!
    '`': -> hs.hints.windowHints(hs.window.filter.default\getWindows!)
    'escape': (hyper) -> hyper.modal\exit!
  }
  'z': apps.Zoom
}

tprint "hyper"

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

tprint 'remote'

-- mouseValet{delay: 10}

flux\apply!
clock\start!
typer\start!
clipper\start!
matte\start!

tprint "go"

hs.notify.show("Hammerspoon", "Configuration", "Configuration reload successfully!")

tprint "done"

initLogger.d("⏰ #{(hs.timer.absoluteTime! - startTime) / 1000000}ms total")
