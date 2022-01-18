class App
  @inherited = {}
  @byID = {}
  @apps: => @inherited

  __inherited: (child) =>
    @inherited[child.__name] = child
    @byID[child.id] = child

  define_apps: (apps) =>
    for name, id in pairs apps
      class extends App
        __name: name
        id: id

  current: => App.byID[hs.application.frontmostApplication!\bundleID!]

  @isFocused: => hs.application.frontmostApplication!\bundleID! == @id
  @isRunning: => hs.application.get(@id) ~= nil
  @whenFocused: => nil
  @handle: =>
    if @isFocused!
      @whenFocused!
    else
      hs.application.launchOrFocusByBundleID(@id)

  @left: => nil
  @right: => nil
  @up: => nil
  @down: => nil
  @rBack: => hs.eventtap.keyStroke({}, 'ESCAPE')

  @toggleAudio: =>
    if @inherited.Zoom\isRunning!
      @inherited.Zoom\toggleAudio!
    elseif @inherited.Chrome\isRunning!
      @inherited.Chrome\rMicrophone!

  @toggleVideo: =>
    if @inherited.Zoom\isRunning!
      @inherited.Zoom\toggleVideo!
    elseif @inherited.Chrome\isRunning!
      @inherited.Chrome\rPower!


class Chrome extends App
  id: 'com.google.Chrome'
  left: => hs.eventtap.keyStroke({'cmd', 'shift'}, '[')
  right: => hs.eventtap.keyStroke({'cmd', 'shift'}, ']')
  rBack: => hs.eventtap.keyStroke({'cmd', 'shift'}, 't')
  rScreen: => hs.eventtap.keyStroke({'cmd', 'shift'}, 'r')
  rMute: => hs.eventtap.keyStrokes('__')
  rLeft: =>  hs.eventtap.keyStrokes('!rLeft')
  rRight: => hs.eventtap.keyStrokes('!rRight')
  rUp: => hs.eventtap.keyStrokes('!rUp')
  rDown: => hs.eventtap.keyStrokes('!rDown')
  rPlus: => hs.eventtap.keyStrokes('zi')
  rMinus: => hs.eventtap.keyStrokes('zo')
  rPlayPause: => hs.eventtap.keyStroke({}, 'SPACE')
  rMicrophone: => hs.eventtap.keyStrokes('!rMicrophone')
  rPower: => hs.eventtap.keyStrokes('!rPower')
  whenFocused: =>
    hs.eventtap.keyStroke({'cmd', 'shift'}, 'a', 200000, hs.application.get(@id))

class iTerm extends App
  id: 'com.googlecode.iterm2'
  left: => hs.eventtap.keyStroke({'cmd', 'shift'}, '[')
  right: => hs.eventtap.keyStroke({'cmd', 'shift'}, ']')
  whenFocused: =>
    hs.eventtap.keyStroke({'cmd', 'shift'}, 'o')
    hs.eventtap.keyStrokes('/f ')

class OnePassword extends App
  id: 'com.agilebits.onepassword7'
  handle: =>
    if Chrome\isFocused!
      hs.eventtap.keyStroke({'cmd','shift'}, 'i')
    else
      hs.application.launchOrFocusByBundleID(@id)

class Emacs extends App
  id: 'org.gnu.Emacs'
  left: => hs.eventtap.keyStrokes(' bp')
  right: => hs.eventtap.keyStrokes(' bp')
  whenFocused: =>
    hs.eventtap.keyStrokes('  ')

class Zoom extends App
  id: 'us.zoom.xos'
  rBack: => hs.eventtap.keyStroke({'cmd'}, 'q')
  toggleAudio: -> hs.eventtap.keyStroke {'cmd', 'shift'}, 'a',
  toggleVideo: -> hs.eventtap.keyStroke {'cmd', 'shift'}, 'v',

class Slack extends App
  id: 'com.tinyspeck.slackmacgap'
  left: => hs.eventtap.keyStroke({'cmd'}, '[')
  right: => hs.eventtap.keyStroke({'cmd'}, ']')
  up: => hs.eventtap.keyStroke({'cmd'}, '.')
  down: => @switchNextUnread!
  rDown: => @switchNextUnread!
  whenFocused: => @focusNextUnread!
  focusNextUnread: =>
    hs.eventtap.keyStroke({'cmd'}, 't')
  switchNextUnread: =>
    hs.eventtap.keyStroke({'cmd'}, 't')
    hs.timer.doAfter 0.01, ->
      hs.eventtap.keyStroke({}, 'return')

class Reeder extends App
  id: 'com.reederapp.5.macOS'
  @rBack: => hs.eventtap.keyStroke({}, 'r')
  @rScreen: => hs.eventtap.keyStroke({}, 'r')
  rLeft: =>  hs.eventtap.keyStroke({}, 'SPACE')
  rRight: => hs.eventtap.keyStroke({}, 'b')
  rUp: => hs.eventtap.keyStroke({}, 'k')
  rDown: => hs.eventtap.keyStroke({}, 'j')

{App, App\apps!}
