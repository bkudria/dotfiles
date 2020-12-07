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

  current: =>
    App.byID[hs.application\frontmostApplication!\bundleID!]

  @isFocused: => hs.application\frontmostApplication!\bundleID! == @id
  @whenFocused: => nil
  @handle: =>
    if false or @isFocused!
      @whenFocused!
    else
      hs.application.launchOrFocusByBundleID(@id)

  @left: => nil
  @right: => nil
  @up: => nil
  @down: => nil

class Chrome extends App
  id: 'com.google.Chrome'
  left: => hs.eventtap.keyStroke({'cmd', 'shift'}, '[')
  right: => hs.eventtap.keyStroke({'cmd', 'shift'}, ']')
  whenFocused: =>
    hs.eventtap.keyStroke({'shift'}, 't')

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

class Slack extends App
  id: 'com.tinyspeck.slackmacgap'
  left: => hs.eventtap.keyStroke({'cmd'}, '[')
  right: => hs.eventtap.keyStroke({'cmd'}, ']')
  down: => @switchNextUnread!
  whenFocused: => @focusNextUnread!
  focusNextUnread: =>
    hs.eventtap.keyStroke({'cmd'}, 't')
  switchNextUnread: =>
    hs.eventtap.keyStroke({'cmd'}, 't')
    hs.timer.doAfter 0.01, ->
      hs.eventtap.keyStroke({}, 'return')

App\define_apps{
  Reeder: 'com.reederapp.macOS'
  Zoom: 'us.zoom.xos'
}

{App, App\apps!}
