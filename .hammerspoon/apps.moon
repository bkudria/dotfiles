class App
  @inherited = {}
  @apps: => @inherited

  __inherited: (child) =>
    @inherited[child.__name] = child

  define_apps: (apps) =>
    for name, id in pairs apps
      class extends App
        __name: name
        id: id

  current: =>
    hs.fnutils.find(App\apps!, (app) -> app\isFocused!)

  @isFocused: => hs.application\frontmostApplication!\bundleID! == @id
  @whenFocused: => nil
  @handle: =>
    if false or @isFocused!
      @whenFocused!
    else
      hs.application.launchOrFocusByBundleID(@id)

  @prev: => nil
  @next: => nil

class Chrome extends App
  id: 'com.google.Chrome'
  prev: => hs.eventtap.keyStroke({'cmd', 'shift'}, '[')
  next: => hs.eventtap.keyStroke({'cmd', 'shift'}, ']')
  whenFocused: =>
    hs.eventtap.keyStroke({'shift'}, 't')

class iTerm extends App
  id: 'com.googlecode.iterm2'
  prev: => hs.eventtap.keyStroke({'cmd', 'shift'}, '[')
  next: => hs.eventtap.keyStroke({'cmd', 'shift'}, ']')
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
  prev: => hs.eventtap.keyStrokes(' bp')
  next: => hs.eventtap.keyStrokes(' bp')
  whenFocused: =>
    hs.eventtap.keyStrokes('  ')

class Slack extends App
  id: 'com.tinyspeck.slackmacgap'
  prev: => hs.eventtap.keyStroke({'cmd'}, '[')
  next: => hs.eventtap.keyStroke({'cmd'}, ']')
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
