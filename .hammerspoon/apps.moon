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

  @isFocused: => hs.application\frontmostApplication!\bundleID! == @id
  @whenFocused: => nil
  @handle: =>
    if false or @isFocused!
      @whenFocused!
    else
      hs.application.launchOrFocusByBundleID(@id)

class Chrome extends App
  id: 'com.google.Chrome'
  whenFocused: =>
    hs.eventtap.keyStroke({'shift'}, 't')

class iTerm extends App
  id: 'com.googlecode.iterm2'
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
  whenFocused: =>
    hs.eventtap.keyStrokes('  ')

App\define_apps{
  Reeder: 'com.reederapp.macOS'
  Zoom: 'us.zoom.xos'
}

App\apps!
