alertStyle = {
  fillColor: {hex: "#282828"}
  strokeColor: {alpha: 0}
  radius: 10
  textSize: 100
  textColor: {hex: "#d65d0e"}
}
class Hyper
  @mods: {"cmd","alt","shift","ctrl"}
  @spec: (key) => @mods, key
  @key: (key) => -> hs.eventtap.keyStroke(@spec key)

  new: (config = {}) =>
    @inHyperspace = false
    @config = config

  handle: (action) =>
    switch type(action)
      when 'function'
        result, err = pcall -> action(self)
        unless result
          hs.logger.new("hyper").e(err)
          hs.reload!
      when 'table' then action\handle!
      when 'nil'
        if @inHyperspace
          @modal\exit!
        else
          @modal\enter!
      else print "unknown action:" and print(hs.inspect(action))

  initModal: =>
    @modal = with hs.hotkey.modal.new!
      .entered = ->
        @alert = if @config.name
          hs.alert.show(@config.name, alertStyle, hs.window.focusedWindow()\screen!, 'always')
        @inHyperspace = true
      .exited = ->
        hs.alert.closeSpecific(@alert)
        @inHyperspace = false

  space: (mappings) =>
    bind = if @config.name -- Sub-space, bind to modal instead
      @initModal!
      (key, fn) -> @modal\bind(nil, key, fn)
    else -- top-level, bind directly
      (key, fn) -> hs.hotkey.bind(@@mods, key, fn)

    for key, action in pairs mappings
      bind key, ->
        @handle(action)
        @config.afterAction(self) if @config.afterAction

    self
