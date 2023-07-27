class Flux
  new: =>
    callback = (watcher, path, key, old, new) ->
        @watcherCallback(watcher, path, key, old, new)

    @sunrise = hs.watchable.watch("state.sunrise", callback)
    @sunset = hs.watchable.watch("state.sunset", callback)
    @mode = hs.watchable.watch("state.mode", callback)


    @watcher = hs.caffeinate.watcher.new((event) ->
      print('caffeinate event', event)
      if event == hs.caffeinate.watcher.systemDidWake
        print('wake!')
        hs.timer.doAfter(10, -> hs.reload!)
    )\start!

  apply: =>
    print("Redshift: #{@startTime!}-#{@endTime!}, to final gamma #{@finalGamma!} over 4h")
    hs.redshift.start(@finalGamma!, @startTime!, @endTime!, '4h')

  watcherCallback: (watcher, path, key, old, new) =>
    if new != old
        @apply!

  startTime: =>
    if @mode\value() == "media"
        string.format("%.2d:%.2d", os.date('%H'), os.date('%M'))
    else
        @sunset\value!

  endTime: =>
    @sunrise\value!

  finalGamma: =>
    if @mode\value() == "media"
        4000
    else
        2500
