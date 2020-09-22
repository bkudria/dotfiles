style = {
  strokeColor: {alpha: 0}
  fillColor: {hex: "#282828", alpha: 0.8}
  radius: 10
  textSize: 200
  textColor: {hex: "#cc241d"}
  textFont: "Futura Bold"
}

class Clock
  new: =>
    @timer = nil
    @time = os.date("%H:%M")

  show: =>
    @time = os.date("%H:%M")
    print(@time)
    hs.alert(@time, style, hs.window.focusedWindow()\screen!)

  start: =>
    nextHour = (os.date('*t').hour + 1) % 24
    print("scheduling time for #{nextHour}")
    @timer = hs.timer.doAt("#{nextHour}:00", ->
      print("clock for #{nextHour}")
      @show!
      @start!)
