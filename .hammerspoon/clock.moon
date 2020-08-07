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
    @time = os.date("%H:%M")

  show: =>
    @time = os.date("%H:%M")
    hs.alert(@time, style, hs.window.focusedWindow()\screen!)

  start: =>
    for hour = 0,23
      hs.timer.doAt("#{hour}:00", "1d", -> @show!)
