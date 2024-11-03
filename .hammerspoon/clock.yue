style = {
  strokeColor: {alpha: 0}
  fillColor: {hex: "#282828", alpha: 0.9}
  radius: 10
  textSize: 200
  textColor: {hex: "#fb4934"}
  textFont: "Futura Bold"
  textStyle: {paragraphStyle: {alignment: 'center'}}
}

class Clock
  new: =>
    @timer = nil
    @time = os.date("%H:%M")
    @state = hs.watchable.watch("state", "*")
    @updateState!

  show: =>
    @time = os.date("%H:%M")
    hs.alert(@time, style, hs.window.focusedWindow()\screen!)

  showDate: =>
    date = os.date("%A\n%B %d")
    hs.alert(date, style, hs.window.focusedWindow()\screen!)

  updateState: =>
    sunrise = os.date("%H:%M", hs.location.sunrise(37.76, -122.42, -7))
    sunset = os.date("%H:%M", hs.location.sunset(37.76, -122.42, -7))
    @state\change("sunrise", sunrise)
    @state\change("sunset", sunset)

  start: =>
    nextHour = (os.date('*t').hour + 1) % 24
    @updateState! if (nextHour > 11) and (nextHour < 14)
    @timer = hs.timer.doAt("#{nextHour}:00", ->
      @show!
      @start!)
