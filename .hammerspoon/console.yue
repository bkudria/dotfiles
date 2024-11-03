hs.console.consoleFont { name: "Iosevka Nerd Font", size: 24 }

gruvbox = require "gruvbox"

mode = if hs.console.darkMode! then 'dark' else 'light'
contrast = 'normal'
colors = gruvbox[mode][contrast]

hs.console.windowBackgroundColor { hex: colors.bg2 }
hs.console.inputBackgroundColor { hex: colors.bg1 }
hs.console.outputBackgroundColor { hex: colors.bg0 }
hs.console.consoleCommandColor { hex: colors.purple }
hs.console.consolePrintColor { hex: colors.fg }
hs.console.consoleResultColor { hex: colors.blue }
