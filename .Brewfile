tap "candid82/brew"
tap "d12frosted/emacs-plus"
tap "homebrew/bundle"
tap "homebrew/cask"
tap "homebrew/cask-fonts"
tap "homebrew/cask-versions"
tap "homebrew/core"
tap "railwaycat/emacsmacport"

tap "yqrashawn/goku"
brew "asciidoc"
brew "aspell"
brew "awscli"
brew "bat"
brew "glib"
brew "cmake"
brew "coreutils"
brew "direnv"
brew "discount"
brew "exa"
brew "fd"
brew "node"
brew "fx"
brew "gh"
brew "git"
brew "git-delta"
brew "python@3.8"
brew "harfbuzz"
brew "pango"
brew "graphviz"
brew "httperf"
brew "httpie"
brew "hub"
brew "ispell"
brew "jq"
brew "leiningen"
brew "libpq"
brew "libvterm"
brew "libxml2"
brew "lua"
brew "luarocks"
brew "mas"
brew "mtr"
brew "ncdu"
brew "nnn"
brew "openjdk"
brew "pgcli"
brew "postgresql"
brew "ruby-build"
brew "rbenv"
brew "ripgrep"
brew "shellcheck"
brew "shfmt"
brew "telnet"
brew "tig", args: ["HEAD"]
brew "tmux"
brew "tree"
brew "ugrep"
brew "watch"
brew "wget"
brew "zsh"
brew "zstd"
brew "yqrashawn/goku/goku"
cask "1password"
cask "bettertouchtool"
cask "dash"
cask "fantastical"
cask "font-iosevka-nerd-font"
cask "google-chrome"
cask "hammerspoon"
cask "hazeover"
cask "iterm2"
cask "karabiner-elements"
cask "mactex-no-gui"
cask "slack"
cask "zoom"

if ENV['PERSONAL_BREW']
  brew "youtube-dl"
  cask "aerial"
  cask "ledger-live"
  cask "steam"
  mas "Reeder", id: 1449412482
  mas "Wallpaper Wizard", id: 1266674560
end

Dir['.Brewfile.*'].each { |brewfile| load brewfile }
