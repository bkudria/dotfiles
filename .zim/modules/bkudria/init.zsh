# Custom configurations for Benjamin Kudria
# Sourced by zimfw during initialization

export DIRENV_LOG_FORMAT=""

# External integrations
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
[[ -f ~/.vterm.zsh ]] && source ~/.vterm.zsh

# Environment variables
export EDITOR='emacs-client-frame'
export VISUAL='emacs-client-frame'

# export PAGE=glow
# export TERM=xterm-256color
# export GOPATH=~/.go
# export TSC_WATCHFILE=UseFsEventsWithFallbackDynamicPolling

## Claude Code
export ANTHROPIC_MODEL="claude-opus-4-8"

# Aliases - only the unique ones not covered by other modules
less() {
    local mode=light
    [[ $(defaults read -g AppleInterfaceStyle 2>/dev/null) == Dark ]] && mode=dark
    local bat_theme=gruvbox-$mode
    local glow_style=$HOME/.config/glow/gruvbox-$mode.json

    if (($# == 0)); then
        command bat --theme=$bat_theme
        return
    fi

    local f
    for f in "$@"; do
        [[ ${f:l} == *.md ]] || {
            command bat --theme=$bat_theme "$@"
            return
        }
    done
    command glow -s "$glow_style" "$@"
}
alias mcp-cli='npx wong2/mcp-cli'
alias senv='env | sort'

alias rbbi='bundle install'
alias rbbr='bundle exec rake'

alias ll='eza -lF  --group-directories-first --icons auto'
alias la='ll -a'
alias lra='lr -a'

# Set EZA options for the exa module
export EZA_ICON_SPACING=2

# Path configuration - only add custom paths not handled by other modules
path=(
    "$HOME/.local/bin" # XDG
    "$HOME/bin"
    "$HOME/.emacs.doom/bin"
    "$HOME/.cargo/bin"
    "/usr/local/opt/node@16/bin"
    $path
)

# CD path
cdpath=($HOME/code)

zle -N llm-cmdcomp
bindkey '^[\t' llm-cmdcomp

# push-line on Ctrl-S. Ctrl-S is the terminal's XOFF flow-control key by
# default, so disable editor flow control for the keypress to reach zle.
unsetopt flow_control
bindkey -r '^[q' '^[Q' '^Q'
bindkey '^S' push-line

# Homebrew
# HOMEBREW_COMMAND_NOT_FOUND_HANDLER="$(brew --repository)/Library/Homebrew/command-not-found/handler.sh"
# if [ -f "$HOMEBREW_COMMAND_NOT_FOUND_HANDLER" ]; then
#     source "$HOMEBREW_COMMAND_NOT_FOUND_HANDLER"
# fi
source /opt/homebrew/Library/Homebrew/command-not-found/handler.sh
