# Custom configurations for Benjamin Kudria
# Sourced by zimfw during initialization

export DIRENV_LOG_FORMAT=""

# External integrations
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
[[ -f ~/.vterm.zsh ]] && source ~/.vterm.zsh

# Environment variables
export EDITOR='emacsclient -q -r'
export VISUAL='emacsclient -q -r'
# export PAGE=glow
# export TERM=xterm-256color
# export GOPATH=~/.go
# export TSC_WATCHFILE=UseFsEventsWithFallbackDynamicPolling

# Aliases - only the unique ones not covered by other modules
alias less=glow
alias mcp-cli='npx wong2/mcp-cli'
alias env='env | sort'

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

# Homebrew
# HOMEBREW_COMMAND_NOT_FOUND_HANDLER="$(brew --repository)/Library/Homebrew/command-not-found/handler.sh"
# if [ -f "$HOMEBREW_COMMAND_NOT_FOUND_HANDLER" ]; then
#     source "$HOMEBREW_COMMAND_NOT_FOUND_HANDLER"
# fi
source /opt/homebrew/Library/Homebrew/command-not-found/handler.sh
