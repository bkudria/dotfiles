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
alias cat=bat
alias less=glow
alias mcp-cli='npx wong2/mcp-cli'
alias env='env | sort'

alias ll='eza -lF  --group-directories-first --icons auto'
alias la='ll -a'

# Set EZA options for the exa module
export EZA_ICON_SPACING=2

# Path configuration - only add custom paths not handled by other modules
path=(
  "$HOME/bin"
  "$HOME/.emacs.doom/bin"
  "$HOME/.local/bin" # uv
  "$HOME/.cargo/bin"
  "/usr/local/opt/node@16/bin"
  $path
)

# CD path
cdpath=($HOME/code)

# Source functions
autoload -Uz ${0:h}/functions/*(.:t)

# Preload the __llm_cmdcomp function
source ${0:h}/functions/__llm_cmdcomp

# Key bindings
bindkey '^[\t' __llm_cmdcomp
