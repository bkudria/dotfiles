export EDITOR='emacsclient -q'
export VISUAL='emacsclient -q'
export TERM=screen-256color
export GOPATH=~/.go
export TSC_WATCHFILE=UseFsEventsWithFallbackDynamicPolling

alias ll='exa -lF --colour-scale --group-directories-first'
alias la='ll -a'
alias git=hub
alias cat=bat

eval "$(direnv hook zsh)"

path=("$HOME/bin" $path)
cdpath=($HOME/Code)

[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh
