export EDITOR='emacsclient -q'
export VISUAL='emacsclient -q'
export TERM=screen-256color
export GOPATH=~/.go

alias ll='exa -lF --colour-scale --group-directories-first'
alias la='ll -a'
alias git=hub
alias cat=bat

path=("$HOME/bin" $path)
cdpath=($HOME/Code)

[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh
