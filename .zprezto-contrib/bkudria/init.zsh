export EDITOR='emacsclient -q'
export VISUAL='emacsclient -q'

alias ll='exa -lF --colour-scale --group-directories-first'
alias la='ll -a'
alias git=hub

path=("$HOME/bin" $path)
cdpath=($HOME/Code)