export EDITOR='emacsclient -q'
export VISUAL='emacsclient -q'

alias ll='exa -lF --colour-scale --group-directories-first'
alias la='ll -a'
alias git=hub
alias cat=bat

path=("$HOME/bin" $path)
cdpath=($HOME/Code)
