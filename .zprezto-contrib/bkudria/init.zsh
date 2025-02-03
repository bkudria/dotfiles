export EDITOR='emacsclient -q'
export VISUAL='emacsclient -q'
export TERM=screen-256color
export GOPATH=~/.go
export TSC_WATCHFILE=UseFsEventsWithFallbackDynamicPolling
export EZA_ICON_SPACING=2

alias ll='eza -lF --colour-scale all --group-directories-first --icons auto'
alias la='ll -a'
alias lt='ll -T'
alias git=hub
alias cat=bat

path=("/opt/homebrew/bin" "/opt/homebrew/sbin" "$HOME/bin" "$HOME/.emacs.doom/bin" "$HOME/.local/bin" "/usr/local/opt/node@16/bin" $path)
cdpath=($HOME/Code)

[[ -f ~/.vterm.zsh ]] && source ~/.vterm.zsh
[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh

[[ -f ~/.local.zsh ]] && source ~/.local.zsh
