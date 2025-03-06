export EDITOR='emacsclient -q -r'
export VISUAL='emacsclient -q -r'
export TERM=screen-256color
export GOPATH=~/.go
export TSC_WATCHFILE=UseFsEventsWithFallbackDynamicPolling
export EZA_ICON_SPACING=2

alias ll='eza -lF --colour-scale all --group-directories-first --icons auto'
alias la='ll -a'
alias lt='ll -T'
alias git=hub
alias cat=bat

alias -g md='| glow'

path=("/opt/homebrew/bin" "/opt/homebrew/sbin" "$HOME/bin" "$HOME/.emacs.doom/bin" "$HOME/.local/bin" "/usr/local/opt/node@16/bin" $path)
cdpath=($HOME/Code)

[[ -f ~/.vterm.zsh ]] && source ~/.vterm.zsh
[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh

[[ -f ~/.local.zsh ]] && source ~/.local.zsh

# Bind Alt-\ to LLM command completion
bindkey '^[/' __llm_cmdcomp

__llm_cmdcomp() {
  local old_cmd=$BUFFER
  local cursor_pos=$CURSOR
  echo # Start the program on a blank line
  local result=$(llm cmdcomp "$old_cmd")
  if [ $? -eq 0 ] && [ ! -z "$result" ]; then
    BUFFER=$result
  else
    BUFFER=$old_cmd
  fi
  zle reset-prompt
}

zle -N __llm_cmdcomp
