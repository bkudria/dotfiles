export EDITOR='emacsclient -q'
export VISUAL='emacsclient -q'
export TERM=screen-256color
export GOPATH=~/.go

alias ll='exa -lF --colour-scale --group-directories-first'
alias la='ll -a'
alias git=hub
alias cat=bat
# alias fzf=fzy

path=("$HOME/bin" $path)
cdpath=($HOME/Code)

MARKER_KEY_GET="^[[Z"
MARKER_KEY_NEXT_PLACEHOLDER="^N"
[[ -s "$HOME/.local/share/marker/marker.sh" ]] && source "$HOME/.local/share/marker/marker.sh"

# [[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh
source "${0:a:h}/romkatv-purepower/.purepower"
typeset -g POWERLEVEL9K_SHORTEN_DIR_LENGTH=1
typeset -g POWERLEVEL9K_SHORTEN_DELIMITER=''
typeset -g POWERLEVEL9K_SHORTEN_STRATEGY='truncate_to_unique'
typeset -g POWERLEVEL9K_DIR_NOT_WRITABLE_FOREGROUND=003
typeset -g POWERLEVEL9K_DIR_{HOME,HOME_SUBFOLDER,ETC,DEFAULT}_FOREGROUND=004
typeset -g POWERLEVEL9K_VCS_CLEAN_FOREGROUND=002
typeset -g POWERLEVEL9K_VCS_UNTRACKED_FOREGROUND=006
typeset -g POWERLEVEL9K_VCS_MODIFIED_FOREGROUND=003
typeset -g POWERLEVEL9K_VCS_LOADING_FOREGROUND=005
typeset -g POWERLEVEL9K_STATUS_ERROR_FOREGROUND=001
typeset -g POWERLEVEL9K_COMMAND_EXECUTION_TIME_FOREGROUND=005
typeset -g POWERLEVEL9K_CUSTOM_RPROMPT_FOREGROUND=004
typeset -g POWERLEVEL9K_CONTEXT_ROOT_FOREGROUND=003
