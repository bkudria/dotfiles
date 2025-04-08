export EDITOR='emacsclient -q -r'
export VISUAL='emacsclient -q -r'
export TERM=screen-256color
export GOPATH=~/.go
export TSC_WATCHFILE=UseFsEventsWithFallbackDynamicPolling
export EZA_ICON_SPACING=2

alias ll='eza -lF --colour-scale all --group-directories-first --icons auto'
alias la='ll -a'
alias lt='ll -T'
alias cat=bat
# alias git=hub
alias env=env | sort

alias brewi='brew info'
alias brewI='brew install'
alias rbbi='bundle install'

path=(
  "/opt/homebrew/bin"
  "/opt/homebrew/sbin"
  "$HOME/bin"
  "$HOME/.emacs.doom/bin"
  "$HOME/.local/bin" # uv
  "$HOME/.cargo/bin"
  "/usr/local/opt/node@16/bin"
  $path
)

cdpath=($HOME/code)

[[ -f ~/.vterm.zsh ]] && source ~/.vterm.zsh
[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh

[[ -f ~/.local.zsh ]] && source ~/.local.zsh

bindkey '^Z^Z' __llm_cmdcomp

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

function llm_prompts() {
    if [[ $# -ne 1 ]]; then
        echo "Usage: llm-template-keys <template_name>"
        return 1
    fi

    local template_path=$(llm templates path)/$1.yaml

    if [[ ! -f "$template_path" ]]; then
        echo "Template '$1.yaml' not found in $(llm templates path)"
        return 1
    fi

    echo "System Keys:"
    yq '.system // {} | keys' "$template_path"

    echo -e "\nPrompt Keys:"
    yq '.prompts // {} | keys' "$template_path"
}

fix_yaml() {
  [ -z "$1" ] && echo "Usage: fix_yaml <file.yaml> [files...]" && return 1
  yq eval -i ' (.. | select(tag == "!!str") | select(test("\\n"))) style = "literal" ' "$@"
}

fix_prompt() {
  [ -z "$1" ] && echo "Usage: fix_prompt <template_name>" && return 1
  local file="$(llm templates path)/${1}.yaml"
  [ ! -f "$file" ] && echo "Error: $file not found" && return 1
  fix_yaml "$file"
}

as_system_prompt() {
  local output
  output="$(cat)" # read all stdin
  llm --system "$output" "$@"
}


