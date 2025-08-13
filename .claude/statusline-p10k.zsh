#!/usr/bin/env zsh

# Read JSON input from stdin
input=$(cat)

# Extract values from JSON
current_dir=$(echo "$input" | jq -r '.workspace.current_dir')
model=$(echo "$input" | jq -r '.model.display_name' | sed 's/Claude //' | sed 's/ Sonnet//')

# Define separators
sep_start=$'\ue0b6' # Rounded start
sep_end=$'\ue0b4'   # Rounded end

# Define colors
if [[ "$model" == *"Opus"* ]]; then
  model_bg=1 # Red
  model_fg=0 # White
elif [[ "$model" == *"Sonnet"* ]]; then
  model_bg=11 # Blue
  model_fg=17 # White
else
  model_bg=2 # Green
  model_fg=0 # Black
fi

dir_bg=4 # Blue
dir_fg=0 # White

# Simple directory icon
if [[ "$current_dir" == */code/* || "$current_dir" == */code ]]; then
  dir_icon=' '
elif [[ "$current_dir" == $HOME || "$current_dir" == $HOME/* ]]; then
  dir_icon=''
else
  dir_icon=''
fi

# Build status line
# Model segment with rounded left start
print -n "\033[38;5;${model_bg}m${sep_start}\033[48;5;${model_bg}m\033[38;5;${model_fg}m ⚡ ${model} "

# Directory segment (no separator)
print -n "\033[48;5;${dir_bg}m\033[38;5;${dir_fg}m ${dir_icon} $(basename "$current_dir") "

# Rounded right end
print -n "\033[38;5;${dir_bg}m\033[49m${sep_end}"

# Reset colors
print -n "\033[0m"
