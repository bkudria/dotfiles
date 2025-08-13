#!/usr/bin/env zsh

# Read JSON input from stdin
input=$(cat)

# Extract values from JSON
current_dir=$(echo "$input" | jq -r '.workspace.current_dir')
model=$(echo "$input" | jq -r '.model.display_name' | sed 's/Claude //' | sed 's/ Sonnet//')
user=$(whoami)

# Source p10k to get configuration (suppressing any output)
{
  source ~/.zim/modules/powerlevel10k/powerlevel10k.zsh-theme
  source ~/.p10k.zsh
} &>/dev/null

# Define Claude-specific configuration locally (without modifying .p10k.zsh)
typeset -g POWERLEVEL9K_CLAUDE_MODEL_ICON='⚡'
typeset -g POWERLEVEL9K_CLAUDE_MODEL_OPUS_BACKGROUND=1      # Red
typeset -g POWERLEVEL9K_CLAUDE_MODEL_OPUS_FOREGROUND=15     # White
typeset -g POWERLEVEL9K_CLAUDE_MODEL_SONNET_BACKGROUND=4    # Blue
typeset -g POWERLEVEL9K_CLAUDE_MODEL_SONNET_FOREGROUND=254  # White
typeset -g POWERLEVEL9K_CLAUDE_MODEL_DEFAULT_BACKGROUND=2   # Green
typeset -g POWERLEVEL9K_CLAUDE_MODEL_DEFAULT_FOREGROUND=0   # Black

# Extract p10k configuration values
dir_bg=${POWERLEVEL9K_DIR_BACKGROUND:-4}
dir_fg=${POWERLEVEL9K_DIR_FOREGROUND:-254}
dir_shortened_fg=${POWERLEVEL9K_DIR_SHORTENED_FOREGROUND:-250}
dir_anchor_fg=${POWERLEVEL9K_DIR_ANCHOR_FOREGROUND:-255}

# Get separators from p10k config
sep_left=${POWERLEVEL9K_LEFT_SEGMENT_SEPARATOR}
sep_subsegment=${POWERLEVEL9K_LEFT_SUBSEGMENT_SEPARATOR}
sep_end=${POWERLEVEL9K_LEFT_PROMPT_LAST_SEGMENT_END_SYMBOL}
sep_start=${POWERLEVEL9K_LEFT_PROMPT_FIRST_SEGMENT_START_SYMBOL}

# If p10k is using "lean" style (empty separators), use powerline separators for statusline
# This provides visual consistency while respecting that statusline needs visible boundaries
if [[ -z $sep_left && -z $sep_end ]]; then
  # P10k is in lean mode, but statusline needs separators for clarity
  sep_left=$'\ue0b0'      # Powerline arrow
  sep_subsegment='│'       # Vertical line (using actual character)
  sep_start=$'\ue0b6'      # Rounded start
  sep_end=$'\ue0b4'        # Rounded end
else
  # Use p10k's configured separators
  [[ -n $sep_left ]] || sep_left=$'\ue0b0'
  [[ -n $sep_subsegment ]] || sep_subsegment='│'
  [[ -n $sep_start ]] || sep_start=$'\ue0b6'
  [[ -n $sep_end ]] || sep_end=$'\ue0b4'
fi

# Determine directory icon using p10k's directory classes
dir_icon=''
if [[ "$current_dir" == */code/* || "$current_dir" == */code ]]; then
  # Check for CODE class icon
  dir_icon=${POWERLEVEL9K_DIR_CODE_VISUAL_IDENTIFIER_EXPANSION}
  [[ -n $dir_icon ]] || dir_icon=${POWERLEVEL9K_DIR_WORK_VISUAL_IDENTIFIER_EXPANSION}
  [[ -n $dir_icon ]] || dir_icon=$'\uf0e7'  # Code icon fallback (lowercase hex)
elif [[ "$current_dir" == $HOME || "$current_dir" == $HOME/* ]]; then
  # Check for HOME class icon
  dir_icon=${POWERLEVEL9K_DIR_HOME_VISUAL_IDENTIFIER_EXPANSION}
  [[ -n $dir_icon ]] || dir_icon=$'\uf015'  # Home icon fallback (lowercase hex)
else
  # Default folder icon
  dir_icon=${POWERLEVEL9K_DIR_DEFAULT_VISUAL_IDENTIFIER_EXPANSION}
  [[ -n $dir_icon ]] || dir_icon=${POWERLEVEL9K_FOLDER_ICON}
  [[ -n $dir_icon ]] || dir_icon=$'\uf07b'  # Folder icon fallback (lowercase hex)
fi

# Determine model colors based on model type
if [[ "$model" == *"Opus"* ]]; then
  model_bg=${POWERLEVEL9K_CLAUDE_MODEL_OPUS_BACKGROUND}
  model_fg=${POWERLEVEL9K_CLAUDE_MODEL_OPUS_FOREGROUND}
elif [[ "$model" == *"Sonnet"* ]]; then
  model_bg=${POWERLEVEL9K_CLAUDE_MODEL_SONNET_BACKGROUND}
  model_fg=${POWERLEVEL9K_CLAUDE_MODEL_SONNET_FOREGROUND}
else
  model_bg=${POWERLEVEL9K_CLAUDE_MODEL_DEFAULT_BACKGROUND}
  model_fg=${POWERLEVEL9K_CLAUDE_MODEL_DEFAULT_FOREGROUND}
fi

# Build and output status line with proper powerline separators
# Using print -n to properly handle escape sequences

# Start with rounded separator (if using rounded style)
print -n "\033[38;5;${model_bg}m${sep_start}"

# Model segment
print -n "\033[48;5;${model_bg}m\033[38;5;${model_fg}m ${POWERLEVEL9K_CLAUDE_MODEL_ICON} ${model} "

# Separator between model and directory segments
# Use vertical line for same-color transitions, powerline arrow for different colors
if [[ $model_bg == $dir_bg ]]; then
  # Same background - use subsegment separator
  print -n "\033[38;5;244m${sep_subsegment}"
else
  # Different backgrounds - use full separator
  print -n "\033[48;5;${dir_bg}m\033[38;5;${model_bg}m${sep_left}"
fi

# Directory segment
print -n "\033[48;5;${dir_bg}m\033[38;5;${dir_fg}m ${dir_icon} $(basename "$current_dir") "

# End separator
print -n "\033[38;5;${dir_bg}m\033[49m${sep_end}"

# Reset colors
print -n "\033[0m"

# Debug segment - show TERM and color values
# Comment this out when not debugging
print -n " \033[48;5;240m\033[38;5;255m TERM=$TERM COLORTERM=$COLORTERM m_bg=${model_bg} d_bg=${dir_bg} \033[0m"