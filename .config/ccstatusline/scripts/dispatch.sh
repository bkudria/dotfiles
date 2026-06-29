#!/usr/bin/env bash
set -euo pipefail

input=$(cat)
pct=$(printf '%s' "$input" | jq -r '(.context_window.used_percentage // 0) | floor')

if [ "${pct:-0}" -ge 20 ]; then
  config="$HOME/.config/ccstatusline/settings-20.json"
elif [ "${pct:-0}" -ge 15 ]; then
  config="$HOME/.config/ccstatusline/settings-15.json"
elif [ "${pct:-0}" -ge 10 ]; then
  config="$HOME/.config/ccstatusline/settings-10.json"
else
  config="$HOME/.config/ccstatusline/settings.json"
fi

printf '%s' "$input" | ccstatusline --config "$config"
