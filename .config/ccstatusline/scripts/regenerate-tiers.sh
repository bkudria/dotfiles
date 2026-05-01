#!/usr/bin/env bash
set -euo pipefail

CONFIG_DIR="$HOME/.config/ccstatusline"
BASE="$CONFIG_DIR/settings.json"

if [ ! -f "$BASE" ]; then
  echo "regenerate-tiers: $BASE not found" >&2
  exit 1
fi

# Palette: most-alarming (index 0) -> calmest (index 7).
# FG is determined by BG; one index captures both fields.
palette='[
  {"color":"black","bg":"bgBrightRed"},
  {"color":"black","bg":"bgYellow"},
  {"color":"black","bg":"bgBrightYellow"},
  {"color":"black","bg":"bgBrightGreen"},
  {"color":"brightWhite","bg":"bgGreen"},
  {"color":"brightWhite","bg":"bgCyan"},
  {"color":"brightWhite","bg":"bgBlue"},
  {"color":"brightWhite","bg":"bgMagenta"}
]'

# tier name : palette shift toward alarming
tiers=(
  "10:1"
  "15:2"
  "20:3"
)
max_shift=3

# Pre-validate everything before touching any output file:
# 1) every segment's (color, bg) must exist in the palette, and
# 2) the smallest palette index in use must be >= max_shift (no underflow).
validation=$(jq -r --argjson palette "$palette" --argjson max_shift "$max_shift" '
  def find_idx($c; $bg):
    ($palette | to_entries | map(select(.value.color == $c and .value.bg == $bg))) as $hits
    | if ($hits | length) == 0 then null else $hits[0].key end;
  [ [.lines[] | .[]] | to_entries[]
    | { i: .key, color: .value.color, bg: .value.backgroundColor,
        idx: find_idx(.value.color; .value.backgroundColor) }
  ] as $info
  | ($info | map(select(.idx == null))) as $unknown
  | if ($unknown | length) > 0 then
      "UNKNOWN:" + ($unknown | map("segment \(.i): \(.color)/\(.bg)") | join("; "))
    else
      ($info | map(.idx) | min) as $minidx
      | if $minidx < $max_shift then
          "UNDERFLOW:min palette index in use is \($minidx); max shift is \($max_shift) (need >= \($max_shift))"
        else
          "OK"
        end
    end
' "$BASE")

case "$validation" in
  OK) ;;
  UNKNOWN:*)
    echo "regenerate-tiers: unknown color pair(s) in $BASE" >&2
    echo "  ${validation#UNKNOWN:}" >&2
    exit 2
    ;;
  UNDERFLOW:*)
    echo "regenerate-tiers: ${validation#UNDERFLOW:}" >&2
    exit 2
    ;;
  *)
    echo "regenerate-tiers: validation failed: $validation" >&2
    exit 2
    ;;
esac

for tier in "${tiers[@]}"; do
  name="${tier%%:*}"
  shift_n="${tier##*:}"
  out="$CONFIG_DIR/settings-$name.json"
  tmp="$(mktemp "$CONFIG_DIR/.settings-$name.json.XXXXXX")"

  jq --argjson palette "$palette" --argjson shift "$shift_n" '
    def find_idx($c; $bg):
      ($palette | to_entries | map(select(.value.color == $c and .value.bg == $bg)))[0].key;
    .lines |= map(map(
      . as $s
      | (find_idx($s.color; $s.backgroundColor)) as $i
      | .color = $palette[$i - $shift].color
      | .backgroundColor = $palette[$i - $shift].bg
    ))
  ' "$BASE" > "$tmp"

  mv "$tmp" "$out"
  echo "wrote $out"
done
