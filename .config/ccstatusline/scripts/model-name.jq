#!/usr/bin/jq -rf
.model.id as $id |
(if ($id | test("fable")) then "Fable"
 elif ($id | test("sonnet")) then "Sonnet"
 elif ($id | test("opus")) then "Opus"
 elif ($id | test("haiku")) then "Haiku"
 else null end) as $family |
# Array-wrapped because capture emits an empty stream on no match, which would
# silently blank the whole segment rather than reaching the display_name fallback.
([$id | capture("claude-(?:fable|sonnet|opus|haiku)-(?<maj>[0-9]+)(?:-(?<min>[0-9]+))?")] | first) as $ver |
(if $family and $ver then
  $family + " " + $ver.maj + (if $ver.min then "." + $ver.min else "" end)
else
  .model.display_name // $id // "Unknown"
end) as $name |
({low: "·", medium: "•", high: "●", xhigh: "⬤", max: "✦", ultracode: "✶"}[.effort.level // ""] // "") as $effort |
# Trailing space pads the effort glyph from the segment edge; ZWNJ (\u200C) keeps
# ccstatusline's whitespace-trimmer from eating the space.
if $effort == "" then $name else $name + " " + $effort + " \u200C" end
