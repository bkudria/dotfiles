#!/usr/bin/jq -rf
.model.id as $id |
(if ($id | test("sonnet")) then "Sonnet"
 elif ($id | test("opus")) then "Opus"
 elif ($id | test("haiku")) then "Haiku"
 else null end) as $family |
(if $family then
  ($id | capture("claude-(?:sonnet|opus|haiku)-(?<maj>[0-9]+)-(?<min>[0-9]+)")) |
  $family + " " + .maj + "." + .min
else
  .model.display_name // $id // "Unknown"
end) as $name |
({low: "·", medium: "•", high: "●", xhigh: "⬤", max: "✦"}[.effort.level // ""] // "") as $effort |
# Trailing space pads the effort glyph from the segment edge; ZWNJ (\u200C) keeps
# ccstatusline's whitespace-trimmer from eating the space.
if $effort == "" then $name else $name + " " + $effort + " \u200C" end
