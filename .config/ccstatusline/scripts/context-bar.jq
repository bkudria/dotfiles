#!/usr/bin/jq -rf
(.context_window.used_percentage // 0) as $pct |
("⣀⣄⣆⣇⣧⣷⣿" | split("")) as $c |
(if $pct >= 100 then 100 elif $pct < 0 then 0 else $pct end) as $clamped |
if $clamped >= 100 then
  ($pct | tostring) + "% " + ($c[6] * 10)
else
  ($clamped / 100) as $p |
  ($p * 10) as $x |
  ($x | floor) as $full |
  (($x - $full) * 6 | floor) as $mid_raw |
  (if $p > 0 and $full == 0 and $mid_raw == 0 then 1 else $mid_raw end) as $mid |
  ($pct | tostring) + "% " + ($c[6] * $full) + $c[$mid] + ($c[0] * (9 - $full))
end
