#!/usr/bin/jq -rf
# Tunable parameters
20 as $width |       # total cells in the bar
20 as $low_pct |     # pct at the regime boundary
15 as $low_cells |   # cells filled at $low_pct (anchor)
"⣀⣄⣆⣇⣧⣷⣿" as $glyphs |

# Derived
($glyphs | split("")) as $c |
(($c | length) - 1) as $full_idx |
$full_idx as $levels |
(($width - $low_cells) / (100 - $low_pct)) as $high_slope |
(2 * $low_cells / $low_pct - $high_slope) as $a |
($high_slope / $low_pct - $low_cells / ($low_pct * $low_pct)) as $b |

(.context_window.used_percentage // 0) as $pct |
(if $pct < 0 then 0 elif $pct >= 100 then 100 else $pct end) as $cl |

(if $cl >= 100 then $width
 elif $cl < $low_pct then ($a * $cl) + ($b * $cl * $cl)
 else $low_cells + ($cl - $low_pct) * $high_slope
 end) as $x |

($x | floor) as $full |
(($x - $full) * $levels | floor) as $mid_raw |
(if $x > 0 and $full == 0 and $mid_raw == 0 then 1 else $mid_raw end) as $mid |
($pct | tostring) + "% " +
  ($c[$full_idx] * $full) +
  (if $full < $width then $c[$mid] + ($c[0] * ($width - 1 - $full)) else "" end)
