#!/usr/bin/env -S jq -srf
# extract-compaction.jq — Extract compaction events from a session transcript.
# Usage: ./extract-compaction.jq session.jsonl
#        jq -sf extract-compaction.jq session.jsonl
#
# Slurps entire file to compute before/after stats around each compaction boundary.

# Helper: count tool_use blocks in an array of entries
def count_tool_uses:
  [ .[] | select(.type == "assistant") | .message.content // [] | .[] | select(.type == "tool_use") ] | length;

# Helper: count entries of a given type
def count_type(t):
  [ .[] | select(.type == t) ] | length;

# Helper: truncate string
def trunc(n):
  if length <= n then . else .[:n] + "..." end;

# Helper: format number with commas (e.g. 167173 -> "167,173")
def comma_fmt:
  tostring | explode | reverse
  | [ foreach .[] as $c (
      {i: 0, out: []};
      if .i > 0 and (.i % 3) == 0
        then {i: (.i + 1), out: (.out + [44, $c])}
        else {i: (.i + 1), out: (.out + [$c])}
      end;
      .out
    ) ] | last | reverse | implode;

# Main: store full array, find boundaries, partition and report
. as $all

| [ to_entries[] | select(.value.type == "system" and .value.subtype == "compact_boundary") ]
  as $boundaries

| if ($boundaries | length) == 0 then
    "No compaction events found."
  else
    "Compaction Events: \($boundaries | length)"
    + (
      [ range($boundaries | length) | . as $num |
        $boundaries[$num].key as $idx |
        $boundaries[$num].value as $entry |

        # Summary entry is the next user message with isCompactSummary
        ( $all[$idx + 1] | if .isCompactSummary == true then
            .message.content | if type == "string" then . else "" end
          else "" end
        ) as $summary |

        # Before: entries from start (or previous boundary) to this boundary
        ( if $num == 0 then 0 else $boundaries[$num - 1].key + 1 end ) as $start |
        $all[$start:$idx] as $before |

        # After: entries from this boundary+1 to next boundary (or end)
        ( if $num + 1 < ($boundaries | length)
          then $boundaries[$num + 1].key
          else ($all | length) end
        ) as $end_idx |
        $all[($idx + 1):$end_idx] as $after |

        "\n#\($num + 1)  \(($entry.timestamp // "unknown") | split(".")[0] | sub("T"; " "))"
        + "\n    Trigger:    \($entry.compactMetadata.trigger // "unknown")"
        + "\n    Pre-tokens: \($entry.compactMetadata.preTokens // 0 | comma_fmt)"
        + "\n    Before:     \($before | count_type("assistant")) assistant, \($before | count_type("user")) user, \($before | count_tool_uses) tool calls"
        + "\n    After:      \($after | count_type("assistant")) assistant, \($after | count_type("user")) user, \($after | count_tool_uses) tool calls"
        + if ($summary | length) > 0
          then "\n    Summary:    \"\($summary | gsub("\n"; " ") | trunc(200))\""
          else "" end
      ] | join("\n")
    )
  end
