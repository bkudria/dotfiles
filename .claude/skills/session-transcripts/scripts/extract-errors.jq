#!/usr/bin/env -S jq -L ~/.claude/skills/session-transcripts/scripts -rnf
# extract-errors.jq — Extract errors and failures from a session transcript.
# Usage: ./extract-errors.jq session.jsonl
#        jq -L ~/.claude/skills/session-transcripts/scripts -rnf extract-errors.jq session.jsonl
#
# Streams with foreach (no slurp) for memory efficiency.
# Carries a state map of tool_use_id -> tool_use block from assistant entries
# to correlate errors with the tool call that caused them.
# Extracts: tool_result errors (is_error: true), non-empty stderr from bash.

import "lib" as lib;

foreach inputs as $entry (
  {};

  # UPDATE: accumulate tool_use blocks from assistant entries into state map
  if ($entry.type == "assistant") then
    reduce ($entry.message.content // [] | .[] | select(.type == "tool_use")) as $tu
      (.; . + {($tu.id): $tu})
  else . end;

  # EXTRACT: emit errors from user entries
  if ($entry.type == "user") then
    . as $state |
    ($entry.timestamp | lib::format_time_only) as $time |

    # Emit tool_result errors with correlated tool description
    (
      $entry.message.content
      | if type == "array" then
          .[]
          | select(.type == "tool_result" and .is_error == true)
          | .tool_use_id as $tid
          | (lib::tool_result_text) as $text
          | ($state[$tid] // null) as $tool
          | ($tool | if . != null then lib::brief_tool_desc else "unknown tool" end) as $tool_desc
          | "\($time)  ERROR  \($tool_desc)\n          \($text | lib::smart_error_truncate(400))"
        else empty
        end
    ),

    # Emit stderr from bash tool results
    if $entry.toolUseResult != null and ($entry.toolUseResult | type) == "object" then
      $entry.toolUseResult
      | select(.stderr != null and .stderr != "")
      | ($entry.message.content | if type == "array" then [.[] | select(.type == "tool_result") | .tool_use_id] | first // null else null end) as $tid
      | ($state[$tid] // null) as $tool
      | ($tool | if . != null then lib::brief_tool_desc else "unknown tool" end) as $tool_desc
      | "\($time)  STDERR  \($tool_desc)\n          \(.stderr | lib::smart_error_truncate(200))"
    else empty end

  else empty end
)
