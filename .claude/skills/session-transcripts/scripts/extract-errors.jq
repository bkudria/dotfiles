#!/usr/bin/env -S jq -L ~/.claude/skills/session-transcripts/scripts -rf
# extract-errors.jq — Extract errors and failures from a session transcript.
# Usage: ./extract-errors.jq session.jsonl
#        jq -L ~/.claude/skills/session-transcripts/scripts -rf extract-errors.jq session.jsonl
#
# Streams line-by-line (no slurp) for memory efficiency.
# Extracts: tool_result errors (is_error: true), non-empty stderr from bash.

import "lib" as lib;

select(.type == "user")

| (.timestamp | lib::format_time_only) as $time

# Emit tool_result errors
| (
    .message.content
    | if type == "array" then
        .[]
        | select(.type == "tool_result" and .is_error == true)
        | .content as $content
        | ($content | if type == "string" then .
           elif type == "array" then [.[] | select(.type == "text") | .text] | join("\n")
           else "" end) as $text
        | "\($time)  ERROR  tool_result  \(.tool_use_id // "?" | lib::truncate(20))  \($text | lib::truncate(200))"
      else empty
      end
  ),

# Emit stderr from bash tool results
if .toolUseResult != null and (.toolUseResult | type) == "object" then
  .toolUseResult
  | select(.stderr != null and .stderr != "")
  | "\($time)  STDERR  bash  \(.stderr | lib::truncate(200))"
else empty
end
