#!/usr/bin/env -S jq -L ~/.claude/skills/session-transcripts/scripts -rf
# session-activity.jq — Chronological turn-by-turn activity summary.
# Usage: ./session-activity.jq session.jsonl
#        jq -L ~/.claude/skills/session-transcripts/scripts -rf session-activity.jq session.jsonl
#
# Streams line-by-line (no slurp) for memory efficiency.
# Outputs a compact timeline — one line per conversational turn.
# Think "git log --oneline" for sessions.

import "lib" as lib;

# Only process conversation entries (skip progress, file-history-snapshot, etc.)
select(.type == "user" or .type == "assistant")

| .timestamp as $ts
| ($ts | lib::format_time_only) as $time

| if .type == "user" then
    # Check if this is a pure tool_result response or has user text
    if lib::has_tool_results then
      lib::tool_result_count as $count
      | (lib::user_text | lib::truncate(80)) as $text
      | if ($text | length) > 0 then
          "\($time)  user       [tool_result ×\($count)] \"\($text)\""
        else
          "\($time)  user       [tool_result ×\($count)]"
        end
    else
      (lib::user_text | lib::truncate(80)) as $text
      | if ($text | length) > 0 then
          "\($time)  user       \"\($text)\""
        else empty
        end
    end

  elif .type == "assistant" then
    (lib::tool_names) as $tools
    | (lib::assistant_text | lib::truncate(80)) as $text
    | (lib::output_tokens | lib::format_tokens) as $tok
    | (if ($tools | length) > 0 then
        "[\($tools | join(", "))]"
      else "" end) as $tool_str
    | # Build the line
      (if ($text | length) > 0 and ($tool_str | length) > 0 then
        "\($time)  assistant  \($tool_str) \"\($text)\" (\($tok) tok)"
      elif ($text | length) > 0 then
        "\($time)  assistant  \"\($text)\" (\($tok) tok)"
      elif ($tool_str | length) > 0 then
        "\($time)  assistant  \($tool_str) (\($tok) tok)"
      else empty
      end)

  else empty
  end
