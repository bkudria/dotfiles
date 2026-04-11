#!/usr/bin/env -S jq -L ~/.claude/skills/session-transcripts/scripts -rf
# extract-conversation.jq — Extract readable conversation from a session transcript.
# Usage: ./extract-conversation.jq session.jsonl
#        jq -L ~/.claude/skills/session-transcripts/scripts -rf extract-conversation.jq session.jsonl
#
# Streams line-by-line (no slurp) for memory efficiency on large files.
# Outputs markdown with ## User / ## Assistant headers.
# Skips thinking blocks, file-history-snapshots, system messages, progress, tool_result payloads.

import "lib" as lib;

# Only process user and assistant messages
select(.type == "user" or .type == "assistant")

# Format based on message type
| if .type == "user" then
    (lib::user_text) as $text
    | if ($text | length) > 0 then
        "## User\n\n\($text)\n"
      else empty  # Skip pure tool_result messages (no user text)
      end

  elif .type == "assistant" then
    (lib::assistant_text) as $text
    | (lib::tool_use_blocks | if length > 0 then
        [.[] | "- \(lib::brief_tool_desc)"] | join("\n")
      else "" end) as $tool_lines
    | if ($text | length) > 0 and ($tool_lines | length) > 0 then
        "## Assistant\n\n\($text)\n\n\($tool_lines)\n"
      elif ($text | length) > 0 then
        "## Assistant\n\n\($text)\n"
      elif ($tool_lines | length) > 0 then
        "## Assistant\n\n\($tool_lines)\n"
      else empty
      end

  else empty
  end
