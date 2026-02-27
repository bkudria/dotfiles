#!/usr/bin/env -S jq -rf
# extract-conversation.jq — Extract readable conversation from a session transcript.
# Usage: ./extract-conversation.jq session.jsonl
#        jq -rf extract-conversation.jq session.jsonl
#
# Streams line-by-line (no slurp) for memory efficiency on large files.
# Outputs markdown with ## User / ## Assistant headers.
# Skips thinking blocks, file-history-snapshots, system messages, progress, tool_result payloads.

# Only process user and assistant messages
select(.type == "user" or .type == "assistant")

# Format based on message type
| if .type == "user" then
    # Extract user text, skip pure tool-result messages
    .message.content as $content
    | if ($content | type) == "string" then
        "## User\n\n\($content)\n"
      elif ($content | type) == "array" then
        # Get text blocks
        ([ $content[] | select(.type == "text") | .text ] | join("\n")) as $text
        | if ($text | length) > 0 then
            "## User\n\n\($text)\n"
          else empty  # Skip pure tool_result messages (no user text)
          end
      else empty
      end

  elif .type == "assistant" then
    .message.content as $content
    | if ($content | type) == "array" then
        # Collect text blocks and tool_use names
        ([ $content[] | select(.type == "text") | .text ] | join("\n")) as $text
        | ([ $content[] | select(.type == "tool_use") | .name ]) as $tools
        | # Build output parts
          (if ($tools | length) > 0 then
            "[Tools: \($tools | join(", "))]"
          else "" end) as $tool_line
        | if ($text | length) > 0 and ($tool_line | length) > 0 then
            "## Assistant\n\n\($text)\n\n\($tool_line)\n"
          elif ($text | length) > 0 then
            "## Assistant\n\n\($text)\n"
          elif ($tool_line | length) > 0 then
            "## Assistant\n\n\($tool_line)\n"
          else empty
          end
      else empty
      end

  else empty
  end
