#!/usr/bin/env -S jq -srf
# session-overview.jq — Quick stats summary of a session transcript.
# Usage: ./session-overview.jq session.jsonl
#        jq -sf session-overview.jq session.jsonl
#
# Slurps entire file to compute aggregates.

# Helper: extract text from user content
def _user_text:
  .message.content
  | if type == "string" then .
    elif type == "array" then
      [ .[] | select(.type == "text") | .text ] | join("\n")
    else ""
    end;

# Compute stats
{
  session_id: (map(select(.sessionId != null) | .sessionId) | first // "unknown"),
  start_time: (map(select(.timestamp != null) | .timestamp) | sort | first // "unknown"),
  end_time: (map(select(.timestamp != null) | .timestamp) | sort | last // "unknown"),
  models: ([ .[] | select(.type == "assistant") | .message.model // empty ] | unique),
  message_counts: {
    user: [ .[] | select(.type == "user") ] | length,
    assistant: [ .[] | select(.type == "assistant") ] | length,
    system: [ .[] | select(.type == "system") ] | length,
    total_entries: length
  },
  tool_usage: (
    [ .[] | select(.type == "assistant") | .message.content // [] | .[] | select(.type == "tool_use") | .name ]
    | group_by(.) | map({key: .[0], value: length}) | from_entries
  ),
  tokens: (
    [ .[] | select(.type == "assistant" and .message.usage != null) | .message.usage ] | {
      input: (map(.input_tokens // 0) | add // 0),
      output: (map(.output_tokens // 0) | add // 0),
      cache_read: (map(.cache_read_input_tokens // 0) | add // 0),
      cache_creation: (map(.cache_creation_input_tokens // 0) | add // 0)
    }
  ),
  first_user_message: (
    [ .[] | select(.type == "user") ] | first
    | if . then _user_text | .[:200] + (if length > 200 then "..." else "" end)
      else "none"
      end
  )
}

# Format as readable text
| "Session: \(.session_id)"
+ "\nTime:    \(.start_time) → \(.end_time)"
+ "\nModel:   \(.models | join(", "))"
+ "\n"
+ "\nMessages: \(.message_counts.user) user, \(.message_counts.assistant) assistant, \(.message_counts.system) system (\(.message_counts.total_entries) total entries)"
+ "\n"
+ "\nTool Usage:"
+ (.tool_usage | to_entries | sort_by(-.value) | map("  \(.key): \(.value)") | join("\n") | if . != "" then "\n" + . else "\n  (none)" end)
+ "\n"
+ "\nTokens:"
+ "\n  Input:          \(.tokens.input | tostring)"
+ "\n  Output:         \(.tokens.output | tostring)"
+ "\n  Cache read:     \(.tokens.cache_read | tostring)"
+ "\n  Cache creation: \(.tokens.cache_creation | tostring)"
+ "\n"
+ "\nFirst user message: \(.first_user_message)"
+ "\n"
