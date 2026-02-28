#!/usr/bin/env -S jq -srf
# session-overview.jq — Quick stats summary of a session transcript.
# Usage: ./session-overview.jq session.jsonl
#        jq -sf session-overview.jq session.jsonl
#
# Slurps entire file to compute aggregates.

# Helper: parse ISO timestamp to epoch seconds
def parse_ts:
  sub("\\.[0-9]+Z$"; "Z") | strptime("%Y-%m-%dT%H:%M:%SZ") | mktime;

# Helper: format duration in seconds to "Xm Ys"
def format_duration:
  if . == null or . < 0 then "unknown"
  elif . < 60 then "\(.)s"
  elif . < 3600 then "\(. / 60 | floor)m \(. % 60)s"
  else "\(. / 3600 | floor)h \(. % 3600 / 60 | floor)m"
  end;

# Helper: format number with commas
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

# Helper: extract text from user content
def _user_text:
  .message.content
  | if type == "string" then .
    elif type == "array" then
      [ .[] | select(.type == "text") | .text ] | join("\n")
    else ""
    end;

# Helper: extract tool_result text length from user content
def _tool_result_chars:
  .message.content
  | if type == "array" then
      [ .[] | select(.type == "tool_result") | .content
        | if type == "string" then length
          elif type == "array" then [ .[] | select(.type == "text") | .text | length ] | add // 0
          else 0 end
      ] | add // 0
    else 0
    end;

# Compute stats
{
  session_id: (map(select(.sessionId != null) | .sessionId) | first // "unknown"),
  start_time: (map(select(.timestamp != null) | .timestamp) | sort | first // "unknown"),
  end_time: (map(select(.timestamp != null) | .timestamp) | sort | last // "unknown"),
  duration: (
    (map(select(.timestamp != null) | .timestamp) | sort) as $ts |
    if ($ts | length) >= 2 then
      (($ts | last | parse_ts) - ($ts | first | parse_ts)) | format_duration
    else "unknown"
    end
  ),
  models: ([ .[] | select(.type == "assistant") | .message.model // empty ] | unique),
  message_counts: {
    user: [ .[] | select(.type == "user") ] | length,
    assistant: [ .[] | select(.type == "assistant") ] | length,
    system: [ .[] | select(.type == "system") ] | length,
    total_entries: length
  },
  compactions: ([ .[] | select(.type == "system" and .subtype == "compact_boundary") ] | length),
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
  content: {
    assistant_chars: ([ .[] | select(.type == "assistant") | .message.content // [] | .[] | select(.type == "text") | .text | length ] | add // 0),
    user_chars: ([ .[] | select(.type == "user") | _user_text | length ] | add // 0),
    tool_result_chars: ([ .[] | select(.type == "user") | _tool_result_chars ] | add // 0)
  },
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
+ "\nDuration: \(.duration)"
+ "\nModel:   \(.models | join(", "))"
+ "\n"
+ "\nMessages: \(.message_counts.user) user, \(.message_counts.assistant) assistant, \(.message_counts.system) system (\(.message_counts.total_entries) total entries)"
+ (if .compactions > 0 then "\nCompactions: \(.compactions)" else "" end)
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
+ "\nContent:"
+ "\n  Assistant text: \(.content.assistant_chars | comma_fmt) chars"
+ "\n  User text:      \(.content.user_chars | comma_fmt) chars"
+ "\n  Tool results:   \(.content.tool_result_chars | comma_fmt) chars"
+ "\n  Total:          \(.content.assistant_chars + .content.user_chars + .content.tool_result_chars | comma_fmt) chars (~\(.content.assistant_chars + .content.user_chars + .content.tool_result_chars | . / 4 | floor | comma_fmt) tokens)"
+ "\n"
+ "\nFirst user message: \(.first_user_message)"
+ "\n"
