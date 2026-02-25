# lib.jq — Shared jq functions for session transcript processing
# Usage: jq -L ~/.claude/skills/session-transcripts/scripts/ 'import "lib" as lib; ...'

# Extract plain text from a user message's content field.
# Handles both string content and array content (with text blocks and tool_result blocks).
def user_text:
  .message.content
  | if type == "string" then .
    elif type == "array" then
      [ .[] | select(.type == "text") | .text ] | join("\n")
    else ""
    end;

# Extract text blocks from an assistant message's content array.
def assistant_text:
  .message.content
  | if type == "array" then
      [ .[] | select(.type == "text") | .text ] | join("\n")
    else ""
    end;

# Extract tool_use block names from an assistant message's content array.
def tool_names:
  [ .message.content // [] | .[] | select(.type == "tool_use") | .name ];

# Decode an encoded project path: "-Users-foo-bar" -> "/Users/foo/bar"
# The encoding replaces "/" with "-" and prepends "-".
def decode_project_path:
  # The directory name starts with "-", which represents the leading "/".
  # Subsequent "-" characters represent "/" separators.
  # However, this is ambiguous with directory names containing dashes.
  # The heuristic: the name always starts with "-Users-" or similar root paths.
  ltrimstr("-")
  | split("-")
  | join("/")
  | "/" + .;

# Format an ISO 8601 timestamp for short display (YYYY-MM-DD HH:MM)
def format_timestamp:
  if . == null then "unknown"
  else split("T") | .[0] as $date |
    (.[1] // "" | split(".")[0] | split("Z")[0] | .[0:5]) as $time |
    "\($date) \($time)"
  end;

# Truncate a string to n chars, adding ellipsis if truncated.
def truncate(n):
  if length <= n then .
  else .[:n] + "..."
  end;

# Check if a JSONL entry is a conversation message (user or assistant).
def is_conversation:
  .type == "user" or .type == "assistant";

# Check if a user message contains tool results (vs plain text).
def has_tool_results:
  .message.content | type == "array" and any(.[]; .type == "tool_result");

# Get total input tokens from an assistant message's usage stats.
def total_input_tokens:
  .message.usage
  | ((.input_tokens // 0) + (.cache_creation_input_tokens // 0) + (.cache_read_input_tokens // 0));

# Get output tokens from an assistant message.
def output_tokens:
  .message.usage.output_tokens // 0;

# Format an ISO 8601 timestamp as time only (HH:MM:SS).
def format_time_only:
  if . == null then "??:??:??"
  else split("T") | (.[1] // "" | split(".")[0] | split("Z")[0] | .[0:8]) // "??:??:??"
  end;

# Extract all tool_use blocks from an assistant message's content array.
def tool_use_blocks:
  [ .message.content // [] | .[] | select(.type == "tool_use") ];

# Compact one-line description of a tool_use block (name + key input).
def brief_tool_desc:
  if .name == "Read" then "Read \(.input.file_path // "?" | truncate(60))"
  elif .name == "Write" then "Write \(.input.file_path // "?" | truncate(60))"
  elif .name == "Edit" then "Edit \(.input.file_path // "?" | truncate(60))"
  elif .name == "Bash" then "Bash \(.input.command // "?" | truncate(60))"
  elif .name == "Grep" then "Grep \(.input.pattern // "?" | truncate(30)) in \(.input.path // "." | truncate(30))"
  elif .name == "Glob" then "Glob \(.input.pattern // "?" | truncate(40)) in \(.input.path // "." | truncate(20))"
  elif .name == "Task" then "Task \(.input.description // "?" | truncate(40)) (\(.input.subagent_type // "?"))"
  elif .name == "WebFetch" then "WebFetch \(.input.url // "?" | truncate(50))"
  elif .name == "WebSearch" then "WebSearch \(.input.query // "?" | truncate(50))"
  elif .name == "AskUserQuestion" then "AskUserQuestion"
  else "\(.name)"
  end;

# Format token count compactly (e.g., 1234 -> "1.2k", 12345 -> "12k").
def format_tokens:
  if . == null or . == 0 then "0"
  elif . < 1000 then tostring
  elif . < 10000 then "\((. / 100 | floor) / 10)k"
  else "\((. / 1000 | floor))k"
  end;

# Count tool_result blocks in a user message's content.
def tool_result_count:
  if .message.content | type == "array" then
    [ .message.content[] | select(.type == "tool_result") ] | length
  else 0
  end;
