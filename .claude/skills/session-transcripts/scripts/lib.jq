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

# Truncate error content smartly: show first line + tail (where errors typically appear).
# Collapses newlines to "  " for single-line output.
def smart_error_truncate(n):
  if length <= n then gsub("\n"; "  ")
  else
    (index("\n") // length) as $nl |
    (if $nl < length then .[:$nl] else . end) as $first |
    if ($first | length) >= n then .[:n] + "..."
    else
      ($first | length) as $fl |
      (n - $fl - 8) as $tail |
      if $tail > 30 then
        $first + " [...] " + (.[-$tail:] | ltrimstr("\n") | gsub("\n"; "  "))
      else
        .[-n:] | ltrimstr("\n") | gsub("\n"; "  ")
      end
    end
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
  elif .name == "Skill" then "Skill \(.input.skill // "?")\(if .input.args then " \(.input.args | truncate(30))" else "" end)"
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

# Check if a JSONL entry is an agent progress entry (sub-agent activity).
def is_agent_progress:
  .type == "progress" and (.data.type // "") == "agent_progress";

# Extract tool_use blocks from an agent_progress entry.
# Progress entries nest content at: .data.message.message.content[]
def progress_tool_blocks:
  [ .data.message.message.content // [] | .[] | select(.type == "tool_use") ];

# Extract the primary path/target from a tool_use block's input.
def tool_file_path:
  if .name == "Read" or .name == "Write" or .name == "Edit" then
    .input.file_path // null
  elif .name == "Grep" or .name == "Glob" then
    .input.path // null
  elif .name == "Bash" then
    .input.command // null
  elif .name == "Skill" then
    .input.skill // null
  elif .name == "Task" then
    .input.description // null
  elif .name == "WebFetch" then
    .input.url // null
  elif .name == "WebSearch" then
    .input.query // null
  else null
  end;

# Content preview for a tool_use block — more detail than brief_tool_desc.
# Shows substantive content (file content for Write, strings for Edit, command for Bash).
def tool_content_preview(n):
  if .name == "Write" then
    (.input.content // "" | gsub("\n"; " ") | truncate(n))
  elif .name == "Edit" then
    "old: \(.input.old_string // "" | gsub("\n"; " ") | truncate(n/2 | floor)) -> new: \(.input.new_string // "" | gsub("\n"; " ") | truncate(n/2 | floor))"
  elif .name == "Bash" then
    (.input.command // "" | gsub("\n"; " ") | truncate(n))
  elif .name == "Skill" then
    "\(.input.skill // "?")\(if .input.args then "  args: \(.input.args | truncate(n - 20))" else "" end)"
  elif .name == "Task" then
    "desc: \(.input.description // "" | truncate(n/3 | floor))  prompt: \(.input.prompt // "" | gsub("\n"; " ") | truncate(n*2/3 | floor))"
  elif .name == "Read" then
    (.input.file_path // "?")
  elif .name == "Grep" then
    "\(.input.pattern // "?") in \(.input.path // ".")"
  elif .name == "Glob" then
    "\(.input.pattern // "?") in \(.input.path // ".")"
  else
    (.input | keys | join(", ") | truncate(n))
  end;
