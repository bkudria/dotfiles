#!/usr/bin/env -S jq -L ~/.claude/skills/session-transcripts/scripts -rf
# extract-subagent-commands.jq — Extract sub-agent tool uses from progress entries.
# Usage: ./extract-subagent-commands.jq session.jsonl
#        jq -L ~/.claude/skills/session-transcripts/scripts -rf extract-subagent-commands.jq session.jsonl
#
# Streams line-by-line (no slurp) for memory efficiency.
# Shows every tool use from sub-agents with agent ID, tool name, and details.

import "lib" as lib;

select(lib::is_agent_progress)

# Only assistant messages contain tool_use blocks
| select(.data.message.type == "assistant")

| (.timestamp | lib::format_time_only) as $time
| (.data.agentId // "?" | lib::truncate(8)) as $agent
| lib::progress_tool_blocks[]

| if .name == "Bash" then
    "\($time)  [\($agent)]  Bash    \(.input.command // "?" | gsub("\n"; " ") | lib::truncate(100))"
  elif .name == "Read" then
    "\($time)  [\($agent)]  Read    \(.input.file_path // "?")"
  elif .name == "Write" then
    "\($time)  [\($agent)]  Write   \(.input.file_path // "?")"
  elif .name == "Edit" then
    "\($time)  [\($agent)]  Edit    \(.input.file_path // "?")"
  elif .name == "Grep" then
    "\($time)  [\($agent)]  Grep    \(.input.pattern // "?" | lib::truncate(30))  in \(.input.path // "." | lib::truncate(40))"
  elif .name == "Glob" then
    "\($time)  [\($agent)]  Glob    \(.input.pattern // "?" | lib::truncate(40))  in \(.input.path // "." | lib::truncate(30))"
  elif .name == "Task" then
    "\($time)  [\($agent)]  Task    \(.input.description // "?" | lib::truncate(50))  (\(.input.subagent_type // "?"))"
  elif .name == "WebFetch" then
    "\($time)  [\($agent)]  Fetch   \(.input.url // "?" | lib::truncate(60))"
  elif .name == "WebSearch" then
    "\($time)  [\($agent)]  Search  \(.input.query // "?" | lib::truncate(60))"
  elif .name == "Skill" then
    "\($time)  [\($agent)]  Skill   \(.input.skill // "?") \(.input.args // "" | lib::truncate(40))"
  else
    "\($time)  [\($agent)]  \(.name | lib::truncate(12))  \(.input | keys | join(", ") | lib::truncate(50))"
  end
