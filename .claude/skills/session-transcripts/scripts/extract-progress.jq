#!/usr/bin/env -S jq -L ~/.claude/skills/session-transcripts/scripts -rf
# extract-progress.jq — Extract progress entries (bash, hook, mcp) from a session transcript.
# Usage: ./extract-progress.jq session.jsonl
#        jq -L ~/.claude/skills/session-transcripts/scripts -rf extract-progress.jq session.jsonl
#
# Streams line-by-line (no slurp) for memory efficiency.
# Shows non-agent progress entries: bash output, hook results, MCP activity.
# For agent progress, use extract-subagent-commands.jq instead.

import "lib" as lib;

select(.type == "progress" and (.data.type // "") != "agent_progress")

| (.timestamp | lib::format_time_only) as $time
| .data as $d

| if $d.type == "bash_progress" then
    "\($time)  bash_progress  elapsed=\($d.elapsedTimeSeconds // 0)s  lines=\($d.totalLines // 0)  \($d.output // "" | gsub("\n"; " ") | lib::truncate(120))"
  elif $d.type == "hook_progress" then
    "\($time)  hook_progress  \($d | tostring | lib::truncate(150))"
  elif $d.type == "mcp_progress" then
    "\($time)  mcp_progress  \($d | tostring | lib::truncate(150))"
  else
    "\($time)  \($d.type // "unknown")  \($d | tostring | lib::truncate(150))"
  end
