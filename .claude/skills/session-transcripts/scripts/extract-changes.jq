#!/usr/bin/env -S jq -L ~/.claude/skills/session-transcripts/scripts -rf
# extract-changes.jq — Extract file operations from a session transcript.
# Usage: ./extract-changes.jq < session.jsonl
#        jq -L ~/.claude/skills/session-transcripts/scripts -rf extract-changes.jq session.jsonl
#
# Streams line-by-line (no slurp) for memory efficiency.
# Shows every file read/write/edit, bash command, and grep/glob search.

import "lib" as lib;

select(.type == "assistant")

| (.timestamp | lib::format_time_only) as $time
| lib::tool_use_blocks[]
| . as $tool

| if .name == "Read" then
    "\($time)  Read    \(.input.file_path // "?")"
  elif .name == "Write" then
    "\($time)  Write   \(.input.file_path // "?")"
  elif .name == "Edit" then
    "\($time)  Edit    \(.input.file_path // "?")"
  elif .name == "Bash" then
    "\($time)  Bash    \(.input.command // "?" | lib::truncate(80))"
  elif .name == "Grep" then
    "\($time)  Grep    \(.input.pattern // "?" | lib::truncate(30))  in \(.input.path // "." | lib::truncate(40))"
  elif .name == "Glob" then
    "\($time)  Glob    \(.input.pattern // "?" | lib::truncate(40))  in \(.input.path // "." | lib::truncate(30))"
  elif .name == "Task" then
    "\($time)  Task    \(.input.description // "?" | lib::truncate(40))  (\(.input.subagent_type // "?"))"
  elif .name == "WebFetch" then
    "\($time)  Fetch   \(.input.url // "?" | lib::truncate(60))"
  elif .name == "WebSearch" then
    "\($time)  Search  \(.input.query // "?" | lib::truncate(60))"
  else
    "\($time)  \(.name | lib::truncate(15))  \(.input | keys | join(", ") | lib::truncate(40))"
  end
