#!/usr/bin/env -S jq -L ~/.claude/skills/session-transcripts/scripts -rf
# extract-changes.jq — Extract file operations from a session transcript.
# Usage: ./extract-changes.jq session.jsonl
#        jq -L ~/.claude/skills/session-transcripts/scripts -rf extract-changes.jq session.jsonl
#
# Streams line-by-line (no slurp) for memory efficiency.
# Shows every file read/write/edit, bash command, and grep/glob search.

import "lib" as lib;

select(.type == "assistant")

| (.timestamp | lib::format_time_only) as $time
| lib::tool_use_blocks[]
| "\($time)  \(lib::format_tool_line)"
