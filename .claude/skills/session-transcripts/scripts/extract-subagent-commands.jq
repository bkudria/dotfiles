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

| "\($time)  [\($agent)]  \(lib::format_tool_line)"
