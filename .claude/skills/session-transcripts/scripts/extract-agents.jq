#!/usr/bin/env -S jq -L ~/.claude/skills/session-transcripts/scripts -rf
# extract-agents.jq — Extract sub-agent (Task tool) spawns from a session transcript.
# Usage: ./extract-agents.jq < session.jsonl
#        jq -L ~/.claude/skills/session-transcripts/scripts -rf extract-agents.jq session.jsonl
#
# Streams line-by-line (no slurp) for memory efficiency.
# Shows every Task agent spawned: type, description, prompt preview.

import "lib" as lib;

select(.type == "assistant")

| (.timestamp | lib::format_time_only) as $time
| lib::tool_use_blocks[]
| select(.name == "Task")

| .input as $in
| ($in.subagent_type // "?") as $type
| ($in.description // "?" | lib::truncate(50)) as $desc
| ($in.prompt // "" | gsub("\n"; " ") | lib::truncate(120)) as $prompt
| ($in.max_turns // "?" | tostring) as $turns
| (if $in.resume then " resume=\($in.resume | lib::truncate(16))" else "" end) as $resume
| (if $in.run_in_background == true then " bg" else "" end) as $bg

| "\($time)  Task  \"\($desc)\" (\($type), max_turns=\($turns)\($resume)\($bg))"
+ "\n              prompt: \"\($prompt)\""
