#!/usr/bin/env -S jq -L ~/.claude/skills/session-transcripts/scripts -rf
# extract-skill-usage.jq — Extract skill invocations from a session transcript.
# Usage: ./extract-skill-usage.jq session.jsonl
#        jq -L ~/.claude/skills/session-transcripts/scripts -rf extract-skill-usage.jq session.jsonl
#
# Streams line-by-line (no slurp) for memory efficiency.
# Shows every Skill tool invocation: timestamp, skill name, and args.

import "lib" as lib;

select(.type == "assistant")

| (.timestamp | lib::format_time_only) as $time
| lib::tool_use_blocks[]
| select(.name == "Skill")

| .input as $in
| ($in.skill // "?") as $skill
| (if $in.args then "  args: \($in.args | lib::truncate(100))" else "" end) as $args

| "\($time)  Skill  \($skill)\($args)"
