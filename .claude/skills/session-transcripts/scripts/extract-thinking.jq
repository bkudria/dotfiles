#!/usr/bin/env -S jq -L ~/.claude/skills/session-transcripts/scripts -rf
# extract-thinking.jq — Extract thinking blocks from a session transcript.
# Usage: ./extract-thinking.jq session.jsonl
#        jq -L ~/.claude/skills/session-transcripts/scripts -rf extract-thinking.jq session.jsonl
#
# Streams line-by-line (no slurp) for memory efficiency.
# Shows every thinking block with timestamp and character count.

import "lib" as lib;

select(.type == "assistant")

| (.timestamp | lib::format_time_only) as $time
| [.message.content // [] | .[] | select(.type == "thinking")]
| select(length > 0)
| .[]
| "\($time)  thinking (\(.thinking | length) chars)"
+ "\n\(.thinking)"
+ "\n"
