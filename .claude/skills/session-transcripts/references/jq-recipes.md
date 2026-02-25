# jq Recipes for Session Transcripts

Ad-hoc jq one-liners for querying session transcript JSONL files.

All recipes assume `FILE` is the path to a `.jsonl` transcript file.

## Extracting Messages

**All user messages as plain text:**
```bash
jq -r 'select(.type == "user") | .message.content | if type == "string" then . elif type == "array" then [.[] | select(.type == "text") | .text] | join("\n") else empty end' "$FILE"
```

**All assistant text (no thinking, no tool_use):**
```bash
jq -r 'select(.type == "assistant") | [.message.content[] | select(.type == "text") | .text] | join("\n")' "$FILE"
```

**All thinking blocks:**
```bash
jq -r 'select(.type == "assistant") | [.message.content[] | select(.type == "thinking") | .thinking] | join("\n---\n")' "$FILE"
```

**User messages that are NOT tool results (pure human input only):**
```bash
jq -r 'select(.type == "user" and (.message.content | type == "string" or (type == "array" and any(.[]; .type == "text") and (any(.[]; .type == "tool_result") | not)))) | .message.content | if type == "string" then . else [.[] | .text] | join("\n") end' "$FILE"
```

## Tool Usage

**List all tools used with frequency:**
```bash
jq -r 'select(.type == "assistant") | .message.content[]? | select(.type == "tool_use") | .name' "$FILE" | sort | uniq -c | sort -rn
```

**All Bash commands executed:**
```bash
jq -r 'select(.type == "assistant") | .message.content[]? | select(.type == "tool_use" and .name == "Bash") | .input.command' "$FILE"
```

**All files read:**
```bash
jq -r 'select(.type == "assistant") | .message.content[]? | select(.type == "tool_use" and .name == "Read") | .input.file_path' "$FILE"
```

**All files written or edited:**
```bash
jq -r 'select(.type == "assistant") | .message.content[]? | select(.type == "tool_use" and (.name == "Write" or .name == "Edit")) | "\(.name): \(.input.file_path)"' "$FILE"
```

**All Grep searches:**
```bash
jq -r 'select(.type == "assistant") | .message.content[]? | select(.type == "tool_use" and .name == "Grep") | "\(.input.pattern) in \(.input.path // ".")"' "$FILE"
```

## Token Usage

**Total tokens per assistant turn:**
```bash
jq -r 'select(.type == "assistant" and .message.usage != null) | "\(.timestamp): in=\(.message.usage.input_tokens) out=\(.message.usage.output_tokens) cache_read=\(.message.usage.cache_read_input_tokens) cache_create=\(.message.usage.cache_creation_input_tokens)"' "$FILE"
```

**Grand totals:**
```bash
jq -s '[.[] | select(.type == "assistant" and .message.usage != null) | .message.usage] | { input: (map(.input_tokens // 0) | add), output: (map(.output_tokens // 0) | add), cache_read: (map(.cache_read_input_tokens // 0) | add), cache_creation: (map(.cache_creation_input_tokens // 0) | add) }' "$FILE"
```

**Model(s) used:**
```bash
jq -r 'select(.type == "assistant") | .message.model // empty' "$FILE" | sort -u
```

## Searching and Filtering

**Find messages mentioning a specific file:**
```bash
jq -r 'select(.type == "user" or .type == "assistant") | select(tostring | test("filename\\.rb")) | "\(.type) [\(.timestamp)]: \(.uuid)"' "$FILE"
```

**Filter by time range:**
```bash
jq -r 'select(.timestamp > "2026-02-08T00:00:00" and .timestamp < "2026-02-09T00:00:00")' "$FILE"
```

**Extract git branches touched:**
```bash
jq -r 'select(.gitBranch != null) | .gitBranch' "$FILE" | sort -u
```

**Count entries by type:**
```bash
jq -r '.type' "$FILE" | sort | uniq -c | sort -rn
```

## Session Metadata

**Session ID and time range:**
```bash
jq -s '{ session_id: (first.sessionId), start: (map(.timestamp) | sort | first), end: (map(.timestamp) | sort | last) }' "$FILE"
```

**All sub-agent interactions:**
```bash
jq -r 'select(.type == "assistant") | .message.content[]? | select(.type == "tool_use" and .name == "Task") | .input | "\(.description // .subagent_type): \(.prompt[:100])..."' "$FILE"
```

## Session Review

**Most expensive turns (by output tokens, top 10):**
```bash
jq -r 'select(.type == "assistant" and .message.usage != null) | "\(.message.usage.output_tokens)\t\(.timestamp | split("T")[1] | split(".")[0])\t\([.message.content[]? | select(.type == "tool_use") | .name] | join(","))\t\([.message.content[]? | select(.type == "text") | .text] | join(" ") | .[:80])"' "$FILE" | sort -rn | head -10
```

**Turn-by-turn timing (gaps between consecutive entries):**
```bash
jq -r 'select(.type == "user" or .type == "assistant") | "\(.timestamp)\t\(.type)"' "$FILE" | awk -F'\t' 'NR>1 { cmd="date -j -f \"%Y-%m-%dT%H:%M:%S\" \"" prev "\" +%s 2>/dev/null"; cmd | getline t1; close(cmd); cmd="date -j -f \"%Y-%m-%dT%H:%M:%S\" \"" substr($1,1,19) "\" +%s 2>/dev/null"; cmd | getline t2; close(cmd); gap=t2-t1; if(gap>30) printf "%s → %s  (%ds gap)  %s\n", prev_type, $2, gap, $1 } { prev=substr($1,1,19); prev_type=$2 }'
```

**Bash commands that produced errors (non-empty stderr in tool results):**
```bash
jq -r 'select(.type == "user" and .toolUseResult != null and .toolUseResult.stderr != null and (.toolUseResult.stderr | length) > 0) | "\(.timestamp | split("T")[1] | split(".")[0])  \(.toolUseResult.stderr[:120])"' "$FILE"
```

**Unique files modified (Write or Edit, deduped):**
```bash
jq -r 'select(.type == "assistant") | .message.content[]? | select(.type == "tool_use" and (.name == "Write" or .name == "Edit")) | .input.file_path' "$FILE" | sort -u
```

**Context compaction markers (session continuations):**
```bash
jq -r 'select(.type == "user") | .message.content | if type == "string" then select(test("continued from a previous conversation|out of context")) | .[:150] elif type == "array" then [.[] | select(.type == "text") | .text | select(test("continued from a previous conversation|out of context"))] | .[0] // empty | .[:150] else empty end' "$FILE"
```

**Sub-agent spawns with cost (prompt length as proxy):**
```bash
jq -r 'select(.type == "assistant") | .message.content[]? | select(.type == "tool_use" and .name == "Task") | "\(.input.description // "?")\t\(.input.subagent_type // "?")\tprompt_len=\(.input.prompt | length)"' "$FILE" | sort -t$'\t' -k3 -rn
```

## Working with lib.jq

Use the shared library for cleaner queries:

```bash
SKILL_DIR="$HOME/.claude/skills/session-transcripts/scripts"
jq -L "$SKILL_DIR" 'import "lib" as lib; select(.type == "user") | lib::user_text | lib::truncate(100)' "$FILE"
```
