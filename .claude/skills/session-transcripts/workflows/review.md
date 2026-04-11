# Reviewing a Session

Toolkit for understanding what happened in a past session — trace decisions, identify missteps, find errors. Use whichever sections are relevant to your task.

## Locate and scope

```bash
SCRIPTS=~/.claude/skills/session-transcripts/scripts
FILE=$($SCRIPTS/find-session.sh <uuid> 2>/dev/null | head -1)
$SCRIPTS/extract-overview.jq "$FILE"
```

## See what happened

```bash
$SCRIPTS/extract-tool-results.sh "$FILE"
```

Every tool call paired with its result, chronologically. The primary view for understanding session behavior — what the LLM did, what came back, and where things went wrong. Use `--tools Bash` to filter by tool name, `--full` for untruncated results.

## Find problems

```bash
$SCRIPTS/extract-tool-results.sh "$FILE" --errors-only
$SCRIPTS/extract-errors.jq "$FILE"
```

Tool calls that returned errors, then broader error patterns (stderr, tool failures).

## Search for specifics

```bash
$SCRIPTS/search-session.sh "$FILE" "<keyword>" --context 3
```

Keyword search across user text, assistant text, tool inputs, and tool result content. Results tagged by role (`user`, `assistant`, `tool_input`, `result`).

## Full conversation

```bash
$SCRIPTS/extract-conversation.jq "$FILE"
```

Conversation narrative with brief tool descriptions. Use when you need the conversational flow.

## Timeline and subagents

```bash
$SCRIPTS/extract-activity.jq "$FILE"
$SCRIPTS/extract-agents.jq "$FILE"
$SCRIPTS/extract-compaction.jq "$FILE"
```

One-line-per-turn timeline for spotting patterns and gaps. Agent spawns and compaction events for long or multi-agent sessions.
