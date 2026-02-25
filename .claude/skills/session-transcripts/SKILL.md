---
name: session-transcripts
description: "Claude Code session transcript tools and schema reference. Use when working with session transcripts, finding past sessions, reading .jsonl transcript files, searching session history, analyzing token usage, extracting conversation from transcripts, summarizing past sessions, or reviewing sessions for issues, missteps, and improvements."
---

# Session Transcripts

Tools and schema reference for working with Claude Code session transcript JSONL files.

## When to Use

- Finding a past session by UUID or content search
- Reading or summarizing a session transcript
- Extracting the conversation flow from a large transcript
- Analyzing token usage or tool usage in a session
- Listing projects or sessions
- Reviewing a session for errors, missteps, or improvement opportunities
- Any task involving `~/.claude/projects/` JSONL files

## Storage Layout

```
~/.claude/
├── projects/
│   └── <encoded-project-path>/       # e.g., -Users-bkudria-code-myproject
│       ├── <session-uuid>.jsonl      # Full session transcripts
│       ├── agent-<agent-id>.jsonl    # Sub-agent transcripts
│       └── CLAUDE.md                 # Project instructions (not a transcript)
└── history.jsonl                     # Lightweight global history index
```

Path encoding: `/Users/bkudria/code/foo` → `-Users-bkudria-code-foo`

## Available Scripts

All scripts are in `~/.claude/skills/session-transcripts/scripts/`.

| Script | Type | Purpose |
|--------|------|---------|
| `find-session.sh <uuid>` | Shell | Find session file by UUID (partial match) |
| `find-session.sh -s <term>` | Shell | Search content across all sessions |
| `list-projects.sh` | Shell | List all projects with session counts |
| `list-sessions.sh [path]` | Shell | List sessions for a project |
| `extract-conversation.jq` | Pure jq | Extract readable conversation (markdown) |
| `session-overview.jq` | Pure jq | Quick stats summary (tokens, tools, counts) |
| `session-activity.jq` | Pure jq | Chronological turn-by-turn timeline |
| `extract-errors.jq` | Pure jq | Find tool errors and failures |
| `extract-changes.jq` | Pure jq | File operations and tool usage log |
| `extract-agents.jq` | Pure jq | Sub-agent (Task) spawns with prompts |
| `lib.jq` | jq module | Shared helper functions |

### Usage Examples

**Find a session by UUID:**
```bash
~/.claude/skills/session-transcripts/scripts/find-session.sh 77d54f93
```

**Extract readable conversation from a transcript:**
```bash
~/.claude/skills/session-transcripts/scripts/extract-conversation.jq < /path/to/session.jsonl
```

**Get session stats:**
```bash
~/.claude/skills/session-transcripts/scripts/session-overview.jq < /path/to/session.jsonl
```

**List all projects:**
```bash
~/.claude/skills/session-transcripts/scripts/list-projects.sh
```

**List sessions for a project:**
```bash
~/.claude/skills/session-transcripts/scripts/list-sessions.sh /Users/bkudria/code/myproject
```

## Reviewing a Session

Structured approach for reviewing what happened in a past session — find errors, trace decisions, identify missteps.

### Step 1: Locate and scope

```bash
SCRIPTS=~/.claude/skills/session-transcripts/scripts
FILE=$($SCRIPTS/find-session.sh <uuid> 2>/dev/null | head -1 | awk '{print $1}')
$SCRIPTS/session-overview.jq < "$FILE"
```

### Step 2: Scan the timeline

```bash
$SCRIPTS/session-activity.jq < "$FILE"
```

One line per turn — scan for patterns: long gaps, repeated tool calls, pivots in approach.

### Step 3: Find problems

```bash
$SCRIPTS/extract-errors.jq < "$FILE"
$SCRIPTS/extract-agents.jq < "$FILE"
```

Errors show tool failures. Agents show Task spawns with prompt previews — look for expensive delegation that could be avoided.

### Step 4: Trace file changes

```bash
$SCRIPTS/extract-changes.jq < "$FILE"
```

Every Read/Write/Edit/Bash/Grep/Glob operation, chronologically. Spot wasted reads, unnecessary writes, repeated searches.

### Step 5: Deep dive (optional)

For full conversation context on specific sections identified above:

```bash
$SCRIPTS/extract-conversation.jq < "$FILE" > /tmp/conversation.md
```

Then read specific line ranges from the temp file.

## Workflow Tips

- **Large transcripts** (>256KB): Use `extract-conversation.jq` first to get a readable version, then read that with the Read tool. This is far more efficient than chunked reading.
- **Quick stats**: Use `session-overview.jq` before reading a transcript to understand its scope.
- **Ad-hoc queries**: See `references/jq-recipes.md` for common jq one-liners.
- **Schema details**: See `references/transcript-schema.md` for full JSONL field documentation.
- **Subagents**: For transcript summarization tasks, pipe `extract-conversation.jq` output to a temp file, then read it — avoids the multi-chunk problem entirely.

## Dependencies

- **jq** — JSON processor (`brew install jq`), required by all `.jq` scripts

## Reference Files

| File | Contents |
|------|----------|
| `references/transcript-schema.md` | Full JSONL schema with all entry types and fields |
| `references/jq-recipes.md` | Ad-hoc jq one-liners for custom queries |
