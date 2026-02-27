---
name: session-transcripts
description: "Search and analyze past Claude Code sessions to understand what happened, trace decisions, and investigate history. IMPORTANT: Always load this skill before searching session history or working with transcripts — it provides optimized jq scripts and shell tools that are far more efficient than raw grep. Use when searching session history, finding past sessions or conversations, tracing how something was created or built, investigating what happened in a previous session, reviewing sessions for issues or missteps, reading .jsonl transcript files, analyzing token usage, extracting conversation from transcripts, or summarizing past sessions."
---

# Session Transcripts

Tools and schema reference for working with Claude Code session transcript JSONL files.

## When to Use

- Searching session history to find past work or conversations
- Tracing how something was created, built, or decided in a past session
- Investigating what happened in a previous session
- Finding a past session by UUID or content search
- Reviewing a session for errors, missteps, or improvement opportunities
- Summarizing or extracting the conversation flow from a session
- Analyzing token usage or tool usage in a session
- Listing projects or sessions
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
| `extract-subagent-commands.jq` | Pure jq | Sub-agent tool uses from progress entries |
| `find-tool-calls.sh <file> [--path P] [--tools T]` | Shell | Find tool calls by path/tool filter |
| `search-session.sh <file> <keyword> [--context N]` | Shell | Search session content with highlighting |
| `lib.jq` | jq module | Shared helper functions |

### Usage Examples

**Find a session by UUID:**
```bash
~/.claude/skills/session-transcripts/scripts/find-session.sh 77d54f93
```

**Extract readable conversation from a transcript:**
```bash
~/.claude/skills/session-transcripts/scripts/extract-conversation.jq /path/to/session.jsonl
```

**Get session stats:**
```bash
~/.claude/skills/session-transcripts/scripts/session-overview.jq /path/to/session.jsonl
```

**List all projects:**
```bash
~/.claude/skills/session-transcripts/scripts/list-projects.sh
```

**List sessions for a project:**
```bash
~/.claude/skills/session-transcripts/scripts/list-sessions.sh /Users/bkudria/code/myproject
```

**Search within a session:**
```bash
~/.claude/skills/session-transcripts/scripts/search-session.sh /path/to/session.jsonl "authentication" --context 3
```

**Find all Write/Edit operations on a specific file:**
```bash
~/.claude/skills/session-transcripts/scripts/find-tool-calls.sh /path/to/session.jsonl --tools "Write,Edit" --path "lib.jq"
```

**See what sub-agents did:**
```bash
~/.claude/skills/session-transcripts/scripts/extract-subagent-commands.jq /path/to/session.jsonl
```

## Reviewing a Session

Structured approach for reviewing what happened in a past session — find errors, trace decisions, identify missteps.

### Step 1: Locate and scope

```bash
SCRIPTS=~/.claude/skills/session-transcripts/scripts
FILE=$($SCRIPTS/find-session.sh <uuid> 2>/dev/null | head -1 | awk '{print $1}')
$SCRIPTS/session-overview.jq "$FILE"
```

### Step 2: Scan the timeline

```bash
$SCRIPTS/session-activity.jq "$FILE"
```

One line per turn — scan for patterns: long gaps, repeated tool calls, pivots in approach.

### Step 3: Find problems

```bash
$SCRIPTS/extract-errors.jq "$FILE"
$SCRIPTS/extract-agents.jq "$FILE"
```

Errors show tool failures. Agents show Task spawns with prompt previews — look for expensive delegation that could be avoided.

### Step 4: Trace file changes

```bash
$SCRIPTS/extract-changes.jq "$FILE"
```

Every Read/Write/Edit/Bash/Grep/Glob operation, chronologically. Spot wasted reads, unnecessary writes, repeated searches.

### Step 5: Deep dive (optional)

For full conversation context on specific sections identified above:

```bash
$SCRIPTS/extract-conversation.jq "$FILE" > /tmp/conversation.md
```

Then read specific line ranges from the temp file.

## Workflow Tips

- **Truncated UUIDs**: `find-session.sh` accepts truncated UUIDs (first segment only, e.g. `b366b3b0`). This matches the short format shown by ccstatusline. Prefix matching is tried first (fast), with substring fallback.
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
