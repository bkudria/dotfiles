---
name: session-transcripts
description: "Search and analyze past Claude Code sessions to understand what happened, trace decisions, and investigate history. IMPORTANT: Always load this skill before searching session history or working with transcripts — it provides optimized jq scripts and shell tools that are far more efficient than raw grep. Use when searching session history, finding past sessions or conversations, tracing how something was created or built, investigating what happened in a previous session, reviewing sessions for issues or missteps, reading .jsonl transcript files, analyzing token usage, extracting conversation from transcripts, summarizing past sessions, or finding which sessions loaded or used a particular skill."
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
- Finding which sessions loaded or used a particular skill
- Any task involving `~/.claude/projects/` JSONL files

## Storage Layout

```
~/.claude/
├── projects/
│   └── <encoded-project-path>/              # e.g., -Users-bkudria-code-myproject
│       ├── <session-uuid>.jsonl             # Full session transcripts
│       ├── agent-<agent-id>.jsonl           # Sub-agent transcripts (old format)
│       ├── <session-uuid>/
│       │   └── subagents/
│       │       └── agent-<agent-id>.jsonl   # Sub-agent transcripts (new format)
│       └── CLAUDE.md                        # Project instructions (not a transcript)
└── history.jsonl                            # Lightweight global history index
```

Path encoding: replace `/` and `.` with `-`. Example: `/Users/me/.claude/skills/foo` → `-Users-me--claude-skills-foo`

## Available Scripts

All scripts are in `~/.claude/skills/session-transcripts/scripts/`.

### Discovery — find and list sessions

| Script | Purpose |
|--------|---------|
| `find-session.sh <uuid>` | Find session file by UUID (partial match, truncated UUIDs OK) |
| `find-session.sh -s <term>` | Search content across all sessions |
| `list-projects.sh` | List all projects with session counts |
| `list-sessions.sh [path]` | List sessions for a project |
| `find-subagent-files.sh <file>` | Find all subagent transcript files for a session |
| `find-skill-usage.sh <name> [--project <path>]` | Find sessions that loaded a specific skill |

### Extraction — structured views of a single session

| Script | Purpose | When to use |
|--------|---------|-------------|
| `extract-overview.jq` | Quick stats (tokens, tools, duration) | First look at any session |
| `extract-tool-results.sh [--full] [--tools T] [--errors-only]` | Tool calls paired with results via tool_use_id | Primary view for understanding what happened |
| `extract-conversation.jq` | Conversation with brief tool descriptions | Understanding the conversational flow |
| `extract-activity.jq` | One-line-per-turn timeline | Spotting patterns and timing gaps |
| `extract-errors.jq` | Tool errors and stderr | Debugging failures |
| `extract-changes.jq` | File operations log | Tracking what files were touched |
| `extract-agents.jq` | Sub-agent spawns with prompts and model | Multi-agent sessions |
| `extract-compaction.jq` | Compaction events with before/after stats | Long sessions hitting context limits |
| `extract-subagent-commands.jq` | Sub-agent tool uses | What sub-agents did |
| `extract-skill-usage.jq` | Skill invocations with timestamps | Which skills were loaded |
| `extract-thinking.jq` | Thinking blocks with timestamps | Understanding Claude's reasoning |
| `extract-progress.jq` | Non-agent progress entries (bash, hook, mcp) | Long-running commands and background activity |

### Search — find specific content within a session

| Script | Purpose | When to use |
|--------|---------|-------------|
| `search-session.sh <file> <keyword> [--context N]` | Keyword search across text, tool inputs, and results | Finding mentions of a topic |
| `search-tool-calls.sh <file> [--path P] [--tools T] [--commands-only]` | Filter tool calls by path/tool name | Finding specific file operations |

### Libraries

| File | Purpose |
|------|---------|
| `lib.jq` | Shared jq functions for all `.jq` scripts |
| `lib.sh` | Shared bash functions for all `.sh` scripts |

### Usage Examples

```bash
SCRIPTS=~/.claude/skills/session-transcripts/scripts

# Find and scope a session
$SCRIPTS/find-session.sh 77d54f93
$SCRIPTS/extract-overview.jq /path/to/session.jsonl

# See what happened
$SCRIPTS/extract-tool-results.sh /path/to/session.jsonl

# Search
$SCRIPTS/search-session.sh /path/to/session.jsonl "authentication" --context 3
$SCRIPTS/search-tool-calls.sh /path/to/session.jsonl --tools "Write,Edit" --path "lib.jq"

# Cross-session queries
$SCRIPTS/find-skill-usage.sh skillcraft
$SCRIPTS/list-sessions.sh /Users/bkudria/code/myproject
```

## Reviewing a Session

For structured session review, read `workflows/review.md`.

## Tips

- **Truncated UUIDs**: `find-session.sh` accepts truncated UUIDs (first segment only, e.g. `b366b3b0`). Prefix matching is tried first (fast), with substring fallback.
- **Temp files**: When saving script output to a file (for later reading, large results, etc.), use `~/.cache/claude-session-transcripts/` — never `/tmp`. Create the directory with `mkdir -p` first if needed.
- **Large transcripts** (>256KB file size): Use `extract-conversation.jq` to produce a readable version, pipe to `~/.cache/claude-session-transcripts/`, then Read that. Check file size first (`ls -l` or `extract-overview.jq`) — most sessions are well under this threshold and can be processed directly.
- **Quick stats**: Use `extract-overview.jq` before reading a transcript to understand its scope.
- **Ad-hoc queries**: See `references/jq-recipes.md` for common jq one-liners.
- **Schema details**: See `references/transcript-schema.md` for full JSONL field documentation.

## Dependencies

- **jq** — JSON processor (`brew install jq`), required by all `.jq` scripts

## Reference Files

| File | Contents |
|------|----------|
| `references/transcript-schema.md` | Full JSONL schema with all entry types and fields |
| `references/jq-recipes.md` | Ad-hoc jq one-liners for custom queries |
| `workflows/review.md` | Structured session review toolkit |
