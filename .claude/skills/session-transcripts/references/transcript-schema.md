# Session Transcript JSONL Schema

Claude Code stores session transcripts as JSONL files (one JSON object per line).

## File Locations

- Session transcripts: `~/.claude/projects/<encoded-project-path>/<session-uuid>.jsonl`
- Agent sub-sessions: `~/.claude/projects/<encoded-project-path>/agent-<agent-id>.jsonl`
- Global history index: `~/.claude/history.jsonl` (lightweight, different format)

### Path Encoding

Project paths are encoded by replacing `/` with `-` and prepending `-`:
- `/Users/bkudria/code/myproject` → `-Users-bkudria-code-myproject`

## Entry Types

Every JSONL line is a JSON object with a `type` field. The main types are:

| Type | Description |
|------|-------------|
| `user` | User message (text input or tool results) |
| `assistant` | Assistant response (text, thinking, tool_use) |
| `system` | System events (commands like /feedback, /clear) |
| `progress` | Progress updates (bash output, etc.) |
| `agent_progress` | Sub-agent progress |
| `hook_progress` | Git hook progress |
| `mcp_progress` | MCP server progress |
| `file-history-snapshot` | File tracking snapshots |
| `create` | Session creation marker |
| `query_update` | Query updates |
| `search_results_received` | Web search results |

## Common Fields

Present on most/all entry types:

```json
{
  "type": "user|assistant|system|...",
  "uuid": "unique-entry-id",
  "timestamp": "2026-02-09T04:42:38.378Z",
  "sessionId": "session-uuid",
  "parentUuid": "parent-entry-uuid (null for first entry)",
  "isSidechain": false,
  "cwd": "/Users/bkudria/project",
  "version": "2.1.37",
  "gitBranch": "develop"
}
```

## User Messages (`type: "user"`)

```json
{
  "type": "user",
  "message": {
    "role": "user",
    "content": "string OR array of content blocks"
  },
  "userType": "external",
  "thinkingMetadata": { "maxThinkingTokens": 31999 },
  "todos": [],
  "permissionMode": "default",
  "toolUseResult": {
    "stdout": "",
    "stderr": "",
    "interrupted": false,
    "isImage": false
  },
  "sourceToolAssistantUUID": "uuid (when this is a tool result response)"
}
```

### User Content Variants

**Plain text input:**
```json
{ "content": "Hello, please help me with..." }
```

**Tool results (response to assistant tool_use):**
```json
{
  "content": [
    {
      "type": "tool_result",
      "tool_use_id": "toolu_01...",
      "content": "stdout output or result text",
      "is_error": false
    },
    {
      "type": "text",
      "text": "(optional accompanying text)"
    }
  ]
}
```

Tool result content can also be an array of blocks (e.g., image results):
```json
{
  "type": "tool_result",
  "tool_use_id": "toolu_01...",
  "content": [
    { "type": "text", "text": "result text" },
    { "type": "image", "source": { "type": "base64", "media_type": "image/png", "data": "..." } }
  ]
}
```

## Assistant Messages (`type: "assistant"`)

```json
{
  "type": "assistant",
  "message": {
    "model": "claude-opus-4-6",
    "id": "msg_01...",
    "type": "message",
    "role": "assistant",
    "content": [ /* content blocks */ ],
    "stop_reason": "end_turn|tool_use|null",
    "stop_sequence": null,
    "usage": { /* token stats */ }
  },
  "requestId": "req_01...",
  "slug": "human-readable-slug"
}
```

### Assistant Content Block Types

**Text block:**
```json
{ "type": "text", "text": "The response text..." }
```

**Thinking block (extended thinking):**
```json
{
  "type": "thinking",
  "thinking": "Internal reasoning...",
  "signature": "base64-encoded-signature"
}
```

**Tool use block:**
```json
{
  "type": "tool_use",
  "id": "toolu_01...",
  "name": "Bash",
  "input": {
    "command": "echo test",
    "description": "Echo a test"
  }
}
```

Tool input fields vary by tool name (Read, Write, Edit, Grep, Glob, Bash, Task, etc.).

### Usage Stats

```json
{
  "usage": {
    "input_tokens": 3,
    "cache_creation_input_tokens": 7862,
    "cache_read_input_tokens": 21395,
    "output_tokens": 2,
    "cache_creation": {
      "ephemeral_5m_input_tokens": 0,
      "ephemeral_1h_input_tokens": 7862
    },
    "service_tier": "standard",
    "inference_geo": "not_available"
  }
}
```

## System Messages (`type: "system"`)

```json
{
  "type": "system",
  "subtype": "local_command",
  "content": "<command-name>/feedback</command-name>\n<command-message>text</command-message>\n<command-args></command-args>",
  "level": "info",
  "isMeta": false
}
```

## Progress Entries (`type: "progress"`)

```json
{
  "type": "progress",
  "data": {
    "type": "bash_progress",
    "output": "partial output...",
    "fullOutput": "full output...",
    "elapsedTimeSeconds": 2.5,
    "totalLines": 10,
    "timeoutMs": 120000
  }
}
```

## File History Snapshots (`type: "file-history-snapshot"`)

```json
{
  "type": "file-history-snapshot",
  "messageId": "uuid",
  "snapshot": {
    "messageId": "uuid",
    "trackedFileBackups": {},
    "timestamp": "2026-02-09T04:42:38.379Z"
  },
  "isSnapshotUpdate": false
}
```

These entries track file state for undo/rollback. They are very common and contain no conversational content — always skip them in conversation extraction.

## Agent Sub-sessions

Agent transcript files (named `agent-<id>.jsonl`) follow the same schema but entries include:
- `agentId`: Short hex ID of the agent
- `isSidechain`: `true` for sub-agent work

## Global History (`~/.claude/history.jsonl`)

A separate, lightweight format — NOT full transcripts:

```json
{
  "display": "user message or command text",
  "pastedContents": {},
  "timestamp": 1759092762410,
  "project": "/Users/bkudria/code/project-name",
  "sessionId": "session-uuid"
}
```

Timestamps here are Unix epoch milliseconds (not ISO 8601).
