# Audit Mode

> **References:** `references/project-yaml-schema.md` (project.yaml schema), `profiles/` (every standard YAML lives in its profile directory; the YAML itself describes what it checks).

The audit runs in three phases: **collect** (deterministic checks), **resolve** (sub-agent verification of prompt-based standards), then **render** (final table + remediation). The audit passes iff zero `FAIL` rows are present in the rendered table.

**GATE — Read-only audit. The audit MUST NOT modify any file in the project under audit, including `project.yaml`. If the runner exits non-zero (schema validation failure, missing project.yaml, etc.), surface its stderr verbatim and halt. Do NOT edit, rewrite, or "migrate" `project.yaml` to make `--collect` succeed — a non-zero runner exit IS the audit result.**

## Steps

### 1. Collect

First, create a per-audit state directory by running:

```bash
STATE_DIR=$(scripts/run-audit.sh --init) && echo "$STATE_DIR"
```

Capture `$STATE_DIR` from stdout and use that literal path in every subsequent tool_use — shell variables don't persist across Bash tool_uses, only files do. Then run:

```bash
scripts/run-audit.sh --collect <project-root> "$STATE_DIR"
```

The runner reads `<project-root>/project.yaml` (and exits with a descriptive error if missing — there's no need to pre-read or `cat` it yourself), validates its schema via `lint-project-yaml.sh` (any unknown top-level keys cause `--collect` to fail-fast with the linter's error output before any standards run), walks every YAML in each selected profile's directory, executes deterministic `check.script` standards immediately, and writes `<state-dir>/collect.json` of the form:

```json
{
  "resolved": [
    { "id": "base/readme", "status": "PASS", "detail": "...", "description": "..." }
  ],
  "pending": [
    { "id": "base/lockfile", "required": false, "description": "...", "rendered_prompt": "..." }
  ],
  "disabled_count": 0,
  "project_context": "Detected project context (auto-detected from manifest files; verify before relying on it):\n- Language/runtime: …\n- Package manager: …\n- Primary manifest: …\n"
}
```

`resolved` entries already carry final `PASS`/`FAIL`/`SUGG` status. `pending` entries need sub-agent verification before the audit table can be rendered. Disabled standards never appear in either array — only their count. `project_context` is an auto-detected summary (language/runtime, package manager, primary manifest) baked into every prompt-based `rendered_prompt` so sub-agents skip a redundant discovery preamble; it is empty when no manifest is recognised.

`<state-dir>/collect.json` is consumed twice in the next step: once by you to enumerate `pending[]` for sub-agent dispatch, and once by the merge phase. Re-running `--collect` re-executes every deterministic `check.script` standard, so capture once.

**GATE — No prompt extraction. Do NOT write `rendered_prompt` content (or any other field of `collect.json`) to bash output, `/tmp/...`, or any side file. To compose the single-message dispatch, Read `$STATE_DIR/collect.json` once with the Read tool — that single Read is the canonical pre-dispatch inspection (it shows every `pending[i].id` and `pending[i].rendered_prompt`). Then copy each `rendered_prompt` directly into the corresponding Agent tool_use's `prompt` parameter. Do NOT `mkdir`, `for`-loop dump, `jq` enumerate, or echo prompts through Bash.**

### 2. Resolve pending standards

For every entry in `pending`, dispatch a `general-purpose` sub-agent with `model: 'haiku'` whose prompt is the entry's `rendered_prompt` from `$STATE_DIR/collect.json`, copied verbatim. Prompt-based standards are uniformly ecosystem-detection + file-presence + brief-summarization checks; haiku is sufficient and the cheapest tier that handles the work, and the explicit model overrides whatever the parent session is using. The runner has already baked in the description, the prompt body, and a directive instructing the agent to write `{"met": true|false, "detail": "<one-line>"}` to its `response_path` using the Write tool. The agent's conversational reply is ignored; only the file matters.

**GATE — Single-message dispatch. Before sending your dispatch message, count the Agent tool_use blocks it contains. That count MUST equal `pending.length` from the collect output. Do NOT split dispatches across multiple messages — a dispatch message with fewer Agent tool_uses than `pending.length` is malformed and must be revised before sending.**

After all sub-agents return, run the merge phase:

```bash
scripts/run-audit.sh --merge "$STATE_DIR"
```

The runner reads each pending entry's response file (raw JSON or, for backward compatibility, a fenced ```json block), validates `met` and `detail`, applies the status rule below, and treats missing files / parse failures / non-bool `met` as `FAIL` regardless of `required:` — the "failure to verify is itself a failure" contract is enforced by the runner, not by narration.

| `met` | `required` | Final status |
|-------|-----------|--------------|
| `true` | any | `PASS` |
| `false` | `true` | `FAIL` |
| `false` | `false` | `SUGG` |

### 3. Render

Pass the state directory to the runner's render phase:

```bash
scripts/run-audit.sh --render "$STATE_DIR"
```

The runner emits, in order:

- A markdown table with three columns (`Standard`, `Status`, `Detail`) listing only `FAIL` and `SUGG` rows, sorted FAIL → SUGG, alphabetical by id within each bucket. PASS rows are intentionally omitted from the table — the per-status count line preserves the PASS total.
- A blank line, then a per-status count: `X PASS, Y FAIL, Z SUGG`.
- Optionally, a single line `N standards disabled in project.yaml` (omitted when N == 0).
- Optionally, a "lock-in" suggestion block. When the audit completely passes (zero `FAIL`, zero `SUGG`) AND at least one PASSing standard is SUGG-style (its standard YAML has `required: false`) AND not already in the project's `required:` list, the runner appends a single sentence and a copy-pasteable YAML block listing those eligible IDs alphabetically. Treat it as a passive suggestion — surface it verbatim without acting on it; deciding which (if any) standards to lock in is the user's call.

The render step never invents `MANUAL`, `SKIP`, or `DISABLED` rows. Every row in the table is `FAIL` or `SUGG`. PASS rows and disabled standards are absent from the table; their existence is signaled only by the count line below the table.

**GATE — Verbatim render. The render phase's stdout — the FAIL/SUGG table, the `X PASS, Y FAIL, Z SUGG` count line, and the optional `N standards disabled` line — IS the audit. Present it verbatim. Do NOT paraphrase detail cells, prepend headings, or edit the count line into prose. The prioritized fix plan in step 4 is an additional section appended below the runner's output, never a substitute for any of it.**

### 4. Synthesize a prioritized fix plan

Below the runner's output, write a **prioritized fix plan**: each item is one line of "do X" guidance, one per FAIL row in the table. Read `$STATE_DIR/merged.json` to look up each failing standard's `description` field — that one-line statement of what the standard verifies — and let it shape the plan item so the guidance addresses the standard's intent, not just the symptom in `detail`. The `description` is for orientation, not verbatim inclusion: the plan item itself is still imperative "do X" guidance.

**GATE — Plan composition. List every FAIL (no upper bound), ordered by the profile sequence in `project.yaml` — e.g., with `profiles: [base, public]`, all `base` FAILs precede all `public` FAILs. Within each profile group, order by your judgement of which fix unlocks the most value (e.g., adding a license is more impactful than adding a code-of-conduct). SUGGs are excluded from the plan whenever any FAIL exists; if zero FAILs exist, list SUGGs using the same profile-then-judgement ordering. If zero FAIL and zero SUGG rows exist, omit the plan entirely.**

### 5. Pass/fail signal

The audit **passes** iff the rendered table contains zero `FAIL` rows. `SUGG` rows are non-blocking.

`--render` is a pure formatter and exits **0** on any successful render (including renders that contain `FAIL` rows). The pass/fail signal lives in a separate verb:

```bash
scripts/run-audit.sh --check "$STATE_DIR"
```

`--check` exits **0** when the audit passes (no `FAIL` rows), **1** when ≥1 `FAIL` row is present, and **≥2** for operational errors (missing `merged.json`, malformed JSON, unresolved pending entries). CI pipelines use `--check`'s exit code directly without grepping stdout.

Splitting the two responsibilities keeps `--render`'s stdout — the audit-table-as-artifact — from being reframed as a tool ERROR by interactive harnesses that interpret any non-zero exit as failure.

## Notes

- The runner does not invoke sub-agents itself — it only walks YAML, executes scripts, and formats output. The sub-agent dispatch in step 2 is interactive (driven by Claude in the main thread) and is not designed to run from CI.
- Standards are activated by directory listing: every `.yaml` file under `profiles/<profile-name>/` is a standard. To skip a standard for a specific project, list it in the project's `disabled:` map with a non-empty reason — see `references/project-yaml-schema.md`.
- A standard's *check* is fully described inside its YAML. The only severity knob `project.yaml` can turn is the `required:` list, which upgrades named standards from `SUGG` to `FAIL`. The check itself remains unparameterized. If you need a stricter check, add a separate standard YAML in a profile (e.g., `public/readme-sections.yaml` is a separate file from `base/readme.yaml`).
