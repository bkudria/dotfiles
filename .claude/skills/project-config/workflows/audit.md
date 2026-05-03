# Audit Mode

> **References:** `references/project-yaml-schema.md` (project.yaml schema), `profiles/` (every standard YAML lives in its profile directory; the YAML itself describes what it checks).

The audit runs in **two collection rounds** plus render:

1. Round 1 — collect required standards, dispatch their pending entries, merge.
2. Gate — if every required standard PASSed, proceed; otherwise skip to render.
3. Round 2 — collect suggested standards, dispatch their pending entries, merge.
4. Render — final table, counts, optional skipped-suggested line, fix plan.

Suggested standards are gated behind required-pass: round 2 only runs when every required standard from round 1 has status PASS. This saves haiku dispatches on failing audits and keeps the output focused on FAILs.

**GATE — Read-only audit. The audit MUST NOT modify any file in the project under audit, including `project.yaml`. If the runner exits non-zero (schema validation failure, missing project.yaml, etc.), surface its stderr verbatim and halt. Do NOT edit, rewrite, or "migrate" `project.yaml` to make `--collect` succeed — a non-zero runner exit IS the audit result.**

## Steps

### 1a. Collect required

First, create a per-audit state directory:

```bash
STATE_DIR=$(scripts/run-audit.sh --init) && echo "$STATE_DIR"
```

Capture `$STATE_DIR` from stdout and use that literal path in every subsequent tool_use — shell variables don't persist across Bash tool_uses, only files do. Then collect the required-scope round:

```bash
scripts/run-audit.sh --collect <project-root> "$STATE_DIR" --scope required
```

The runner reads `<project-root>/project.yaml` (and exits with a descriptive error if missing — there's no need to pre-read or `cat` it yourself), validates its schema via `lint-project-yaml.sh` (any unknown top-level keys cause `--collect` to fail-fast with the linter's error output before any standards run), walks every YAML in each selected profile's directory, **filters to effective-required standards only** (intrinsic `required: true` OR id in project.yaml's `required:` overrides), executes deterministic `check.script` standards immediately, and writes `<state-dir>/collect-required.json` of the form:

```json
{
  "resolved": [
    { "id": "base/readme", "status": "PASS", "detail": "...", "description": "..." }
  ],
  "pending": [
    { "id": "base/lockfile", "required": true, "description": "...", "prompt_path": "<state-dir>/prompts/base/lockfile.txt", "response_path": "<state-dir>/responses/base/lockfile.txt" }
  ],
  "disabled_count": 0,
  "suggested_total": 4,
  "project_context": "Detected project context (auto-detected from manifest files; verify before relying on it):\n- Language/runtime: …\n- Package manager: …\n- Primary manifest: …\n"
}
```

`suggested_total` is the count of would-have-been-suggested standards that round 1 *deliberately skipped* — render uses it to surface the skipped count if the gate later trips.

**GATE — No prompt extraction. Do NOT write prompt content to bash output, `/tmp/...`, or any side file. Read `$STATE_DIR/collect-required.json` once for the index — it lists each pending entry's `id`, `required`, `description`, `prompt_path`, and `response_path`. Then, for each pending entry, Read the file at its `prompt_path` and copy that file's contents verbatim into the corresponding Agent tool_use's `prompt` parameter. Do NOT `mkdir`, `for`-loop dump, `jq` enumerate, or echo prompts through Bash; the per-entry prompt files are the only sanctioned source of prompt text. Issue all `pending.length` Reads as parallel tool_use blocks in **one** assistant message — do not stream them across multiple turns.**

**GATE — Single-message Reads (round 1). Before sending your Read message, count the Read tool_use blocks it contains. That count MUST equal `pending.length` from `collect-required.json`. Do NOT split Reads across multiple messages — a Read message with fewer Read tool_uses than `pending.length` is malformed and must be revised before sending.**

For every entry in `pending`, dispatch a `general-purpose` sub-agent with `model: 'haiku'` whose prompt is the contents of the file at the entry's `prompt_path`, copied verbatim. The runner has already baked in the description, the prompt body, and a directive instructing the agent to write `{"met": true|false, "detail": "<one-line>"}` to its `response_path` using the Write tool. The agent's conversational reply is ignored; only the file matters.

**GATE — Single-message dispatch (round 1). Before sending your dispatch message, count the Agent tool_use blocks it contains. That count MUST equal `pending.length` from `collect-required.json`. Do NOT split dispatches across multiple messages — a dispatch message with fewer Agent tool_uses than `pending.length` is malformed and must be revised before sending.**

After all sub-agents return, run merge:

```bash
scripts/run-audit.sh --merge "$STATE_DIR"
```

The runner reads `collect-required.json` plus each pending entry's response file, validates `met` and `detail`, applies the status rule below, and treats missing files / parse failures / non-bool `met` as `FAIL` regardless of `required:`.

| `met` | `required` | Final status |
|-------|-----------|--------------|
| `true` | any | `PASS` |
| `false` | `true` | `FAIL` |
| `false` | `false` | `SUGG` |

### 1b. Gate

Run the gate to decide whether round 2 should run:

```bash
scripts/run-audit.sh --gate "$STATE_DIR"
```

Exit codes:
- **0** — every effective-required entry has status PASS. Proceed to step 1c.
- **1** — at least one effective-required entry has status FAIL. **Skip directly to step 3 (render)** — round 2 never runs; render will surface the skipped count line.
- **≥2** — operational error (missing/malformed merged.json). Halt and surface stderr.

A required prompt-based standard whose sub-agent failed to write a response file is treated as FAIL by merge (matching the existing "failure to verify is itself a failure" contract) and trips the gate just like a deterministic FAIL.

### 1c. Collect suggested (only after gate=0)

```bash
scripts/run-audit.sh --collect <project-root> "$STATE_DIR" --scope suggested
```

The runner walks the same profiles but **filters to effective-suggested standards only** (intrinsic `required: false` AND id NOT in project.yaml's `required:` overrides). Output is `<state-dir>/collect-suggested.json` with the same shape as round 1 (minus `suggested_total`).

**GATE — No prompt extraction (round 2). Same rule as round 1: Read `collect-suggested.json` once for the index, then Read each pending entry's `prompt_path` to copy that file's contents verbatim into its Agent block. Do NOT mkdir, jq enumerate, or echo prompts through Bash. Issue all `pending.length` Reads as parallel tool_use blocks in **one** assistant message — do not stream them across multiple turns.**

**GATE — Single-message Reads (round 2). Before sending your Read message, count the Read tool_use blocks it contains. That count MUST equal `pending.length` from `collect-suggested.json`. Do NOT split Reads across multiple messages — a Read message with fewer Read tool_uses than `pending.length` is malformed and must be revised before sending.**

For every entry in this round's `pending`, dispatch a `general-purpose` sub-agent with `model: 'haiku'` whose prompt is the contents of the file at the entry's `prompt_path`, copied verbatim. After all return, run merge again:

```bash
scripts/run-audit.sh --merge "$STATE_DIR"
```

The second merge reads BOTH `collect-required.json` and `collect-suggested.json`, unions their resolved/pending arrays, re-reads response files, and writes `merged.json` with `scopes_collected: ["required","suggested"]`.

**GATE — Single-message dispatch (round 2). Before sending your dispatch message, count the Agent tool_use blocks it contains. That count MUST equal `pending.length` from `collect-suggested.json`. The single-message dispatch GATE applies independently to each round.**

### 3. Render

```bash
scripts/run-audit.sh --render "$STATE_DIR"
```

The runner emits, in order:

- A markdown table with three columns (`Standard`, `Status`, `Detail`) listing only `FAIL` and `SUGG` rows, sorted FAIL → SUGG, alphabetical by id within each bucket. PASS rows are intentionally omitted from the table — the per-status count line preserves the PASS total.
- A blank line, then a per-status count: `X PASS, Y FAIL, Z SUGG`.
- Optionally, a single line `N standards disabled in project.yaml` (omitted when N == 0).
- Optionally, a single line `N suggested standards skipped (required failures present)` — emitted iff `scopes_collected` lacks `"suggested"` AND `suggested_total > 0` (i.e., round 2 was gated out and there were suggesteds to skip).
- Optionally, a "lock-in" suggestion block. Triggers iff round 2 ran (`scopes_collected` includes `"suggested"`) AND zero `FAIL` AND zero `SUGG` AND at least one PASSing standard is SUGG-style (its YAML has `required: false`) AND not already in the project's `required:` list.

The render step never invents `MANUAL`, `SKIP`, or `DISABLED` rows. Every row in the table is `FAIL` or `SUGG`. PASS rows and disabled standards are absent from the table; their existence is signaled only by the count line below the table.

**GATE — Verbatim render. The render phase's stdout — the FAIL/SUGG table, the `X PASS, Y FAIL, Z SUGG` count line, the optional `N standards disabled` line, and the optional `N suggested standards skipped` line — IS the audit. Present it verbatim. Do NOT paraphrase detail cells, prepend headings, or edit the count line into prose. The prioritized fix plan in step 4 is an additional section appended below the runner's output, never a substitute for any of it.**

### 4. Synthesize a prioritized fix plan

Below the runner's output, write a **prioritized fix plan**: each item is one line of "do X" guidance, one per FAIL row in the table. Read `$STATE_DIR/merged.json` to look up each failing standard's `description` field — that one-line statement of what the standard verifies — and let it shape the plan item so the guidance addresses the standard's intent, not just the symptom in `detail`. The `description` is for orientation, not verbatim inclusion: the plan item itself is still imperative "do X" guidance.

**GATE — Plan composition. List every FAIL (no upper bound), ordered by the profile sequence in `project.yaml` — e.g., with `profiles: [base, public]`, all `base` FAILs precede all `public` FAILs. Within each profile group, order by your judgement of which fix unlocks the most value (e.g., adding a license is more impactful than adding a code-of-conduct). SUGGs are excluded from the plan whenever any FAIL exists; if zero FAILs exist, list SUGGs using the same profile-then-judgement ordering. If zero FAIL and zero SUGG rows exist, omit the plan entirely.**

### 5. Pass/fail signal

The audit **passes** iff the rendered table contains zero `FAIL` rows. `SUGG` rows are non-blocking. When required FAILs caused suggesteds to skip, `merged.json` contains required entries only — `--check` still gives the correct CI signal because the FAIL rows are present in the file regardless of whether round 2 ran.

`--render` is a pure formatter and exits **0** on any successful render (including renders that contain `FAIL` rows). The pass/fail signal lives in `--check`:

```bash
scripts/run-audit.sh --check "$STATE_DIR"
```

`--check` exits **0** when the audit passes (no `FAIL` rows), **1** when ≥1 `FAIL` row is present, and **≥2** for operational errors (missing `merged.json`, malformed JSON, unresolved pending entries). CI pipelines use `--check`'s exit code directly without grepping stdout.

`--gate` (used between rounds) and `--check` (used at the end) have the same exit-code shape but answer different questions: `--gate` filters to effective-required entries only; `--check` looks at every entry in merged.json. Don't conflate them.

## Notes

- The runner does not invoke sub-agents itself — it only walks YAML, executes scripts, and formats output. The sub-agent dispatch in steps 1a and 1c is interactive (driven by Claude in the main thread) and is not designed to run from CI.
- Standards are activated by directory listing: every `.yaml` file under `profiles/<profile-name>/` is a standard. To skip a standard for a specific project, list it in the project's `disabled:` map with a non-empty reason — see `references/project-yaml-schema.md`.
- A standard's *check* is fully described inside its YAML. The only severity knob `project.yaml` can turn is the `required:` list, which upgrades named standards from `SUGG` to `FAIL`. The check itself remains unparameterized. If you need a stricter check, add a separate standard YAML in a profile (e.g., `public/readme-sections.yaml` is a separate file from `base/readme.yaml`).
- The two-pass flow is the only audit flow. There is no single-pass `--collect` (without `--scope`); it errors clearly. The state-dir holds `collect-required.json` (always written), `collect-suggested.json` (only after gate=0), and `merged.json` (rebuilt on each `--merge` from the union of present collect files).
- The `project_context` block prepended to every prompt includes a `git ls-files` listing (capped at 200 entries) alongside language/manifest metadata, so sub-agents start with a project-tree map instead of running their own `find`. Use it as a starting point; Read individual files when their content matters to the standard. Non-git projects get the metadata lines but no listing.
