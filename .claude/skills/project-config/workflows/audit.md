# Audit Mode

> **References:** `references/project-yaml-schema.md` (project.yaml schema), `profiles/` (every standard YAML lives in its profile directory; the YAML itself describes what it checks).

The audit runs in three phases: **collect** (deterministic checks), **resolve** (sub-agent verification of prompt-based standards), then **render** (final table + remediation). The audit passes iff zero `FAIL` rows are present in the rendered table.

## Steps

### 1. Collect

Run:

```bash
scripts/run-audit.sh --collect <project-root> > /tmp/audit-collect.json
```

The runner reads `<project-root>/project.yaml` (and exits with a descriptive error if missing — there's no need to pre-read or `cat` it yourself), walks every YAML in each selected profile's directory, executes deterministic `check.script` standards immediately, and emits JSON of the form:

```json
{
  "resolved": [
    { "id": "base/readme", "status": "PASS", "detail": "...", "description": "..." }
  ],
  "pending": [
    { "id": "public/lockfile", "required": false, "description": "...", "rendered_prompt": "..." }
  ],
  "disabled_count": 0
}
```

`resolved` entries already carry final `PASS`/`FAIL`/`SUGG` status. `pending` entries need sub-agent verification before the audit table can be rendered. Disabled standards never appear in either array — only their count.

The collect output is persisted to a file because it is consumed twice in the next step: once by you to enumerate `pending[]` for sub-agent dispatch, and once as the merge phase's first argument. Re-running `--collect` re-executes every deterministic `check.script` standard, so capture once. Shell variables don't persist across Bash tool_uses — only files do.

To inspect the pending entries before dispatch:

```bash
jq -r '.pending[].id' /tmp/audit-collect.json
```

### 2. Resolve pending standards

For every entry in `pending`, dispatch a `general-purpose` sub-agent. Each sub-agent's prompt is the rendered prompt prefixed with the response-format instruction below.

**GATE — Single-message dispatch. Before sending your dispatch message, count the Agent tool_use blocks it contains. That count MUST equal `pending.length` from the collect output. Do NOT split dispatches across multiple messages — a dispatch message with fewer Agent tool_uses than `pending.length` is malformed and must be revised before sending.**

> Verify the standard described below against the project. Use any tools you need (Read, Grep, Bash, etc.) to confirm or refute. End your response with a fenced JSON block of the form `\`\`\`json\n{"met": <bool>, "detail": "<one-line summary>"}\n\`\`\`` and nothing after that block.
>
> Standard description: `<description from pending entry>`
>
> Standard verification prompt:
>
> `<rendered_prompt from pending entry>`

After all sub-agents return, build a responses directory and pipe it to the merge phase:

1. Create a temp directory: `RESPONSES_DIR=$(mktemp -d) && echo "$RESPONSES_DIR"`. Capture the path from stdout and use that literal in every subsequent tool_use; no separate persistence file is needed.
2. For each pending entry, write the sub-agent's full raw response text to `$RESPONSES_DIR/<id>.txt`. Slashes in `id` become subdirectory separators (e.g., `base/coverage-run` → `$RESPONSES_DIR/base/coverage-run.txt`). The Write tool autocreates parent directories, so no `mkdir -p` is needed beforehand. Prefer one Write tool_use per file in a single parallel-dispatch message — same shape as the sub-agent dispatch above.
3. Merge:

```bash
scripts/run-audit.sh --merge /tmp/audit-collect.json "$RESPONSES_DIR" > /tmp/audit-merged.json
```

The runner extracts the LAST fenced JSON block from each response (ignoring prose before it and any runtime-appended trailers after it), validates `met` and `detail`, applies the status rule below, and treats missing files / parse failures / non-bool `met` as `FAIL` regardless of `required:` — the "failure to verify is itself a failure" contract is enforced by the runner, not by narration.

| `met` | `required` | Final status |
|-------|-----------|--------------|
| `true` | any | `PASS` |
| `false` | `true` | `FAIL` |
| `false` | `false` | `SUGG` |

### 3. Render

Pass the merged results JSON to the runner's render phase:

```bash
scripts/run-audit.sh --render /tmp/audit-merged.json
```

(Or pipe via stdin: `cat /tmp/audit-merged.json | scripts/run-audit.sh --render -`.)

The runner emits, in order:

- A markdown table with three columns (`Standard`, `Status`, `Detail`) listing only `FAIL` and `SUGG` rows, sorted FAIL → SUGG, alphabetical by id within each bucket. PASS rows are intentionally omitted from the table — the per-status count line preserves the PASS total.
- A blank line, then a per-status count: `X PASS, Y FAIL, Z SUGG`.
- Optionally, a single line `N standards disabled in project.yaml` (omitted when N == 0).
- A blank line, then `## Remediation` listing every FAIL/SUGG row's `id` + `description` + `detail`.

The render step never invents `MANUAL`, `SKIP`, or `DISABLED` rows. Every row in the table is `FAIL` or `SUGG`. PASS rows and disabled standards are absent from the table; their existence is signaled only by the count line below the table.

**GATE — Verbatim render. The render phase's stdout — the FAIL/SUGG table, the `X PASS, Y FAIL, Z SUGG` count line, the optional `N standards disabled` line, and the `## Remediation` section — IS the audit. Present it verbatim. Do NOT paraphrase detail cells, prepend headings, edit the count line into prose, or replace the `## Remediation` section. The prioritized fix plan in step 4 is an additional section appended below the runner's output, never a substitute for any of it.**

### 4. Synthesize a prioritized fix plan

The runner's remediation list is mechanical — every FAIL/SUGG entry, ordered by status. Below the runner's output, write a **prioritized fix plan**: each item is one line of "do X" guidance.

**GATE — Plan composition. List every FAIL (no upper bound), ordered by the profile sequence in `project.yaml` — e.g., with `profiles: [base, public]`, all `base` FAILs precede all `public` FAILs. Within each profile group, order by your judgement of which fix unlocks the most value (e.g., adding a license is more impactful than adding a code-of-conduct). SUGGs are excluded from the plan whenever any FAIL exists; if zero FAILs exist, list SUGGs using the same profile-then-judgement ordering. If zero FAIL and zero SUGG rows exist, omit the plan entirely.**

### 5. Pass/fail signal

The audit **passes** iff the rendered table contains zero `FAIL` rows. `SUGG` rows are non-blocking. The runner mirrors this: `--render` exits **1** when the table contains ≥1 `FAIL` row, **0** otherwise. Runtime/input errors (missing project.yaml, malformed standard, unresolved pending entries, etc.) also exit non-zero. CI pipelines can use `$?` directly without grepping stdout.

## Notes

- The runner does not invoke sub-agents itself — it only walks YAML, executes scripts, and formats output. The sub-agent dispatch in step 2 is interactive (driven by Claude in the main thread) and is not designed to run from CI.
- Standards are activated by directory listing: every `.yaml` file under `profiles/<profile-name>/` is a standard. To skip a standard for a specific project, list it in the project's `disabled:` map with a non-empty reason — see `references/project-yaml-schema.md`.
- A standard's behaviour is fully described inside its YAML. There is no parameterization from `project.yaml`. If you need a stricter variant, add a separate standard YAML in a profile (e.g., `public/readme-sections.yaml` is a separate file from `base/readme.yaml`).
