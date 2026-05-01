# Audit Mode

> **References:** `references/project-yaml-schema.md` (project.yaml schema), `profiles/` (every standard YAML lives in its profile directory; the YAML itself describes what it checks).

The audit runs in three phases: **collect** (deterministic checks), **resolve** (sub-agent verification of prompt-based standards), then **render** (final table + remediation). The audit passes iff zero `FAIL` rows are present in the rendered table.

## Steps

### 1. Collect

Run:

```bash
scripts/run-audit.sh --collect <project-root>
```

The runner reads `<project-root>/project.yaml`, walks every YAML in each selected profile's directory, executes deterministic `check.script` standards immediately, and emits JSON of the form:

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

Save the JSON to a temp file (or capture it in a shell variable). You will need the full payload again for the render step.

### 2. Resolve pending standards

For every entry in `pending`, dispatch a `general-purpose` sub-agent — **all in a single message of parallel `Agent` tool calls**. Each sub-agent's prompt is the rendered prompt prefixed with the response-format instruction below:

> Verify the standard described below against the project. Use any tools you need (Read, Grep, Bash, etc.) to confirm or refute. End your response with a fenced JSON block of the form `\`\`\`json\n{"met": <bool>, "detail": "<one-line summary>"}\n\`\`\`` and nothing after that block.
>
> Standard description: `<description from pending entry>`
>
> Standard verification prompt:
>
> `<rendered_prompt from pending entry>`

After all sub-agents return, parse the trailing JSON block from each response. Combine `met` with the entry's `required` flag using this rule:

| `met` | `required` | Final status |
|-------|-----------|--------------|
| `true` | any | `PASS` |
| `false` | `true` | `FAIL` |
| `false` | `false` | `SUGG` |

Append each resolved entry to the `resolved` list (carry over `id`, `description`, the sub-agent's one-line `detail`, and the computed `status`). The merged JSON should look like the original `resolved` plus the newly-resolved formerly-pending entries.

**Sub-agent failure handling.** If a sub-agent's response cannot be parsed as the expected JSON block, or the sub-agent errors out, treat that standard as `FAIL` regardless of `required:` and put the parse/error reason in `detail`. Failure to verify is itself a failure.

### 3. Render

Pass the merged results JSON to the runner's render phase:

```bash
echo "$MERGED_JSON" | scripts/run-audit.sh --render -
```

(Or write the JSON to a file and pass the path.)

The runner emits, in order:

- A markdown table with three columns (`Standard`, `Status`, `Detail`) sorted FAIL → SUGG → PASS, alphabetical by id within each bucket.
- A blank line, then a per-status count: `X PASS, Y FAIL, Z SUGG`.
- Optionally, a single line `N standards disabled in project.yaml` (omitted when N == 0).
- A blank line, then `## Remediation` listing every FAIL/SUGG row's `id` + `description` + `detail`.

The render step never invents `MANUAL`, `SKIP`, or `DISABLED` rows. Every row in the table is one of `PASS`, `FAIL`, `SUGG`. Disabled standards are absent from the table entirely; their existence is only signaled by the count line below the table.

### 4. Synthesize a prioritized fix plan

The runner's remediation list is mechanical — every FAIL/SUGG entry, ordered by status. Below the runner's output, write a brief **prioritized fix plan**: top 3-5 highest-impact items first, FAILs ahead of SUGGs by default, each with one-line "do X" guidance. Use your judgement about which fixes unlock the most value (e.g., adding a license is more impactful than adding a code-of-conduct).

If the table contains zero `FAIL` and zero `SUGG` rows, omit the prioritized plan.

### 5. Pass/fail signal

The audit **passes** iff the rendered table contains zero `FAIL` rows. `SUGG` rows are non-blocking. The runner mirrors this: `--render` exits **1** when the table contains ≥1 `FAIL` row, **0** otherwise. Runtime/input errors (missing project.yaml, malformed standard, unresolved pending entries, etc.) also exit non-zero. CI pipelines can use `$?` directly without grepping stdout.

## Notes

- The runner does not invoke sub-agents itself — it only walks YAML, executes scripts, and formats output. The sub-agent dispatch in step 2 is interactive (driven by Claude in the main thread) and is not designed to run from CI.
- Standards are activated by directory listing: every `.yaml` file under `profiles/<profile-name>/` is a standard. To skip a standard for a specific project, list it in the project's `disabled:` map with a non-empty reason — see `references/project-yaml-schema.md`.
- A standard's behaviour is fully described inside its YAML. There is no parameterization from `project.yaml`. If you need a stricter variant, add a separate standard YAML in a profile (e.g., `public/readme-sections.yaml` is a separate file from `base/readme.yaml`).
