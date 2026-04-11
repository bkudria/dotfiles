# Audit Mode

> **References:** `references/standards-catalog.md` (file paths, detection rules), `references/project-yaml-schema.md` (schema details). Read `standards-catalog.md` for standards marked SKIP by the script, or when writing remediation for FAIL standards.

## Steps

1. Run `scripts/check-standards.sh <project-root> --json` first — this performs deterministic file existence and config checks for all standards. The JSON output includes a `metadata` block with `language`, `test_framework`, and `test_directory`. Use the results array as the baseline for the compliance table; use the metadata to avoid redundant file reads.
2. For any standards marked `SKIP` by the script, run the appropriate manual check (see `references/standards-catalog.md`)
3. Follow symlinks — a symlink to the right content counts as meeting the standard
4. **File existence checks — root-only, exact filenames:**
   - Check **only the project root** — never search recursively into `node_modules/`, `vendor/`, `.git/`, or other dependency directories
   - **Do not use Glob for root-only checks** — Glob always searches recursively and will match files deep in `node_modules/`, wasting tokens. Instead, use `Read` on specific filenames (it returns an error if the file doesn't exist) or `Bash ls <project-root>/<filename>`.
   - Prefer the script's `--json` metadata (`test_framework`, `test_directory`) over manual file searches when possible
5. **Runtime Verification** — run the actual tools. Skip a `.run` check if its parent infrastructure check FAILed (no point running tests if no test directory exists).
   - **Linter**: If `linter` passed, run the project's linter. Report as a `linter.run` row — PASS if clean (exit 0), FAIL if violations found. Include violation count in Detail.
   - **Tests + Coverage**: If both `tests` and `coverage` passed, run the test suite **with coverage enabled** in a single command to avoid running tests twice. Report `tests.run` (pass/fail counts) and `coverage.run` (coverage percentage) from the same run. If only `tests` passed (coverage FAILed), run tests without coverage.
   - If a command fails due to missing dependencies, attempt one install-and-retry cycle.
   - `.run` rows are binary PASS/FAIL only — no WARN.
6. Output a markdown compliance table with exactly these columns:

```
| Standard     | Status | Detail                          |
|-------------|--------|---------------------------------|
| readme      | PASS   | README.md exists, has title     |
| tests       | PASS   | tests/ exists, vitest config    |
| tests.run   | PASS   | vitest: 14/14 passing           |
| license     | WARN   | File exists but no SPDX match   |
| linter      | PASS   | eslint.config.js found          |
| linter.run  | PASS   | eslint: clean (0 problems)      |
| coverage    | PASS   | Configured with ratchet         |
| coverage.run| PASS   | 94.2% line coverage             |
```

`.run` rows appear immediately after their parent infrastructure row. They are only present when the parent passed.

Status values: `PASS`, `FAIL`, `WARN`. Use these exact words.

7. If the project uses profiles, also check for redundant entries — fields in `standards:` or metadata that are identical to profile defaults. Report redundant entries as WARN rows in the compliance table (e.g., `| readme.required | WARN | Redundant — inherited from base profile |`, `| status | WARN | Redundant — matches base profile default |`).
8. Below the table, output a summary line: `**X/Y checks passing.**` where X = number of PASS rows and Y = total rows in the table. Count all rows including `.run` rows. WARN counts as not passing (same as FAIL for counting purposes).
9. Below the summary, output a **prioritized remediation plan** — rank fixes by impact, most impactful first. Explain WHY each fix matters and what to do. If all standards pass, omit the remediation section entirely. When writing remediation for FAILed standards, it can be useful to run the tool even though the infrastructure check failed — e.g., running coverage on a project without a ratchet to discover the current coverage level and inform the fix.
