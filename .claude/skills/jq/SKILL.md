---
name: jq
description: "jq language and CLI reference for writing filters, transforming JSON, and debugging expressions. Use when writing jq commands, building jq pipelines, analyzing JSON files, debugging jq errors, reviewing jq in shell scripts, or working with JSON data transformation."
---

# jq

Command-line JSON processor reference. Covers the jq filter language, all builtins, CLI invocation, streaming, modules, and practical patterns.

## When This Skill Applies

- Writing jq filter expressions for JSON transformation
- Building complex jq pipelines with reduce, foreach, or streaming
- Debugging jq errors or unexpected output
- Reviewing jq usage in shell scripts
- Converting between data formats (CSV, TSV, JSON, shell-quoted)
- Processing multiple JSON files or newline-delimited JSON (NDJSON)

## Quick Reference

| Area | Key Concepts |
|------|-------------|
| Basic filters | `.`, `.field`, `.[]`, `.[N]`, `.[N:M]`, `|`, `,` |
| Types | `null`, `boolean`, `number`, `string`, `array`, `object` |
| Operators | `+`, `-`, `*`, `/`, `%`, `//` (alternative), `?//` (try-alternative) |
| Comparison | `==`, `!=`, `<`, `<=`, `>`, `>=` |
| Logic | `and`, `or`, `not`, `if-then-elif-else-end` |
| String | `length`, `split`, `join`, `test`, `match`, `capture`, `gsub`, `sub`, `ltrimstr`, `rtrimstr`, `ascii_downcase`, `ascii_upcase`, `startswith`, `endswith` |
| Array | `map`, `select`, `sort_by`, `group_by`, `unique_by`, `flatten`, `first`, `last`, `nth`, `range`, `limit`, `indices`, `inside`, `contains` |
| Object | `keys`, `values`, `has`, `in`, `to_entries`, `from_entries`, `with_entries`, `add`, `del`, `getpath`, `setpath`, `delpaths`, `path` |
| Reduce/Iterate | `reduce EXP as $var (INIT; UPDATE)`, `foreach EXP as $var (INIT; UPDATE; EXTRACT)` |
| I/O | `input`, `inputs`, `debug`, `stderr`, `input_filename`, `input_line_number` |
| Format strings | `@base64`, `@base64d`, `@html`, `@uri`, `@csv`, `@tsv`, `@json`, `@text`, `@sh` |
| Error handling | `try-catch`, `?`, `?//` (alternative operator) |
| Advanced | `label $name \| ... break $name`, `$__loc__`, streaming (`--stream`, `tostream`, `fromstream`, `truncate_stream`) |
| Modules | `import`, `include`, `modulemeta`, `$ENV`, `env`, `builtins` |
| Assignment | `=`, `\|=`, `+=`, `-=`, `*=`, `/=`, `%=`, `//=` |

## Key Patterns

### Safe field access with defaults

```jq
.config.timeout // 30
```

### Transform array of objects

```jq
[.users[] | select(.active) | {name, email}]
```

### Group, sort, and reshape

```jq
group_by(.department) | map({key: .[0].department, value: [.[] | .name] | sort}) | from_entries
```

### Reduce to accumulate

```jq
reduce .[] as $item (0; . + $item.amount)
```

### Join two arrays (SQL-style)

```jq
(INDEX(.users[]; .id)) as $users | [.orders[] | . + {user_name: $users[.user_id | tostring].name}]
```

### Process NDJSON with inputs

```bash
jq -n '[inputs | select(.level == "error")]' app.log.json
```

### CSV output

```jq
.[] | [.name, .age, .email] | @csv
```

## Instructions

1. **Functions are filters, not values.** Arguments to `def` are passed as filters (callbacks), not values. Use `def f($x):` shorthand to bind a value argument.
2. **Generators produce multiple outputs.** `.[]`, `range`, `,`, and `recurse` are generators. Wrap in `[...]` to collect into an array.
3. **`//` is the alternative operator**, not logical OR. `null // "default"` returns `"default"`. `false // "default"` also returns `"default"` (both `null` and `false` are considered "falsey").
4. **`?//` is the try-alternative operator.** `expr ?// fallback` — try `expr`; if it errors or produces no output, use `fallback`. Different from `try expr catch fallback` which only catches errors, not empty output.
5. **Use `-n` with `inputs`** to process all inputs uniformly. Without `-n`, the first input is consumed by `.` and `inputs` gets the rest.
6. **Assignment is immutable.** `|=` runs the RHS with the current value as input. `=` runs the RHS with the original `.` as input. Prefer `|=` for updates, `=` for replacement.
7. **`try` suppresses errors.** `try .foo` is equivalent to `.foo?`. `try expr catch msg` captures the error message.
8. **Streaming for large files.** Use `--stream` flag or `tostream`/`fromstream` builtins to process large JSON without loading everything into memory.
9. **Use `@base64d` to decode**, `@base64` to encode. No `@base32` exists in jq.
10. **`env` and `$ENV` both access environment variables.** `$ENV.HOME` or `env.HOME` — functionally equivalent, `$ENV` is more common.

## Common Gotchas

| Gotcha | Explanation |
|--------|-------------|
| `.foo` on `null` produces `null`, not error | Use `.foo? // error("missing field")` for strict access |
| `if` requires `then` AND `end` | `if .x then .y end` — `else` is optional, defaults to `.` |
| String interpolation uses `\(expr)` | Not `${expr}` or `$(expr)` — this is jq, not bash |
| `limit(n; expr)` uses semicolons | Inside jq functions, `;` separates arguments, not `,` |
| `group_by` pre-sorts | Output groups are sorted by the grouping key |
| `to_entries` / `from_entries` keys are always strings | Object keys in jq are always strings |
| `empty` backtracks | `empty` is not `null` — it produces zero outputs and triggers backtracking |
| `--arg` passes strings only | Use `--argjson` for numbers, booleans, arrays, objects |

## Reference Files

| File | Purpose |
|------|---------|
| `references/language-basics.md` | Types, operators, conditionals, string interpolation, basic filters |
| `references/builtins.md` | Complete catalog of all builtin functions by category |
| `references/advanced-features.md` | reduce, foreach, generators, streaming, modules, try-catch, label-break |
| `references/cli-reference.md` | All command-line flags and invocation patterns |
| `references/cookbook.md` | Practical recipes for common data transformation tasks |
| `references/gotchas.md` | Common mistakes, tricky behaviors, and FAQ answers |
