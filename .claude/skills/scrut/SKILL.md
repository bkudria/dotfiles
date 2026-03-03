---
name: scrut
description: "Scrut CLI snapshot testing framework reference. Use when writing, reading, or editing scrut test files (.md with scrut code blocks), debugging scrut test failures, creating CLI snapshot tests, understanding scrut output expectations (glob, regex, quantifiers), configuring scrut test documents (front matter, timeouts, environment variables), or working with scrut update/create commands. TRIGGER when: encountering .t files (deprecated Cram format — migrate to Markdown), working with CLI snapshot or integration tests, or examining test failures in projects that use scrut. DO NOT use Cram/.t format for new tests — always use Markdown."
---

# Scrut CLI Testing Reference

Reference for the Scrut CLI snapshot testing framework — tests terminal programs using Markdown or Cram files.

## When to Use

- Writing, reading, or editing scrut test files (`.md` with `scrut` code blocks)
- Debugging scrut test failures or unexpected output
- Creating new CLI snapshot tests
- Understanding output expectation syntax (glob, regex, quantifiers, escaped)
- Configuring test documents (front matter, timeouts, environment, output streams)
- Using `scrut update` or `scrut create` commands
- Encountering `.t` files (deprecated Cram format) — migrate to Markdown

## Important: Cram/.t Format Is Deprecated

**Never create new `.t` (Cram) test files.** Always use Markdown (`.md`) format. When encountering existing `.t` files, migrate them to Markdown. Cram format lacks inline configuration support and defaults to combined stdout/stderr, making tests harder to maintain.

## Quick Reference

| Area | Key Syntax |
|------|-----------|
| **Run tests** | `scrut test <file.md>` |
| **Update expectations** | `scrut update -r -y <file.md>` |
| **Create from command** | `scrut create --output test.md -- cmd` |
| **Exact match** | `output` or `output (equal)` |
| **Glob match** | `output (glob)` — `*` any chars, `?` one char |
| **Regex match** | `output (regex)` — full line match |
| **Quantifiers** | `(?)` 0-1, `(*)` 0+, `(+)` 1+ lines |
| **Exit codes** | `[1]`, `[42]` |
| **STDERR testing** | `` ```scrut {output_stream: stderr} `` |
| **Env vars** | `$TESTDIR`, `$TESTFILE`, `$TMPDIR`, `$SCRUT_TEST` |

## Important

- Each `scrut` code block can contain ONLY ONE command/output pair
- Multiple commands require multiple separate `scrut` blocks
- Markdown format defaults to STDOUT only; Cram defaults to combined
- Environment inherits between test cases in the same document

## Quick Start Example

A complete scrut test file (`test-greeting.md`):

````markdown
# Greeting CLI Tests

## Basic greeting

```scrut
$ echo "Hello, World!"
Hello, World!
```

## Greeting with name argument

```scrut
$ echo "Hello, Alice!"
Hello, * (glob)
```

## Missing argument shows usage

```scrut {output_stream: stderr}
$ grep --invalid-flag 2>&1
grep: * (glob)
[2]
```
````

Run with:
```bash
scrut test test-greeting.md
scrut update -r -y test-greeting.md  # update expectations in-place
```

## Dependencies

- **scrut** — CLI snapshot testing framework (`cargo install scrut` or see [GitHub](https://github.com/facebookincubator/scrut))

## Instructions

Consult `references/reference.md` for complete syntax details, configuration options, file testing patterns, and advanced features (detached processes, wait conditions, bootstrap/setup).
