---
name: markdown
description: Programmatically read, query, extract, filter, update, and transform Markdown files using mq and mdq CLI tools. Use when programmatically reading markdown, extracting sections or elements from .md files, querying markdown structure, filtering markdown content, transforming markdown documents, updating markdown files in-place, selecting headings or lists or tables or links from markdown, or processing markdown in shell pipelines.
---

# Markdown Tools

Two CLI tools for programmatically working with Markdown files: **mq** (a full programming language) and **mdq** (a concise selector-based tool).

## When to Use Which

| Need | Tool | Why |
|------|------|-----|
| Quick extraction of a section, list, or table | `mdq` | Selectors mirror Markdown syntax — fast to write |
| Complex multi-step transformations | `mq` | Full language with variables, loops, pattern matching |
| In-place file editing | `mq` | `-U` flag modifies files directly; mdq is read-only |
| Pipe markdown into `jq` | `mdq` | Clean, typed JSON output (`-o json`) |
| Shell scripting conditionals | `mdq` | `-q` quiet mode with exit codes |
| Generate markdown from scratch | `mq` | `to_h()`, `to_link()`, `to_code()`, etc. |
| Table column/row filtering | `mdq` | `:-: col :-: row` syntax is concise and intuitive |
| Task list queries (checked/unchecked) | `mdq` | `- [x]`, `- [ ]`, `- [?]` selectors |
| Front matter access | Either | mq: `.yaml`/`.toml`; mdq: `+++` |
| HTML element selection by tag | `mdq` | `</> tag` is direct; mq requires string matching |
| Format conversion (MD to HTML) | `mq` | `-F html`, `-F json`, `-F text` |
| Batch processing many files | `mq` | `-A` aggregate, `-P` parallel, `-S` separator |
| Heading level manipulation | `mq` | `increase_header_level`, `<<`, `>>` operators |

### Decision Shortcut

- **Reading/extracting?** Start with `mdq`. If the query gets complex, switch to `mq`.
- **Writing/transforming?** Use `mq` — mdq is read-only.

## Quick Examples

### mdq — Extract by selector

```bash
# Get a section by title
mdq '# Installation' README.md

# List items from a section
mdq '# Features | -' README.md

# Filter table columns and rows
mdq ':-: Name :-: /Rust/' README.md

# Check if tasks are complete (for CI)
mdq -q '- [ ]' TODO.md && echo "incomplete tasks!"

# Links in a section, as JSON
mdq -o json '# Resources | []()' README.md | jq '.items'
```

### mq — Query and transform

```bash
# All h2 headings as plain text
mq '.h2 | to_text' README.md

# Code blocks filtered by language
mq 'nodes | filter(fn(x): and(is_code(x), eq(attr(x, "lang"), "python"));)' doc.md

# Promote all headings up one level
mq '.h | decrease_header_level' doc.md

# Edit file in-place: set all code block languages to "sh"
mq -U '.code | set_attr("lang", "sh")' script-doc.md

# Extract front matter
mq '.yaml' README.md

# Generate a table of contents
mq 'include "section" | nodes | sections(.) | toc(.)' doc.md
```

## Reference Files

| File | Purpose |
|------|---------|
| `references/mq-reference.md` | Complete mq reference: selectors, functions, operators, modules, CLI |
| `references/mdq-reference.md` | Complete mdq reference: selectors, string matching, output formats, CLI |

## Key Differences Summary

| Aspect | mq | mdq |
|--------|----|----|
| Philosophy | Full programming language for Markdown ASTs | Selector-based query tool mirroring Markdown syntax |
| Learning curve | Steep (new language) | Gentle (selectors look like Markdown) |
| Modification | Read + write (`-U` in-place) | Read-only (output-only `!s` replacement) |
| Output formats | markdown, html, text, json | markdown, json, plain |
| Input formats | markdown, mdx, html, text, raw | markdown only |
| Extensibility | User modules, macros, REPL, LSP | None |

## Dependencies

- **mq** — `brew install mq` (v0.5.16+; section/table modules require manual install to `~/.mq/`)
- **mdq** — `brew install mdq`
