# mdq Reference

Complete reference for mdq — a Markdown query tool with selectors that mirror Markdown syntax.

## CLI Usage

```
mdq [OPTIONS] [SELECTORS] [FILES]...
```

Input defaults to stdin if no files specified. A path of `-` explicitly means stdin.

### Options

| Flag | Description |
|------|-------------|
| `-o`, `--output` | Output format: `markdown`/`md` (default), `json`, `plain` |
| `-q`, `--quiet` | No stdout; exit 0 if match, 1 if not |
| `-l`, `--link-format` | Link style: `keep`, `inline`, `never-inline` (default) |
| `--link-pos` | Link reference placement: `section` (default), `doc` |
| `--footnote-pos` | Footnote placement (defaults to `--link-pos`) |
| `--renumber-footnotes` | Renumber footnotes in output (default: true) |
| `--wrap-width N` | Wrap text at N characters (markdown output only) |
| `--[no]-br` | Include breaks between elements (default: true for md, false for plain) |

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Match found |
| 1 | No match, or error |

## Selectors

Selectors mirror the Markdown syntax they match. Chain with `|` (pipe) to drill into nested structures.

### Selector Types

| Syntax | Selects | Matcher matches... |
|--------|---------|-------------------|
| `# matcher` | Sections | Header title |
| `- matcher` | Unordered list items | Item text |
| `1. matcher` | Ordered list items | Item text |
| `- [ ] matcher` | Uncompleted tasks | Item text |
| `- [x] matcher` | Completed tasks | Item text |
| `- [?] matcher` | Any task item | Item text |
| `1. [ ]`, `1. [x]`, `1. [?]` | Ordered task variants | Item text |
| `[matcher](matcher)` | Links | First: link text, second: URL |
| `![matcher](matcher)` | Images | First: alt text, second: URL |
| `> matcher` | Block quotes | Contents |
| `` ```lang content`` | Code blocks | First: language, second: content |
| `</> matcher` | HTML tags | Tag contents (including angle brackets) |
| `P: matcher` | Paragraphs | Paragraph text |
| `:-: cols :-: rows` | Tables | First: column headers, second: row content |
| `+++ matcher` | Front matter | Front matter content |

### Important Notes

- `#` matches headings at **any** level (h1-h6). There is no `##` or `###` syntax.
- `1.` is required exactly — no other numbers work.
- Spaces between selector token and matcher are required (unless matcher is empty).
- `*` or empty matcher matches all elements of that type.
- Use `--` before selectors that start with `-` to prevent flag interpretation.

## Piping Selectors

```bash
mdq '# Features | - | []()'
#     ^^^^^^^^^^   ^   ^^^^
#     section      list links within
#     "Features"   items list items
```

Each selector filters the output of the previous one.

## String Matching

### Unquoted Strings

- Must start with a letter
- Case-insensitive
- Matches any substring
- Leading/trailing whitespace trimmed
- No escape sequences

```bash
mdq '# features'          # matches "Features", "FEATURES", "Key Features"
```

### Quoted Strings

- Delimited by `"` or `'`
- Case-sensitive
- Matches any substring

Escape sequences:

| Sequence | Produces |
|----------|----------|
| `\'`, `\"` | Single/double quote |
| `` \` `` | Single quote (NOT backtick — shell convenience) |
| `\\` | Backslash |
| `\n`, `\r`, `\t` | Newline, carriage return, tab |
| `\u{XXXX}` | Unicode code point (1-6 hex digits, braces required) |

```bash
mdq '# "Features"'        # case-sensitive match
mdq $'# "don\`t"'         # match apostrophe (use $'...' in shell)
```

### Anchors

| Anchor | Position | Behavior |
|--------|----------|----------|
| `^` | Before matcher | Must match at start |
| `$` | After matcher | Must match at end |
| `^...$` | Both | Exact match |

Anchors go outside quotes: `^"Features"$`

```bash
mdq '# ^Install'          # sections starting with "Install"
mdq '# setup$'            # sections ending with "setup"
mdq '# ^"Setup"$'         # exact match, case-sensitive
```

### Regex

Delimited by `/`. Searches for match anywhere (use `^` to anchor).

```bash
mdq '# /^(Install|Setup)/'     # starts with Install or Setup
mdq '# /v\d+\.\d+/'            # version patterns
mdq '```/rust|python/'          # Rust or Python code blocks
```

Case-insensitive: `(?i)` flag inside pattern.

### Regex Replacement

```
!s/pattern/replacement/
```

Replaces matched text in output (does NOT modify source file).

```bash
mdq $'# !s/Hello/Goodbye/' file.md    # replace in heading text
mdq $'P: !s/old/new/' file.md         # replace in paragraphs
mdq $'- !s/TODO/DONE/' file.md        # replace in list items
```

**Shell note**: `!` triggers history expansion in bash/zsh. Use `$'...'` quoting.

Limitations:
- Single-line matches only
- Ignores inline formatting during matching
- Cannot span link/image boundaries

## Selector Details

### Sections (`#`)

```bash
mdq '#' file.md                    # all sections
mdq '# Install' file.md           # section titled "Install" (substring)
mdq '# ^Install$' file.md         # exact title match
```

Returns the section title and all content including subsections. Does not differentiate heading levels.

### Lists (`-`, `1.`)

```bash
mdq '-' file.md                    # all unordered list items
mdq '1.' file.md                   # all ordered list items
mdq '- [x]' file.md               # completed tasks
mdq '- [ ]' file.md               # uncompleted tasks
mdq '- [?]' file.md               # any task
mdq '- [?] todo' file.md          # tasks containing "todo"
```

### Links and Images

```bash
mdq '[]()' file.md                 # all links
mdq '[Google]()' file.md           # links with "Google" text
mdq '[](^https://github)' file.md  # links to GitHub
mdq '![](*.png)' file.md           # images (note: * is substring, not glob)
```

### Code Blocks

~~~bash
mdq '```' file.md                  # all code blocks
mdq '```python' file.md            # Python code blocks
mdq '``` "def foo"' file.md        # code blocks containing "def foo"
mdq '```rust "fn main"' file.md    # Rust blocks containing "fn main"
~~~

### Tables (`:-:`)

```bash
mdq ':-: * :-:' file.md            # full table (column matcher required)
mdq ':-: Name :-:' file.md         # just the Name column
mdq ':-: * :-: Rust' file.md       # all columns, rows containing "Rust"
mdq ':-: /Name|Lang/ :-: /Rust|Go/' file.md  # specific cols and rows
```

- Column matcher matches header row text
- Row matcher matches any column in data rows
- Header row is always included in output
- Column matcher is required (use `*` for all)

### HTML (`</>`)

```bash
mdq '</>' file.md                  # all HTML elements
mdq '</> "<details>"' file.md      # details tags (must quote < >)
```

Note: matches individual tags, not tag pairs. `<span>text</span>` produces two HTML elements.

### Paragraphs (`P:`)

```bash
mdq 'P: hello' file.md            # paragraphs containing "hello"
```

Note: `P:` matches text content broadly — including inside list items and blockquotes, not just top-level paragraphs.

### Front Matter (`+++`)

```bash
mdq '+++' file.md                  # all front matter
mdq '+++ title' file.md            # front matter containing "title"
```

## Output Formats

### Markdown (default)

Renders matched elements as valid Markdown. Multiple results separated by `---`.

Links converted to reference style by default. Control with `--link-format` and `--link-pos`.

### JSON (`-o json`)

```json
{
  "items": [ /* matched elements */ ],
  "links": { "1": { "url": "...", "title": "..." } },
  "footnotes": { "a": [ /* ... */ ] }
}
```

Item types: `document`, `section`, `paragraph`, `code_block`, `link`, `image`, `block_quote`, `list`, `list_item`, `table`, `thematic_break`, `front_matter`

**Section**:
```json
{ "section": { "depth": 2, "title": "...", "body": [...] } }
```

**Code block**:
```json
{ "code_block": { "code": "...", "type": "code", "language": "rust" } }
```

**Table**:
```json
{ "table": { "alignments": [...], "rows": [["h1","h2"], ["d1","d2"]] } }
```

**Link**:
```json
{ "link": { "display": "...", "url": "..." } }
```

**List**:
```json
{ "list": [{ "item": [...], "index": 1, "checked": false }] }
```

### Plain Text (`-o plain`)

Strips all formatting. Links rendered as display text only. Footnotes removed.

## Common Patterns

### Extract section content
```bash
mdq '# API Reference' docs.md
```

### Get all links from a section
```bash
mdq '# Resources | []()' README.md
```

### Count incomplete tasks
```bash
mdq -o json '- [ ]' TODO.md | jq '.items | length'
```

### CI check: all tasks complete
```bash
if mdq -q '- [ ]' TODO.md; then
  echo "Incomplete tasks found!"
  exit 1
fi
```

### Filter table to specific columns
```bash
mdq ':-: /Name|Status/ :-:' status.md
```

### Extract code blocks by language as JSON
```bash
mdq -o json '```python' notebook.md | jq '.items[].code_block.code'
```

### Get all image URLs
```bash
mdq -o json '![]()' doc.md | jq '.items[].image.url'
```

### Nested list items
```bash
mdq '# Config | - | -' doc.md     # sub-items within a section's list
```

### Find completed tasks containing text
```bash
mdq '- [x] deploy' changelog.md
```

### Replace text in output
```bash
mdq $'P: !s/v1\\.0/v2.0/' release-notes.md
```

## Shell Escaping Tips

- Always single-quote the selector string: `mdq '# foo'`
- For `!` (images, replacements), use `$'...'`: `mdq $'![](*.png)'`
- For selectors starting with `-`, use `--`: `mdq -- '- [x]' file.md`
- Unquoted matchers handle `#`, `(`, `)` literally — no escaping needed inside the selector
- For apostrophes in matchers, use escaped backtick: `mdq '# don\`t'`
