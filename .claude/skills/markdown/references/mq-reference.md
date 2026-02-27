# mq Reference

Complete reference for mq — a jq-like query language for Markdown processing.

## CLI Usage

```
mq [OPTIONS] [QUERY] [FILES]...
mq repl                          # Interactive REPL
mq fmt [--check] file.mq         # Format .mq files
mq docs                          # Show function reference
```

### Key Flags

| Flag | Description |
|------|-------------|
| `-U`, `--update` | Edit file in-place |
| `-f FILE` | Load query from .mq file |
| `-I FORMAT` | Input format: `markdown`, `mdx`, `html`, `text`, `null`, `raw` |
| `-F FORMAT` | Output format: `markdown` (default), `html`, `text`, `json`, `none` |
| `-A` | Aggregate multiple files into single array |
| `-S QUERY` | Insert separator query between files |
| `-P N` | Parallel processing threshold (default: 10) |
| `--stream` | Line-by-line streaming for large files |
| `-o FILE` | Write output to file |
| `-C` | Colorize output |
| `--args NAME VALUE` | Set runtime string variable |
| `--rawfile NAME FILE` | Load file into runtime variable |
| `--list-style` | `dash` (default), `plus`, `star` |

### Module Flags

| Flag | Module | Key Functions |
|------|--------|---------------|
| `--json` | JSON | `json_parse`, `json_stringify`, `json_to_markdown_table` |
| `--csv` | CSV | `csv_parse`, `csv_stringify`, `csv_to_json`, `tsv_parse` |
| `--yaml` | YAML | `yaml_parse`, `yaml_stringify`, `yaml_to_json` |
| `--toml` | TOML | `toml_parse`, `toml_stringify`, `toml_to_json` |
| `--xml` | XML | `xml_parse`, `xml_stringify`, `xml_to_markdown_table` |
| `--fuzzy` | Fuzzy | `fuzzy_match`, `fuzzy_filter`, `fuzzy_best_match`, `jaro_winkler`, `levenshtein` |

### Includable Modules (from `~/.mq/`)

| Module | Include | Key Functions |
|--------|---------|---------------|
| Section | `include "section"` | `sections`, `title`, `content`, `toc`, `filter_sections`, `map_sections`, `split` |
| Table | `include "table"` | `tables`, `add_row`, `add_column`, `remove_row`, `filter_rows`, `sort_rows`, `to_csv` |

**Note**: These are `.mq` library files. If not installed, download from the mq repo to `~/.mq/`:
```bash
curl -sL 'https://raw.githubusercontent.com/harehare/mq/main/crates/mq-lang/modules/section.mq' -o ~/.mq/section.mq
curl -sL 'https://raw.githubusercontent.com/harehare/mq/main/crates/mq-lang/modules/table.mq' -o ~/.mq/table.mq
```

## Selectors

Selectors use `.` prefix to extract Markdown elements.

### Element Selectors

| Selector | Description |
|----------|-------------|
| `.h` | All headings (any level) |
| `.h1` through `.h6` | Heading at specific level |
| `.heading` | Alias for `.h` |
| `.text` | Text nodes |
| `.code` | Fenced code blocks |
| `.code_inline` | Inline code |
| `.strong` | Bold text |
| `.emphasis` | Italic text |
| `.delete` | Strikethrough |
| `.link` | Links |
| `.link_ref` | Link references |
| `.image` | Images |
| `.image_ref` | Image references |
| `.list` | List items |
| `.blockquote` | Blockquotes |
| `.table` | Table cells |
| `.table_align` | Table alignment |
| `.html`, `.<>` | HTML nodes |
| `.footnote` | Footnotes |
| `.footnote_ref` | Footnote references |
| `.definition` | Definitions |
| `.math` | Math blocks |
| `.math_inline`, `.inline_math` | Inline math |
| `.horizontal_rule` | Horizontal rules |
| `.break` | Line breaks |
| `.yaml` | YAML front matter |
| `.toml` | TOML front matter |

MDX selectors: `.mdx_flow_expression`, `.mdx_js_esm`, `.mdx_jsx_flow_element`, `.mdx_jsx_text_element`, `.mdx_text_expression`

### Attribute Access

| Node | Attributes |
|------|------------|
| `.h` | `.depth` / `.level` (1-6), `.value` (text) |
| `.code` | `.lang` / `.language`, `.value` (content), `.meta`, `.fence` |
| `.link` | `.url`, `.title`, `.value` (text) |
| `.image` | `.url`, `.alt`, `.title` |
| `.list` | `.index`, `.level` (nesting), `.ordered` (bool), `.checked` (bool), `.value` |
| `.[row][col]` | `.row`, `.column`, `.value`, `.last_cell_in_row`, `.last_cell_of_in_table` |
| `.link_ref` | `.ident`, `.label` |
| `.image_ref` | `.ident`, `.label`, `.alt` |
| `.footnote` | `.ident`, `.text` |
| `.footnote_ref` | `.ident`, `.label` |
| `.definition` | `.ident`, `.url`, `.title`, `.label` |
| `.text`, `.html`, `.yaml`, `.toml`, `.math` | `.value` |

### Setting Attributes

```mq
.code | set_attr("lang", "python")
.link | set_attr("url", "https://new-url.com")
.h | set_attr("level", 2)
```

### The `nodes` Keyword

`nodes` returns ALL markdown nodes as a flat array:

```mq
nodes                              # all nodes
nodes | len()                      # count nodes
nodes | filter(fn(x): is_h(x);)   # all headings
nodes | select(.code)              # code blocks only
```

## Operators

### Pipe (`|`)
Chains expressions — output of left becomes input of right:
```mq
.h2 | to_text | upcase
```

### Arithmetic
`+`, `-`, `*`, `/`, `%` (modulo), `//` (floor division)

### Comparison
`==`, `!=`, `>`, `>=`, `<`, `<=`

### Logical
`&&`, `||`, `!`

### Regex Match (`=~`)
```mq
"hello world" =~ "hello"    # true
```

### Shift (`<<`, `>>`)

Context-dependent:

| Type | `<<` | `>>` |
|------|------|------|
| Number | Bitwise left shift | Bitwise right shift |
| String | Remove chars from start | Remove chars from end |
| Array | Append to end | Add to beginning |
| Heading | Decrease depth (promote) | Increase depth (demote) |

### Conversion (`@`)

Convert values between types:

```mq
"Hello" @ :h1              # => # Hello
"Important" @ ">"          # => > Important
"mq" @ "https://mqlang.org"  # => [mq](https://mqlang.org)
node @ :html               # => HTML string
node @ :text               # => plain text
"data" @ :base64           # => base64 encoded
```

| Target | Produces |
|--------|----------|
| `:h1` through `:h6` | Heading at level |
| `:html` | HTML string |
| `:text` | Plain text |
| `:sh` | Shell-escaped |
| `:base64` | Base64 encoded |
| `:uri` | URL-encoded |
| `">"` | Blockquote |
| `"-"` | List item |
| `"~~"` | Strikethrough |
| `"**"` | Bold |
| `"--"` | Horizontal rule |
| `"<url>"` | Link with URL |

### Range (`..`)
```mq
1..5          # [1, 2, 3, 4, 5]
'a'..'e'      # ["a", "b", "c", "d", "e"]
```

### Update (`|=`)
```mq
.code.value |= "new content"
```

### Error Suppression (`?`)
```mq
get("missing")?    # None instead of error
```

## Data Types

| Type | Examples |
|------|----------|
| Number | `42`, `3.14`, `-10` |
| String | `"hello"`, `"\u{1F600}"` |
| Symbol | `:value`, `:ok`, `:error` |
| Boolean | `true`, `false` |
| Array | `[1, 2, 3]`, `array(1, 2, 3)` |
| Dict | `{"key": "val"}`, `dict(["key", "val"])` |
| Function | `fn(x): x + 1;` |
| None | `None` |

### Array Access
```mq
arr[0]        # first element
arr[1:4]      # slice (end exclusive)
```

### Dict Access
```mq
d["name"]     # key access
get(d, "name") # function form
```

## Variables

```mq
let x = 42                    # immutable
var counter = 0               # mutable
counter = counter + 1         # reassign

s"Hello, ${name}!"            # string interpolation (s"..." prefix)
```

### Built-in Variables

| Variable | Description |
|----------|-------------|
| `__FILE__` | Path to current file |
| `__FILE_NAME__` | Filename without path |
| `__FILE_STEM__` | Filename without extension |
| `$ENV_VAR` | Shell environment variable |

## Control Flow

### If/Elif/Else
```mq
if (condition): expr1
elif (condition2): expr2
else: expr3
```

### While
```mq
var x = 5 |
while (x > 0):
  x = x - 1 | x
end
```

### Foreach
```mq
foreach (item, items):
  item * 2
end
```

### Loop (infinite with break/continue)
```mq
var x = 0 |
loop:
  x = x + 1 |
  if (x > 5): break: x
end
```

### Pattern Matching
```mq
match (value):
  | 1: "one"
  | x if (x > 0): "positive"
  | [head, ..tail]: head
  | {name, age}: s"${name} is ${age}"
  | _: "default"
end
```

### Try-Catch
```mq
try: expression catch: fallback
```

## Functions

### User-Defined
```mq
def double(x): mul(x, 2);
def greet(name, greeting="Hello"): greeting + " " + name;
```

### Anonymous
```mq
fn(x): x + 1;
nodes | map(fn(x): to_text(x);)
```

## Built-in Functions (200+)

### String Functions

| Function | Description |
|----------|-------------|
| `upcase(s)` | Uppercase |
| `downcase(s)` | Lowercase |
| `trim(s)` | Trim whitespace |
| `ltrim(s)`, `rtrim(s)` | Trim left/right |
| `split(s, sep)` | Split string |
| `join(arr, sep)` | Join array |
| `replace(s, from, to)` | Replace substring |
| `gsub(s, regex, to)` | Replace regex matches |
| `contains(s, sub)` | Check substring |
| `starts_with(s, prefix)` | Check prefix |
| `ends_with(s, suffix)` | Check suffix |
| `index(s, sub)` | Find first occurrence |
| `len(s)` | Length |
| `slice(s, start, end)` | Extract substring |
| `repeat(s, n)` | Repeat string |
| `lpad(s, n, pad)` | Left-pad |
| `rpad(s, n, pad)` | Right-pad |
| `ltrimstr(s, prefix)` | Remove prefix |
| `rtrimstr(s, suffix)` | Remove suffix |
| `explode(s)` | Split to char array |
| `implode(arr)` | Join chars to string |

### Regex Functions

| Function | Description |
|----------|-------------|
| `test(s, pattern)` | Test if regex matches |
| `regex_match(s, pattern)` | Find all matches |
| `capture(s, pattern)` | Named group capture as dict |
| `is_regex_match(s, pattern)` | Test match (alias) |

### Array Functions

| Function | Description |
|----------|-------------|
| `map(arr, f)` | Transform each element |
| `filter(arr, f)` | Keep matching elements |
| `reject(arr, f)` | Remove matching elements |
| `flat_map(arr, f)` | Map and flatten |
| `compact(arr)` | Remove None values |
| `compact_map(arr, f)` | Map then remove Nones |
| `fold(arr, init, f)` | Reduce to single value |
| `sort(arr)` | Sort |
| `sort_by(arr, f)` | Sort by key function |
| `reverse(arr)` | Reverse |
| `flatten(arr)` | Flatten nested arrays |
| `uniq(arr)` | Remove duplicates |
| `unique_by(arr, f)` | Deduplicate by key |
| `first(arr)` | First element |
| `last(arr)` | Last element |
| `take(arr, n)` | Take first n |
| `skip(arr, n)` | Skip first n |
| `take_while(arr, f)` | Take while predicate true |
| `skip_while(arr, f)` | Skip while predicate true |
| `find_index(arr, f)` | First matching index |
| `group_by(arr, f)` | Group by key function |
| `partition(arr, f)` | Split into [matching, rest] |
| `any(arr, f)` | True if any match |
| `all(arr, f)` | True if all match |
| `in(arr, elem)` | Check membership |
| `count_by(arr, f)` | Count matching elements |
| `sum_by(arr, f)` | Sum after transform |
| `transpose(matrix)` | Transpose 2D array |
| `fill(val, n)` | Array of n copies |
| `range(start, end, step)` | Create range |
| `select(v, f)` | Return value if condition true, else None |
| `del(arr, i)` | Delete at index |
| `insert(arr, i, val)` | Insert at index |

### Dict Functions

| Function | Description |
|----------|-------------|
| `keys(d)` | Array of keys |
| `values(d)` | Array of values |
| `entries(d)` | Array of [key, value] pairs |
| `get(d, key)` | Get value |
| `set(d, key, val)` | Set value |
| `get_or(d, key, default)` | Get with default |

### Markdown Creation Functions

| Function | Description |
|----------|-------------|
| `to_h(text, depth)` | Create heading |
| `to_code(text, lang)` | Create code block |
| `to_code_inline(text)` | Create inline code |
| `to_link(url, text, title)` | Create link |
| `to_image(url, alt, title)` | Create image |
| `to_em(text)` | Create emphasis |
| `to_strong(text)` | Create strong/bold |
| `to_hr()` | Create horizontal rule |
| `to_md_list(text, indent)` | Create list item |
| `to_md_text(text)` | Create text node |
| `to_md_table_cell(val, row, col)` | Create table cell |
| `to_md_table_row(cells)` | Create table row |
| `to_math(text)` | Create math block |
| `to_math_inline(text)` | Create inline math |

### Markdown Inspection/Manipulation

| Function | Description |
|----------|-------------|
| `to_text(node)` | Extract plain text |
| `to_html(node)` | Convert to HTML |
| `to_markdown_string(node)` | Convert to markdown string |
| `to_md_name(node)` | Node type name |
| `attr(node, name)` | Get attribute value |
| `set_attr(node, name, val)` | Set attribute value |
| `get_title(node)` | Get title |
| `get_url(node)` | Get URL |
| `increase_header_level(node)` | Demote heading (h1 -> h2) |
| `decrease_header_level(node)` | Promote heading (h2 -> h1) |
| `set_code_block_lang(node, lang)` | Set code language |
| `set_list_ordered(node, bool)` | Set list ordering |
| `set_check(node, bool)` | Set checkbox state |
| `load_markdown(path)` | Load and parse markdown file |
| `to_markdown(str)` | Parse markdown string to nodes |

### Type Checking

| Function | Description |
|----------|-------------|
| `type(v)` | Type as string |
| `is_string(v)`, `is_number(v)`, `is_array(v)`, `is_dict(v)`, `is_bool(v)`, `is_none(v)` | Primitive type checks |
| `is_markdown(v)` | Markdown node check |
| `is_empty(v)` | Empty check (string/array/dict) |
| `is_h(v)`, `is_h1(v)` through `is_h6(v)`, `is_h_level(v, n)` | Heading checks |
| `is_code(v)`, `is_text(v)`, `is_list(v)`, `is_em(v)`, `is_html(v)` | Element checks |
| `is_yaml(v)`, `is_toml(v)`, `is_table_cell(v)`, `is_table_align(v)` | More element checks |

### Conversion

| Function | Description |
|----------|-------------|
| `to_number(v)` | Convert to number |
| `to_string(v)` | Convert to string |
| `to_array(v)` | Convert to array |
| `now()` | Current timestamp |
| `from_date(str)` | Date string to timestamp |
| `to_date(ts, fmt)` | Timestamp to date string |

### I/O

| Function | Description |
|----------|-------------|
| `print(msg)` | Print to stdout, return current value |
| `stderr(msg)` | Print to stderr, return current value |
| `input()` | Read line from stdin |
| `read_file(path)` | Read file contents |
| `debug(args...)` | Print debug info to stderr |
| `inspect(v)` | Print and return value |

### Encoding

| Function | Description |
|----------|-------------|
| `base64(s)` | Base64 encode |
| `base64d(s)` | Base64 decode |
| `url_encode(s)` | URL encode |

### Math

| Function | Description |
|----------|-------------|
| `abs(n)` | Absolute value |
| `ceil(n)` | Round up |
| `floor(n)` | Round down |
| `round(n)` | Round to nearest |
| `pow(base, exp)` | Exponentiation |
| `min(a, b)` | Minimum |
| `max(a, b)` | Maximum |

### Utility

| Function | Description |
|----------|-------------|
| `identity(x)` | Return unchanged |
| `coalesce(a, b)` | First non-None |
| `between(v, min, max)` | Inclusive range check |
| `error(msg)` | Raise error |
| `halt(code)` | Exit with code |
| `assert(a, b)` | Assert equality |

## Module System

### Define
```mq
module math:
  def add(a, b): a + b;
end
```

### Import (qualified access)
```mq
import "math"
math::add(5, 3)
```

### Include (direct access)
```mq
include "math"
add(5, 3)
```

### Search Paths
- `$HOME/.mq/`
- `$ORIGIN/../lib/mq/`
- `$ORIGIN/../lib/`
- `$ORIGIN/`

## Section Module Reference

Requires `include "section"` and `section.mq` in `~/.mq/`.

Usage pattern: `include "section" | nodes | sections(.) | ...`

| Function | Description |
|----------|-------------|
| `sections(nodes)` | Split nodes into sections by headers |
| `section(nodes, pattern)` | Sections whose title contains pattern |
| `split(nodes, level)` | Split at specified header level |
| `title(section)` | Section title text |
| `titles(sections)` | Extract all titles |
| `level(section)` | Header level (1-6) |
| `content(section)` | Section content (without header) |
| `all_nodes(section)` | Header + content |
| `has_content(section)` | Check if section has content |
| `nth(sections, n)` | nth section (0-indexed) |
| `toc(sections)` | Generate table of contents |
| `filter_sections(nodes, pred)` | Filter sections by predicate |
| `map_sections(nodes, mapper)` | Map over sections |
| `title_contains(sections, text)` | Filter by title text |
| `title_match(sections, pattern)` | Filter by title regex |
| `flatten(sections)` | Flatten back to markdown nodes |

**Known issues** (v0.5.16): `section()` and `title_contains()` can hit recursion limits. Use `sections()` + manual filtering as a workaround.

## Table Module Reference

Requires `include "table"` and `table.mq` in `~/.mq/`.

Usage pattern: `include "table" | nodes | tables(.) | ...`

| Function | Description |
|----------|-------------|
| `tables(nodes)` | Extract table structures |
| `add_row(table, row)` | Add row |
| `add_column(table, col)` | Add column |
| `remove_row(table, idx)` | Remove row by index |
| `remove_column(table, idx)` | Remove column by index |
| `filter_rows(table, pred)` | Filter rows |
| `filter_tables(tables, pred)` | Filter tables |
| `map_rows(table, f)` | Map over rows |
| `sort_rows(table, col_idx)` | Sort by column |
| `set_align(table, align)` | Set alignment |
| `to_csv(table, delim)` | Export as CSV |
| `to_markdown(table)` | Convert back to markdown |

## Common Patterns

### Extract all headings as text
```mq
.h | to_text
```

### Filter code blocks by language
```mq
nodes | filter(fn(x): and(is_code(x), eq(attr(x, "lang"), "python"));)
```

### Generate table of contents
```mq
include "section" | nodes | sections(.) | toc(.)
```

### Extract all URLs
```mq
.link | get_url
```

### Count nodes by type
```mq
nodes | group_by(fn(x): to_md_name(x);) | map(fn(g): [first(g), len(g)];)
```

### Merge files with separator
```bash
mq -S '"---"' '.' *.md
```

### Process front matter as data
```bash
mq --yaml '.yaml | to_text | yaml_parse' doc.md
```

### Promote all headings
```mq
.h | decrease_header_level
```
