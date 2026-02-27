# YAML-Specific Features

Features specific to YAML processing: anchors, aliases, comments, multi-document, tags, and styles.

## Anchors & Aliases

### Read

```bash
.a | anchor                   # get anchor name: &billyBob -> "billyBob"
.a | alias                    # get alias target: *billyBob -> "billyBob"
```

### Set

```bash
.a anchor = "myanchor"        # a: cat -> a: &myanchor cat
.a alias = "targetname"       # set alias (target anchor must exist)
```

### Explode (Dereference)

Resolve all aliases to their actual values and remove anchor names:

```bash
explode(.)                    # dereference entire document
explode(.f)                   # dereference under .f only
```

Alias keys are dereferenced to their values:

```yaml
# Input:                    # After explode:
f:                          f:
  a: &a cat                   a: cat
  *a: b                       cat: b
```

### Dereference and Update

```bash
.thing |= (explode(.) | sort_keys(.)) * {"value": false}
```

### Merge Anchors (`<<:`)

yq retains support for the `<<:` merge key (removed in YAML 1.2). The parser auto-applies `!!merge` tag.

**Flag**: `--yaml-fix-merge-anchor-to-spec`

| Setting | Behavior |
|---------|----------|
| `false` (default) | Legacy: later merge anchors override; existing keys can be overridden |
| `true` | Spec-compliant: explicit keys take precedence; earlier merged keys win |

Will default to `true` in future versions.

**Example difference:**

```yaml
foo: &foo
  thing: foo_thing
  c: foo_c
bar: &bar
  thing: bar_thing
  c: bar_c
foobar:
  c: foobar_c
  <<: [*foo, *bar]
  thing: foobar_thing
```

- Legacy (`false`): `.foobar.c` = `foo_c` (merged override)
- Fixed (`true`): `.foobar.c` = `foobar_c` (explicit key wins)

## Comments

### Three Comment Types

- **line comment**: after value on same line (`# comment`)
- **head comment**: above the node
- **foot comment**: trailing after the node

**Important**: Line comments on maps/arrays are on the **key** node, not the value.

### Read Comments

```bash
.a | line_comment              # get line comment
. | head_comment               # get head comment
. | foot_comment               # get foot comment
```

### Set Comments

```bash
.a line_comment = "my note"    # set line comment
. head_comment = "file header" # set head comment
. foot_comment = .a            # set foot comment (dynamic value)
(.a | key) head_comment = "above key"  # comment on map key
```

### Relative Update

```bash
.. line_comment |= .           # set each node's comment to its own value
```

### Remove Comments

```bash
.a line_comment = ""           # remove specific comment
... comments = ""              # remove ALL comments (use ... to include key nodes)
```

**Important**: Use `...` (three dots, includes keys), not `..` (two dots, values only).

### Comment Discovery (Diagnostic)

```bash
yq '[... | {"p": path | join("."), "isKey": is_key, "hc": headComment, "lc": lineComment, "fc": footComment}]' file.yaml
```

## Multi-Document Files

YAML files can contain multiple documents separated by `---`.

### Target by Document Index

```bash
select(di == 0)                # first document
select(di == 1)                # second document
select(documentIndex == 2)     # third document (long form)
```

### Update Specific Document

```bash
yq '(select(di == 1) | .key) = "value"' multi.yaml
```

### Split Nodes into Documents

```bash
.items[] | split_doc            # each item becomes its own document
```

### Suppress Document Separators

```bash
yq -N '.key' file.yaml         # omit --- in output
```

## Tags (YAML Types)

### Read Tag

```bash
.a | tag                       # !!str, !!int, !!float, !!bool, !!map, !!seq, !!null
.a | type                      # alias for tag
```

### Set Tag (Type Coercion)

```bash
.a tag = "!!int"               # convert to integer
.a tag = "!!str"               # convert to string
.a tag = "!!float"             # convert to float
.a tag = "!!bool"              # convert to boolean
```

### Bulk Type Conversion

```bash
(.. | select(tag == "!!int")) tag = "!!str"           # all ints to strings
(.. | select(tag == "!!str")) |= from_yaml            # auto-detect types from strings
```

### Custom Tags

```bash
.a tag = "!!customtag"         # set custom tag
.a tag = "!!mytag"             # a: !mytag value
```

Custom tags are preserved during assignment by default. Use `=c` flag to clobber.

## Styles

YAML supports multiple representation styles for values.

### Available Styles

| Style | Example | Usage |
|-------|---------|-------|
| `""` (default) | `value` | Auto-determined by yq |
| `"double"` | `"value"` | Double-quoted string |
| `"single"` | `'value'` | Single-quoted string |
| `"literal"` | `\|` block | Preserves newlines |
| `"folded"` | `>` block | Folds newlines to spaces |
| `"flow"` | `{a: 1, b: 2}` | Inline/compact form |
| `"tagged"` | `!!str value` | Explicit type tag |

### Read Style

```bash
.a | style                     # get current style
.. | style                     # all value styles
```

### Set Style

```bash
.a style = "double"            # double-quote a value
.a style = "flow"              # inline/compact
.a style = ""                  # reset to default
.. style = "flow"              # all values to flow (compact)
... style = "flow"             # all nodes including keys
```

### Pretty Print

```bash
yq -P file.yaml                # reset all styles to default (idiomatic YAML)
```

Equivalent to `... style=""`.

## Kind

Node kind indicates the structural type:

```bash
.a | kind                      # "scalar", "seq", or "map"
select(kind == "scalar")       # filter scalars
select(kind == "seq")          # filter sequences (arrays)
select(kind == "map")          # filter mappings (objects)
```

### Use in Conditional Logic

```bash
.[] | (
  (select(kind == "scalar") | key + "=" + .),
  (select(kind == "seq") | key + "=(" + (map(.) | join(",")) + ")")
)
```

## Metadata Operators

```bash
.a | line                      # source line number
.a | column                    # source column number
.a | parent                    # parent node
filename                       # current file name
```
