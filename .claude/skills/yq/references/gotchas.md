# Gotchas

Common mistakes, tricky behaviors, and jq-vs-yq differences.

## Common Mistakes

### 1. Missing parentheses on filtered updates

```bash
# WRONG: silently does nothing
.items[] | select(.name == "x") | .value = "new"

# RIGHT: wrap entire LHS in parentheses
(.items[] | select(.name == "x") | .value) = "new"
```

The outer parentheses tell yq to update within the original document context.

### 2. Using `eval` instead of `eval-all` for multi-file operations

```bash
# WRONG: processes each file independently, fileIndex is always 0
yq eval 'select(fi == 0) * select(fi == 1)' a.yaml b.yaml

# RIGHT: loads all files for cross-file operations
yq eval-all 'select(fi == 0) * select(fi == 1)' a.yaml b.yaml
```

### 3. Arrays replaced instead of appended during merge

```bash
# Default: arrays are replaced
yq '.a * .b'  # if both have arrays, b's array wins

# Append arrays:
yq '.a *+ .b'

# Deep merge arrays by position:
yq '.a *d .b'
```

### 4. XML values are all strings

After parsing XML, all values are `!!str`. Numbers and booleans need explicit conversion:

```bash
# Parse XML and auto-convert types
yq -p=xml '(.. | select(tag == "!!str")) |= from_yaml' file.xml
```

### 5. `//` is alternative, not regex or logical OR

```bash
.a // "default"     # returns .a if truthy, else "default"
.a or .b            # logical OR (returns boolean)
```

`//` treats both `null` and `false` as falsy (returns RHS).

### 6. Using `reduce` instead of `ireduce`

```bash
# WRONG: yq does not support prefix reduce
reduce .[] as $x (0; . + $x)

# RIGHT: yq uses infix reduce
.[] as $x ireduce (0; . + $x)
```

### 7. Forgetting `-i` modifies only the first file

```bash
yq -i 'expression' a.yaml b.yaml    # only a.yaml is modified
```

### 8. Comments removed by `..` instead of `...`

```bash
# WRONG: misses comments on map keys
.. comments = ""

# RIGHT: includes key nodes
... comments = ""
```

`..` traverses value nodes only. `...` includes map key nodes where line comments live.

### 9. `env()` auto-types, `strenv()` keeps string

```bash
myvar="true" yq '.a = env(myvar)'    # a: true  (boolean!)
myvar="true" yq '.a = strenv(myvar)' # a: "true" (string)

myvar="42" yq '.a = env(myvar)'      # a: 42    (integer!)
myvar="42" yq '.a = strenv(myvar)'   # a: "42"  (string)
```

Use `strenv()` when the value should remain a string regardless of content.

### 10. Anchors not considered by sort_keys

`sort_keys` may produce invalid YAML if the document uses merge anchors, because it can reorder keys without respecting anchor dependencies.

## Tricky Behaviors

### Truthiness

Only `null` and `false` are falsy. These are all **truthy**:
- `0` (zero)
- `""` (empty string)
- `[]` (empty array)
- `{}` (empty object)

This differs from many languages where `0` and `""` are falsy.

### YAML 1.2: "yes"/"no" are strings

In YAML 1.2 (which yq follows), `yes`, `no`, `on`, `off` are **strings**, not booleans. Only `true` and `false` are booleans.

### Custom tags preserved on assignment

```bash
# Input: {a: !horse meow, b: !dog woof}
yq '.a = .b'
# Output: {a: !horse woof, b: !dog woof}
# Note: LHS tag (!horse) preserved, only value changes
```

Use `=c` to clobber tags: `yq '.a =c .b'` produces `{a: !dog woof, b: !dog woof}`.

### Merge keeps LHS style

When merging objects, the result uses the LHS node's style (flow vs block).

### Null merging returns non-null

```bash
null * {"a": 1}     # returns {a: 1}
{"a": 1} * null     # returns {a: 1}
```

### Auto-created paths

Assignment to non-existent paths creates the intermediate structure:

```bash
# Input: {}
yq '.a.b.c = "deep"'
# Output: a: {b: {c: deep}}

# Input: {}
yq '.a[0] |= "first"'
# Output: a: [first]
```

### Document separators in output

yq outputs `---` between documents. Suppress with `-N`:

```bash
yq -N '.key' file.yaml
```

### Anchor preservation on update

```bash
# Input: a: &cool cat
yq '.a = "dog"'
# Output: a: &cool dog    (anchor preserved)
```

## jq vs mikefarah/yq Differences

This table helps agents avoid transplanting jq patterns into yq.

| Feature | jq | mikefarah/yq |
|---------|-----|-------------|
| Multi-file | `--slurpfile`, `inputs` | `eval-all`, `fileIndex` |
| Reduce | `reduce .[] as $x (init; body)` | `.[] as $x ireduce (init; body)` |
| Type coercion | `tonumber`, `tostring` | `tag = "!!int"`, `tag = "!!str"`, `to_number` |
| Default value | `// "default"` | `// "default"` (same) |
| String format | `@csv`, `@tsv`, `@html` | `@csv`, `@tsv` (no `@html`) |
| If/else | `if cond then a else b end` | Use `with(select(cond); a)` pattern |
| Define functions | `def name(args): body;` | Not supported |
| Modules | `import`, `include` | Not supported |
| Streaming | `--stream`, `tostream` | Not applicable (YAML has different streaming model) |
| Object construction | `{name, email}` | `{name, email}` (same for simple cases) |
| `limit(n; expr)` | Supported | Not supported |
| `foreach` | Supported | Not supported |
| `gsub` | Supported (global sub) | Use `sub` with global behavior |
| `ascii_downcase` | Supported | Use `downcase` (unicode-aware) |
| `ascii_upcase` | Supported | Use `upcase` (unicode-aware) |
| `ltrimstr`/`rtrimstr` | Supported | Use `sub` with regex |
| `startswith`/`endswith` | Supported | Use `test` with regex anchors |
| `indices`/`index` | Supported | Not directly supported |
| `input_filename` | Supported | Use `filename` |
| `$__loc__` | Source location | Not supported |
| `env`/`$ENV` | `$ENV.VAR`, `env.VAR` | `env(VAR)`, `strenv(VAR)` |
| Error handling | `try-catch` | `?` optional operator |
| Comments | Not applicable (JSON) | `line_comment`, `head_comment`, `foot_comment` |
| Anchors/Aliases | Not applicable (JSON) | `anchor`, `alias`, `explode()` |
| Styles | Not applicable (JSON) | `style = "flow"`, `style = "double"`, etc. |
| Multi-document | Not applicable (JSON) | `documentIndex` / `di`, `---` separators |
| Merge operator | `*` (multiply only) | `*` (deep merge objects), `*+`, `*d`, `*?`, `*n` |
| Scoped updates | Not directly available | `with(path; updates)` |

## FAQ

### How do I merge arrays by a key field (like SQL JOIN)?

yq merges arrays by position (`*d`) or replaces them (`*`). For key-based merging, use a complex ireduce pattern:

```bash
yq eval-all '
  (((.arr1 + .arr2) | .[] | {(.id): .}) as $item
    ireduce ({}; . * $item)) as $merged |
  ($merged | to_entries | .[]) as $item
    ireduce ([]; . + $item.value)
' file1.yml file2.yml
```

### How do I do conditional updates without if/else?

Use `with` + `select`:

```bash
yq '.[] |= (
  with(select(.type == "a"); .val = 1) |
  with(select(.type == "b"); .val = 2)
)' file.yaml
```

### Why does my update silently do nothing?

Most likely missing parentheses around the LHS of a filtered assignment:

```bash
# Wrong:  .items[] | select(.x) | .y = "z"
# Right: (.items[] | select(.x) | .y) = "z"
```

### How do I handle Windows/PowerShell quoting?

Use double-double-quotes for embedded quotes:

```powershell
yq -n '.test = ""something""'
```

### How do I validate YAML?

```bash
yq --exit-status 'tag == "!!map" or tag == "!!seq"' file.yaml > /dev/null
```

Exit code 1 means invalid or non-structured YAML.
