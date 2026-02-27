# Gotchas and FAQ

Common mistakes, tricky behaviors, and frequently asked questions about jq.

---

## Common Mistakes

### `//` is alternative, not logical OR

```jq
# WRONG: trying to use // as logical OR
.a // .b                     # returns .a if non-null/false, else .b

# RIGHT: use `or` for logical OR
.a or .b                    # logical OR of two booleans

# Both null AND false trigger //
null // "default"            # "default"
false // "default"           # "default"
0 // "default"               # 0 (0 is truthy in jq!)
"" // "default"              # "" (empty string is truthy!)
```

### `--arg` always passes strings

```bash
# WRONG: --arg passes a string, not a number
jq --arg n 42 'select(.count > $n)' data.json    # string comparison!

# RIGHT: use --argjson for non-string values
jq --argjson n 42 'select(.count > $n)' data.json

# RIGHT: or convert inside the filter
jq --arg n 42 'select(.count > ($n | tonumber))' data.json
```

### `empty` is not `null`

```jq
# empty produces zero outputs — it backtracks
[1, empty, 2]                # [1, 2] — empty is skipped entirely

# null is a value that exists
[1, null, 2]                 # [1, null, 2]

# This matters in conditionals
if .x then .y else empty end   # produces nothing when .x is falsey
if .x then .y else null end    # produces null when .x is falsey

# And in reductions
[.[] | select(. > 3)]          # select uses empty to filter
```

### `.foo` on null silently returns null

```jq
# No error — just returns null
null | .foo                    # null
null | .foo.bar.baz            # null

# For strict access, check explicitly
if . == null then error("null input") else .foo end

# Or use ? with // for defaults
.foo? // error("missing foo")
```

### `if` requires `end`

```jq
# WRONG — missing end
if .x then .y                  # parse error

# RIGHT
if .x then .y end             # else defaults to identity (.)
if .x then .y else .z end     # explicit else
```

### String interpolation uses `\(expr)`, not `${expr}`

```jq
# WRONG (bash syntax)
"Hello ${.name}"               # literal string, no interpolation

# RIGHT (jq syntax)
"Hello \(.name)"               # interpolates .name
"Count: \(.items | length)"   # expressions work inside \()
```

### Semicolons separate function arguments, not commas

```jq
# WRONG
range(0, 10)                   # generates TWO separate values: range(0) and range(10)

# RIGHT
range(0; 10)                   # generates 0 through 9

# This matters everywhere:
limit(3; .[]  )                # first 3 elements
reduce .[] as $x (0; . + $x)  # sum
```

### `group_by` pre-sorts the groups

```jq
# group_by sorts groups by the key — this is by design
[{a:2},{a:1},{a:2}] | group_by(.a)
# [[{a:1}],[{a:2},{a:2}]] — groups sorted by .a

# If order matters, note that elements within groups keep original order
```

### Object keys are always strings

```jq
# Keys are coerced to strings
{(1): "one"}                  # {"1": "one"}

# INDEX keys are strings too
[{id: 1, name: "a"}] | INDEX(.id)
# {"1": {"id": 1, "name": "a"}} — key is "1", not 1

# Must convert when looking up numeric IDs
$idx[.user_id | tostring]    # convert number to string for lookup
```

### `=` vs `|=` semantics

```jq
# = : RHS sees original input
{"a": {"b": 10}, "b": 20} | .a = .b      # {"a": 20, "b": 20}

# |= : RHS sees value AT the path
{"a": {"b": 10}, "b": 20} | .a |= .b     # {"a": 10, "b": 20}

# With = , the RHS . refers to the whole input
# With |= , the RHS . refers to the current value at the LHS path
```

---

## Tricky Behaviors

### Multiple outputs from generators

```jq
# Comma produces multiple outputs — each flows independently through the pipeline
.name, .age | tostring
# Outputs: "Alice", "30" — two separate outputs

# To combine them, wrap in array first
[.name, .age] | join(", ")
# "Alice, 30" — single output
```

### `select` filters by backtracking

```jq
# select(f) outputs its input if f is truthy, otherwise produces empty
# This means select REMOVES non-matching elements from the stream

# To get a boolean instead:
if f then "match" else "no match" end
```

### Recursive descent `..` includes containers

```jq
# .. yields ALL values including arrays and objects
{"a": [1, 2]} | [.. | numbers]   # [1, 2]
{"a": [1, 2]} | [..]             # [{"a":[1,2]}, [1,2], 1, 2]
```

### `add` on empty array is null

```jq
[] | add            # null (not 0, not "", not [])
[] | add // 0       # 0 — use // for defaults
```

### `first` and `last` with empty

```jq
first(empty)        # produces empty (not an error in jq 1.7+)
last(empty)         # produces empty
```

### `limit` and `first` with side effects

```jq
# limit stops generating but doesn't undo prior side effects
# In practice this rarely matters since jq is functional
```

---

## FAQ

### How to process multiple JSON values in one file?

```bash
# Each line is a separate JSON value (NDJSON)
jq '.field' multi.json         # processes each value separately

# Collect all into an array
jq -s '.' multi.json           # slurp into array

# Using inputs (more explicit)
jq -n '[inputs]' multi.json
```

### How to find a value at any depth?

```jq
# Recursive descent with optional access
[.. | .target_key? // empty]

# With path information
[paths(scalars) as $p | select(getpath($p) == "target") | $p]
```

### How to modify a deeply nested value?

```jq
# Use path-based update
.data.users[0].address.city = "New York"

# Conditional deep update
(.data.users[] | select(.name == "Alice") | .address.city) = "New York"

# walk for blanket transformations
walk(if type == "string" then ascii_downcase else . end)
```

### How to merge/combine two JSON files?

```bash
# Shallow merge (right wins)
jq -s '.[0] * .[1]' base.json override.json

# Deep recursive merge
jq -s '.[0] * .[1]' base.json override.json  # * does recursive merge for objects

# Concatenate arrays
jq -s '.[0] + .[1]' list1.json list2.json
```

### How to use jq in a shell pipeline safely?

```bash
# Use @sh for shell-safe output
eval "$(jq -r '@sh "NAME=\(.name) AGE=\(.age)"' data.json)"

# Use --raw-output0 with xargs -0 for filenames
jq -r '.files[]' --raw-output0 manifest.json | xargs -0 ls -la

# Use -e for exit status in conditionals
if jq -e '.enabled' config.json > /dev/null 2>&1; then
    echo "Feature is enabled"
fi
```

### How to handle missing or optional fields?

```jq
# Alternative operator for defaults
.config.timeout // 30

# Optional object access
.metadata?.labels?.app // "unknown"

# try for error-prone operations
try (.value | tonumber) catch 0

# ?// for both errors and empty
.items[0] ?// {name: "default"}
```

### How to compare two JSON structures?

```bash
# Sort keys and diff
diff <(jq -S '.' a.json) <(jq -S '.' b.json)

# Find keys in A not in B
jq -n --slurpfile a a.json --slurpfile b b.json '
  ($a[0] | keys) - ($b[0] | keys)'
```

### How to pretty-print JSON?

```bash
# Default behavior — pretty prints with 2-space indent
jq '.' data.json

# With tabs
jq --tab '.' data.json

# With custom indent
jq --indent 4 '.' data.json

# Compact (one line)
jq -c '.' data.json
```

### What's the XPath `//` equivalent?

```jq
# XPath // selects nodes at any depth
# jq equivalent: recursive descent (..)
.. | .book? // empty           # all "book" values at any depth

# Find paths to specific keys
path(.. | .book? // empty)     # paths to "book" fields
```

### How to debug a jq expression?

```jq
# Insert debug to see intermediate values (output goes to stderr)
.users | debug | map(.name)

# debug with message
.users | debug("after filtering") | map(.name)

# Step-by-step debugging: break pipeline apart
.users | length | debug         # see count
```
