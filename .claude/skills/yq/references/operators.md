# Operators

Complete operator reference for mikefarah/yq v4, grouped by category.

## Navigation & Traversal

### Traverse (Dot Notation)

```bash
.a              # map key access
.a.b.c          # nested access
.[]             # splat: iterate all children (array elements or map values)
.[]?            # optional splat: no error on scalar
.[0]            # array index
.[1][0]         # nested array index
.[0, 2]         # multiple indices
.["key.dots"]   # dots in key names
.["red rabbit"] # spaces in key names
.["{}"]         # special characters in keys
.[.b]           # dynamic key: use value of .b as lookup key
.a."*a*"        # wildcard key matching
.a?             # optional: suppress type-mismatch errors
```

Non-existent paths return `null` without error.

### Recursive Descent

```bash
..              # all value nodes recursively (excludes map keys)
...             # all nodes including map keys
[.. | select(has("name"))]   # find nodes with specific keys
.. | select(. == "frog")     # find nodes with specific values
```

Aliases and merge docs (`<<:`) are NOT traversed by `..` or `...`.

### Path

```bash
.a.b | path                     # returns ["a", "b"]
.a.b | path | .[-1]             # returns "b" (last element)
setpath(["a", "b"]; "value")    # set value at path
setpath(["a", 0]; "thing")      # set array element at path
delpaths([["a", 0]])            # delete by path (must be array of path arrays)
```

### Keys

```bash
keys                            # map keys or array indices as array
.a | key                        # individual key name
(.a.x | key) = "meow"           # rename key
is_key                          # boolean: is this a key node?
```

## Selection & Filtering

### Select

```bash
.[] | select(. == "cat")           # exact match
.[] | select(. == "*at")           # wildcard suffix
.[] | select(. == "go*")           # wildcard prefix
.[] | select(. == "*go*")          # wildcard both
.[] | select(test("[a-z]+_[0-9]$"))  # regex
.[] | select(. > 3)                # comparison
.[] | select(.name == "foo" and .age > 10)  # compound
```

### Has & Contains

```bash
has("key")                        # map key existence (true even for null values)
has(1)                            # array index existence
contains(["bar", "baz"])          # array containment (substring matching)
contains("bar")                   # string containment
contains({"bar": [{"x": 12}]})   # object containment
```

### With Entries (filter by key)

```bash
with_entries(select(.key | test("ame$")))     # keep keys matching regex
with_entries(select(.value != null))           # remove null values
```

## Assignment & Update

### Plain Assign (`=`)

RHS evaluates against root context. Returns updated document.

```bash
.a.b = "cat"                    # set nested field
(.a, .c) = "potato"             # set multiple paths
(.arr[] | select(.name == "x") | .val) = "new"  # filtered (parens required)
```

### Relative Assign (`|=`)

RHS evaluates with each LHS node as context.

```bash
.a |= .b                       # set .a to value of .a.b
.[] |= . * 2                   # double all array elements
.a.b |= "bogs"                 # also creates path if missing
```

### Other Assignment Operators

```bash
.a += .b                       # add/append
.a -= 1                        # subtract
.a *= .b                       # multiply/merge
```

### Clobber Flag (`=c`)

Normally, LHS custom tag is preserved on assignment. Use `=c` to clobber with RHS tag:

```bash
.a =c .b     # copies value AND tag from .b
.a *=c .b    # merge, clobber tags
```

## Multiply & Merge

### Arithmetic

```bash
3 * 4                           # 12
"banana" * 4                    # bananabananabananabanana
```

### Object Merge (deep by default)

```bash
.a * .b                         # deep merge: b overrides a
. * {"a": .b}                   # merge inline object
. *= load("other.yml")          # merge from file
```

### Merge Flags

| Flag | Effect |
|------|--------|
| `+` | Append arrays instead of replacing |
| `d` | Deeply merge arrays by index position |
| `?` | Only update existing fields (ignore new) |
| `n` | Only add new fields (don't override existing) |
| `c` | Clobber custom YAML tags |

Flags combine: `*+?`, `*dn`, etc.

```bash
.a *+ .b                       # merge, append arrays
.a *? .b                       # merge, existing fields only
.a *n .b                       # merge, new fields only
.a *d .b                       # merge, deep-merge arrays by position
.a *+? .b                      # merge, append arrays, existing only
```

### Multi-File Merge

```bash
# Merge two files
yq ea 'select(fi == 0) * select(fi == 1)' f1.yml f2.yml

# Merge all files (ireduce)
yq ea '. as $item ireduce ({}; . * $item)' *.yml
```

### Null Handling

Merging with `null` returns the non-null operand.

## Add & Subtract

### Add (`+`)

```bash
.a + .b                        # numbers: addition
"hello" + " world"             # strings: concatenation
[1, 2] + [3, 4]                # arrays: concatenation
.map1 + .map2                  # objects: shallow merge
.a += ["new"]                  # append to array
null + "cat"                   # returns "cat"
```

### Date Arithmetic

```bash
.date += "3h10m"               # add duration to RFC3339 date
```

### Subtract (`-`)

```bash
.a - .b                        # number subtraction
```

## Comparison & Boolean

### Comparison

```bash
.a == .b                       # equals
.a != .b                       # not equals
.a > .b                        # greater than
.a >= .b                       # greater or equal
.a < .b                        # less than
.a <= .b                       # less or equal
```

### Boolean

```bash
.a and .b                      # logical AND
.a or .b                       # logical OR
.a | not                       # logical NOT
any                            # any element truthy (array)
all                            # all elements truthy (array)
any_c(. > 3)                   # any element matches condition
all_c(. > 3)                   # all elements match condition
```

**Truthiness**: Only `null` and `false` are falsy. `0`, `""`, `[]`, `{}` are all truthy.

### Alternative (Default)

```bash
.a // "default"                # returns .a if truthy, else "default"
.a // .b                       # returns .a if truthy, else .b
(.a // (.a = 0)) += 1          # initialize-and-increment
```

## String Operators

```bash
"text" | length                # character count
"text" | upcase                # TEXT (unicode-aware)
"text" | downcase              # text (unicode-aware)
"text" | trim                  # strip whitespace
"a;b;c" | split(";")           # ["a", "b", "c"]
["a","b"] | join(",")          # "a,b"
. | to_string                  # convert to string representation
```

### Regex

```bash
test("pattern")                # boolean match
test("(?i)pattern")            # case-insensitive
match("pattern")               # returns {string, offset, length, captures}
[match("pat"; "g")]            # global match (all occurrences)
capture("(?P<name>[a-z]+)")    # named capture groups -> map
sub("old", "new")              # replace first match
sub("(a)", "${1}r")            # with capture backreferences
```

### String Interpolation

```bash
"I like \(.value) and \(.other)"   # interpolate expressions
```

## Array Operators

```bash
[.a, .b, .c]                  # collect into array
[]                             # empty array
.arr | length                  # element count
.arr | first                   # first element
.arr | last                    # last element
.arr | reverse                 # reverse order
.arr | flatten                 # recursive flatten
.arr | flatten(1)              # flatten one level
.arr | shuffle                 # randomize order
.arr | unique                  # deduplicate
.arr | unique_by(.name)        # deduplicate by field
.arr | sort                    # sort values
.arr | sort_by(.name)          # sort by field
.arr | sort_by(.a, .b)         # multi-field sort
.arr | group_by(.type)         # group into nested arrays
.arr | max                     # maximum value
.arr | min                     # minimum value
.arr | map(. * 2)              # transform elements
.arr | map_values(. + 1)       # transform values (preserves keys)
del(.arr[1])                   # delete by index
.arr[2:5]                      # slice
.arr |= sort_by(.name)         # sort in-place
```

**Null ordering**: nulls first, then false, true, numbers, strings. Sort is stable.

## Object Operators

```bash
keys                           # key names as array
values                         # values as array
to_entries                     # [{key: k, value: v}, ...]
from_entries                   # reverse of to_entries
with_entries(expr)             # shorthand: to_entries | expr | from_entries
pick(.a, .b)                   # keep only specified fields
omit(.a, .b)                   # remove specified fields
sort_keys(.)                   # sort map keys alphabetically
sort_keys(..)                  # recursive sort all map keys
```

## Delete

```bash
del(.b)                        # delete map key
del(.a.a1)                     # delete nested key
del(.[1])                      # delete array element
del(.[] | select(. == "*at"))  # conditional delete
del(.. | select(has("name")).name)  # recursive delete
```

Deleting non-existent paths produces no error.

## Reduce

```bash
# Syntax: <exp> as $var ireduce (<init>; <block>)
.[] as $x ireduce (0; . + $x)               # sum
.[] as $item ireduce ({}; . * $item)         # merge all
.[] as $i ireduce ({}; .[$i.name] = $i.val)  # array to map
```

**Important**: yq uses `ireduce` (infix), not `reduce`.

## Variable Operators

```bash
.a as $x | .b as $y | .b = $x | .a = $y     # swap values
.names as $n | .posts[] | $n[.author]        # lookup pattern
.a.b ref $x | $x = "new" | $x style="double" # reference binding
```

## With (Scoped Updates)

```bash
with(.a.deeply.nested; . = "new" | . style="single")
with(.a.deeply; .nested = "val1" | .other = "val2")
with(.arr[]; .b = .a + " yum")              # relative array updates
```

## Environment Variables

```bash
env(VAR)                       # parse as YAML (auto-types: int, bool, map, etc.)
strenv(VAR)                    # always string
envsubst                       # interpolate ${VAR} in strings
envsubst(nu)                   # fail if var not set
envsubst(ne)                   # fail if var empty
envsubst(nu, ff)               # fail fast on first missing
```

Default values: `${VAR-default}` in envsubst strings.

Recursive: `(.. | select(tag == "!!str")) |= envsubst`

Security: `--security-disable-env-ops` disables all env operations.

## Load

```bash
load("file.yml")               # load YAML file
load_xml("file.xml")           # load XML file
load_props("file.properties")  # load properties file
load_str("file.txt")           # load as plain string
load_base64("file.b64")        # load and decode base64
. *= load("other.yml")         # merge loaded file
```

Dynamic paths: `.x |= load("dir/" + .file)`

Security: `--security-disable-file-ops` disables all file operations.

## Encode & Decode

| Format | Encode | Decode |
|--------|--------|--------|
| YAML | `to_yaml` / `@yaml` | `from_yaml` / `@yamld` |
| JSON | `to_json` / `@json` | `from_json` / `@jsond` |
| Properties | `to_props` / `@props` | `from_props` / `@propsd` |
| CSV | `to_csv` / `@csv` | `from_csv` / `@csvd` |
| TSV | `to_tsv` / `@tsv` | `from_tsv` / `@tsvd` |
| XML | `to_xml` / `@xml` | `from_xml` / `@xmld` |
| Base64 | `@base64` | `@base64d` |
| URI | `@uri` | `@urid` |
| Shell | `@sh` | — |

## Type & Metadata

### Tag (Type)

```bash
.a | tag                       # read type (!!str, !!int, !!map, etc.)
.a tag = "!!int"               # convert to integer
.a tag = "!!str"               # convert to string
(.. | select(tag == "!!int")) tag = "!!str"  # bulk convert
```

Standard tags: `!!map`, `!!str`, `!!int`, `!!float`, `!!bool`, `!!seq`, `!!null`

### Kind

```bash
.a | kind                      # "scalar", "seq", or "map"
select(kind == "scalar")       # filter by node kind
```

### Length

```bash
. | length                     # maps: key count, arrays: element count, strings: char count, null: 0
```

### Parent, Column, Line

```bash
.a | parent                    # parent node
.a | line                      # line number in source
.a | column                    # column number in source
```

## Document & File Operators

```bash
documentIndex                  # current document index (alias: di)
select(di == 1)                # target second document
fileIndex                      # current file index (alias: fi)
select(fi == 0)                # target first file
filename                       # current file name
split_doc                      # split nodes into separate documents
```

## Evaluate Expression

The `yq` tool's expression evaluator can also evaluate dynamically constructed expressions:

```bash
# Dynamic path from env var
pathEnv=".a.b" yq 'strenv(pathEnv)' file.yaml
```

## Miscellaneous

```bash
pivot                          # transpose array of objects
array_to_map                   # convert numeric-indexed to map
```
