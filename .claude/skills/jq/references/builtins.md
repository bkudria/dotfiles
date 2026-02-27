# Builtin Functions

Complete catalog of jq builtin functions organized by category.

---

## Self-Discovery

```jq
# List all builtins with their arities
builtins | sort                        # ["acos/0", "acosh/0", "add/0", ...]

# List all 2-arity builtins
builtins | map(select(endswith("/2"))) | sort

# Check if a builtin exists
builtins | map(select(startswith("INDEX")))

# Location information
$__loc__                               # {"file": "...", "line": N} — current source location
```

---

## String Functions

| Function | Description | Example |
|----------|-------------|---------|
| `length` | String length (Unicode codepoints) | `"café" \| length` → `4` |
| `utf8bytelength` | Byte length in UTF-8 | `"café" \| utf8bytelength` → `5` |
| `split(s)` | Split by separator | `"a,b,c" \| split(",")` → `["a","b","c"]` |
| `join(s)` | Join array with separator | `["a","b"] \| join(",")` → `"a,b"` |
| `ltrimstr(s)` | Remove prefix | `"hello" \| ltrimstr("he")` → `"llo"` |
| `rtrimstr(s)` | Remove suffix | `"hello" \| rtrimstr("lo")` → `"hel"` |
| `startswith(s)` | Test prefix | `"hello" \| startswith("he")` → `true` |
| `endswith(s)` | Test suffix | `"hello" \| endswith("lo")` → `true` |
| `ascii_downcase` | Lowercase | `"HELLO" \| ascii_downcase` → `"hello"` |
| `ascii_upcase` | Uppercase | `"hello" \| ascii_upcase` → `"HELLO"` |
| `trim` | Strip leading/trailing whitespace | `" hi " \| trim` → `"hi"` |
| `ltrim` | Strip leading whitespace | `" hi " \| ltrim` → `"hi "` |
| `rtrim` | Strip trailing whitespace | `" hi " \| rtrim` → `" hi"` |
| `explode` | String to codepoint array | `"AB" \| explode` → `[65,66]` |
| `implode` | Codepoint array to string | `[65,66] \| implode` → `"AB"` |
| `tostring` | Convert to string | `42 \| tostring` → `"42"` |
| `tonumber` | Convert to number | `"42" \| tonumber` → `42` |
| `tojson` | Serialize to JSON string | `{a:1} \| tojson` → `"{\"a\":1}"` |
| `fromjson` | Parse JSON string | `"{\"a\":1}" \| fromjson` → `{"a":1}` |

### Regex Functions

All regex functions use PCRE (Perl-compatible) syntax. Flags: `x` (extended), `g` (global), `i` (case-insensitive), `m` (multiline), `s` (single-line), `n` (no-capture).

```jq
# test — returns true/false
"Hello World" | test("hello"; "i")            # true

# match — returns match object
"Hello 42 World" | match("([0-9]+)")
# {"offset": 6, "length": 2, "string": "42", "captures": [{...}]}

# capture — returns named groups as object
"2024-01-15" | capture("(?<y>\\d{4})-(?<m>\\d{2})-(?<d>\\d{2})")
# {"y": "2024", "m": "01", "d": "15"}

# scan — find all matches (returns array of match strings)
"12 apples and 34 oranges" | [scan("[0-9]+")]  # ["12", "34"]

# sub — replace first match
"hello world" | sub("world"; "jq")             # "hello jq"

# gsub — replace all matches
"aabba" | gsub("a"; "x")                       # "xxbbx"

# splits — stream of strings split by regex
"a1b2c3" | [splits("[0-9]+")]                  # ["a", "b", "c", ""]

# With flags as second argument
"Hello" | test("hello"; "i")                   # true
"aAbBa" | gsub("a"; "x"; "i")                 # "xxbBx"
```

---

## Array Functions

| Function | Description | Example |
|----------|-------------|---------|
| `length` | Array length | `[1,2,3] \| length` → `3` |
| `map(f)` | Apply filter to each element | `[1,2,3] \| map(. * 2)` → `[2,4,6]` |
| `map_values(f)` | Apply filter to values (arrays and objects) | `{"a":1,"b":2} \| map_values(. + 10)` → `{"a":11,"b":12}` |
| `select(f)` | Keep elements where f is truthy | `[1,2,3,4] \| map(select(. > 2))` → `[3,4]` |
| `empty` | Produce zero outputs (backtrack) | `[1, empty, 2]` → `[1,2]` |
| `add` | Sum/concatenate all elements | `[1,2,3] \| add` → `6`, `["a","b"] \| add` → `"ab"` |
| `any` | True if any element is truthy | `[false, true] \| any` → `true` |
| `any(f)` | True if any element satisfies f | `[1,2,3] \| any(. > 2)` → `true` |
| `all` | True if all elements are truthy | `[true, true] \| all` → `true` |
| `all(f)` | True if all elements satisfy f | `[1,2,3] \| all(. > 0)` → `true` |
| `flatten` | Flatten nested arrays | `[[1,[2]],3] \| flatten` → `[1,2,3]` |
| `flatten(n)` | Flatten to depth n | `[[1,[2]],3] \| flatten(1)` → `[1,[2],3]` |
| `sort` | Sort elements | `[3,1,2] \| sort` → `[1,2,3]` |
| `sort_by(f)` | Sort by key function | `[{a:2},{a:1}] \| sort_by(.a)` → `[{a:1},{a:2}]` |
| `reverse` | Reverse array | `[1,2,3] \| reverse` → `[3,2,1]` |
| `group_by(f)` | Group elements (pre-sorts) | `[{a:1,b:2},{a:1,b:3}] \| group_by(.a)` → `[[{a:1,b:2},{a:1,b:3}]]` |
| `unique` | Remove duplicates (sorts) | `[2,1,2,3] \| unique` → `[1,2,3]` |
| `unique_by(f)` | Remove duplicates by key | `[{a:1,b:2},{a:1,b:3}] \| unique_by(.a)` → `[{a:1,b:2}]` |
| `min`, `max` | Minimum/maximum | `[3,1,2] \| min` → `1` |
| `min_by(f)`, `max_by(f)` | Min/max by key | `[{a:2},{a:1}] \| min_by(.a)` → `{a:1}` |
| `indices(x)` | All positions of x | `"abcabc" \| indices("bc")` → `[1,4]` |
| `index(x)` | First position of x | `"abcabc" \| index("bc")` → `1` |
| `rindex(x)` | Last position of x | `"abcabc" \| rindex("bc")` → `4` |
| `contains(x)` | Deep containment test | `[1,2,3] \| contains([2,3])` → `true` |
| `inside(x)` | Inverse of contains | `[2,3] \| inside([1,2,3])` → `true` |
| `limit(n; expr)` | Take first n outputs | `[limit(3; .[])]` with `[0..9]` → `[0,1,2]` |
| `skip(n; expr)` | Skip first n outputs | `[skip(3; .[])]` with `[0..9]` → `[3,4,5,6,7,8,9]` |
| `first(expr)` | First output of expr | `first(range(10))` → `0` |
| `last(expr)` | Last output of expr | `last(range(10))` → `9` |
| `nth(n; expr)` | Nth output of expr | `nth(5; range(10))` → `5` |
| `range(n)` | Generate 0 to n-1 | `[range(4)]` → `[0,1,2,3]` |
| `range(a;b)` | Generate a to b-1 | `[range(2;5)]` → `[2,3,4]` |
| `range(a;b;s)` | Generate with step | `[range(0;10;3)]` → `[0,3,6,9]` |
| `until(cond; update)` | Loop until condition | `0 \| until(. >= 5; . + 1)` → `5` |
| `while(cond; update)` | Loop while condition | `[1 \| while(. < 100; . * 2)]` → `[1,2,4,8,16,32,64]` |
| `repeat(f)` | Repeat f forever | `1 \| [limit(5; repeat(. * 2))]` → `[2,4,8,16,32]` |
| `transpose` | Transpose array of arrays | `[[1,2],[3,4]] \| transpose` → `[[1,3],[2,4]]` |
| `input` | Read next JSON input | Use with `-n` flag |
| `inputs` | Read all remaining inputs | Use with `-n` flag |
| `isempty(expr)` | True if expr produces no outputs | `isempty(empty)` → `true` |

---

## Object Functions

| Function | Description | Example |
|----------|-------------|---------|
| `keys` | Sorted key array | `{"b":1,"a":2} \| keys` → `["a","b"]` |
| `keys_unsorted` | Key array in original order | `{"b":1,"a":2} \| keys_unsorted` → `["b","a"]` |
| `values` | Value array | `{"a":1,"b":2} \| values` → `[1,2]` |
| `has(k)` | Test if key exists | `{"a":1} \| has("a")` → `true` |
| `in(obj)` | Test if input is key in obj | `"a" \| in({"a":1})` → `true` |
| `to_entries` | Object to key-value pairs | `{"a":1} \| to_entries` → `[{"key":"a","value":1}]` |
| `from_entries` | Key-value pairs to object | `[{"key":"a","value":1}] \| from_entries` → `{"a":1}` |
| `with_entries(f)` | Transform entries | `{"a":1} \| with_entries(.value += 10)` → `{"a":11}` |
| `add` | Merge array of objects | `[{"a":1},{"b":2}] \| add` → `{"a":1,"b":2}` |
| `del(path)` | Delete at path | `{"a":1,"b":2} \| del(.b)` → `{"a":1}` |
| `getpath(p)` | Get value at path | `{"a":{"b":1}} \| getpath(["a","b"])` → `1` |
| `setpath(p;v)` | Set value at path | `null \| setpath(["a","b"]; 1)` → `{"a":{"b":1}}` |
| `delpaths(ps)` | Delete multiple paths | `{"a":1,"b":2} \| delpaths([["b"]])` → `{"a":1}` |
| `path(expr)` | Get paths matching expr | `{"a":{"b":1}} \| path(.a.b)` → `["a","b"]` |
| `paths` | All paths (leaf nodes) | `{"a":{"b":1}} \| [paths]` → `[["a"],["a","b"]]` |
| `paths(f)` | Paths where f is true | `{"a":1,"b":"x"} \| [paths(type == "number")]` → `[["a"]]` |
| `leaf_paths` | Paths to leaf values | `{"a":{"b":1}} \| [leaf_paths]` → `[["a","b"]]` |
| `getpath(p)` | Value at path | `{"a":{"b":1}} \| getpath(["a","b"])` → `1` |
| `INDEX(f)` | Build lookup object keyed by f | `[{id:1,n:"a"},{id:2,n:"b"}] \| INDEX(.id)` → `{"1":{...},"2":{...}}` |
| `INDEX(stream; f)` | Build lookup from stream | `INDEX(.users[]; .id)` |
| `IN(stream)` | Test membership in stream | `2 \| IN(range(5))` → `true` |
| `GROUP_BY(f)` | Group stream by f | Similar to `group_by` for streams |

---

## Path Functions

```jq
# path — get the path to a value
{"a":{"b":1}} | path(.a.b)              # ["a","b"]

# paths — all paths in the document
{"a":{"b":1},"c":2} | [paths]           # [["a"],["a","b"],["c"]]

# leaf_paths — paths to scalar values only
{"a":{"b":1},"c":2} | [leaf_paths]      # [["a","b"],["c"]]

# getpath — retrieve value at a path
{"a":{"b":1}} | getpath(["a","b"])       # 1

# setpath — set value at a path (creates intermediate objects/arrays)
null | setpath(["a","b"]; 42)            # {"a":{"b":42}}

# delpaths — delete multiple paths
{"a":1,"b":2,"c":3} | delpaths([["a"],["c"]])  # {"b":2}

# path expressions in assignments
{"a":{"b":1}} | .a.b = 42               # {"a":{"b":42}}

# Find all paths to a specific key
{"x":{"id":1},"y":{"id":2}} | [path(.. | objects | .id)]
# [["x"],["y"]]
```

---

## Format Strings

| Format | Description | Input → Output |
|--------|-------------|---------------|
| `@base64` | Base64 encode | `"hello" \| @base64` → `"aGVsbG8="` |
| `@base64d` | Base64 decode | `"aGVsbG8=" \| @base64d` → `"hello"` |
| `@html` | HTML entity encode | `"<b>" \| @html` → `"&lt;b&gt;"` |
| `@uri` | Percent-encode (URL) | `"a b" \| @uri` → `"a%20b"` |
| `@csv` | Array to CSV row | `["a","b c",1] \| @csv` → `"\"a\",\"b c\",1"` |
| `@tsv` | Array to TSV row | `["a","b",1] \| @tsv` → `"a\tb\t1"` |
| `@json` | Serialize to JSON | `{a:1} \| @json` → `"{\"a\":1}"` |
| `@text` | Identity (same as tostring) | `42 \| @text` → `"42"` |
| `@sh` | Shell-quote | `"hello world" \| @sh` → `"'hello world'"` |

Format strings can be used in string interpolation:

```jq
# Embed CSV in a string
"Data: \(["a","b"] | @csv)"

# Shell-safe arguments
"cmd \(.filename | @sh)"
```

---

## Math Functions

### Zero-argument (operate on input)

`acos`, `acosh`, `asin`, `asinh`, `atan`, `atanh`, `cbrt`, `ceil`, `cos`, `cosh`, `erf`, `erfc`, `exp`, `exp2`, `expm1`, `fabs`, `floor`, `lgamma`, `log`, `log10`, `log2`, `nearbyint`, `rint`, `round`, `sin`, `sinh`, `sqrt`, `tan`, `tanh`, `tgamma`, `trunc`

### Two-argument (ignore input)

`atan2(y;x)`, `copysign(x;y)`, `fmax(x;y)`, `fmin(x;y)`, `fmod(x;y)`, `hypot(x;y)`, `pow(x;y)`, `remainder(x;y)`

### Three-argument

`fma(x;y;z)` — fused multiply-add

### Constants

```jq
# No built-in constants, but derive them:
1 | atan * 4                 # pi (~3.14159)
1 | exp                      # e (~2.71828)
(1/0)                        # infinite
(-1/0)                       # negative infinite
nan                          # NaN
```

---

## Date Functions

```jq
# Current time (seconds since epoch)
now                                      # 1706000000.123

# Parse date string to epoch
"2024-01-23T10:30:00Z" | fromdateiso8601  # 1706003400

# Format epoch to date string
1706003400 | todateiso8601               # "2024-01-23T10:30:00Z"

# strftime-style formatting
1706003400 | strftime("%Y-%m-%d")        # "2024-01-23"

# Parse with strptime (returns broken-down time)
"2024-01-23" | strptime("%Y-%m-%d")      # broken-down time array

# Convert broken-down time to epoch
"2024-01-23" | strptime("%Y-%m-%d") | mktime  # epoch seconds

# gmtime / localtime
now | gmtime                             # broken-down time array (UTC)

# Date arithmetic
now | . + 86400 | todateiso8601          # tomorrow
```

---

## Miscellaneous

```jq
# env — access environment variables
$ENV.HOME                    # "/Users/alice"
$ENV.PATH                   # "/usr/local/bin:..."
env.HOME                    # same as $ENV.HOME

# ascii — character codes
65 | implode                 # "A" (from codepoint)
"A" | explode                # [65] (to codepoints)

# Numeric properties
42 | infinite                # false
(1/0) | infinite             # true
(0/0) | nan                  # true

# halt / halt_error
"error message\n" | halt_error(1)    # exit with code 1 and message to stderr

# Type conversions for comparisons
null < false                 # true
false < true                 # true
true < 0                     # true (type ordering)
0 < ""                       # true
"" < []                      # true
[] < {}                      # true
```
