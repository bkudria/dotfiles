# Language Basics

Core jq language elements: types, operators, basic filters, conditionals, and string interpolation.

---

## Types

jq supports the same types as JSON:

| Type | Examples | Notes |
|------|----------|-------|
| `null` | `null` | The absence of a value |
| `boolean` | `true`, `false` | |
| `number` | `42`, `3.14`, `1e10` | IEEE754 64-bit float (no separate integer type) |
| `string` | `"hello"` | Unicode strings |
| `array` | `[1, 2, 3]` | Ordered, heterogeneous |
| `object` | `{"key": "value"}` | Keys are always strings |

### Type checking

```jq
# type/0 returns the type name as a string
42 | type                    # "number"
"hi" | type                  # "string"

# Type-specific predicates
null | isnull                # true — only true for null
42 | isnan                   # false
(1/0) | isinfinite           # true
42 | isfinite                # true
42 | isnormal                # true

# Filtering by type
[1, "a", null, true] | map(select(type == "string"))  # ["a"]
```

### Type conversion

```jq
"42" | tonumber              # 42
42 | tostring                # "42"
null | not                   # true
"hello" | explode            # [104,101,108,108,111]  (codepoints)
[104,101,108,108,111] | implode  # "hello"
```

---

## Identity and Field Access

```jq
# Identity — pass input through unchanged
.                            # the entire input

# Field access
.name                        # access field "name"
.["name"]                    # same, bracket notation (required for special chars)
.name?                       # same, but suppress errors if . is not an object

# Nested access
.user.address.city           # chain field access
.user["first name"]          # bracket notation for spaces

# Optional field access
.foo?                        # null if .foo doesn't exist or . is not an object
.foo.bar?                    # suppress error on last step only
.foo?.bar?                   # suppress errors on both steps
```

---

## Array Operations

```jq
# Index access (0-based)
.[0]                         # first element
.[-1]                        # last element
.[2]                         # third element

# Slicing (start inclusive, end exclusive)
.[2:5]                       # elements at indices 2, 3, 4
.[:3]                        # first 3 elements
.[-2:]                       # last 2 elements

# Iteration
.[]                          # generate all elements (also works on objects: yields values)
.[].name                     # access .name on each element

# Array construction
[.[] | . * 2]                # map: double each element
[range(5)]                   # [0, 1, 2, 3, 4]

# String slicing (same syntax)
"Hello, World"[0:5]          # "Hello"
"Hello, World"[-5:]          # "World"
```

---

## Pipe and Comma

```jq
# Pipe — feed output of left into input of right
.users | .[] | .name         # equivalent to .users[].name

# Comma — generate multiple outputs
.name, .age                  # outputs .name then .age
1, 2, 3                      # three separate outputs

# Parentheses for grouping
(.a + .b) * .c               # arithmetic grouping
(.a, .b) | . + 1             # apply . + 1 to each of .a and .b
```

---

## Operators

### Arithmetic

| Operator | Types | Behavior |
|----------|-------|----------|
| `+` | number + number | Addition |
| `+` | string + string | Concatenation |
| `+` | array + array | Concatenation |
| `+` | object + object | Merge (right wins) |
| `+` | null + x | Returns x |
| `-` | number - number | Subtraction |
| `-` | array - array | Remove elements |
| `*` | number * number | Multiplication |
| `*` | string * object | String interpolation (rare) |
| `*` | object * object | Recursive merge |
| `/` | number / number | Division |
| `/` | string / string | Split string |
| `%` | number % number | Modulo |

### Comparison

```jq
# All produce true or false
.a == .b                     # equal (deep comparison)
.a != .b                     # not equal
.a < .b                      # less than (works across types with ordering: null < false < true < number < string < array < object)
.a <= .b                     # less than or equal
.a > .b                      # greater than
.a >= .b                     # greater than or equal
```

### Logic

```jq
true and false               # false
true or false                # true
true | not                   # false
null | not                   # true (null is falsey)
false | not                  # true (false is falsey)

# if-then-else-end
if .count > 0 then "has items" elif .count == 0 then "empty" else "negative" end

# if without else defaults to identity
if .x > 0 then .x end       # outputs .x if positive, otherwise outputs entire input unchanged
```

### Alternative operator (`//`)

```jq
# Returns left side if not null/false, otherwise right side
.name // "unknown"           # "unknown" if .name is null or false
.config.timeout // 30        # default value pattern

# Chaining alternatives
.primary // .secondary // .fallback // "none"
```

---

## String Interpolation

jq uses `\(expr)` inside double-quoted strings:

```jq
"Hello, \(.name)!"           # "Hello, Alice!"
"Count: \(.items | length)"  # "Count: 5"
"\(.first) \(.last)"         # "Alice Smith"

# Nested interpolation
"Result: \(if .ok then "success" else "failure: \(.error)" end)"
```

---

## Object Construction

```jq
# Literal construction
{name: .user.name, age: .user.age}

# Shorthand (key from variable name)
{name, age}                  # same as {name: .name, age: .age}

# Computed keys (parentheses required)
{(.key): .value}             # key from expression

# From entries
[{key: "a", value: 1}, {key: "b", value: 2}] | from_entries
# {"a": 1, "b": 2}

# To entries
{"a": 1, "b": 2} | to_entries
# [{key: "a", value: 1}, {key: "b", value: 2}]

# With entries (shorthand for to_entries | map(f) | from_entries)
{"a": 1, "b": 2} | with_entries(.value += 10)
# {"a": 11, "b": 12}
```

---

## Variable Binding

```jq
# Bind a value to a variable
.user as $u | {name: $u.name, upper: ($u.name | ascii_upcase)}

# Destructuring bind
. as {name: $n, age: $a} | "\($n) is \($a)"

# Array destructuring
. as [$first, $second] | $first + $second

# Multiple bindings
.x as $x | .y as $y | $x + $y
```

---

## Recursive Descent

```jq
# .. generates all values at all depths (recursive descent)
.. | numbers              # all numbers anywhere in the structure
.. | strings              # all strings anywhere in the structure
[.. | .id? // empty]      # all "id" values at any depth

# recurse/0 is equivalent to ..
recurse | select(type == "object" and has("error"))

# recurse with a filter
recurse(.children[]?)     # recursively descend through .children arrays
```

---

## Comments

```jq
# Single-line comment — everything after # to end of line
[1, 2, 3] | map(. * 2)   # doubles each element

# Backslash continues comment to next line
# This is a multi-line \
  comment that spans two lines
```
