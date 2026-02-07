# Syntax Basics

## Whitespace

Yue is whitespace-sensitive. Indentation (spaces or tabs) defines code blocks for functions, control structures, and value lists. Tabs equal four spaces; mixing is discouraged.

Statements end at line breaks. Semicolons explicitly terminate statements for multiple per line:

```yue
a = 1; b = 2; print a + b
```

Multiline chaining maintains consistent indentation:

```yue
Rx.Observable
  .fromRange 1, 8
  \filter (x) -> x % 2 == 0
  \map (value) -> value .. '!'
  \subscribe print
```

## Comments

Single-line comments start with `--`:

```yue
-- this is a comment
x = 5 -- inline comment
```

Multi-line comments use `--[[` and `]]`:

```yue
--[[ This is a
multi-line comment ]]
result = 1 + --[[ inline ]] 2
```

## Literals

### Strings

Line breaks are allowed inside single and double quote strings without escaping. Double-quoted strings support interpolation with `#{}`:

```yue
multi = "This string
  spans multiple lines."

print "I am #{math.random! * 100}% sure."
```

YAML-style multiline strings use the `|` prefix. Leading whitespace is auto-detected and stripped:

```yue
str = |
  key: value
  list:
    - item1
    - #{expr}
```

Internal indentation relative to the minimum is preserved. Special characters (quotes, backslashes) are auto-escaped.

### Numbers

Underscores improve readability:

```yue
integer = 1_000_000
hex = 0xEF_BB_BF
binary = 0B10011
```

## Operators

All standard Lua binary and unary operators are supported, plus:

| Operator | Purpose | Example |
|----------|---------|---------|
| `!=` | Not-equal (alternative to `~=`) | `a != b` |
| `\` or `::` | Method call / function chaining | `obj\method!` |
| `[] =` | Table append | `[] = value` |
| `...` | Spread operator | `[...items, extra]` |
| `#-n` | Reversed indexing | `data[#-1]` (second-to-last) |
| `<>` | Metatable manipulation | `<> = mt` |
| `?` | Existence check | `obj?.field`, `func? arg` |
| `\|>` | Pipe | `value \|> func` |
| `??` | Nil coalescing | `x ?? "default"` |
| `??=` | Conditional nil assignment | `x ??= "default"` |
| `*` / `-` | Implicit table list prefixes | Indented blocks create tables |

### Chained Comparisons

Multiple comparisons chain naturally (middle expression evaluated once):

```yue
if 1 < x <= 10
  print "in range"
```

### Spread Operator

Expand table elements inline:

```yue
items = [1, 2, 3]
all = [0, ...items, 4]  -- [0, 1, 2, 3, 4]
```

### Existence Operator `?`

Safe-navigate through potentially nil chains:

```yue
-- Safe property access
name = user?.profile?.name

-- Safe function call
result = callback? arg1, arg2

-- Conditional block
if value?
  print value
```

### Pipe Operator `|>`

Chain function calls left-to-right:

```yue
result = "hello" |> string.upper |> string.rep _, 3
```

### Implicit Object Lists

`*` or `-` prefixed lists in indented blocks create implicit tables:

```yue
inventory =
  * "sword"
  * "shield"
  * "potion"
```

## Attributes

Lua 5.4 `const` and `close` attributes work across all target Lua versions:

```yue
const a = 123
close _ = <close>: -> print "Out of scope."

-- Destructuring with const
const {:a, :b, c, d} = tb

-- Global const
global const Constant = 123
```
