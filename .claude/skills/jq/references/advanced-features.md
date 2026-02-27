# Advanced Features

reduce, foreach, generators, streaming, modules, error handling, label-break, and defining functions.

---

## Defining Functions

```jq
# Basic function definition
def double: . * 2;

# With filter arguments (passed as callbacks, not values)
def apply_twice(f): f | f;
5 | apply_twice(. * 2)                  # 20 (5*2=10, 10*2=20)

# With value arguments (use $ shorthand)
def add_n($n): . + $n;
5 | add_n(3)                            # 8

# Multiple arguments
def clamp($lo; $hi): if . < $lo then $lo elif . > $hi then $hi else . end;
150 | clamp(0; 100)                      # 100

# Recursive functions
def factorial: if . <= 1 then 1 else . * ((. - 1) | factorial) end;
5 | factorial                            # 120

# Multiple definitions (different arities coexist)
def f: . + 1;
def f(x): . + x;
# f/0 and f/1 are separate functions
```

### Functions are filters

Arguments are passed as filters, not values. This is the most important concept:

```jq
# f is a filter — it runs with different inputs each time
def apply(f): f;
5 | apply(. * 2)              # 10 — f is `. * 2`, input is 5

# Multiple invocations of same argument use current input
def apply_twice(f): f | f;
5 | apply_twice(. + 3)        # 11 — first f: 5+3=8, second f: 8+3=11

# To capture a value, bind to variable
def apply_value(f): f as $v | . + $v;
5 | apply_value(. * 2)        # 15 — $v=10, then 5+10=15
```

---

## Scoping

Symbols are scoped lexically — visible only "to the right" of their definition.

```jq
# Variable scoping
1 as $x | (2 as $x | $x), $x    # 2, 1 — inner $x shadows outer

# Function scoping
def f: "outer";
def g: f;
def f: "inner";
g                                 # "outer" — g captured f at definition time
f                                 # "inner"

# Parentheses limit scope
(5 as $x | $x), $x              # error — $x not visible outside parens
```

---

## reduce

Accumulate all outputs of an expression into a single result.

```jq
# Syntax: reduce EXPR as $var (INIT; UPDATE)
reduce .[] as $item (0; . + $item)       # sum array

# With destructuring
reduce .[] as [$k, $v] ({}; . + {($k): $v})  # pairs to object

# With object destructuring
reduce .[] as {$name, $score} (
  {};
  .[$name] += $score
)

# Practical: word frequency counter
reduce (split(" ")[] | ascii_downcase) as $w (
  {};
  .[$w] += 1
)

# Practical: running maximum
reduce .[] as $x (
  -infinite;
  if $x > . then $x else . end
)
```

---

## foreach

Like reduce but emits intermediate values. Form: `foreach EXPR as $var (INIT; UPDATE; EXTRACT)`.

```jq
# Running total (emits each intermediate sum)
foreach .[] as $item (0; . + $item)
# Input [1,2,3,4,5] → outputs: 1, 3, 6, 10, 15

# With EXTRACT expression
foreach .[] as $item (0; . + $item; {sum: ., item: $item})
# Emits {sum:1,item:1}, {sum:3,item:2}, ...

# Indexing with foreach
foreach .[] as $item (0; . + 1; {index: ., $item})
# Emits {index:1,item:"foo"}, {index:2,item:"bar"}, ...

# State machine: track state transitions
foreach .events[] as $e (
  {state: "idle", count: 0};
  if $e == "start" then .state = "running" | .count += 1
  elif $e == "stop" then .state = "idle"
  else . end;
  {event: $e, state: .state, count: .count}
)
```

When EXTRACT is omitted, identity is used (emits the accumulator state).

---

## Generators

Generators produce zero or more outputs. Core generators:

```jq
# .[] — iterate array/object values
[1,2,3] | .[]                           # 1, 2, 3

# range — numeric sequences
range(5)                                 # 0, 1, 2, 3, 4
range(2; 5)                              # 2, 3, 4
range(0; 10; 3)                          # 0, 3, 6, 9

# comma — concatenate outputs
1, 2, 3                                  # three outputs

# empty — zero outputs (backtrack)
1, empty, 2                              # 1, 2

# recurse — recursive descent
recurse(.children[]?; . != null)         # walk tree

# while — generate while condition holds
1 | [while(. < 100; . * 2)]             # [1,2,4,8,16,32,64]

# until — iterate until condition (returns final value, not intermediate)
1 | until(. >= 100; . * 2)              # 128

# repeat — infinite repetition (use with limit)
1 | [limit(5; repeat(. * 2))]           # [2,4,8,16,32]

# Custom generators using recursion
def range_step(a; b; step):
  def _r: if (step > 0 and . < b) or (step < 0 and . > b)
           then ., ((. + step) | _r)
           else empty end;
  a | _r;
```

### Collecting generator outputs

```jq
# Wrap in array
[.[] | select(. > 2)]                   # collect filtered results

# Wrap in object with from_entries
[.[] | {key: .name, value: .score}] | from_entries

# first/last/nth
first(range(100))                        # 0
last(range(10))                          # 9
nth(5; range(100))                       # 5

# limit — take first N
[limit(3; .[])]                         # first 3 elements

# isempty — test if generator produces anything
isempty(.[] | select(. > 100))          # true if no elements > 100
```

---

## Error Handling

### try-catch

```jq
# Suppress errors
try .foo                                 # null if . is not an object (no error)

# try with catch
try (.value | tonumber) catch "not a number"

# catch receives the error message string
try error("bad input") catch "Error: \(.)"   # "Error: bad input"

# ? suffix is shorthand for try
.foo?                                    # same as try .foo
.[]?                                     # iterate without error if not iterable
```

### Alternative operator (`?//`)

Different from `try-catch`: triggers on both errors AND empty output.

```jq
# ?// tries left side; if it errors or produces empty, uses right side
.foo ?// "default"                       # "default" if .foo errors or is empty

# vs try-catch which only catches errors
try .foo catch "default"                 # catches errors but NOT empty

# vs // which handles null/false but not errors
.foo // "default"                        # "default" if .foo is null or false

# Practical: safe array access with fallback
.[0] ?// "no elements"
```

### Explicit errors

```jq
# Raise an error
error("something went wrong")

# Conditional error
if .age < 0 then error("negative age") else . end

# halt with exit code
"Fatal error\n" | halt_error(1)          # prints to stderr, exits with code 1
halt                                     # exit with code 0
```

---

## label-break

Control flow for early exit from expressions.

```jq
# Syntax: label $name | EXPR
# Break with: ..., break $name
label $out | foreach .[] as $x (
  0;
  . + $x;
  if . > 100 then ., break $out else . end
)
# Emits running sums until exceeding 100, then stops
```

---

## Streaming

Process large JSON documents incrementally using path-value pairs.

### CLI streaming

```bash
# Parse input as a stream of path-value pairs
jq --stream '.' large.json
# Output: [[0],"first"], [[1],"second"], [[1]], ...

# Reconstruct from stream
jq --stream 'fromstream(.)' large.json
```

### Stream builtins

```jq
# tostream — convert value to stream of [path, value] pairs
{"a":1,"b":[2,3]} | tostream
# [["a"],1], [["b",0],2], [["b",1],3], [["b",1]], [["b"]], ...

# fromstream — reconstruct value from stream
fromstream(tostream)                     # identity

# truncate_stream — remove leading path components
# Input: number of components to strip
1 | truncate_stream({"a":{"b":1}} | tostream)
# Strips one level, yielding the inner object's stream

# Practical: extract nested values from large documents
jq -n --stream 'fromstream(1 | truncate_stream(inputs | select(.[0][0] == "results")))' huge.json
```

### Stream format

| Form | Meaning |
|------|---------|
| `[[path], value]` | Scalar value at path |
| `[[path]]` | End of array/object at path |

---

## Modules

jq's library/module system for reusable code.

### Import and include

```jq
# Import a module with namespace prefix
import "utils" as utils;
.data | utils::transform

# Include a module (symbols merged into current namespace)
include "helpers";
.data | transform

# Import JSON data
import "config" as $config;
$config::config.timeout

# Module metadata
import "mylib" as mylib {search: "./libs"};
```

### Module files

Module files have `.jq` extension. Search path: `~/.jq`, `$ORIGIN/../lib/jq`, `$ORIGIN/../lib`.

```jq
# ~/.jq/utils.jq
module {version: "1.0"};

def normalize: ascii_downcase | gsub("\\s+"; " ") | trim;
def count_by(f): group_by(f) | map({key: .[0] | f, value: length}) | from_entries;
```

### Search path

| Prefix | Substitution |
|--------|-------------|
| `~/` | User's home directory |
| `$ORIGIN/` | Directory containing jq executable |
| `./` | Directory of the including file |

Override with `-L path` flag. A module `foo` is found at `path/foo.jq` or `path/foo/foo.jq`.

### `~/.jq` auto-sourcing

If `~/.jq` exists as a file (not directory), it is automatically sourced into every jq program.

---

## Assignment Operators

All assignments are immutable — they produce new values, not mutations.

```jq
# Plain assignment (RHS sees original input)
.a = .b                                 # set .a to value of .b

# Update assignment (RHS sees current value at path)
.a |= . + 1                             # increment .a

# Difference between = and |=
{"a": {"b": 10}, "b": 20} | .a = .b     # {"a": 20, "b": 20}
{"a": {"b": 10}, "b": 20} | .a |= .b    # {"a": 10, "b": 20}

# Arithmetic update assignments
.count += 1                              # increment
.total -= .discount                      # subtract
.price *= 1.1                            # multiply (10% increase)
.value /= 2                             # divide
.n %= 3                                  # modulo
.name //= "anonymous"                   # set if null/false

# Complex path on LHS
.users[].score |= . * 100              # update all user scores
(.users[] | select(.active)) |= . + {notified: true}

# Delete (|= empty removes the path)
.users |= map(select(.active))          # keep only active users
del(.users[] | select(.deleted))        # delete flagged users
```

---

## `$ARGS`

Access command-line arguments passed via `--arg`, `--argjson`, `--args`, `--jsonargs`.

```bash
# Named arguments
jq -n --arg name "Alice" --argjson age 30 '$ARGS'
# {"positional":[],"named":{"name":"Alice","age":30}}

# Positional arguments
jq -n '$ARGS.positional[]' --args "a" "b" "c"
# "a", "b", "c"

# JSON positional arguments
jq -n '$ARGS.positional' --jsonargs '{"x":1}' '[2,3]'
# [{"x":1},[2,3]]
```
