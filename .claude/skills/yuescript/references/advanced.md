# Advanced Features

## Do Blocks

Works like Lua `do` for scoping. Also usable as an expression (returns last statement):

```yue
do
  var = "hello"
  print var
print var  -- nil

-- As expression
counter = do
  i = 0
  ->
    i += 1
    i

print counter!  -- 1
print counter!  -- 2

-- Inline in tables
tbl = {
  key: do
    print "assigning key!"
    1234
}
```

## Try/Catch

### Standard Try-Catch

```yue
try
  func_that_may_fail!
catch err
  print "Error:", err

-- With assignment (captures success status + result)
success, result = try func!

-- Single-line
success, result = try dangerous_call!

-- With if-assignment
if success, result := try risky_func!
  print result
```

### Try? (Simplified)

Returns result on success or nil on failure (no boolean status):

```yue
result = try? func_that_may_fail!

-- With nil coalescing
value = try? get_value! ?? "default"

-- With catch
result = try? func!
catch err
  print "failed:", err
```

## Modules

### Import

```yue
-- Destructure from module
import insert, concat from table

-- From require (implicit)
import C, Ct, Cmt from require "lpeg"

-- Shortcut: string = implicit require
import x, y, z from 'mymodule'

-- Python-style
from 'module' import a, b, c

-- Module require shortcut
import 'module'
import "d-a-s-h-e-s"
import "module.part"

-- With aliasing
import "player" as PlayerModule
import "lpeg" as :C, :Ct, :Cmt
import "export" as {one, two, Something:{umm:{ch}}}
```

Imported items are `const` by default.

### Import Global

```yue
-- Specific globals to locals
import tostring
import table.concat

-- Auto-import ALL undeclared names as local const globals
do
  import global
  print "hello"       -- auto-imported as const
  math.random 3       -- auto-imported as const
  -- print = nil      -- ERROR: imported globals are const

  -- Explicit globals exempt from const
  global FLAG
  FLAG = 123
```

### Export

#### Named Export

Creates local variable and adds to exported table:

```yue
export a, b, c = 1, 2, 3
export cool = "cat"

export class Something
  umm: "cool"

-- With destructuring
export :loadstring, to_lua: tolua = yue

-- Without creating locals
export.itemA = tb
export["a-b-c"] = 123
```

#### Unnamed Export

Adds to array part of exported table:

```yue
d, e, f = 3, 2, 1
export d, e, f

export if this then 123 else 456
```

#### Default Export

Replaces entire exported table:

```yue
export default ->
  print "hello"
  123
```

## Macros

Compile-time code generation. Macros evaluate strings and insert code:

```yue
macro stringify = (value) -> "'#{value}'"
print $stringify hello  -- prints: hello

-- Returning Lua code directly
macro lua_code = (code) -> {
  code: "print('raw lua')"
  type: "lua"
}
```

### Macro Import/Export

```yue
-- Export macros from a module
-- In helper.yue:
export macro add = (a, b) -> "#{a} + #{b}"

-- In main.yue:
import "helper" as $  -- import all macros
result = $add 1, 2
```

### Built-in Macros

| Macro | Value |
|-------|-------|
| `$FILE` | Current module name |
| `$LINE` | Current line number |

### Argument Type Validation

Declare expected AST types with backtick syntax:

```yue
macro check = (val `Num`) -> "assert(#{val} > 0)"
```

## Line Decorators

Postfix control flow for single statements:

```yue
print "hello world" if name == "Rob"
print "item: ", item for item in *items
game\update! while game\isRunning!
reader\parse_line! until reader\eof!
```
