---
name: yuescript
description: "Yue/Yuescript language reference for writing, reviewing, and debugging code that compiles to Lua. Use when editing .yue files, writing Yuescript code, converting Lua to idiomatic Yue, reviewing Yue syntax, or debugging Yue compilation errors."
---

# Yuescript Language Reference

Yuescript (Yue) is a dynamic language that compiles to Lua. It emphasizes expressive, concise code with significant whitespace, implicit returns, and rich syntactic sugar over Lua.

## When to Use

- Writing or editing `.yue` files
- Reviewing Yue code for correctness and idiomatic style
- Converting Lua code to idiomatic Yuescript
- Debugging Yue compilation errors
- Writing new Yue modules, classes, or macros

## Key Differences from Lua

| Feature | Lua | Yue |
|---------|-----|-----|
| Scope | Variables are global by default | Variables are **local** by default |
| Tables | `{ key = value }` | `{ key: value }` (colon, not equals) |
| Array tables | `{ 1, 2, 3 }` | `[1, 2, 3]` (square brackets) |
| Not-equal | `~=` | `!=` (also accepts `~=`) |
| String interpolation | Not built-in | `"Hello #{name}"` |
| Functions | `function(x) return x end` | `(x) -> x` |
| Method call | `obj:method()` | `obj\method!` |
| Self reference | `self.field` | `@field` |
| Fat arrow | N/A | `=>` auto-creates `self` param |
| No-arg call | `func()` | `func!` (preferred) |
| Returns | Explicit `return` | Implicit return (last expression) |
| Classes | Manual metatables | `class Name extends Base` |
| Comprehensions | N/A | `[x * 2 for x in *items]` |

## Quick Syntax Cheat Sheet

```yue
-- Assignment (local by default)
name = "world"
x, y, z = 1, 2, 3
x += 10

-- String interpolation
greeting = "Hello #{name}!"

-- Functions
add = (a, b) -> a + b
method = (num) => @value + num  -- fat arrow: self as first arg

-- Tables
array = [1, 2, 3]
hash = { name: "Yue", version: 1 }
profile =
  height: "4 feet"
  shoe_size: 13

-- Destructuring
{:concat, :insert} = table
[first, ...rest] = items

-- Control flow
if condition
  action!
print "hello" if condition  -- postfix

-- Comprehensions
doubled = [x * 2 for x in *items]
filtered = {k, v for k, v in pairs tbl when k != "skip"}

-- Classes
class Animal
  new: (@name) =>
  speak: => print "I am #{@name}"

class Dog extends Animal
  speak: =>
    super!
    print "Woof!"

-- Pipe operator
result = data |> transform |> format

-- Existence check
value = obj?.nested?.field
func? arg1, arg2

-- Nil coalescing
x = value ?? "default"

-- With statement
with canvas
  .width = 100
  .height = 200
  \drawRect!

-- Import/Export
import insert, concat from table
export class MyModule
```

## Dependencies

- **yuescript** compiler — `brew install yuescript` or build from [source](https://github.com/pigpigyyy/Yuescript)
- **Lua** runtime — Yue compiles to Lua 5.1+ / LuaJIT

## Reference Files

Read these for detailed syntax and examples on specific topics.

| File | Contents |
|------|----------|
| `references/syntax-basics.md` | Whitespace, comments, literals, operators, attributes |
| `references/assignment.md` | Assignment, destructuring, if-assignment, varargs, using clause |
| `references/functions.md` | Function literals, fat arrows, backcalls, stubs, parameter destructuring |
| `references/control-flow.md` | Conditionals, for/while/repeat loops, continue, switch, line decorators |
| `references/data-structures.md` | Table literals, list/table comprehensions, slicing |
| `references/oop.md` | Classes, inheritance, super, class variables, mixins |
| `references/advanced.md` | Macros, modules (import/export), do blocks, try/catch |
