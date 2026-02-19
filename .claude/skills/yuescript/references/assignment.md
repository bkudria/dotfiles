# Assignment

## Basic Assignment

Variables are local by default. Use `local` and `global` to change scope:

```yue
hello = "world"
a, b, c = 1, 2, 3

-- Update operators
x += 1
x -= 1
x *= 10
x /= 10
x %= 10
s ..= "world"
arg or= "default value"

-- Chaining
a = b = c = d = 0
```

## Explicit Locals and Globals

```yue
do
  local *          -- forward-declare ALL variables as local
  x = -> 1 + y + z
  y, z = 2, 3

do
  local ^          -- only forward-declare UPPERCASE variables as local
  a = 1            -- global
  B = 2            -- local

do
  global *         -- declare ALL variables as global
  global ^         -- only UPPERCASE variables as global
```

## Destructuring

Extract values from tables by name or position.

### Table Destructuring

```yue
obj = { hello: "world", day: "tuesday" }

-- Full form
{hello: hello, day: the_day} = obj

-- Shorthand with : prefix (key = variable name)
{:hello, :day} = obj

-- Simple single-field shorthand (no braces needed)
:day = obj

-- With defaults
{:name = "nameless", :job = "jobless"} = person

-- Nested
{numbers: [first, second], properties: {:color}} = obj2

-- Mixed shorthand and rename
{:mix, :max, random: rand} = math
```

### Array Destructuring

```yue
[a, b] = [1, 2]

-- Placeholder with _
[_, two, _, four] = items
```

### Range Destructuring (Spread)

```yue
orders = ["first", "second", "third", "fourth", "last"]

[first, ...bulk, last] = orders
-- first = "first", bulk = {"second","third","fourth"}, last = "last"

[first, ...rest] = orders       -- everything after first
[...start, last] = orders       -- everything before last
[first, ..._, last] = orders    -- discard middle
```

### Destructuring in For Loops

```yue
tuples = [["hello", "world"], ["egg", "head"]]
for [left, right] in *tuples
  print left, right
```

## If Assignment (Walrus Operator)

Use `:=` in `if`/`elseif`/`while` to assign and test in one expression. The variable is scoped to the block body:

```yue
if user := database.find_user "moon"
  print user.name

if hello := os.getenv "hello"
  print "You have hello", hello
elseif world := os.getenv "world"
  print "you have world", world
else
  print "nothing :("

-- Multiple return values (first value checked)
if success, result := pcall -> "safe result"
  print result

-- While assignment
while byte := stream\read_one!
  print byte
```

## Varargs Assignment

```yue
ok, ... = fn true
count = select '#', ...
first = select 1, ...
```

## The Using Clause

Control which enclosing variables a function can modify. `using nil` prevents overwriting any closed variables:

```yue
i = 100
my_func = (using nil) ->
  i = "hello"  -- creates a NEW local, does not modify outer i
my_func!
print i  -- 100

-- Allow specific variables
my_func = (add using k, i) ->
  i += add   -- modifies outer i
  k += add   -- modifies outer k
```
