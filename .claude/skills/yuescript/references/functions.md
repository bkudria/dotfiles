# Functions

## Function Literals

Create functions with the arrow `->`. The body is indented or inline:

```yue
my_function = ->
my_function!  -- call with ! (preferred for no-arg calls)

func_a = -> print "hello world"

func_b = ->
  value = 100
  print "The value:", value
```

### Arguments

```yue
sum = (x, y) -> print "sum", x + y
sum 10, 20          -- parentheses optional
print sum 10, 20    -- args apply to closest function left
```

Parentheses required to disambiguate:

```yue
print "x:", sum(10, 20), "y:", sum(30, 40)
```

No space allowed between function name and opening parenthesis.

### Implicit Return

The last expression is automatically returned:

```yue
sum = (x, y) -> x + y
mystery = (x, y) -> x + y, x - y  -- multiple return values
```

### Argument Defaults

```yue
my_function = (name = "something", height = 100) ->
  print "Hello I am", name

-- Defaults can reference earlier args
some_args = (x = 100, y = x + 1000) ->
  print x + y
```

### Prefixed Return Expression

Declare the implicit return value before the arrow for readability in complex functions:

```yue
findFirstEven = (list): nil ->
  for item in *list
    if type(item) == "table"
      for sub in *item
        if sub % 2 == 0
          return sub
-- Returns nil if no even number found (declared before ->)
```

### Named Varargs

Collect varargs into a named table with `.n` field:

```yue
f = (...t) ->
  print "count:", t.n
  for i = 1, t.n
    print t[i]
```

### Parameter Destructuring

```yue
-- Shorthand destructuring
f1 = (:a, :b, :c) ->
  print a, b, c
f1 a: 1, b: "2", c: {}

-- With defaults
f2 = ({a: a1 = 123, :b = 'abc'}, c = {}) ->
  print a1, b, c
```

## Fat Arrow `=>`

Automatically adds `self` as first argument. `@` is shorthand for `self.`:

```yue
func = (num) => @value + num
-- Equivalent to: func = (self, num) -> self.value + num
```

## Multi-line Arguments

Continue argument lists across lines by ending with a comma. Next line must be indented deeper:

```yue
my_func 5, 4, 3,
  8, 9, 10

-- Nested calls use indentation to determine ownership
my_func 5, 6, 7,
  6, another_func 6, 7, 8,
    9, 1, 2,
  5, 4
```

## Whitespace Sensitivity

Whitespace affects parsing of `-` (negation vs subtraction) and string literals:

```yue
a = x - 10    -- subtraction
b = x-10      -- subtraction
c = x -y      -- function call: x(-y)
d = x- z      -- subtraction

-- String literal: no space = function call takes precedence
x = func"hello" + 100   -- (func("hello")) + 100
y = func "hello" + 100  -- func("hello" + 100)
```

## Backcalls

Unnest callbacks with left-pointing arrows. The function body continues below without extra indent:

```yue
x <- f
print "hello" .. x

-- Fat arrow backcall
<= f
print @value

-- With placeholder for position
(x) <- map _, [1, 2, 3]
x * 2

-- Chain backcalls inside do blocks
result, msg = do
  data <- readAsync "filename.txt"
  print data
  info <- processAsync data
  check info
```

## Function Stubs

Bundle an object with its method for passing as a callback. Same syntax as method call but with no argument list:

```yue
my_object = {
  value: 1000
  write: => print "the value:", @value
}

-- Wrong: loses self reference
run_callback my_object.write

-- Correct: function stub preserves self
run_callback my_object\write
```
