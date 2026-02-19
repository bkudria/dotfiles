# Data Structures

## Table Literals

### Hash Tables (Curly Braces)

Keys use `:` (not `=` like Lua):

```yue
some_values = {
  name: "Bill"
  age: 200
  ["favorite food"]: "rice"
}
```

Curly braces optional when assigning a single table of key-value pairs:

```yue
profile =
  height: "4 feet"
  shoe_size: 13
  favorite_foods: ["ice cream", "donuts"]
```

Newlines work as delimiters (commas optional):

```yue
values = {
  1, 2, 3, 4
  5, 6, 7, 8
  name: "superman"
  occupation: "crime fighting"
}
```

Single-line without braces:

```yue
my_function dance: "Tango", partner: "none"
y = type: "dog", legs: 4, tails: 1
```

### Shorthand `:` Prefix

When key matches variable name:

```yue
hair = "golden"
height = 200
person = { :hair, :height, shoe_size: 40 }
print_table :hair, :height
```

### Expression Keys

```yue
t = {
  [1 + 2]: "hello"
  "hello world": true
}
```

### Array Tables (Square Brackets)

Semantic distinction from hash tables. Key-value pairs not allowed:

```yue
some_values = [1, 2, 3, 4]
list_with_one_element = [1, ]
```

Keywords can be used as table keys without escaping:

```yue
tbl = { do: "something", end: "hunger" }
```

## Comprehensions

### List Comprehensions

Produce array-like tables. Use `*` for array iteration:

```yue
items = [1, 2, 3, 4]

-- Basic
doubled = [item * 2 for i, item in ipairs items]

-- With * operator (preferred for arrays)
doubled = [item * 2 for item in *items]

-- With when clause
slice = [item for item in *items when item > 2]

-- Numeric for
evens = [i for i = 1, 100 when i % 2 == 0]

-- Multiple for clauses (nested loops)
points = [[x, y] for x in *x_coords for y in *y_coords]
```

### Flat Map with Spread

```yue
data = { a: [1, 2, 3], b: [4, 5, 6] }
flat = [...v for k, v in pairs data]
-- [1, 2, 3, 4, 5, 6]
```

### Table Comprehensions

Produce key-value tables. Use `{` and `}`:

```yue
thing = { color: "red", name: "fast", width: 123 }

-- Copy
thing_copy = {k, v for k, v in pairs thing}

-- Filter
no_color = {k, v for k, v in pairs thing when k != "color"}

-- Transform
numbers = [1, 2, 3, 4]
sqrts = {i, math.sqrt i for i in *numbers}

-- From pairs
tuples = [["hello", "world"], ["foo", "bar"]]
tbl = {unpack tuple for tuple in *tuples}
```

## Slicing

Restrict iteration bounds with `*` operator. Syntax: `items[min, max, step]`:

```yue
-- Items 1 through 5
slice = [item for item in *items[1, 5]]

-- Everything from index 2 onward
slice = [item for item in *items[2,]]

-- Odd-indexed items (step of 2)
slice = [item for item in *items[,,2]]

-- Last 4 items (negative indexing)
slice = [item for item in *items[-4, -1]]

-- Reversed
reversed = [item for item in *items[-1, 1, -1]]
```

### Slicing as Expression

```yue
sub_list = items[2, 4]
last_four = items[-4, -1]
```
