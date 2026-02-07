# Control Flow

## Conditionals

### If/Else

```yue
have_coins = false
if have_coins
  print "Got coins"
else
  print "No coins"

-- Single-line with then
if have_coins then print "Got coins" else print "No coins"
```

Conditionals are expressions (assignable):

```yue
result = if condition then "yes" else "no"
```

### Unless

Executes when condition is false:

```yue
unless os.date("%A") == "Monday"
  print "it is not Monday!"
```

### Postfix Conditionals

```yue
print "hello world" if name == "Rob"
print "not Monday" unless day == "Monday"
```

### In Expression

Test membership in a list:

```yue
if a in [1, 3, 5, 7]
  print "found in literal list"

if a in some_list
  print "found in variable list"
```

## For Loops

### Numeric For

```yue
for i = 10, 20
  print i

for k = 1, 15, 2  -- with step
  print k

-- Single-line
for j = 1, 10, 3 do print j
```

### Generic For

```yue
for key, value in pairs object
  print key, value

-- Array iteration with * operator
for item in *items
  print item

-- With slicing
for item in *items[2, 4]
  print item

-- Single-line
for item in *items do print item
```

### For as Expression

The last statement is accumulated into a result table:

```yue
doubled_evens = for i = 1, 20
  if i % 2 == 0
    i * 2
  else
    i

-- Break with return value
first_large = for n in *numbers
  break n if n > 10
```

For loops at end of a function body do NOT auto-accumulate (returns nil). Use explicit `return`:

```yue
func_a = -> for i = 1, 10 do print i      -- returns nil
func_b = -> return for i = 1, 10 do i     -- returns table
```

## While / Until Loops

```yue
i = 10
while i > 0
  print i
  i -= 1

while running == true do my_function!

-- Until (opposite condition)
until i == 0
  print i
  i -= 1
```

While loops can also be used as expressions (must explicitly return).

## Repeat Loop

```yue
i = 10
repeat
  print i
  i -= 1
until i == 0
```

## Continue

Skip the current iteration:

```yue
i = 0
while i < 10
  i += 1
  continue if i % 2 == 0
  print i
```

In loop expressions, `continue` prevents the iteration from accumulating:

```yue
odds = for x in *my_numbers
  continue if x % 2 == 0
  x
```

## Switch

```yue
name = "Dan"
switch name
  when "Robert"
    print "You're Bob"
  when "Dan", "Daniel"       -- multiple values
    print "Hi Dan"
  else
    print "I don't know you"

-- As expression
result = switch value
  when 1 then "one"
  when 2 then "two"
  else "other"

-- With assignment
switch name := "Dan"
  when "Dan"
    print name
```

### Table Matching in Switch

```yue
switch item
  when {:name, :age}
    print "Person: #{name}, age #{age}"
  when [first, ...rest]
    print "Array starting with #{first}"
  when {:type = "default"}
    print "Has type with default"
```

## Line Decorators

Postfix `for`, `if`, `while`, `until` on single statements:

```yue
print "hello world" if name == "Rob"
print "item: ", item for item in *items
game\update! while game\isRunning!
reader\parse_line! until reader\eof!
```
