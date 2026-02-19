# Object-Oriented Programming

## Classes

Declare with `class` followed by a table-like body. `new` is the constructor. Use fat arrow `=>` for methods (auto-injects `self`). `@` is shorthand for `self.`:

```yue
class Inventory
  new: =>
    @items = {}

  add_item: (name) =>
    if @items[name]
      @items[name] += 1
    else
      @items[name] = 1

inv = Inventory!
inv\add_item "t-shirt"
inv\add_item "pants"
```

All class properties are shared among instances. Mutable state belongs in the constructor:

```yue
-- WRONG: shared mutable state
class Person
  clothes: []

-- CORRECT: per-instance state
class Person
  new: =>
    @clothes = []
```

## Inheritance

Use `extends` to inherit. Call `super` for parent methods:

```yue
class BackPack extends Inventory
  size: 10
  add_item: (name) =>
    if #@items > size then error "backpack is full"
    super name  -- calls parent's add_item
```

If no constructor defined on child, parent's constructor is used. The `__inherited` class method is called on the parent when inherited:

```yue
class Shelf
  @__inherited: (child) =>
    print @__name, "was inherited by", child.__name

class Cupboard extends Shelf  -- triggers __inherited
```

## Super

- **As function call**: calls same-named method on parent, auto-passes `self`
- **As value**: reference to parent class object

```yue
class MyClass extends ParentClass
  a_method: =>
    super "hello", "world"            -- call parent's a_method
    super\a_method "hello", "world"   -- same effect
    super.a_method self, "hello"      -- raw function call
    assert super == ParentClass       -- super as value
```

## Class Variables

Use `@` prefix in class body for class-level (not instance) properties. `@@` accesses `self.__class`:

```yue
class Counter
  @count: 0

  new: =>
    @@count += 1

Counter!
Counter!
print Counter.count  -- 2
```

Class body statements execute after properties are set. Local variables in class body are private:

```yue
class MoreThings
  secret = 123
  log = (msg) -> print "LOG:", msg

  some_method: =>
    log "hello world: " .. secret
```

## Constructor Property Promotion

`@` and `@@` in parameter list auto-assign to instance/class:

```yue
class Something
  new: (@foo, @bar, @@biz, @@baz) =>
-- Equivalent to:
class Something
  new: (foo, bar, biz, baz) =>
    @foo = foo
    @bar = bar
    @@biz = biz
    @@baz = baz
```

## Special Properties

| Property | Description |
|----------|-------------|
| `__class` | Class object of an instance |
| `__name` | String name of the class |
| `__base` | Base table (metatable for instances) |
| `__parent` | Parent class (if extends) |

```yue
b = BackPack!
assert b.__class == BackPack
print BackPack.__name    -- "BackPack"
```

## Class Expressions and Anonymous Classes

```yue
-- Class as expression
x = class Bucket
  drops: 0
  add_drop: => @drops += 1

-- Anonymous class
BigBucket = class extends Bucket
  add_drop: => @drops += 10

-- Blank anonymous class
x = class
```

## Class Mixing

Copy functions from a table or class with `using`:

```yue
MyIndex = __index: var: 1

class X using MyIndex
  func: => print 123

x = X!
print x.var  -- 1

class Y using X
y = Y!
y\func!
-- X is NOT parent of Y (mixing, not inheritance)
```

## With Statement

Reduce repetition when configuring objects. `.` sets properties, `\` calls methods:

```yue
with Person!
  .name = "Oswald"
  \add_relative my_dad
  \save!
  print .name

-- As expression (returns the object)
player = with Player!
  .x = 100
  .y = 200

-- Named binding
with obj := create_object!
  print obj

-- Safe with (nil check)
with? possibly_nil
  .field = "value"
```
