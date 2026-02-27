# KDL Ruby Gem Reference

## Core API

```ruby
KDL.parse(string, **options)        # Parse KDL string
KDL.load_file(path, **options)      # Load from file
KDL.build { ... }                   # Build document via DSL

# Global defaults
KDL.default_version = 2             # Parser version (1 or 2)
KDL.default_output_version = 2      # Serialization version
```

### Parse Options

| Option | Type | Description |
|--------|------|-------------|
| `version:` | `1\|2\|nil` | Force parser version or auto-detect |
| `output_version:` | `1\|2` | Serialization version |
| `parse_types:` | `Boolean` | Enable type parsing (default: true) |
| `type_parsers:` | `Hash` | Custom type handlers |

## KDL.build DSL

```ruby
doc = KDL.build do
  # Method 1: explicit node()
  node "name", arg1, arg2, prop: val, type: "annotation" do
    child_node
  end

  # Method 2: _ alias (useful when name conflicts with Ruby methods)
  _ "if", condition: true

  # Method 3: method_missing - any undefined method becomes node name
  package do
    name "my-app"
    version "1.0.0"
  end

  # Within a node block:
  arg value, type: "annotation"     # Add positional argument
  prop key, value, type: "ann"      # Add property
end
```

### Builder Shorthand

Arguments and properties can be mixed inline:

```ruby
node "pokemon", "snorlax", level: 10, { "type" => "normal" }, "jigglypuff"
# => pokemon snorlax jigglypuff type=normal level=10
```

Hash arguments become properties; other args become positional.

## Data Structures

### Document

```ruby
doc = KDL::Document.new(nodes = [])
doc[0]              # Node by index
doc["name"]         # First node with name (String or Symbol)
doc[:name]          # Same
doc.arg(:node_name) # First arg of named node's first arg
doc.args(:name)     # All args of named node
doc.each { |node| } # Iterate nodes (Enumerable)
doc.to_s            # Serialize to KDL string
```

### Node

```ruby
node = KDL::Node.new(name, arguments: [], properties: {}, children: [], type: nil)

# Access
node[0]              # Argument by index
node["prop"]         # Property by name (String or Symbol)
node[:prop]          # Same
node.name            # Node name
node.type            # Type annotation or nil
node.arguments       # Array of Values
node.properties      # Hash of String => Value
node.children        # Array of child Nodes

# Child access
node.child(0)        # Child by index
node.child(:name)    # First child with name
node.arg(:child)     # First arg of named child
node.args(:child)    # All args of named child
node.dash_vals(:child) # All first args of "-" children under named child

# Mutation
node << child_node   # Append child
node.each { |c| }    # Iterate children (Enumerable)
```

### Value Classes

```ruby
KDL::Value::Int.new(42)
KDL::Value::Float.new(3.14)
KDL::Value::Boolean.new(true)
KDL::Value::String.new("text")
KDL::Value::Null                   # Singleton
KDL::Value::Custom                 # Base for typed values

# Auto-conversion from Ruby
KDL::Value.from(42)      # => Int
KDL::Value.from(3.14)    # => Float
KDL::Value.from(true)    # => Boolean
KDL::Value.from("text")  # => String
KDL::Value.from(nil)     # => Null
```

Value instances delegate unknown methods to underlying `.value`:

```ruby
KDL::Value::String.new("foo").upcase  # => "FOO"
```

### Special Float Values

```ruby
KDL::Value::Float.new(Float::INFINITY)   # => #inf
KDL::Value::Float.new(-Float::INFINITY)  # => #-inf
KDL::Value::Float.new(Float::NAN)        # => #nan
```

## type_parsers

Custom type handlers transform annotated values/nodes during parsing.

```ruby
doc = KDL.parse(kdl_string, type_parsers: {
  'my-type' => ->(value, type) {
    return nil unless value.is_a?(KDL::Value::String)
    MyCustomValue.new(value.value, type: type)
  }
})
```

### Parser Callable Contract

```ruby
# Signature
def call(value_or_node, type_string)
  # For values: value_or_node is KDL::Value subclass
  # For nodes: value_or_node is KDL::Node

  # Return:
  # - KDL::Value::Custom instance (for value parsers)
  # - KDL::Node::Custom instance (for node parsers)
  # - nil (keeps original with type annotation set)
end
```

### Creating Custom Types

```ruby
class MyValue < KDL::Value::Custom
  def self.call(value, type = 'my-type')
    return nil unless value.is_a?(KDL::Value::String)
    new(transformed_value, type: type)
  end
end

class MyNode < KDL::Node::Custom
  def self.call(node, type = 'my-node')
    new(node.name,
        arguments: node.arguments,
        properties: node.properties,
        children: node.children,
        type: type)
  end
end
```

### Built-in Type Parsers

All registered in `KDL::Types::MAPPING`:

| Type | Ruby Class | Notes |
|------|------------|-------|
| `date-time` | Time | ISO8601 |
| `date` | Date | ISO8601 |
| `time` | - | ISO8601 time portion |
| `duration` | - | ISO8601 duration (P1Y2M3D) |
| `decimal` | BigDecimal | Arbitrary precision |
| `currency` | - | ISO4217 codes |
| `country-2` | - | ISO3166-1 alpha-2 |
| `country-3` | - | ISO3166-1 alpha-3 |
| `country-subdivision` | - | ISO3166-2 |
| `ipv4` | IPAddr | |
| `ipv6` | IPAddr | |
| `url` | URI | Requires scheme |
| `url-reference` | URI | Allows relative |
| `irl` | - | Internationalized URL |
| `irl-reference` | - | Internationalized, relative OK |
| `url-template` | - | RFC6570 |
| `uuid` | - | RFC4122 |
| `regex` | Regexp | |
| `base64` | String | Decoded |
| `hostname` | - | |
| `idn-hostname` | - | Internationalized |
| `email` | - | |
| `idn-email` | - | Internationalized |

Disable with `parse_types: false`.

## as_type Method

Both `Node` and `Value` support `as_type`:

```ruby
# Annotation only (no transformation)
node.as_type("my-type")
# => Sets node.type = "my-type", returns self

value.as_type("my-type")
# => Returns new Value with type set

# With parser (transformation)
node.as_type("my-type", MyNodeParser)
# => Calls MyNodeParser.call(node, "my-type")
# => Returns Custom instance or falls back to annotation-only if nil

value.as_type("my-type", MyValueParser)
# => Calls MyValueParser.call(value, "my-type")
# => Returns Custom instance or falls back to annotation-only if nil
```

### Parser Return Behavior

| Return Value | Result |
|--------------|--------|
| `Custom` instance | Used as result |
| `nil` | Original with type annotation |
| Other | Raises `ArgumentError` |

## Version Conversion

```ruby
doc.to_v1  # Convert to v1 format
doc.to_v2  # Convert to v2 format
doc.version  # Current version (1 or 2)

# Same for Node and Value
node.to_v1
value.to_v2
```

### v1 vs v2 Differences

| Feature | v1 | v2 |
|---------|----|----|
| Boolean | `true`/`false` | `#true`/`#false` |
| Null | `null` | `#null` |
| Infinity | N/A | `#inf`/`#-inf` |
| NaN | N/A | `#nan` |
| Multiline strings | Escape sequences | Triple-quoted `"""` |

## Serialization

```ruby
doc.to_s     # KDL string output
node.to_s    # Single node as string
value.to_s   # Value as string (quotes if needed)

node.inspect  # Debug format (always quoted strings)
value.inspect # Debug format
```

## Error Handling

All parse errors raise subclasses of `KDL::Error` (which inherits from `StandardError`). Each error exposes `filename`, `line`, and `column` attributes.

```ruby
begin
  doc = KDL.parse(untrusted_input)
rescue KDL::ParseError => e
  # Syntax error during parsing
  puts "Parse error at line #{e.line}, column #{e.column}: #{e.message}"
rescue KDL::VersionMismatchError => e
  # Document version does not match parser version
  puts "Version mismatch: expected v#{e.parser_version}, got v#{e.version}"
rescue KDL::UnsupportedVersionError => e
  # Unrecognized KDL version marker
  puts "Unsupported version: #{e.version}"
rescue KDL::Error => e
  # Catch-all for any other KDL error
  puts "KDL error: #{e.message}"
end
```

### Error Hierarchy

| Class | When Raised |
|-------|-------------|
| `KDL::Error` | Base class for all KDL errors |
| `KDL::ParseError` | Invalid KDL syntax |
| `KDL::VersionMismatchError` | Document version conflicts with forced parser version |
| `KDL::UnsupportedVersionError` | Unrecognized version marker in document |

### File Loading Errors

`KDL.load_file` can also raise `Errno::ENOENT` (file not found) or `Errno::EACCES` (permission denied) before KDL parsing begins.

```ruby
begin
  config = KDL.load_file("config.kdl")
rescue Errno::ENOENT
  puts "Config file not found"
rescue KDL::Error => e
  puts "Invalid KDL: #{e.message}"
end
```

## Equality

Values compare equal to their underlying Ruby values:

```ruby
KDL::Value::Int.new(42) == 42          # true
KDL::Value::String.new("foo") == "foo" # true
KDL::Value::Null == nil                # true
```
