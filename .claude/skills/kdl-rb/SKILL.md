---
name: kdl-rb
description: KDL Ruby gem API reference for parsing, building, and manipulating KDL documents in Ruby. Use when parsing KDL with KDL.parse or KDL.load_file, building KDL documents with KDL.build DSL, accessing KDL nodes/arguments/properties/children, creating custom type parsers, converting between KDL v1 and v2 formats, or debugging KDL Ruby gem issues. Complements the kdl-syntax-reference skill which covers KDL language syntax.
---

# KDL Ruby Gem Reference

Ruby gem API for parsing, building, and manipulating KDL documents.

## When This Skill Applies

- Parsing KDL strings or files with `KDL.parse` / `KDL.load_file`
- Building KDL documents programmatically with `KDL.build`
- Accessing node data: arguments, properties, children
- Creating custom type parsers for annotated values
- Converting between KDL v1 and v2 formats
- Understanding KDL Value/Node class hierarchy
- Debugging KDL parsing or serialization issues

## Quick Reference

| API | Purpose |
|-----|---------|
| `KDL.parse(str)` | Parse KDL string to Document |
| `KDL.load_file(path)` | Parse KDL file |
| `KDL.build { ... }` | Build Document via DSL |
| `doc["name"]` / `doc[:name]` | First node by name |
| `node[0]` | Argument by index |
| `node["prop"]` / `node[:prop]` | Property by name |
| `node.children` | Child nodes array |
| `node.child(:name)` | First child by name |
| `node.arg(:child)` | First arg of named child |
| `KDL::Value.from(ruby_val)` | Auto-convert Ruby value |
| `doc.to_s` | Serialize to KDL string |

## Relationship to kdl-syntax-reference

- **kdl-syntax-reference**: KDL v2 language syntax (nodes, values, strings, comments)
- **kdl-rb** (this skill): Ruby gem API for parsing/building/manipulating KDL

## Common Examples

### Parsing
```ruby
doc = KDL.parse(<<~KDL)
  server "main" port=8080 {
    host "localhost"
    tls true
  }
KDL

doc["server"][0]         # => "main" (first argument)
doc["server"]["port"]    # => 8080 (property)
doc["server"].child(:host).arg(0) # => "localhost"
```

### Building
```ruby
doc = KDL.build do
  package do
    name "my-app"
    version "1.0.0"
    dependencies do
      node "rails", "~> 7.0"
      node "puma", "~> 6.0"
    end
  end
end

puts doc.to_s
```

### Loading files
```ruby
config = KDL.load_file("config.kdl")
db_host = config["database"].child(:host).arg(0)
```

## Dependencies

- **kdl** Ruby gem — `gem install kdl` or add `gem "kdl"` to Gemfile

## Instructions

Consult `references/reference.md` for complete API details including KDL.build DSL, data structures (Document, Node, Value), custom type parsers, built-in type mappings, version conversion, and serialization.

## Reference Files

| File | Purpose |
|------|---------|
| `references/reference.md` | Complete KDL Ruby gem API reference |
