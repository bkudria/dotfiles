---
name: kdl-syntax-reference
description: KDL v2 syntax reference for writing correct KDL documents. Use when writing KDL code blocks in responses, proposing changes to .kdl files, designing KDL syntax or node structure, reviewing or analyzing KDL documents, debugging KDL parsing errors, or answering questions about KDL nodes, values, strings, numbers, properties, arguments, comments, type annotations, identifiers, or escapes. Also covers JSON-IN-KDL (JiK/JIK) - the microsyntax for encoding JSON within KDL.
---

# KDL v2 Syntax Reference

Provides comprehensive KDL v2 syntax guidance for writing correct KDL documents.

## When This Skill Applies

This skill helps with:
- Writing KDL code blocks (in responses, examples, or illustrations)
- Proposing changes to `.kdl` files
- Designing or suggesting KDL syntax/structure
- Reviewing or analyzing existing KDL documents
- Debugging KDL parsing errors
- Answering questions about KDL features
- Encoding or decoding JSON in KDL (JSON-IN-KDL / JiK)

## Quick Reference

| Area | Description |
|------|-------------|
| **Nodes** | Structure with arguments, properties, and optional children |
| **Values** | Strings (bare, quoted, raw, multiline), numbers, keywords |
| **Type Annotations** | Reserved types for integers, floats, and strings |
| **Comments** | Line (`//`), block (`/* */`), slashdash (`/-`) |
| **Identifiers** | Bare, quoted, and raw forms with specific restrictions |
| **JSON-IN-KDL (JiK)** | Microsyntax for losslessly encoding JSON within KDL |

## Quick KDL Examples

### Node with arguments and properties
```kdl
title "Hello World"
server "main" port=8080 tls=true
```

### Node with children
```kdl
database {
  host "localhost"
  port 5432
  name "myapp"
}
```

### Type annotations
```kdl
published (date)"2024-01-15"
timeout (duration)"30s"
```

### Multiline strings
```kdl
description """
  This is a multiline
  raw string in KDL v2
  """
```

### Comments
```kdl
// Line comment
node "value" /* inline comment */ key="val"
/- disabled-node "skipped"
```

## Instructions

1. Consult `references/reference.md` for complete KDL v2 syntax details before writing KDL
2. Provide code examples using `kdl` code blocks
3. Reference specific sections when citing syntax rules
4. Common gotchas: `.1` is illegal (use `0.1`), bare identifiers have restrictions
5. Consult `references/json-in-kdl.md` when encoding JSON in KDL, converting between JSON and JiK, or using `(array)`/`(object)` type annotations for JiK

## Dependencies

- [KDL v2 specification](https://kdl.dev) -- the authoritative language specification. Consult for edge cases or features not covered in this skill's reference files.

## Reference Files

| File | Contents |
|------|----------|
| `references/reference.md` | Complete KDL v2 syntax specification |
| `references/json-in-kdl.md` | JSON-IN-KDL (JiK) microsyntax reference |
