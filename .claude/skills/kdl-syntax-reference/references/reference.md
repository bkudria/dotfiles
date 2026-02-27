# KDL v2 Reference

Compact syntax reference for KDL v2 covering nodes, values, strings, numbers, keywords, properties, comments, type annotations, identifiers, escapes, and whitespace rules. Consult this file when writing or reviewing any KDL document, debugging parse errors, or verifying syntax details.

Optional version marker: `/- kdl-version 2` can precede document content.

## Nodes

```kdl
node-name arg1 arg2 prop=value {
    child-node
}
```

- Name + optional arguments + optional properties + optional children
- Multiple nodes per line: `node1; node2; node3`
- Node terminates at: newline, `;`, closing `}`, or EOF
- Line continuation: `\` + optional whitespace + optional `// comment` + newline

## Values

### Strings

**Bare identifiers**: `foo`, `my-value` - unquoted strings (same rules as identifier names)

**Quoted**: `"hello\nworld"` - supports escapes

**Raw**: `#"C:\path"#` - no escape processing; add `#` to disambiguate: `##"has "# inside"##`; cannot represent disallowed code points

**Multiline** (quoted and raw): literal newline required after opening `"""`; closing `"""` must be on whitespace-only line

```kdl
text """
    first line
    second line
    """

raw-text #"""
    no \n escape processing
    """#
```

Both quoted and raw multiline strings auto-dedent to closing `"""` indentation.

### Numbers

```kdl
decimal     1_000_000
positive    +42
negative    -3.14
float       1.234e-42
hex         0xdeadbeef
octal       0o755
binary      0b10101101
```

- `.1` is **illegal** - must have integer digit (use `0.1`)
- `+`/`-` prefix allowed on all numeric formats
- Special (no sign prefix): `#inf`, `#-inf`, `#nan`

### Keywords

`#true` `#false` `#null`

## Properties & Arguments

```kdl
node arg1 prop1=val1 arg2 prop2=val2
node spaced = "value"
```

- Whitespace allowed around `=` in properties
- Arguments: order preserved
- Properties: order not guaranteed; duplicate keys → rightmost wins (parsers MUST use rightmost)

## Comments

```kdl
// line comment
/* block comment (nestable) */
/-commented-node arg prop=val
node /-commented-arg kept-arg
node /-prop=val kept=val
/-{ commented-children }
```

Slashdash `/-` comments out next element:
- Node (with all args/props/children)
- Argument value
- Entire property (key=value together, not value-only)
- Children block `{}`

Whitespace/newlines/comments allowed between `/-` and target.

Note: A slashdashed children block prevents any non-slashdashed children block on the same node.

## Type Annotations

```kdl
node (u8)255 (f32)1.5
node uuid=(uuid)"123e4567-e89b-12d3-a456-426614174000"
(mytype)node-name
```

Whitespace allowed inside parens and between annotation and target: `( u8 )255`, `(type) node`, `( type ) node`

**Reserved type annotations:**
- Integers: `i8`, `i16`, `i32`, `i64`, `i128`, `u8`, `u16`, `u32`, `u64`, `u128`, `isize`, `usize`
- Floats: `f32`, `f64`, `decimal64`, `decimal128`
- Strings: `date-time`, `date`, `time`, `duration`, `decimal`, `currency`, `country-2`, `country-3`, `country-subdivision`, `email`, `idn-email`, `hostname`, `idn-hostname`, `ipv4`, `ipv6`, `url`, `url-reference`, `irl`, `irl-reference`, `url-template`, `uuid`, `regex`, `base64`

## Identifiers

Node names and property keys can be:
- Bare: `my-node`, `prop_name`, Unicode allowed
- Quoted: `"node with spaces"`
- Raw: `#"weird#name"#`

**Bare identifier restrictions:**
- Cannot contain: `= { } ( ) [ ] / \ " # ;` or whitespace (both forward-slash `/` and backslash `\` prohibited)
- Cannot start with digit
- Cannot be reserved: `true`, `false`, `null`, `inf`, `-inf`, `nan` 
- `+`/`-` start: second char cannot be digit; if second is `.`, third cannot be digit
- `.` start: second char cannot be digit
- Valid identifier: 
```kdl
-<123~!$@%^&*,.:'`|?+>
```

## String Escapes

In quoted/multiline strings:

`\n` `\r` `\t` `\\` `\"` `\b` `\f` `\s` `\u{1-6 hex}`

Whitespace escape: `\` + whitespace/newlines → consumed (for line continuation in strings).

## Whitespace & Encoding

- UTF-8 required
- Newlines: LF, CR, CRLF (normalized to LF), NEL (U+0085), VT (U+000B), FF (U+000C), LS (U+2028), PS (U+2029)
- Nodes separated by newlines or `;`

## Disallowed Code Points

The following cannot appear literally in KDL documents:
- Control characters: U+0000-0008, U+000E-001F, U+007F
- Surrogates: U+D800-DFFF
- Bidirectional controls: U+200E-200F, U+202A-202E, U+2066-2069
- BOM (U+FEFF) except at document start
