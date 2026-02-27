---
name: yq
description: "yq (mikefarah) CLI reference for querying, transforming, and converting YAML, JSON, XML, Properties, and CSV/TSV files. Use when writing yq expressions, transforming YAML files, merging YAML documents, converting between data formats with yq, debugging yq syntax, or working with multi-document YAML."
---

# yq

Command-line YAML/JSON/XML processor reference. Covers mikefarah/yq v4 expression language, all operators, CLI invocation, format conversion, and YAML-specific features.

## When This Skill Applies

- Writing yq expressions to query or transform YAML files
- Merging multiple YAML files or documents
- Converting between YAML, JSON, XML, Properties, CSV, and TSV
- Processing Kubernetes manifests, Helm charts, or CI/CD configs
- Working with YAML anchors, aliases, comments, or multi-document files
- Debugging yq errors or unexpected output
- Reviewing yq usage in shell scripts

## Quick Reference

| Area | Key Concepts |
|------|-------------|
| Navigation | `.`, `.field`, `.a.b.c`, `.[]`, `.[0]`, `.["key with spaces"]` |
| Assignment | `=` (plain), `\|=` (relative), `+=`, `-=`, `*=` |
| Merge | `*` (deep), `*+` (append arrays), `*d` (deep arrays), `*?` (existing only), `*n` (new only) |
| Select/Filter | `select(condition)`, `has("key")`, `contains()`, `test("regex")` |
| String | `sub()`, `match()`, `capture()`, `test()`, `split()`, `join()`, `trim`, `upcase`, `downcase` |
| Array | `sort_by()`, `group_by()`, `unique_by()`, `flatten`, `reverse`, `map()`, `first`, `last` |
| Object | `keys`, `values`, `to_entries`, `from_entries`, `with_entries()`, `pick()`, `omit()` |
| Path | `path`, `setpath()`, `delpaths()`, `..` (recursive), `...` (recursive + keys) |
| Type | `tag` (read/set YAML type), `kind` (scalar/seq/map), `type` (alias for tag) |
| Comments | `line_comment`, `head_comment`, `foot_comment`, `... comments=""` |
| Reduce | `<exp> as $var ireduce (<init>; <block>)` |
| Multi-file | `eval-all` / `ea`, `fileIndex` / `fi`, `filename` |
| Formats | `-p=json\|xml\|props\|csv\|tsv`, `-o=json\|xml\|props\|csv\|tsv` |
| Load | `load()`, `load_xml()`, `load_props()`, `load_str()`, `load_base64()` |
| Env vars | `env(VAR)`, `strenv(VAR)`, `envsubst`, `envsubst(nu,ne,ff)` |
| Encode/Decode | `@base64` / `@base64d`, `@json` / `@jsond`, `@yaml` / `@yamld`, `@uri` / `@urid`, `@sh` |
| YAML features | `anchor`, `alias`, `explode()`, `style`, `di` (document index) |

## Key Patterns

### Query a nested field

```bash
yq '.spec.template.spec.containers[0].image' deployment.yaml
```

### Update a field in-place

```bash
yq -i '.metadata.labels.version = "v2"' deployment.yaml
```

### Find and update array items

```bash
yq '(.spec.containers[] | select(.name == "app") | .image) = "nginx:latest"' pod.yaml
```

### Merge two YAML files (second overrides first)

```bash
yq eval-all 'select(fileIndex == 0) * select(fileIndex == 1)' base.yaml overlay.yaml
```

### Merge with array append

```bash
yq eval-all 'select(fileIndex == 0) *+ select(fileIndex == 1)' base.yaml overlay.yaml
```

### Convert YAML to JSON

```bash
yq -o=json '.' config.yaml
```

### Create YAML from scratch

```bash
yq -n '.apiVersion = "v1" | .kind = "ConfigMap" | .metadata.name = "my-config"'
```

## Instructions

1. **This is mikefarah/yq, not kislyuk/yq.** The kislyuk/yq is a jq wrapper for YAML. mikefarah/yq has its own expression language. Do not use jq-style `-s` (slurp), `-y` (YAML output), or jq function syntax.
2. **Use `eval-all` (or `ea`) for multi-file operations.** Plain `eval` processes files sequentially and independently. `eval-all` loads all files into memory for cross-file operations like merging. Reference files with `select(fileIndex == N)` or `select(fi == N)`.
3. **Wrap LHS in parentheses for filtered updates.** `(.items[] | select(.name == "foo") | .value) = "bar"` — the outer parentheses are required, otherwise the update silently fails.
4. **`*` is the merge operator, not multiplication (except for numbers).** Object `*` does deep merge. Add flags: `*+` (append arrays), `*d` (deep merge arrays by index), `*?` (existing fields only), `*n` (new fields only). Flags combine: `*+?`.
5. **Type coercion uses `tag`, not `tonumber`/`tostring`.** Set `.a tag = "!!int"` to convert to integer, `.a tag = "!!str"` for string. `to_number` exists as an alias. Use `from_yaml` to auto-detect types from strings.
6. **`env()` parses as YAML, `strenv()` keeps as string.** `myvar="true" yq '.a = env(myvar)'` produces `a: true` (boolean). Use `strenv(myvar)` to get `a: "true"` (string).
7. **Recursive descent: `..` excludes keys, `...` includes keys.** `.. style="flow"` sets style on values only. `... comments=""` removes all comments including those on map keys.
8. **`ireduce` not `reduce`.** yq uses infix notation: `.[] as $x ireduce (0; . + $x)`.
9. **YAML truthiness differs from jq.** Only `null` and `false` are falsy. Empty strings, `0`, empty arrays/objects are all truthy.
10. **Use `with()` for scoped updates.** `with(.a.deeply.nested; . = "new" | . style="single")` avoids repeating long paths.

## Common Gotchas

| Gotcha | Explanation |
|--------|-------------|
| `eval` vs `eval-all` | `eval` processes files one at a time; `eval-all` loads all for cross-file ops |
| Missing parentheses on LHS | `(.arr[] \| select(.x == 1) \| .y) = "z"` — outer parens required |
| `*` replaces arrays by default | Use `*+` to append arrays during merge |
| `sub()` exists in yq | Unlike early versions, v4 has `sub()` and `match()` but syntax differs from jq |
| `//` is alternative, not regex or OR | `.a // "default"` returns `.a` if truthy, else `"default"` |
| Whitespace in keys | Use bracket notation: `.["my key"]` or `.my_key` |
| `-i` modifies first file only | In-place edit applies only to the first file argument |
| Multi-doc needs `---` | Multiple YAML docs in one file separated by `---` |
| `di` for document index | `select(di == 1)` targets the second document in a multi-doc file |
| XML values are strings | After parsing XML, use `from_yaml` to convert types: `(.. \| select(tag == "!!str")) \|= from_yaml` |

## Reference Files

| File | Purpose |
|------|---------|
| `references/operators.md` | Complete operator catalog grouped by category with syntax and examples |
| `references/cli-reference.md` | Commands, flags, input/output modes, invocation patterns |
| `references/format-conversion.md` | Converting between YAML, JSON, XML, Properties, CSV, and TSV |
| `references/yaml-features.md` | Anchors, aliases, comments, multi-document, tags, styles |
| `references/cookbook.md` | Practical recipes for common YAML transformation tasks |
| `references/gotchas.md` | Common mistakes, jq-vs-yq differences, and tricky behaviors |
