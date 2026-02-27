# CLI Reference

Command-line invocation patterns, commands, and flags for mikefarah/yq v4.

## Commands

### eval (default)

Process each file sequentially and independently. Default command since v4.18.1.

```bash
yq 'expression' file.yaml              # evaluate expression
yq file.yaml                           # display file
yq 'expression' f1.yml f2.yml          # process multiple files sequentially
cat file.yml | yq 'expression'         # pipe input
cat file.yml | yq 'expression' f1.yml - f2.yml  # combine pipe and files
yq -n '.a.b = "cat"'                   # null input: generate YAML
yq -i '.a = "new"' file.yaml           # in-place modification
```

Aliases: `eval`, `e`

### eval-all

Load ALL files/documents into memory for cross-file operations.

```bash
yq eval-all 'expression' f1.yml f2.yml
yq ea 'expression' f1.yml f2.yml       # short alias
```

Use `eval-all` when the expression references multiple files (merging, comparing). Higher memory usage than `eval`.

**Key pattern — file references:**

```bash
select(fileIndex == 0)    # or: select(fi == 0)
select(fileIndex == 1)    # or: select(fi == 1)
```

**Common merge pattern:**

```bash
yq eval-all 'select(fi == 0) * select(fi == 1)' base.yml overlay.yml
```

**In-place merge (modifies first file):**

```bash
yq eval-all --inplace 'select(fi == 0) * select(fi == 1)' base.yml overlay.yml
```

## Flags

### Output Flags

| Flag | Short | Description |
|------|-------|-------------|
| `--output-format=FMT` | `-o=FMT` | Output format: `yaml`, `json`, `xml`, `props`, `csv`, `tsv` |
| `--indent N` | `-I N` | Indentation level (default: 2) |
| `--prettyPrint` | `-P` | Idiomatic formatting (shorthand for `... style=""`) |
| `--unwrapScalar` | | Unwrap scalar values (default: true) |
| `--no-doc` | `-N` | Omit document separators (`---`) |
| `--colors` | `-C` | Force colored output |
| `--no-colors` | `-M` | Force no colors |
| `--tojson` | `-j` | Output JSON (legacy; prefer `-o=json`) |

### Input Flags

| Flag | Short | Description |
|------|-------|-------------|
| `--input-format=FMT` | `-p=FMT` | Parse input as: `yaml`, `json`, `xml`, `props`, `csv`, `tsv` |
| `--null-input` | `-n` | Evaluate without reading input |

### Behavior Flags

| Flag | Short | Description |
|------|-------|-------------|
| `--inplace` | `-i` | Modify first file directly |
| `--exit-status` | `-e` | Set exit code if no matches or result is null/false |
| `--verbose` | `-v` | Verbose output |

### Format-Specific Flags

| Flag | Default | Description |
|------|---------|-------------|
| `--xml-attribute-prefix` | `+@` | Prefix for XML attribute fields |
| `--xml-content-name` | `+content` | Label for text content in mixed elements |
| `--xml-directive-name` | `+directive` | Label for DOCTYPE declarations |
| `--xml-proc-inst-prefix` | `+p_` | Prefix for processing instructions |
| `--xml-strict-mode` | false | Enforce XML spec compliance |
| `--xml-keep-namespace` | true | Preserve namespace prefixes |
| `--xml-raw-token` | true | Skip namespace URL translation |
| `--xml-skip-proc-inst` | false | Omit processing instructions |
| `--xml-skip-directives` | false | Omit DOCTYPE declarations |
| `--properties-separator` | ` = ` | Key-value separator for properties output |
| `--properties-array-brackets` | false | Use `key[0]` instead of `key.0` (Spring Boot) |
| `--csv-auto-parse` | true | Auto-parse YAML/JSON in CSV cells |

### YAML Behavior Flags

| Flag | Default | Description |
|------|---------|-------------|
| `--yaml-fix-merge-anchor-to-spec` | false | Fix merge anchor behavior per YAML 1.2 spec |

### Security Flags

| Flag | Description |
|------|-------------|
| `--security-disable-env-ops` | Disable all environment variable operations |
| `--security-disable-file-ops` | Disable all file load operations |

## Common Invocation Patterns

### Read a value

```bash
yq '.path.to.value' file.yaml
```

### In-place edit

```bash
yq -i '.path = "value"' file.yaml
```

### Pipe from stdin

```bash
kubectl get deploy -o yaml | yq '.items[].metadata.name'
echo '{"a": 1}' | yq -p=json '.a'
```

### Generate YAML (no input)

```bash
yq -n '.a.b = "cat" | .x.y = 42'
```

### Validate YAML

```bash
yq --exit-status 'tag == "!!map" or tag == "!!seq"' file.yaml > /dev/null
```

### Multi-line expressions

```bash
yq --inplace '
  with(.a.deeply.nested;
    . = "newValue" | . style="single"
  ) |
  with(.b.another;
    . = "cool" | . style="folded"
  )
' file.yaml
```

### Update multiple files

```bash
find . -name '*.yaml' -exec yq -i '.version = "2.0"' {} \;
```

### Multiple stdin streams (process substitution)

```bash
yq '.key' <(curl -s https://example.com/data.yaml) <(cat local.yml)
```

### Combine multiple YAML files

```bash
yq '.' dir/*.yaml          # outputs with --- separators
```

### Bash array from YAML

```bash
readarray actions < <(yq '.actions[]' config.yaml)
```

### Loop over YAML in bash

```bash
readarray items < <(yq -o=j -I=0 '.items[]' data.yml)
for item in "${items[@]}"; do
    name=$(echo "$item" | yq '.name' -)
    echo "Processing: $name"
done
```

### Compare YAML files

```bash
diff <(yq -P 'sort_keys(..)' -o=props f1.yml) <(yq -P 'sort_keys(..)' -o=props f2.yml)
```

### Special characters in values

```bash
VAL='.a |!@ == "complex"' yq '.a = strenv(VAL)' file.yaml
```
