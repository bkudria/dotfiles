# CLI Reference

All jq command-line flags and invocation patterns.

---

## Basic Invocation

```bash
jq [OPTIONS] FILTER [FILE...]

# From stdin
echo '{"a":1}' | jq '.a'

# From file(s)
jq '.name' users.json

# Multiple files
jq '.count' file1.json file2.json

# Inline filter
jq '.' data.json

# Filter from file
jq -f transform.jq data.json
```

---

## Output Flags

| Flag | Long | Description |
|------|------|-------------|
| `-c` | `--compact-output` | Compact output (no pretty-printing) |
| `-r` | `--raw-output` | Output raw strings (no JSON quotes) |
| `--raw-output0` | | Like `-r` but NUL-delimited (for `xargs -0`) |
| `-j` | `--join-output` | Like `-r` but no trailing newline |
| `-S` | `--sort-keys` | Sort object keys in output |
| `--tab` | | Use tabs for indentation |
| `--indent n` | | Use n spaces for indentation (default 2) |
| `-C` | `--color-output` | Force color output |
| `-M` | `--monochrome-output` | Disable color output |
| `--jsonargs` | | Remaining args are JSON (into `$ARGS.positional`) |

---

## Input Flags

| Flag | Long | Description |
|------|------|-------------|
| `-n` | `--null-input` | Don't read input; use `null` as input. Use with `input`/`inputs`. |
| `-R` | `--raw-input` | Read each line as a raw string instead of JSON |
| `-s` | `--slurp` | Read all inputs into a single array |
| `--slurpfile name f` | | Read file f as JSON array, bind to `$name` |
| `--rawfile name f` | | Read file f as raw string, bind to `$name` |
| `-f` | `--from-file f` | Read filter from file f |
| `--args` | | Remaining args are strings (into `$ARGS.positional`) |
| `--jsonargs` | | Remaining args are JSON (into `$ARGS.positional`) |

---

## Argument Passing

| Flag | Description | Example |
|------|-------------|---------|
| `--arg name val` | Bind string val to `$name` | `--arg user "alice"` |
| `--argjson name val` | Bind JSON val to `$name` | `--argjson count 42` |
| `--args` | Remaining args → `$ARGS.positional[]` (strings) | `--args "a" "b"` |
| `--jsonargs` | Remaining args → `$ARGS.positional[]` (JSON) | `--jsonargs '{"x":1}'` |
| `--slurpfile name f` | Bind file content as JSON array to `$name` | `--slurpfile data config.json` |
| `--rawfile name f` | Bind file content as string to `$name` | `--rawfile tmpl template.txt` |

---

## Other Flags

| Flag | Long | Description |
|------|------|-------------|
| `-e` | `--exit-status` | Exit 0 if last output is truthy, 1 if falsey, 5 if no output |
| `-L dir` | | Prepend dir to module search path |
| `--seq` | | Use application/json-seq MIME type (RFC 7464) |
| `--stream` | | Parse input in streaming fashion (path-value pairs) |
| `--stream-errors` | | Like `--stream` but emit errors as data |

---

## Common Invocation Patterns

### Process NDJSON (newline-delimited JSON)

```bash
# Filter NDJSON lines
cat events.jsonl | jq 'select(.level == "error")'

# Collect all NDJSON into an array
jq -s '.' events.jsonl

# Using inputs (more memory-efficient)
jq -n '[inputs | select(.level == "error")]' events.jsonl
```

### Read raw text and process

```bash
# Read each line as a string
cat file.txt | jq -R 'split(",") | {name: .[0], age: .[1] | tonumber}'

# Slurp all lines into one string
cat file.txt | jq -Rs 'split("\n") | map(select(length > 0))'
```

### Pass external data

```bash
# String argument
jq --arg name "$USER" '.users[] | select(.name == $name)' db.json

# JSON argument (number, boolean, array, object)
jq --argjson threshold 50 '.[] | select(.score > $threshold)' scores.json

# Load another file
jq --slurpfile config config.json '.settings = $config[0]' app.json

# Multiple positional arguments
jq -n '$ARGS.positional | map(tonumber) | add' --args "1" "2" "3"
```

### Combine with shell tools

```bash
# Process each output with xargs
jq -r '.urls[]' config.json | xargs -I{} curl -s "{}"

# NUL-delimited for filenames with spaces
jq -r '.files[]' manifest.json --raw-output0 | xargs -0 rm

# Build shell commands
jq -r '.users[] | "useradd \(.name | @sh)"' users.json | sh

# Process output in bash loop
while IFS= read -r line; do
    echo "Processing: $line"
done < <(jq -r '.items[].name' data.json)
```

### Multiple file processing

```bash
# Process each file separately (default)
jq '.count' file1.json file2.json
# Outputs count from each file

# Merge all files
jq -s 'add' file1.json file2.json

# Join data from two files
jq -s '.[0].users as $u | .[1].orders | map(. + {user: $u[.uid]})' users.json orders.json

# Using --slurpfile for secondary data
jq --slurpfile users users.json '.orders | map(. + {user: $users[0][.uid]})' orders.json
```

### Output formats

```bash
# Raw string output (strip JSON quotes)
jq -r '.name' data.json

# CSV output
jq -r '.[] | [.name, .age, .email] | @csv' users.json

# TSV output
jq -r '.[] | [.name, .age] | @tsv' users.json

# Shell-safe output
jq -r '.filename | @sh' config.json

# Compact JSON (for piping)
jq -c '.' data.json

# Sorted keys (for diffing)
jq -S '.' data.json
```

---

## Environment Variables

| Variable | Description |
|----------|-------------|
| `JQ_COLORS` | Colon-separated ANSI color codes for output (null:false:true:number:string:array:object:key) |
| `$ENV` | Object of all environment variables accessible in filters |
| `HOME` | Used for `~/.jq` module search path |
| `ORIGIN` | jq executable directory (for module search) |

```bash
# Custom colors
JQ_COLORS="1;31:0;33:0;33:0;33:0;32:1;39:1;39:1;34" jq '.' data.json

# Access env vars in filters
jq -n '$ENV.HOME'
jq -n 'env.USER'
```
