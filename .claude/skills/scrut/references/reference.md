# Scrut AI Agent Reference

CLI testing framework for terminal programs. Tests live in Markdown (`.md`) or Cram (`.t`) files.

## Core Commands
```bash
scrut test <file.md>                    # Run single test file
scrut test <dir>/                       # Run all tests in directory
scrut test -r pretty <file.md>          # Default output renderer
scrut test -r diff <file.md>            # Unified diff output
scrut test --absolute-line-numbers      # Show absolute line numbers

scrut update <file.md>                  # Update expectations (creates .new file)
scrut update -r <file.md>               # Update in-place
scrut update -r -y <file.md>            # Update without confirmation

scrut create --output test.md -- cmd    # Create test from command
echo "cmd" | scrut create -             # Create test from stdin
```

## Test Document Structure

**Important**: Each `scrut` code block can contain ONLY ONE command/output pair. Multiple commands require multiple separate `scrut` blocks.

### Markdown Format
````markdown
---
# Document-level configuration (optional)
total_timeout: 30s
shell: /bin/bash
prepend: ["setup.md"]
append: ["teardown.md"]
defaults:
  timeout: 5s
  environment:
    FOO: "bar"
---

# Test Title (optional)

Test description (optional)

```scrut
$ command
expected output
```

## Another test case

```scrut {timeout: 10s, environment: {"VAR": "value"}}
$ command with inline config
output
```
````

### Shell Expressions
Each example shows different shell expression types (each would be in its own scrut block):

```scrut
$ simple command                        # Basic command
output
```

```scrut
$ cmd1 && cmd2                         # Command chaining
combined output
```

```scrut
$ cmd1 || cmd2                         # Alternative execution
output from successful command
```

```scrut
$ long command \                       # Line continuation
> with multiple \
> lines
output
```

```scrut
$ cmd1 | cmd2 | cmd3                   # Piping
final output
```

## Output Expectations

By default, output expectations match against STDOUT. To test against STDERR, use the `output_stream` configuration directive in the code block language flag.

### Types & Syntax
```
output                                  # Equal (default): exact match
output (equal)                         # Explicit equal
output (glob)                          # Glob: * = any chars, ? = one char
output (regex)                         # Regex: full line match with ^...$
output (escaped)                       # Escaped: \t \x00 \xAB etc
output (no-eol)                        # No end-of-line (no trailing \n)
```

### Quantifiers
```
output (?)                             # Zero or one line
output (*)                             # Zero or more lines
output (+)                             # One or more lines
output (equal+)                        # Explicit type with quantifier
output (glob*)                         # Glob with quantifier
output (regex?)                        # Regex with quantifier
```

### Examples
```scrut
$ echo -e "foo\nbar\nbaz"
foo                                    # Exact match first line
ba* (glob)                            # Glob match second line
.*z$ (regex)                          # Regex match third line
```

```scrut
$ echo -n "no newline"
no newline (no-eol)                   # No trailing newline
```

```scrut
$ printf "tab\tseparated"
tab\tseparated (escaped)              # Tab character
```

```scrut
$ echo -e "line1\nline1\nline2"
line1 (+)                             # One or more "line1"
line2                                 # Followed by "line2"
```

### Testing STDERR
```scrut {output_stream: stderr}
$ >&2 echo "error message"
error message
```

```scrut {output_stream: stderr}
$ command-that-fails 2>&1
Error: command not found
```

### Edge Cases
```scrut
$ echo "output (equal)"
output (equal) (equal)                # When output contains expectation syntax
```

## Exit Codes
```scrut
$ true                                # Default expects exit code 0
$ false
[1]                                   # Expect exit code 1
$ exit 42
[42]                                  # Expect exit code 42
```

## Configuration

### Document-Level (Front Matter)
```yaml
---
shell: /bin/zsh                       # Custom shell
total_timeout: 60s                    # Max time for all tests
prepend: ["setup.md", "env.md"]       # Execute before tests
append: ["cleanup.md"]                # Execute after tests
defaults:                             # Default test config
  timeout: 10s
  environment:
    PATH: "/custom/path:$PATH"
  output_stream: combined
---
```

### Test-Level (Inline)
```scrut {option: value}
$ command
```

**Options:**
- `detached: true` - Don't validate output/exit code
- `detached_kill_signal: "term"` - Signal for detached process (term/int/kill)
- `environment: {"KEY": "value"}` - Set env vars
- `keep_crlf: true` - Preserve Windows line endings
- `output_stream: "stderr"` - Choose stdout/stderr/combined
- `shell: "/bin/zsh"` - Custom shell for this test
- `skip_document_code: 2` - Skip document if exit code matches
- `strip_ansi_escaping: true` - Remove ANSI codes
- `timeout: "30s"` - Test timeout (ms or duration)
- `wait: {timeout: "5s", path: "file"}` - Wait for condition

## Environment Variables

### Provided by Scrut
```bash
$TESTDIR                              # Directory containing test file
$TESTFILE                             # Test document filename
$TESTSHELL                            # Shell path (/bin/bash)
$TMPDIR                               # Temp directory (auto-cleaned)
$SCRUT_TEST                           # test.md:123 (file:line)
$PWD                                  # Working directory (per-document)

# Set by Scrut
CDPATH=""                             # Empty
COLUMNS=80                            # Fixed width
LANG=C LANGUAGE=C LC_ALL=C            # Locale
TZ=GMT                                # Timezone
```

### Control Variables
```bash
export SCRUT_DEFAULT_SHELL=/bin/zsh   # Override default shell
```

## Execution Model

### Working Directory
- Each document gets temporary working directory
- Cleaned up after execution (success or failure)
- Shared between all test cases in document
- Override with `--work-directory <path>` (not cleaned)

### Shell Environment
- Test cases inherit environment from previous cases
- Each test runs in new bash process
- Environment saved/restored via state file
- Exception: `detached` tests don't inherit

### Output Handling
- **Markdown default**: STDOUT only
- **Cram default**: Combined STDOUT/STDERR
- Control with `output_stream` config
- Manual control: `2>&1`, `2>/dev/null`, etc
- **Testing STDERR**: Use `{output_stream: stderr}` in code block:
  ```scrut {output_stream: stderr}
  $ command 2>&1
  error message
  ```

## File Testing Patterns

### Basic File Operations
```scrut
$ echo "content" > file.txt
```

```scrut
$ cat file.txt
content
```

```scrut
$ sed -i 's/old/new/' file.txt
```

```scrut
$ grep new file.txt
new content
```

### Using Fixtures
```scrut
$ cp "$TESTDIR"/fixtures/template.json .
```

```scrut
$ jq '.version = "2.0"' template.json > tmp && mv tmp template.json
```

```scrut
$ jq -r .version template.json
2.0
```

### Directory Operations
```scrut
$ mkdir -p project/{src,test,docs}
```

```scrut
$ touch project/src/{main.c,util.c}
```

```scrut
$ find project -name "*.c" | sort
project/src/main.c
project/src/util.c
```

### Safe Destructive Testing
```scrut
$ mkdir -p build/
```

```scrut
$ touch build/{a.o,b.o,output}
```

```scrut
$ rm -rf build/
```

```scrut
$ test ! -d build && echo "removed"
removed
```

## Advanced Features

### Multi-line Output
```scrut
$ cat multiline.txt
line 1
line 2
line 3
```

### Optional Output
```scrut
$ command-that-may-warn
Warning: deprecated (?)
Result: success
```

### Variable Output
```scrut
$ date
* (glob)
```

```scrut
$ ls *.txt
file*.txt (glob+)
```

### Detached Processes
```scrut {detached: true, detached_kill_signal: "term"}
$ nohup server --start &
```

```scrut
$ sleep 1
```

```scrut
$ curl localhost:8080
OK
```

### Wait for Conditions
```scrut {wait: {timeout: "10s", path: "ready.flag"}}
$ test -f ready.flag && echo "Ready"
Ready
```

### Bootstrap/Setup
````markdown
# Setup aliases and functions
```scrut
$ alias ll='ls -la'
```

```scrut
$ function cleanup() { rm -f *.tmp; }
```

```scrut
$ export PROJECT_ROOT="$PWD"
```

# Available in all subsequent tests
```scrut
$ ll *.txt
-rw-r--r-- * file.txt (glob)
```
````

## Common Patterns

### File Existence
```scrut
$ test -f file.txt && echo "exists"
exists
```

```scrut
$ test ! -f missing.txt && echo "not found"
not found
```

### Content Verification
```scrut
$ grep -q "pattern" file.txt && echo "found"
found
```

```scrut
$ wc -l < file.txt
42
```

### JSON/YAML Processing
```scrut
$ jq -r .name config.json
myapp
```

```scrut
$ yq '.version' chart.yaml
1.2.3
```

### Process Management
```scrut
$ pgrep myapp > /dev/null && echo "running"
running
```

```scrut
$ pkill myapp || true
[?]
```

### Error Handling
```scrut
$ command-that-fails || echo "handled"
handled
```

```scrut
$ set -e; false || true
```

## Format Notes

### Markdown vs Cram
- **Markdown**: Preferred, supports inline config, STDOUT-only default
- **Cram**: Legacy, no inline config, combined output default
- Both share working directory per document
- Both support prepend/append

### Line Endings
- Unix/Mac: LF (`\n`)
- Windows: CRLF (`\r\n`)
- Default: Convert CRLF→LF
- Override: `keep_crlf: true`
