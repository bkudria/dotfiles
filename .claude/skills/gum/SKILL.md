---
name: gum
description: Reference for using the `gum` CLI tool from Charmbracelet to create glamorous shell scripts with interactive TUI components. Use when building interactive prompts, styling terminal output, displaying spinners, or creating user-facing CLI experiences. CRITICAL - many gum commands are interactive TUIs that block waiting for user input.
---

# Gum CLI Reference

Gum is a tool for glamorous shell scripts, providing interactive TUI components and text styling.

## Critical: Interactive vs Non-Interactive Commands

**INTERACTIVE (block for user input - cannot be used directly by Claude):**
- `choose` - select from list
- `confirm` - yes/no prompt
- `file` - file picker
- `filter` - fuzzy filter list
- `input` - single-line text input
- `pager` - scroll through content
- `table` - interactive table selection
- `write` - multi-line text input

**NON-INTERACTIVE (safe for Claude to use directly):**
- `style` - apply colors/borders to text
- `format` - format markdown/code/emoji
- `join` - combine text blocks
- `log` - structured logging output
- `spin` - show spinner while running a command (runs the command, not interactive)

## Command Reference

### choose - Select from options
```bash
# Basic usage
gum choose "Option 1" "Option 2" "Option 3"

# From stdin
echo -e "red\ngreen\nblue" | gum choose

# Multiple selection
gum choose --limit 3 "a" "b" "c" "d"
gum choose --no-limit "a" "b" "c"  # Unlimited

# With header
gum choose --header "Pick a color:" "red" "blue" "green"

# Pre-selected items
gum choose --selected "b,c" "a" "b" "c" "d"

# Custom prefixes
gum choose --cursor-prefix "[ ] " --selected-prefix "[x] " "a" "b"
```

Key flags:
- `--limit=N` - max selections (default 1)
- `--no-limit` - unlimited selections
- `--header="text"` - header above list
- `--height=N` - list height (default 10)
- `--selected="a,b"` - pre-selected items
- `--ordered` - maintain selection order
- `--timeout=5s` - auto-timeout

### confirm - Yes/No prompt
```bash
# Basic - returns exit code 0 (yes) or 1 (no)
gum confirm "Delete this file?" && rm file.txt

# Custom button text
gum confirm "Proceed?" --affirmative="Do it" --negative="Cancel"

# Default to yes
gum confirm --default "Continue?"

# With timeout (returns default or selected)
gum confirm --timeout=10s --default "Auto-continue in 10s?"
```

Key flags:
- `--default` - pre-select yes
- `--affirmative="text"` - yes button text
- `--negative="text"` - no button text
- `--timeout=Ns` - auto-select after N seconds

### input - Single-line text input
```bash
# Basic
NAME=$(gum input --placeholder "Enter your name")

# With prompt and initial value
gum input --prompt "Email: " --value "user@"

# Password input (masked)
gum input --password --placeholder "Enter password"

# With header
gum input --header "Configuration" --placeholder "API key"

# Character limit
gum input --char-limit 10
```

Key flags:
- `--placeholder="text"` - placeholder text
- `--prompt="> "` - prompt prefix
- `--value="text"` - initial value
- `--password` - mask input
- `--header="text"` - header above input
- `--char-limit=N` - max characters (0=unlimited)
- `--width=N` - input width

### write - Multi-line text input
```bash
# Basic (Ctrl+D or Esc to submit)
BODY=$(gum write --placeholder "Enter description...")

# With dimensions
gum write --width 80 --height 10

# With line numbers
gum write --show-line-numbers

# With initial value
gum write --value "Starting text..."
```

Key flags:
- `--width=N` - text area width
- `--height=N` - text area height (default 5)
- `--placeholder="text"` - placeholder
- `--header="text"` - header above
- `--show-line-numbers` - show line numbers
- `--char-limit=N` - max characters

### filter - Fuzzy filter list
```bash
# From stdin
cat files.txt | gum filter

# From arguments
gum filter "apple" "banana" "cherry"

# Multiple selection
cat options.txt | gum filter --no-limit

# With placeholder
gum filter --placeholder "Search files..." < files.txt

# Disable fuzzy (prefix match only)
gum filter --no-fuzzy
```

Key flags:
- `--limit=N` / `--no-limit` - selection limits
- `--placeholder="text"` - search placeholder
- `--header="text"` - header
- `--fuzzy` / `--no-fuzzy` - fuzzy vs prefix match
- `--reverse` - display from bottom
- `--height=N` - list height
- `--strict` - only return if match found

### file - File picker

**WARNING: `gum file` has a known bug where files may be invisible but still selectable, leading to incorrect selections. See:**
- https://github.com/charmbracelet/gum/issues/977
- https://github.com/charmbracelet/gum/issues/969

**Recommended alternative:** Use `fzf` or `fzf-tmux` for file picking instead:
```bash
# Using fzf for file selection (more reliable)
find . -type f | fzf
fd --type f | fzf  # if fd is installed

# With preview
find . -type f | fzf --preview 'head -50 {}'
```

<details>
<summary>gum file reference (use with caution)</summary>

```bash
# Pick from current directory
FILE=$(gum file)

# Start from specific path
gum file /path/to/start

# Show hidden files
gum file --all

# Only directories
gum file --directory

# Show file info
gum file --permissions --size
```

Key flags:
- `-a, --all` - show hidden files
- `--file` - allow file selection
- `--directory` - allow directory selection
- `-p, --permissions` - show permissions
- `-s, --size` - show file sizes
- `--height=N` - max files displayed

</details>

### table - Interactive table
```bash
# From CSV
cat data.csv | gum table

# Custom separator
gum table -s "\t" < data.tsv

# With column names
gum table --columns "Name,Age,City" < data.csv

# Set column widths
gum table --widths 20,10,15 < data.csv

# Return specific column
gum table --return-column 2 < data.csv

# Static print (non-interactive)
gum table --print < data.csv
```

Key flags:
- `-s, --separator=","` - field separator
- `-c, --columns="A,B"` - column names
- `-w, --widths=N,N` - column widths
- `-r, --return-column=N` - return specific column (0=whole row)
- `-p, --print` - static print (non-interactive!)
- `-f, --file="path"` - read from file
- `-b, --border="rounded"` - border style

### spin - Spinner while running command
```bash
# Basic spinner
gum spin --title "Installing..." -- npm install

# Show command output
gum spin --show-output --title "Building..." -- make

# Different spinner style
gum spin --spinner dot --title "Loading..." -- sleep 2
# Styles: line, dot, minidot, jump, pulse, points, globe, moon, monkey, meter, hamburger

# Show output only on error
gum spin --show-error --title "Testing..." -- npm test
```

Key flags:
- `--title="text"` - spinner message
- `-s, --spinner="dot"` - spinner style
- `--show-output` - show command stdout/stderr
- `--show-error` - show output only if command fails
- `-a, --align="left"` - spinner alignment
- `--timeout=Ns` - abort after N seconds

### style - Apply styling to text
```bash
# Colors
gum style --foreground 212 --background 0 "Pink on black"

# Named/hex colors
gum style --foreground "#FF0000" "Red text"

# Borders
gum style --border double --padding "1 2" "Boxed text"
# Border styles: none, hidden, normal, rounded, thick, double

# Text formatting
gum style --bold --italic --underline "Formatted"

# Dimensions and alignment
gum style --width 40 --align center "Centered"

# Combine with echo
echo "Status: OK" | gum style --foreground 10

# Multiple lines (from stdin)
cat file.txt | gum style --border rounded
```

Key flags:
- `--foreground="color"` - text color (ANSI code, hex, or name)
- `--background="color"` - background color
- `--border="style"` - border style
- `--padding="V H"` - padding (vertical horizontal)
- `--margin="V H"` - margin
- `--width=N` - text width
- `--height=N` - text height
- `--align="left|center|right"` - text alignment
- `--bold`, `--italic`, `--underline`, `--strikethrough`, `--faint`

### format - Format text
```bash
# Markdown (default)
gum format "# Heading" "Some **bold** text"

# Code with syntax highlighting
gum format --type code --language go 'func main() {}'

# Emoji shortcodes
gum format --type emoji ":rocket: Launch!"

# Template
gum format --type template '{{ Bold "Hello" }}'
```

Key flags:
- `-t, --type="markdown"` - format type (markdown, code, emoji, template)
- `-l, --language=""` - language for code highlighting
- `--theme="pink"` - glamour theme for markdown

### join - Combine text blocks
```bash
# Horizontal join
gum join --horizontal "Left" "Right"

# Vertical join
gum join --vertical "Top" "Bottom"

# With alignment
gum join --align center --vertical "A" "B" "C"

# Combine styled blocks
A=$(gum style --border rounded "Box A")
B=$(gum style --border rounded "Box B")
gum join --horizontal "$A" "$B"
```

Key flags:
- `--horizontal` - join side by side
- `--vertical` - join top to bottom
- `--align="left|center|right"` - alignment

### log - Structured logging
```bash
# Basic levels
gum log --level info "Server started"
gum log --level warn "Low memory"
gum log --level error "Connection failed"
gum log --level debug "Request received"

# With timestamp
gum log --time kitchen --level info "Event occurred"

# Structured key-value
gum log --structured --level info "Request" method GET path /api

# With prefix
gum log --prefix "myapp" --level info "Starting"

# To file
gum log --file app.log --level error "Critical error"
```

Key flags:
- `-l, --level="info"` - log level (debug, info, warn, error, fatal, none)
- `--min-level=""` - minimum level to display
- `-t, --time="kitchen"` - time format
- `-s, --structured` - structured key=value format
- `--prefix="text"` - prefix
- `-o, --file="path"` - log to file
- `--formatter="text"` - output format

### pager - Scroll through content
```bash
# From stdin
cat long-file.txt | gum pager

# With line numbers
gum pager --show-line-numbers < log.txt

# Soft wrap
gum pager --soft-wrap < wide-content.txt
```

Key flags:
- `--show-line-numbers` - show line numbers
- `--soft-wrap` - wrap long lines
- `--timeout=Ns` - auto-exit after N seconds

## Styling Colors

Colors can be specified as:
- ANSI codes: `212`, `0`, `255`
- Hex: `"#FF0000"`, `"#0FF"`
- Named (some terminals): `"red"`, `"blue"`

Common ANSI codes:
- `212` - pink/magenta
- `99` - purple
- `10` - green
- `9` - red
- `11` - yellow
- `240` - gray

## Environment Variables

All flags can be set via environment variables:
- Pattern: `GUM_<COMMAND>_<FLAG>`
- Examples:
  - `GUM_INPUT_PLACEHOLDER="Enter value"`
  - `GUM_CHOOSE_HEIGHT=15`
  - `GUM_SPIN_SPINNER=dot`

## Exit Codes

- `gum confirm`: 0 = yes, 1 = no
- `gum filter --strict`: 1 if no match
- Other commands: 0 on success, non-zero on error/cancel

## Common Patterns

### Conventional commit message
```bash
TYPE=$(gum choose "feat" "fix" "docs" "style" "refactor" "test" "chore")
SCOPE=$(gum input --placeholder "scope (optional)")
SUMMARY=$(gum input --placeholder "summary")
BODY=$(gum write --placeholder "details (optional)")

if [ -n "$SCOPE" ]; then
  COMMIT="$TYPE($SCOPE): $SUMMARY"
else
  COMMIT="$TYPE: $SUMMARY"
fi

if [ -n "$BODY" ]; then
  COMMIT="$COMMIT

$BODY"
fi

echo "$COMMIT"
```

### Styled output boxes
```bash
gum style \
  --border rounded \
  --border-foreground 212 \
  --padding "1 2" \
  --margin 1 \
  "Welcome to the installer"
```

### Progress with spinner
```bash
gum spin --title "Downloading..." -- curl -O "$URL"
gum spin --title "Extracting..." -- tar xf archive.tar.gz
gum spin --title "Installing..." -- ./install.sh
```
