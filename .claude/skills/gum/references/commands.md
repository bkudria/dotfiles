# Gum Command Reference

Complete reference for all gum commands with flags and examples.

---

## choose - Select from options
```bash
# Basic usage
gum choose "React" "Vue" "Angular"

# From stdin
echo -e "postgres\nmysql\nsqlite" | gum choose

# Multiple selection
gum choose --limit 3 "Auth" "Database" "Cache" "Queue"
gum choose --no-limit "Auth" "Database" "Cache"  # Unlimited

# With header
gum choose --header "Pick a database:" "PostgreSQL" "MySQL" "SQLite"

# Pre-selected items
gum choose --selected "Auth,Cache" "Auth" "Database" "Cache" "Queue"

# Custom prefixes
gum choose --cursor-prefix "[ ] " --selected-prefix "[x] " "Auth" "Database"
```

Key flags:
- `--limit=N` - max selections (default 1)
- `--no-limit` - unlimited selections
- `--header="text"` - header above list
- `--height=N` - list height (default 10)
- `--selected="a,b"` - pre-selected items
- `--ordered` - maintain selection order
- `--timeout=5s` - auto-timeout

## confirm - Yes/No prompt
```bash
# Basic - returns exit code 0 (yes) or 1 (no)
gum confirm "Delete all test fixtures?" && rm -rf test/fixtures

# Custom button text
gum confirm "Deploy to production?" --affirmative="Deploy" --negative="Cancel"

# Default to yes
gum confirm --default "Continue with migration?"

# With timeout (returns default or selected)
gum confirm --timeout=10s --default "Auto-continue in 10s?"
```

Key flags:
- `--default` - pre-select yes
- `--affirmative="text"` - yes button text
- `--negative="text"` - no button text
- `--timeout=Ns` - auto-select after N seconds

## input - Single-line text input
```bash
# Basic
NAME=$(gum input --placeholder "Enter project name")

# With prompt and initial value
gum input --prompt "Email: " --value "user@example.com"

# Password input (masked)
gum input --password --placeholder "Enter API key"

# With header
gum input --header "Configuration" --placeholder "Database URL"

# Character limit
gum input --char-limit 64
```

Key flags:
- `--placeholder="text"` - placeholder text
- `--prompt="> "` - prompt prefix
- `--value="text"` - initial value
- `--password` - mask input
- `--header="text"` - header above input
- `--char-limit=N` - max characters (0=unlimited)
- `--width=N` - input width

## write - Multi-line text input
```bash
# Basic (Ctrl+D or Esc to submit)
BODY=$(gum write --placeholder "Enter commit description...")

# With dimensions
gum write --width 80 --height 10

# With line numbers
gum write --show-line-numbers

# With initial value
gum write --value "## Summary\n\n"
```

Key flags:
- `--width=N` - text area width
- `--height=N` - text area height (default 5)
- `--placeholder="text"` - placeholder
- `--header="text"` - header above
- `--show-line-numbers` - show line numbers
- `--char-limit=N` - max characters

## filter - Fuzzy filter list
```bash
# From stdin
git branch --format='%(refname:short)' | gum filter

# From arguments
gum filter "react" "vue" "angular" "svelte" "solid"

# Multiple selection
npm ls --depth=0 --json | jq -r '.dependencies | keys[]' | gum filter --no-limit

# With placeholder
gum filter --placeholder "Search packages..." < packages.txt

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

## file - File picker

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

## table - Interactive table
```bash
# From CSV
cat users.csv | gum table

# Custom separator
gum table -s "\t" < metrics.tsv

# With column names
gum table --columns "Name,Role,Team" < employees.csv

# Set column widths
gum table --widths 20,10,15 < employees.csv

# Return specific column
gum table --return-column 2 < employees.csv

# Static print (non-interactive)
gum table --print < employees.csv
```

Key flags:
- `-s, --separator=","` - field separator
- `-c, --columns="A,B"` - column names
- `-w, --widths=N,N` - column widths
- `-r, --return-column=N` - return specific column (0=whole row)
- `-p, --print` - static print (non-interactive!)
- `-f, --file="path"` - read from file
- `-b, --border="rounded"` - border style

## spin - Spinner while running command
```bash
# Basic spinner
gum spin --title "Installing dependencies..." -- npm install

# Show command output
gum spin --show-output --title "Building project..." -- make build

# Different spinner style
gum spin --spinner dot --title "Running migrations..." -- rails db:migrate
# Styles: line, dot, minidot, jump, pulse, points, globe, moon, monkey, meter, hamburger

# Show output only on error
gum spin --show-error --title "Running tests..." -- npm test
```

Key flags:
- `--title="text"` - spinner message
- `-s, --spinner="dot"` - spinner style
- `--show-output` - show command stdout/stderr
- `--show-error` - show output only if command fails
- `-a, --align="left"` - spinner alignment
- `--timeout=Ns` - abort after N seconds

## style - Apply styling to text
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
cat README.md | gum style --border rounded
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

## format - Format text
```bash
# Markdown (default)
gum format "# Release Notes" "Some **important** changes"

# Code with syntax highlighting
gum format --type code --language go 'func main() { fmt.Println("hello") }'

# Emoji shortcodes
gum format --type emoji ":rocket: Deployed successfully!"

# Template
gum format --type template '{{ Bold "Hello" }}'
```

Key flags:
- `-t, --type="markdown"` - format type (markdown, code, emoji, template)
- `-l, --language=""` - language for code highlighting
- `--theme="pink"` - glamour theme for markdown

## join - Combine text blocks
```bash
# Horizontal join
gum join --horizontal "Left panel" "Right panel"

# Vertical join
gum join --vertical "Header" "Body" "Footer"

# With alignment
gum join --align center --vertical "Title" "Subtitle" "Content"

# Combine styled blocks
A=$(gum style --border rounded "Server Status")
B=$(gum style --border rounded "Client Status")
gum join --horizontal "$A" "$B"
```

Key flags:
- `--horizontal` - join side by side
- `--vertical` - join top to bottom
- `--align="left|center|right"` - alignment

## log - Structured logging
```bash
# Basic levels
gum log --level info "Server started on port 8080"
gum log --level warn "Connection pool running low"
gum log --level error "Failed to connect to database"
gum log --level debug "Processing request /api/users"

# With timestamp
gum log --time kitchen --level info "Backup completed"

# Structured key-value
gum log --structured --level info "Request handled" method GET path /api/users status 200

# With prefix
gum log --prefix "myapp" --level info "Starting worker"

# To file
gum log --file app.log --level error "Unrecoverable error"
```

Key flags:
- `-l, --level="info"` - log level (debug, info, warn, error, fatal, none)
- `--min-level=""` - minimum level to display
- `-t, --time="kitchen"` - time format
- `-s, --structured` - structured key=value format
- `--prefix="text"` - prefix
- `-o, --file="path"` - log to file
- `--formatter="text"` - output format

## pager - Scroll through content
```bash
# From stdin
cat server.log | gum pager

# With line numbers
gum pager --show-line-numbers < access.log

# Soft wrap
gum pager --soft-wrap < wide-output.txt
```

Key flags:
- `--show-line-numbers` - show line numbers
- `--soft-wrap` - wrap long lines
- `--timeout=Ns` - auto-exit after N seconds
