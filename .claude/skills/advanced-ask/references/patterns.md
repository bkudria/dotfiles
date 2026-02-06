# Advanced Ask Patterns

Extended patterns and examples for complex questioning scenarios.

## Pattern: Conditional Follow-up Questions

Ask follow-up questions based on previous answers:

```bash
#!/bin/bash
SCRIPTS="$HOME/.claude/skills/advanced-ask/scripts"

# Initial question
db_type=$("$SCRIPTS/ask-choose.sh" --header "Database type" \
    "PostgreSQL" "MySQL" "SQLite" "MongoDB")

# Conditional follow-up
case "$db_type" in
    PostgreSQL|MySQL)
        host=$("$SCRIPTS/ask-input.sh" --header "Database host" --placeholder "localhost")
        port=$("$SCRIPTS/ask-input.sh" --header "Port" --value "5432")
        ;;
    SQLite)
        path=$("$SCRIPTS/ask-file.sh" --header "Database file")
        ;;
    MongoDB)
        uri=$("$SCRIPTS/ask-input.sh" --header "Connection URI" --placeholder "mongodb://...")
        ;;
esac
```

## Pattern: Validation Loop

Re-ask until valid input received:

```bash
#!/bin/bash
SCRIPTS="$HOME/.claude/skills/advanced-ask/scripts"

while true; do
    email=$("$SCRIPTS/ask-input.sh" --header "Email" --placeholder "user@example.com")

    # Validate email format
    if [[ "$email" =~ ^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$ ]]; then
        break
    fi

    echo "Invalid email format. Please try again."
done
```

## Pattern: Progressive Disclosure Form

Break complex forms into sections:

```bash
#!/bin/bash
SCRIPTS="$HOME/.claude/skills/advanced-ask/scripts"

# Section 1: Basic info
basic=$("$SCRIPTS/ask-form.sh" --inline '{
    "questions": [
        {"question": "Project name?", "type": "input", "key": "name"},
        {"question": "Description?", "type": "write", "key": "desc"}
    ]
}')

# Section 2: Technical choices
if "$SCRIPTS/ask-confirm.sh" "Configure technical options?"; then
    tech=$("$SCRIPTS/ask-form.sh" --inline '{
        "questions": [
            {"question": "Language?", "type": "choose", "options": ["TypeScript", "Python", "Go"], "key": "lang"},
            {"question": "Framework?", "type": "choose", "options": ["Express", "FastAPI", "Gin"], "key": "framework"}
        ]
    }')
fi

# Merge results
echo "$basic" "$tech" | jq -s 'add'
```

## Pattern: Dynamic Options from Command Output

Generate options from system state:

```bash
#!/bin/bash
SCRIPTS="$HOME/.claude/skills/advanced-ask/scripts"

# Git branches as options
branch=$(git branch --format='%(refname:short)' | \
    "$SCRIPTS/ask-filter.sh" --header "Select branch")

# Docker containers
container=$(docker ps --format '{{.Names}}' | \
    "$SCRIPTS/ask-filter.sh" --header "Select container")

# Files matching pattern
file=$(find . -name "*.ts" -type f | \
    "$SCRIPTS/ask-filter.sh" --header "Select TypeScript file")
```

## Pattern: Wizard with Back Navigation

Multi-step wizard allowing users to go back:

```bash
#!/bin/bash
SCRIPTS="$HOME/.claude/skills/advanced-ask/scripts"

declare -A answers
step=1
max_steps=3

while [[ $step -le $max_steps ]]; do
    case $step in
        1)
            result=$("$SCRIPTS/ask-choose.sh" --header "Step 1: Project type" \
                "Web App" "CLI Tool" "Library" "← Back (cancel)")
            [[ "$result" == "← Back (cancel)" ]] && exit 0
            answers[type]="$result"
            ;;
        2)
            result=$("$SCRIPTS/ask-choose.sh" --header "Step 2: Language" \
                "TypeScript" "Python" "Go" "← Back")
            [[ "$result" == "← Back" ]] && { ((step--)); continue; }
            answers[lang]="$result"
            ;;
        3)
            result=$("$SCRIPTS/ask-multi.sh" --header "Step 3: Features" \
                "Testing" "CI/CD" "Docker" "Documentation" "← Back")
            if echo "$result" | grep -q "← Back"; then
                ((step--))
                continue
            fi
            answers[features]="$result"
            ;;
    esac
    ((step++))
done

# Output collected answers
for key in "${!answers[@]}"; do
    echo "$key: ${answers[$key]}"
done
```

## Pattern: Confirmation with Preview

Show what will happen before confirming:

```bash
#!/bin/bash
SCRIPTS="$HOME/.claude/skills/advanced-ask/scripts"

# Collect info
name=$("$SCRIPTS/ask-input.sh" --header "Name")
email=$("$SCRIPTS/ask-input.sh" --header "Email")

# Show preview using gum style (non-interactive, safe to run directly)
echo ""
gum style --border rounded --padding "1 2" "
Name:  $name
Email: $email
"
echo ""

# Confirm
if "$SCRIPTS/ask-confirm.sh" --yes "Create" --no "Edit" "Create user with these details?"; then
    echo "Creating user..."
else
    echo "Cancelled - edit your inputs"
fi
```

## Pattern: Batch Selection with Search

Select multiple items from a large list:

```bash
#!/bin/bash
SCRIPTS="$HOME/.claude/skills/advanced-ask/scripts"

# Get all npm packages in project
packages=$(npm ls --depth=0 --json 2>/dev/null | jq -r '.dependencies | keys[]')

# Let user filter and multi-select
selected=$(echo "$packages" | "$SCRIPTS/ask-filter.sh" \
    --header "Select packages to update" \
    --limit 0)  # 0 = unlimited

echo "Selected packages:"
echo "$selected"
```

## Pattern: Password/Secret Input

Securely collect sensitive information:

```bash
#!/bin/bash
SCRIPTS="$HOME/.claude/skills/advanced-ask/scripts"

# API key (masked input)
api_key=$("$SCRIPTS/ask-input.sh" \
    --header "API Key" \
    --placeholder "Enter your API key" \
    --password)

# Don't echo the key, just confirm it was received
echo "API key received (${#api_key} characters)"
```

## Pattern: Table-based Selection

When options have multiple attributes, format as a table first:

```bash
#!/bin/bash
SCRIPTS="$HOME/.claude/skills/advanced-ask/scripts"

# Create formatted options
options=(
    "react     | 18.2.0  | UI Library"
    "vue       | 3.3.4   | UI Framework"
    "angular   | 16.0.0  | Full Framework"
    "svelte    | 4.0.0   | Compiler"
)

# Let user filter through formatted options
selection=$("$SCRIPTS/ask-filter.sh" --header "Select package" "${options[@]}")

# Extract just the package name
package=$(echo "$selection" | cut -d'|' -f1 | tr -d ' ')
echo "Selected: $package"
```

## Comparison: When to Use Each Script

| Need | Script | Example |
|------|--------|---------|
| Pick one from 5+ options | `ask-choose` | Framework selection |
| Pick multiple from list | `ask-multi` | Feature toggles |
| Free text (short) | `ask-input` | Names, URLs, keys |
| Free text (long) | `ask-write` | Descriptions, code |
| Browse filesystem | `ask-file` | Config file selection |
| Search through many items | `ask-filter` | Git branches, packages |
| Yes/No with custom text | `ask-confirm` | Deployment approval |
| Multiple questions at once | `ask-form` | Project scaffolding |
