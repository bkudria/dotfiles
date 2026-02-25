# Gum Common Patterns

Reusable patterns combining multiple gum commands.

---

## Conventional commit message
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

## Styled output boxes
```bash
gum style \
  --border rounded \
  --border-foreground 212 \
  --padding "1 2" \
  --margin 1 \
  "Welcome to the installer"
```

## Progress with spinner
```bash
gum spin --title "Downloading..." -- curl -sO "$URL"
gum spin --title "Extracting..." -- tar xf archive.tar.gz
gum spin --title "Installing..." -- ./install.sh
```

## Interactive setup wizard
```bash
PROJECT=$(gum input --header "New Project" --placeholder "Project name")
LANG=$(gum choose --header "Language" "TypeScript" "Python" "Go" "Rust")
FEATURES=$(gum choose --no-limit --header "Features" "Tests" "CI" "Docker" "Docs")
gum confirm "Create $LANG project '$PROJECT'?" && echo "Creating..."
```

## Dashboard layout
```bash
STATUS=$(gum style --border rounded --width 30 "Status: Running")
METRICS=$(gum style --border rounded --width 30 "CPU: 45% | RAM: 2.1GB")
gum join --horizontal "$STATUS" "$METRICS"
```
