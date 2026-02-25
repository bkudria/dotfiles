# Form JSON Schema

Complete schema reference for `ask-form.sh`.

## Top-Level Structure

```json
{
  "questions": [
    { "question": "...", "type": "...", "key": "...", ... }
  ]
}
```

## Question Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `question` | string | Yes | Display text shown to the user |
| `type` | string | Yes | One of: `input`, `choose`, `multi`, `write`, `file`, `filter`, `confirm` |
| `key` | string | Yes | Key name in the results JSON object |
| `options` | string[] | For choose/multi/filter | List of selectable options |
| `placeholder` | string | No | Hint text shown in empty input fields |
| `default` | string | No | Pre-filled default value |
| `descriptions` | boolean | No | Parse options as `"label\|description"` format |
| `other` | boolean | No | Add "Other..." option for custom input |
| `skippable` | boolean | No | Add "Skip" option (returns empty) |
| `chattable` | boolean | No | Add "Chat about this" option (exits form with code 2) |
| `limit` | number | No | (multi only) Maximum number of selections |
| `yes` | string | No | (confirm only) Custom label for yes button |
| `no` | string | No | (confirm only) Custom label for no button |

## Type Reference

| Type | Input Method | Output |
|------|-------------|--------|
| `input` | Single-line text field | String |
| `choose` | Single selection from list | String |
| `multi` | Multiple selections from list | Array of strings |
| `write` | Multi-line text editor | String |
| `file` | File/directory picker | String (path) |
| `filter` | Fuzzy search through options | String |
| `confirm` | Yes/No buttons | Boolean |

## Example: Full-Featured Form

```json
{
  "questions": [
    {
      "question": "Project name?",
      "type": "input",
      "key": "name",
      "placeholder": "my-project"
    },
    {
      "question": "Language?",
      "type": "choose",
      "key": "lang",
      "descriptions": true,
      "other": true,
      "options": [
        "TypeScript|Full type safety",
        "Python|Great for scripting",
        "Go|Fast and compiled",
        "Rust|Memory safe"
      ]
    },
    {
      "question": "Features?",
      "type": "multi",
      "key": "features",
      "skippable": true,
      "limit": 5,
      "options": ["Tests", "CI/CD", "Docker", "Documentation", "Linting"]
    },
    {
      "question": "Description?",
      "type": "write",
      "key": "desc",
      "placeholder": "Describe your project..."
    },
    {
      "question": "Config file?",
      "type": "file",
      "key": "config"
    },
    {
      "question": "Ready to create?",
      "type": "confirm",
      "key": "proceed",
      "yes": "Create",
      "no": "Cancel"
    }
  ]
}
```

Output:
```json
{
  "name": "my-project",
  "lang": "TypeScript",
  "features": ["Tests", "CI/CD", "Docker"],
  "desc": "A web service for...",
  "config": "~/project/config.yaml",
  "proceed": true
}
```
