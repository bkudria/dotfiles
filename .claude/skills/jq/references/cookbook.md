# Cookbook

Practical jq recipes for common data transformation tasks.

---

## Reshaping Data

### Flatten nested structure

```jq
# Input: {"users": {"admin": {"name": "Alice"}, "guest": {"name": "Bob"}}}
.users | to_entries | map({role: .key} + .value)
# [{"role":"admin","name":"Alice"},{"role":"guest","name":"Bob"}]
```

### Pivot rows to columns

```jq
# Input: [{"date":"2024-01","metric":"views","value":100},{"date":"2024-01","metric":"clicks","value":10}]
group_by(.date) | map({
  date: .[0].date,
  data: (map({(.metric): .value}) | add)
} | . + .data | del(.data))
# [{"date":"2024-01","views":100,"clicks":10}]
```

### Transpose key-value pairs

```jq
# Input: {"a": 1, "b": 2, "c": 3}
to_entries | map({key: (.value | tostring), value: .key}) | from_entries
# {"1":"a","2":"b","3":"c"}
```

### Merge arrays of objects by key

```jq
# Input: {"left": [{id:1,a:"x"},{id:2,a:"y"}], "right": [{id:1,b:"p"},{id:2,b:"q"}]}
INDEX(.left[]; .id) as $l | INDEX(.right[]; .id) as $r |
[$l | keys[] | $l[.] * $r[.]]
# [{id:1,a:"x",b:"p"},{id:2,a:"y",b:"q"}]
```

---

## Filtering and Selecting

### Filter by multiple conditions

```jq
.[] | select(.age > 18 and .status == "active" and (.role | IN("admin", "editor")))
```

### Find duplicates

```jq
group_by(.email) | map(select(length > 1)) | map(.[0].email)
```

### Top N by field

```jq
sort_by(-.score) | limit(10; .[])
# Or: sort_by(.score) | reverse | .[:10]
```

### Find differences between two arrays

```jq
# Input: {"old": [1,2,3,4], "new": [3,4,5,6]}
{
  added: (.new - .old),
  removed: (.old - .new),
  unchanged: [.old[] as $x | .new[] | select(. == $x)] | unique
}
# {"added":[5,6],"removed":[1,2],"unchanged":[3,4]}
```

---

## Aggregation

### Count by category

```jq
group_by(.category) | map({category: .[0].category, count: length}) | sort_by(-.count)
```

### Sum, average, min, max

```jq
{
  sum: (map(.value) | add),
  avg: (map(.value) | add / length),
  min: (map(.value) | min),
  max: (map(.value) | max),
  count: length
}
```

### Running total

```jq
# Using foreach
[foreach .[] as $x (0; . + $x)]
# Input: [1,2,3,4,5] → [1,3,6,10,15]
```

### Histogram / frequency count

```jq
group_by(.) | map({value: .[0], count: length}) | sort_by(-.count)
```

### Word frequency

```jq
split(" ") | map(ascii_downcase) | group_by(.) |
map({word: .[0], count: length}) | sort_by(-.count)
```

---

## String Processing

### Parse key=value lines

```jq
# Input (raw, use -R -s): "key1=value1\nkey2=value2\n"
split("\n") | map(select(length > 0) | split("=") | {(.[0]): .[1:]| join("=")}) | add
```

### Extract with regex

```jq
# Parse log lines
capture("^(?<timestamp>\\S+) (?<level>\\w+) (?<message>.*)$")
```

### Template expansion

```jq
# Input: {"name": "Alice", "role": "admin"}
"Hello, \(.name)! Your role is \(.role)."
```

### Convert camelCase to snake_case

```jq
gsub("(?<a>[a-z])(?<b>[A-Z])"; "\(.a)_\(.b)") | ascii_downcase
```

---

## JSON ↔ Other Formats

### JSON to CSV

```jq
# With headers
(.[0] | keys_unsorted) as $headers |
[$headers | @csv] + [.[] | [.[$headers[]]] | @csv] | join("\n")
```

### CSV to JSON (with jq -R -s)

```bash
jq -Rs '
  split("\n") | map(select(length > 0)) |
  (.[0] | split(",")) as $headers |
  .[1:] | map(
    split(",") as $vals |
    [$headers, $vals] | transpose | map({(.[0]): .[1]}) | add
  )
' data.csv
```

### JSON to shell variables

```jq
to_entries | map("export \(.key)=\(.value | @sh)") | join("\n")
```

### Markdown table from JSON

```jq
(.[0] | keys_unsorted) as $h |
([$h | join(" | ")] + ["---" * ($h|length) | split("") | map("---") | join(" | ")] +
[.[] | [$h[] as $k | .[$k] | tostring] | join(" | ")]) | join("\n")
```

---

## Working with APIs

### Paginated API results

```bash
# Collect pages using bash loop
for page in $(seq 1 10); do
  curl -s "https://api.example.com/items?page=$page"
done | jq -s '[.[].items[]]'
```

### Flatten nested API response

```jq
# Input: {"data": {"users": {"edges": [{"node": {"name": "Alice"}}]}}}
.data.users.edges | map(.node)
```

### Build API request body

```jq
# Construct a request from environment
jq -n --arg token "$API_TOKEN" --arg query "$SEARCH" '{
  headers: {Authorization: "Bearer \($token)"},
  query: $query,
  limit: 100
}'
```

---

## Tree and Graph Operations

### Walk and transform (recursive)

```jq
# walk/1 applies f to every value bottom-up
walk(if type == "string" then ascii_downcase else . end)
```

### Flatten nested tree to paths

```jq
# Input: {"a": {"b": {"c": 1}, "d": 2}}
[paths(scalars)] | map(join("."))
# ["a.b.c", "a.d"]

# With values
[paths(scalars) as $p | {path: ($p | join(".")), value: getpath($p)}]
```

### Find all values of a key at any depth

```jq
[.. | .name? // empty]
```

### Recursive children processing

```jq
# Input: tree nodes with .children arrays
def descendants: ., (.children[]? | descendants);
[descendants | del(.children)]
```

---

## SQL-Style Operations

### JOIN

```jq
# Inner join on user_id
INDEX(.users[]; .id) as $users |
[.orders[] | . + {user_name: $users[.user_id | tostring].name}]
```

### LEFT JOIN

```jq
INDEX(.departments[]; .id) as $depts |
[.employees[] | . + {dept_name: ($depts[.dept_id | tostring].name // "Unknown")}]
```

### GROUP BY with aggregation

```jq
group_by(.department) | map({
  department: .[0].department,
  count: length,
  avg_salary: (map(.salary) | add / length),
  employees: map(.name)
})
```

### DISTINCT / UNIQUE

```jq
[.[] | .category] | unique
# Or with complex keys:
unique_by(.email)
```

### WHERE with multiple conditions

```jq
.[] | select(
  .age >= 18 and
  .age <= 65 and
  .status == "active" and
  (.tags | contains(["premium"]))
)
```

---

## Performance Patterns

### Use INDEX for O(1) lookups

```jq
# Instead of: .users[] | select(.id == $target_id)
# Build index once, then lookup:
INDEX(.users[]; .id) as $idx | $idx[$target_id | tostring]
```

### limit for early termination

```jq
# Stop after finding first match
first(.[] | select(.type == "error"))

# Take only first N results
[limit(10; .[] | select(.score > 90))]
```

### Streaming for large files

```bash
# Process 1GB file without loading into memory
jq --stream 'select(.[0][-1] == "error_count" and .[1] > 0)' huge.json
```

### Avoid repeated path traversal

```jq
# Instead of: {a: .data.results[0].value, b: .data.results[0].name}
# Bind once:
.data.results[0] as $r | {a: $r.value, b: $r.name}
```
