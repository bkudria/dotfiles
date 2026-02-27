# Cookbook

Practical recipes for common yq tasks.

## Find & Update Array Items

### Find item by field value

```bash
yq '.items[] | select(.name == "Foo")' data.yaml
```

### Update a matching item's field

```bash
yq '(.items[] | select(.name == "Foo") | .count) |= . + 1' data.yaml
```

Parentheses around the LHS are required for filtered updates.

### Update multiple fields on matching items

```bash
yq 'with(.items[] | select(.name == "Foo"); .count += 1 | .updated = true)' data.yaml
```

### Replace entire matching item

```bash
yq '(.items[] | select(.name == "old")) = {"name": "new", "value": 42}' data.yaml
```

### Delete matching items

```bash
yq 'del(.items[] | select(.name == "unwanted"))' data.yaml
```

## Sort, Filter, Unique

### Sort array by field

```bash
yq '.items |= sort_by(.name)' data.yaml
```

### Sort descending

```bash
yq '.items |= (sort_by(.priority) | reverse)' data.yaml
```

### Multi-field sort

```bash
yq '.items |= sort_by(.category, .name)' data.yaml
```

### Filter, flatten, sort, unique

```bash
yq '[.[] | select(.type == "service") | .tags] | flatten | sort | unique' data.yaml
```

### Deduplicate by field

```bash
yq '.items |= unique_by(.id)' data.yaml
```

## Merging Files

### Simple merge (second overrides first)

```bash
yq ea 'select(fi == 0) * select(fi == 1)' base.yaml overlay.yaml
```

### Merge with array append

```bash
yq ea 'select(fi == 0) *+ select(fi == 1)' base.yaml overlay.yaml
```

### Merge all files in directory

```bash
yq ea '. as $item ireduce ({}; . * $item)' configs/*.yaml
```

### Merge with source tracking (comments)

```bash
yq ea '(.. lineComment |= filename + ":" + line) | select(fi == 0) * select(fi == 1)' a.yaml b.yaml
```

### Merge from loaded file

```bash
yq '. *= load("defaults.yaml")' config.yaml
```

### Selective merge (existing fields only)

```bash
yq ea 'select(fi == 0) *? select(fi == 1)' base.yaml patch.yaml
```

### Additive merge (new fields only)

```bash
yq ea 'select(fi == 0) *n select(fi == 1)' existing.yaml defaults.yaml
```

## Kubernetes Patterns

### Extract all container images

```bash
yq '.spec.template.spec.containers[].image' deployment.yaml
```

### Update a specific container's image

```bash
yq -i '(.spec.template.spec.containers[] | select(.name == "app") | .image) = "nginx:2.0"' deployment.yaml
```

### Add a label

```bash
yq -i '.metadata.labels.env = "production"' deployment.yaml
```

### Add an environment variable to a container

```bash
yq -i '(.spec.template.spec.containers[] | select(.name == "app") | .env) += [{"name": "DEBUG", "value": "true"}]' deployment.yaml
```

### Extract all resource limits

```bash
yq '.spec.template.spec.containers[] | {"name": .name, "limits": .resources.limits}' deployment.yaml
```

### Process all YAML files in a Helm chart

```bash
find templates/ -name '*.yaml' -exec yq -i '.metadata.labels.chart = "my-chart-1.0"' {} \;
```

## Conditional Updates

yq has no if/else -- use `with` + `select`:

```bash
yq '.[] |= (
  with(select(.animal == "cat"); .noise = "meow" | .whiskers = true) |
  with(select(.animal == "dog"); .noise = "woof" | .happy = true) |
  with(select(.noise == null); .noise = "???")
)' animals.yaml
```

## Environment Variables

### Set value from env var

```bash
VERSION="2.0" yq -i '.version = strenv(VERSION)' config.yaml
```

### Replace all variable references in document

```bash
export DB_HOST="db.example.com" DB_PORT="5432"
yq '(.. | select(tag == "!!str")) |= envsubst' config.yaml
```

### Embed special characters

```bash
VAL='.a |!@ "weird"' yq '.a = strenv(VAL)' file.yaml
```

## Reshaping Data

### Array of objects to flat object

```bash
yq '.[] as $i ireduce ({}; .[$i.name] = $i.value)' data.yaml
```

### Object to array of objects

```bash
yq 'to_entries | map({"name": .key, "value": .value})' data.yaml
```

### Pivot (transpose)

```bash
yq 'pivot' data.yaml
```

### Deep prune (keep only selected paths)

```bash
yq '(
  .. |
  select(has("keep1") or has("keep2")) |
  (.keep1, .keep2) |
  select(.)
) as $i ireduce ({};
  setpath($i | path; $i)
)' data.yaml
```

## Export Patterns

### Export as shell environment variables

```bash
yq '.[] | (
  (select(kind == "scalar") | key + "='\''" + . + "'\''"),
  (select(kind == "seq") | key + "=(" + (map("'\''" + . + "'\''") | join(",")) + ")")
)' config.yaml
```

### Export nested as flat env vars

```bash
yq '.. | (
  (select(kind == "scalar" and parent | kind != "seq") | (path | join("_")) + "='\''" + . + "'\''"),
  (select(kind == "seq") | (path | join("_")) + "=(" + (map("'\''" + . + "'\''") | join(",")) + ")")
)' config.yaml
```

## Creating YAML

### From scratch

```bash
yq -n '.apiVersion = "v1" | .kind = "ConfigMap" | .metadata.name = "my-config"'
```

### With nested structure

```bash
yq -n '
  .apiVersion = "apps/v1" |
  .kind = "Deployment" |
  .metadata.name = "app" |
  .spec.replicas = 3 |
  .spec.template.spec.containers[0].name = "app" |
  .spec.template.spec.containers[0].image = "nginx:latest"
'
```

### Create new file

```bash
yq -n '.key = "value"' > new-file.yaml
```

## Multi-Document Processing

### Target specific document

```bash
yq '(select(di == 1) | .key) = "value"' multi.yaml
```

### Target by content

```bash
yq '(select(.kind == "Service") | .spec.type) = "LoadBalancer"' resources.yaml
```

### Process NDJSON to YAML

```bash
yq -p=json multi.json           # each JSON doc becomes a YAML doc with ---
```

## Comparing Files

### Side-by-side diff

```bash
diff <(yq -P 'sort_keys(..)' -o=props a.yml) <(yq -P 'sort_keys(..)' -o=props b.yml)
```

### Normalize for diffing

```bash
yq -i -P 'sort_keys(..)' file.yaml
```

## Bash Integration

### Read YAML array into bash array

```bash
readarray actions < <(yq '.actions[]' config.yaml)
```

### Loop over YAML objects in bash

```bash
readarray items < <(yq -o=j -I=0 '.items[]' data.yml)
for item in "${items[@]}"; do
    name=$(echo "$item" | yq '.name' -)
    echo "Processing: $name"
done
```

### Validate YAML

```bash
yq --exit-status 'tag == "!!map" or tag == "!!seq"' file.yaml > /dev/null
```
