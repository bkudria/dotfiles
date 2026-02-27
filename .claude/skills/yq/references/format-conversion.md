# Format Conversion

Converting between YAML, JSON, XML, Properties, CSV, and TSV formats.

## Quick Conversion Table

| From | To | Command |
|------|----|---------|
| YAML | JSON | `yq -o=json file.yaml` |
| YAML | JSON (compact) | `yq -o=json -I=0 file.yaml` |
| JSON | YAML | `yq -p=json file.json` |
| YAML | XML | `yq -o=xml file.yaml` |
| XML | YAML | `yq -p=xml file.xml` |
| YAML | Properties | `yq -o=props file.yaml` |
| Properties | YAML | `yq -p=props file.properties` |
| YAML | CSV | `yq -o=csv file.yaml` |
| CSV | YAML | `yq -p=csv file.csv` |
| YAML | TSV | `yq -o=tsv file.yaml` |
| TSV | YAML | `yq -p=tsv file.tsv` |
| JSON | XML | `yq -p=json -o=xml file.json` |

## JSON

### YAML to JSON

```bash
yq -o=json '.' file.yaml              # formatted JSON
yq -o=json -I=0 '.' file.yaml         # compact (single-line)
yq -o=json -I=4 '.' file.yaml         # 4-space indent
```

Comments are dropped during JSON conversion. YAML anchors are dereferenced.

### JSON to YAML

```bash
yq -p=json file.json                  # parse JSON, output YAML
```

JSON is a subset of YAML — no special parser needed for single documents.

### NDJSON (JSON Lines)

```bash
yq -p=json -o=json -I=0 file.json                               # roundtrip compact
yq -p=json -o=json -I=0 '(select(di == 1) | .field) = "new"' file.json  # target by doc index
yq -p=json -o=json -I=0 '(select(has("key")) | .key) = "val"' file.json # target by content
yq -p=json file.json                                             # decode to YAML (--- separators)
```

Multiple JSON documents output as separate documents. Use `di` (documentIndex) to target specific ones.

## XML

### XML to YAML

```bash
yq -p=xml file.xml                     # parse XML to YAML
yq --xml-skip-directives -p=xml f.xml  # skip DOCTYPE
```

### YAML to XML

```bash
yq -o=xml file.yaml                    # encode YAML to XML
```

### XML Mapping Conventions

| XML Concept | YAML Representation |
|-------------|-------------------|
| Attributes | Prefixed with `+@` (configurable via `--xml-attribute-prefix`) |
| Text content | Under `+content` key (configurable via `--xml-content-name`) |
| Repeated elements | Treated as arrays |
| Processing instructions | Prefixed with `+p_` |
| DOCTYPE | Under `+directive` |

### Type Conversion After XML Parse

All XML values parse as strings. Convert types:

```bash
yq -p=xml '(.. | select(tag == "!!str")) |= from_yaml' file.xml
```

### Force Single-Item Arrays

Consecutive nodes with the same name become arrays automatically. Force array for single items:

```bash
yq -p=xml '.zoo.animal |= ([] + .)' file.xml
```

### Custom Attribute Prefix

```bash
yq -p=xml --xml-attribute-prefix='@' file.xml
```

### Namespace Handling

```bash
yq -p=xml --xml-keep-namespace=false file.xml    # remove namespace prefixes
yq -p=xml --xml-raw-token=false file.xml         # translate to full namespace URLs
```

## Properties

### YAML to Properties

```bash
yq -o=props file.yaml                                   # basic
yq -o=props --properties-array-brackets file.yaml        # key[0] format (Spring Boot)
yq -o=props --properties-separator=" :@ " file.yaml      # custom separator
yq -o=props --unwrapScalar=false file.yaml               # quote strings with spaces
yq -o=props '... comments = ""' file.yaml                # strip comments
```

Empty maps and arrays are excluded by default. Include them:

```bash
yq -o=props '(.. | select((tag == "!!map" or tag == "!!seq") and length == 0)) = ""' file.yaml
```

### Properties to YAML

```bash
yq -p=props file.properties                              # basic
yq -p=props '(.. | select(tag == "!!str")) |= from_yaml' file.properties  # type conversion
yq -p=props '.things |= array_to_map' file.properties    # numeric indices to map
```

### Roundtrip

```bash
yq -p=props -o=props '.person.pets.0 = "dog"' file.properties
```

Preserves comments and formatting during modification.

## CSV / TSV

### YAML to CSV/TSV

**Arrays of arrays:**

```yaml
- [name, age, city]
- [Alice, 30, NYC]
```

```bash
yq -o=csv file.yaml     # name,age,city\nAlice,30,NYC
yq -o=tsv file.yaml     # tab-separated
```

**Arrays of objects (auto-headers from keys):**

```yaml
- name: Alice
  age: 30
- name: Bob
  age: 25
```

```bash
yq -o=csv file.yaml     # name,age\nAlice,30\nBob,25
```

First entry determines headers. Missing fields produce blank cells.

**Custom headers with field selection:**

```bash
yq -o=csv '[["Name", "Age"]] + [.[] | [.name, .age]]' file.yaml
```

### CSV/TSV to YAML

```bash
yq -p=csv file.csv                     # first row becomes keys
yq -p=tsv file.tsv
yq -p=csv --csv-auto-parse=f file.csv  # keep values as strings
```

Default: auto-parses YAML/JSON-formatted cell values. Use `--csv-auto-parse=f` to preserve as strings.

### CSV Roundtrip

```bash
yq -p=csv -o=csv '(.[] | select(.name == "Alice") | .age) = 31' file.csv
```

## Inline Encoding/Decoding

Encode/decode values within expressions (not file-level conversion):

```bash
.data | @base64                     # encode to base64
.data | @base64d                    # decode from base64
.data | @json                       # encode to JSON string
.data | @jsond                      # decode JSON string to node
.data | @yaml                       # encode to YAML string
.data | @yamld                      # decode YAML string to node
.data | @uri                        # URI-encode
.data | @urid                       # URI-decode
.data | @sh                         # shell-quote
.data | to_json(2)                  # encode with custom indent
.data | to_yaml(4)                  # encode with custom indent
```
