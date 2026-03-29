# Config Precedence

The eval pipeline merges configuration across three tools and multiple layers. When debugging unexpected behavior, trace through this chain to find which layer is responsible.

---

## The Full Chain

From lowest to highest precedence (later layers override earlier ones):

```
1. scuttlerun defaults     ← Built-in Zod schema defaults
2. base.yml (scuttlerun)   ← Shared config for all scenarios
3. scenario.yml scuttlerun ← Per-scenario overrides
4. scenario.yml prompt     ← Mapped to scuttlerun's prompt field
5. CLI flags               ← --agent-model, --grader-model, --repeats
6. Per-assertion model      ← assertion-level model override in pincenez
```

### Layer Details

**1. Scuttlerun defaults** (built into scuttlerun's Zod schema)
- `model: claude-haiku-4-5`
- `max_turns: 50`
- `effort: high`
- `permission_mode: bypassPermissions`
- `user.turn_policy: single`
- `user.oracle_model: claude-haiku-4-5`
- `user.max_user_turns: 5`

Run `scuttlerun list <config>` to see the fully resolved config after all defaults are applied.

**2. base.yml** (shared config for all scenarios)
- Written by the eval author in the `evals/` directory
- Contains two types of fields:
  - **craboodle fields**: `version`, `min_pass_rate` — consumed by craboodle, not passed to scuttlerun
  - **scuttlerun fields**: everything else (`model`, `tools`, `user`, `project`, etc.) — passed through to scuttlerun

**3. scenario.yml `scuttlerun:` block** (per-scenario overrides)
- Deep-merged with base.yml's scuttlerun fields
- Objects merge recursively; arrays and scalars replace
- Craboodle does not validate this block — errors surface when scuttlerun runs (or when `craboodle list` invokes `scuttlerun list`)

**4. scenario.yml `prompt`** (always applied)
- Mapped to scuttlerun's `prompt:` field in the override config
- Replaces any prompt from base.yml

**5. CLI flags** (runtime overrides)
- `--agent-model MODEL` → overrides `model` for all scuttlerun sessions
- `--grader-model MODEL` → overrides model for all pincenez assertions
- `--repeats N` → overrides default repeat count (but not per-scenario `repeats:`)
- `--concurrency N` → pool size (no config file equivalent)

**6. Per-assertion `model:`** (pincenez only)
- An assertion's `model:` field overrides `--grader-model` for that specific assertion
- Useful for using a stronger model on tricky assertions while keeping the default cheap

---

## Merge Semantics

Scuttlerun merges multiple YAML files using deep merge:
- **Objects**: merge recursively (keys from later files override same keys)
- **Arrays**: replace entirely (later file's array wins)
- **Scalars**: replace (later file's value wins)

Example:
```yaml
# base.yml
tools: [Read, Write, Bash]
user:
  turn_policy: single
  max_user_turns: 5

# scenario.yml scuttlerun: block
tools: [Read, Glob, Grep]        # Replaces the array entirely
user:
  persona: "A developer"          # Adds to the user object
  # turn_policy: single           # Inherited from base
  # max_user_turns: 5             # Inherited from base
```

Result: `tools` is `[Read, Glob, Grep]`, `user` has all three fields.

---

## Debugging Tips

1. **"What will scuttlerun actually see?"** — Run `scuttlerun list base.yml override.yml` to see the fully resolved config after merging and defaults
2. **"Is it a craboodle schema error or a scuttlerun schema error?"** — Run `craboodle list <evals-dir>` — it validates both layers and reports which failed
3. **"My setting isn't taking effect"** — Check if a later layer is overriding it: base.yml → scenario scuttlerun block → CLI flags
4. **"Array was replaced, not merged"** — This is by design. If you set `tools:` in a scenario, it replaces the base.yml tools entirely. To add a tool, repeat the full list plus your addition
