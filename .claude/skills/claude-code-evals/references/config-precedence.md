# Config Precedence

The eval pipeline merges configuration across three tools (craboodle, scuttlerun, pincenez) and multiple layers. When debugging unexpected behavior, trace through this chain to find which layer is responsible.

---

## The Full Chain

From lowest to highest precedence (later layers override earlier ones):

```
1. scuttlerun defaults     ← Built-in Zod schema defaults
2. base.yaml (scuttlerun)  ← Shared scuttlerun config for all scenarios
3. scenario.yaml           ← Per-scenario scuttlerun overrides (top-level fields)
4. CLI flags               ← --agent-model, --grader-model, --repeats
5. Per-check model         ← check-level model override in pincenez (checks.yaml)
```

### Layer Details

**1. Scuttlerun defaults** (built into scuttlerun's Zod schema)
- `model: claude-haiku-4-5`
- `max_turns: 50`
- `effort: high`
- `permission_mode: bypassPermissions`
- `user.max_turns: 0`
- `user.oracle_model: claude-haiku-4-5`

Run `scuttlerun <config> --dry-run` to see the fully resolved config after all defaults are applied.

**2. base.yaml** (shared scuttlerun config for all scenarios)
- Written by the eval author in the `evals/` directory
- Contains ONLY scuttlerun fields (`model`, `tools`, `user`, `project`, etc.)
- Does NOT contain craboodle fields — those live in `craboodle.yaml`

**3. scenario.yaml** (per-scenario scuttlerun overrides)
- Fields are top-level scuttlerun fields (NOT nested under a `scuttlerun:` block)
- `prompt` is just a regular top-level field here — it maps directly to scuttlerun's `prompt:` field
- Deep-merged with base.yaml by craboodle before passing to scuttlerun
- Objects merge recursively; arrays and scalars replace
- Craboodle does not validate scuttlerun fields — errors surface when scuttlerun runs (or when `craboodle list` invokes `scuttlerun`)

**4. CLI flags** (runtime overrides)
- `--agent-model MODEL` → overrides `model` for all scuttlerun sessions
- `--grader-model MODEL` → overrides model for all pincenez checks
- `--repeats N` → overrides default repeat count (but not per-scenario `repeats:`)
- `--concurrency N` → pool size (no config file equivalent)

**5. Per-check `model:`** (pincenez only, in checks.yaml)
- A check's `model:` field overrides `--grader-model` for that specific check
- Useful for using a stronger model on tricky checks while keeping the default cheap

### Separate Config Files

**craboodle.yaml** (pipeline config at evals root)
- Contains pipeline-level settings: `version`, `min_pass_rate`, `max_budget_usd`, `repeats`
- NOT part of the scuttlerun precedence chain — these fields are consumed by craboodle only
- Lives at the evals root directory alongside `base.yaml`

**checks.yaml** (pincenez config per scenario)
- Contains context and checks (id-as-key format)
- Lives alongside `scenario.yaml` in each scenario directory
- Per-check `model:` overrides apply here (layer 5)

---

## Merge Semantics

Scuttlerun merges multiple YAML files using deep merge:
- **Objects**: merge recursively (keys from later files override same keys)
- **Arrays**: replace entirely (later file's array wins)
- **Scalars**: replace (later file's value wins)

Example:
```yaml
# base.yaml
tools: [Read, Write, Bash]
user:
  max_turns: 0

# scenario.yaml (top-level scuttlerun fields)
tools: [Read, Glob, Grep]        # Replaces the array entirely
user:
  persona: "A developer"          # Adds to the user object
  # max_turns: 0                  # Inherited from base
```

Result: `tools` is `[Read, Glob, Grep]`, `user` has all three fields.

---

## Debugging Tips

1. **"What will scuttlerun actually see?"** — Run `scuttlerun base.yaml scenario.yaml` to see the fully resolved config after merging and defaults
2. **"Is it a craboodle schema error or a scuttlerun schema error?"** — Run `craboodle list <evals-dir>` — it validates both layers and reports which failed
3. **"My setting isn't taking effect"** — Check if a later layer is overriding it: base.yaml → scenario.yaml → CLI flags
4. **"Array was replaced, not merged"** — This is by design. If you set `tools:` in a scenario, it replaces the base.yaml tools entirely. To add a tool, repeat the full list plus your addition
