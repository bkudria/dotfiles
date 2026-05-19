# Config Precedence

The eval pipeline merges configuration across three tools (craboodle, scuttlerun, pincenez) and multiple layers. When debugging unexpected behavior, trace through this chain to find which layer is responsible.

---

## The Full Chain

From lowest to highest precedence (later layers override earlier ones):

```
1. scuttlerun defaults         ← Built-in Zod schema defaults
2. scenarios.base in evals.yaml ← Shared scuttlerun config for all scenarios
3. scenario.yaml               ← Per-scenario scuttlerun overrides (top-level fields)
4. CLI flags                   ← --agent-model, --grader-model, --repeats
5. Per-check model             ← check-level model override in pincenez (checks.yaml)
```

**Merge order (matters when partially overriding nested objects):** scuttlerun first deep-merges layers 2–4 on the raw YAML, then applies layer-1 defaults to fill in any keys still unset. Defaults are *not* overlaid first and then overwritten — they fill the gaps last. This means partially overriding a nested object (e.g. setting `user: { persona: "X" }` in scenario.yaml) does *not* erase sibling default fields like `user.max_turns` or `user.oracle_model` — those defaults still apply to keys you didn't set.

Mechanically, craboodle materializes `scenarios.base` into a `.craboodle-base.yaml` file inside the staged eval root (under `$TMPDIR`) and then invokes scuttlerun against that staged base + each scenario.yaml. From scuttlerun's perspective the merge looks the same as the old two-file layout did — only the source of layer 2 changed.

### Layer Details

**1. Scuttlerun defaults** (built into scuttlerun's Zod schema)
- `model: claude-haiku-4-5`
- `max_turns: 50`
- `effort: high`
- `permission_mode: bypassPermissions`
- `user.max_turns: 0`
- `user.oracle_model: claude-haiku-4-5`

Run `scuttlerun <config> --dry-run` to see the fully resolved config after all defaults are applied.

**2. `scenarios.base` in evals.yaml** (shared scuttlerun config for all scenarios)
- Nested under `scenarios.base` in `<root>/evals.yaml`
- Contains scuttlerun fields (`model`, `tools`, `additional_tools`, `user`, `project`, etc.)
- Craboodle does NOT validate fields here — errors surface when scuttlerun runs (or when `craboodle list` invokes scuttlerun)

**3. scenario.yaml** (per-scenario scuttlerun overrides)
- Fields are top-level scuttlerun fields (NOT nested under a `scuttlerun:` block)
- `prompt` is just a regular top-level field here — it maps directly to scuttlerun's `prompt:` field
- Deep-merged with the materialized `scenarios.base` (via `.craboodle-base.yaml` in the staged eval root) before scuttlerun runs
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

### Other Config Files

**evals.yaml** (single config file at the eval root)
- Top level: pipeline keys consumed by craboodle only (`version`, `min_pass_rate`, `max_budget_usd`, `repeats`, `artifact_retention_days`, `scenarios.path`)
- Under `scenarios.base`: scuttlerun base config (layer 2 above)
- The two halves are partitioned by location — top-level pipeline keys are NOT passed to scuttlerun, and `scenarios.base` is NOT validated by craboodle

**checks.yaml** (pincenez config per scenario)
- Contains context and checks (id-as-key format)
- Lives alongside `scenario.yaml` in each scenario directory under `evals/`
- Per-check `model:` overrides apply here (layer 5)

---

## Merge Semantics

Scuttlerun merges multiple YAML files using deep merge:
- **Objects**: merge recursively (keys from later files override same keys)
- **Arrays**: replace entirely (later file's array wins)
- **Scalars**: replace (later file's value wins)

Example:
```yaml
# evals.yaml (the scenarios.base half)
scenarios:
  base:
    tools: [Read, Write, Bash]
    user:
      max_turns: 0

# evals/<scenario-id>/scenario.yaml (top-level scuttlerun fields)
tools: [Read, Glob, Grep]        # Replaces the array entirely
user:
  persona: "A developer"          # Adds to the user object
  # max_turns: 0                  # Inherited from scenarios.base
```

Result: `tools` is `[Read, Glob, Grep]`, `user` has all three fields.

---

## Debugging Tips

1. **"What will scuttlerun actually see?"** — Run `craboodle list <root>` for static validation, or `craboodle run --repeats 1 <root>` once and inspect `.craboodle-base.yaml` in the artifact_dir for the materialized base config that scuttlerun was invoked against
2. **"Is it a craboodle schema error or a scuttlerun schema error?"** — `craboodle list <root>` validates the evals.yaml top-level keys itself, then delegates `scenarios.base` validation to scuttlerun — the error message tells you which layer rejected the input
3. **"My setting isn't taking effect"** — Walk the chain: scuttlerun default → `scenarios.base` in evals.yaml → scenario.yaml → CLI flags. A later layer is probably overriding it
4. **"Array was replaced, not merged"** — This is by design. If you set `tools:` in a scenario, it replaces the base tools entirely. To extend instead, use `additional_tools:` (appended and deduped against scuttlerun's defaults), or repeat the full list plus your addition
