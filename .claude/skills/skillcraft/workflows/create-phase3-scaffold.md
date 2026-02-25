# Phase 3: Scaffold

Run the scaffolding script to create the skill directory and initial files in one shot.

## Run Scaffold Script

```bash
scripts/scaffold.sh \
    "{name}" --path "{location}" --type "{type}" \
    [--references] [--scripts] [--assets]
```

- `{name}` — From Phase 1 interview
- `{location}` — From Phase 1 (e.g., `~/.claude/skills` or `.claude/skills`)
- `{type}` — From Phase 2 skill type selection: `knowledge`, `workflow`, `tool`, or `hybrid`
- Add `--references`, `--scripts`, `--assets` flags based on Phase 2 resource type selections

The script creates the directory, generates a type-specific SKILL.md template with TODO placeholders, and creates selected resource directories with placeholder files. See `references/skill-templates.md` for the templates used.

## Post-Scaffold

After the script runs, update the generated SKILL.md:

1. **Frontmatter** — Fill in `description` from Phase 1 data: `{purpose}. Use when {trigger1}, {trigger2}, or {trigger3}.` Ensure 10-1024 characters with specific verb+noun trigger phrases (see `references/naming-conventions.md`).
2. **Add selected features** — From Phase 2 frontmatter feature selections (allowed-tools, model, context, hooks, agent, etc.)
3. **Rename placeholder files** — Rename `references/TODO-rename-me.md` and `scripts/TODO-rename-me.sh` to meaningful names

**Next**: Proceed to Phase 4 (read `workflows/create-phase4-author.md`)
