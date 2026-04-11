---
name: project-config
description: Audit, scaffold, and guide projects toward consistent standards using project.yaml as the single source of truth. Use when auditing a project, checking project compliance, creating a new project, setting up project structure, or configuring project standards. Trigger phrases include "audit this project", "audit the project", "project audit".
argument-hint: "[audit|scaffold|check]"
---

# Project Config

Manage project standards compliance through `project.yaml` — a per-project configuration file that declares metadata, required standards, and their parameters. Projects without `project.yaml` are not tracked.

## When to Use

- Auditing an existing project's standards compliance
- Scaffolding a new project with the right structure
- Checking what standards a project should meet
- Creating or updating a project.yaml file
- Setting up a new project from scratch ("new project", "project setup")

## Modes

| Mode | Trigger | Action |
|------|---------|--------|
| **Audit** | "audit this project", "project audit", "check compliance" | Read `workflows/audit.md` |
| **Scaffold** | "new project", "scaffold project" | Read `workflows/scaffold.md` |
| **Check** | "project config", "what standards" | Read project.yaml, show current configuration and status summary |

## Dependencies

- **yq** — YAML processing for reading project.yaml (`brew install yq`)
- **jq** — JSON processing for script output (`brew install jq`)
- **mq** — Markdown processing for section heading checks (`brew install mq`); optional — section checks are skipped if not installed

## Reference Files

| File | Purpose |
|------|---------|
| `workflows/audit.md` | Audit mode — compliance table + runtime verification |
| `workflows/scaffold.md` | Scaffold mode — interview, project.yaml generation, file creation |
| `references/standards-catalog.md` | Detailed check logic for each standard |
| `references/project-yaml-schema.md` | Complete project.yaml schema with all fields and types |
| `references/profiles/` | Profile definition directory (add profiles as needed) |
| `scripts/check-standards.sh` | Deterministic file existence and config checks (`--json` for machine-readable output) |
| `scripts/lint-project-yaml.sh` | Lint project.yaml for redundant profile fields (`--fix` to auto-clean) |
