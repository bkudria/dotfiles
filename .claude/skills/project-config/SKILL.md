---
name: project-config
description: "Manage project standards compliance — audit existing projects, scaffold new ones, or check what standards apply. Triggers: \"audit this project\", \"project audit\", \"check compliance\", \"new project\", \"scaffold project\", \"project setup\"."
argument-hint: "[audit|scaffold|check]"
---

# Project Config

Manage project standards compliance through `project.yaml` — a per-project configuration file that declares which **profiles** (directories under `profiles/`) apply, optionally which inherited standards are **disabled** (with a reason), and optionally which suggested standards are **required** (severity upgraded from `SUGG` to `FAIL`). Projects without `project.yaml` are not tracked.

Each standard is a self-contained YAML file under `profiles/<profile>/`: it declares whether it's required, a one-line description, and exactly one of `check.script` (deterministic shell) or `check.prompt` (sub-agent verification). Standard identity in audits is `<profile>/<basename>`.

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
| **Check** | "project config", "what standards" | Read `project.yaml`, list selected profiles + disabled entries, summarize what would be audited |

## Dependencies

- **yq** — YAML processing for reading project.yaml and standard YAMLs (`brew install yq`)
- **jq** — JSON processing for runner output (`brew install jq`)

## Reference Files

| File | Purpose |
|------|---------|
| `workflows/audit.md` | Audit mode — collect → resolve pending via sub-agents → render |
| `workflows/scaffold.md` | Scaffold mode — interview, project.yaml generation, file creation |
| `references/project-yaml-schema.md` | project.yaml + standard YAML schema reference |
| `profiles/<profile>/<basename>.yaml` | Self-contained standard YAMLs (`required`, `description`, `check.{script,prompt}`, optional `notes`). Filename is the standard's identity. |
| `scripts/run-audit.sh` | Audit runner. Five verbs, all operating on a state-dir:<br>• `--init` → emits a fresh state-dir on stdout<br>• `--collect <project-root> <state-dir>` → writes `<state-dir>/collect.json`<br>• `--merge <state-dir>` → folds `<state-dir>/responses/<id>.txt` into `<state-dir>/merged.json`<br>• `--render <state-dir>` → formats the FAIL/SUGG table (always exits 0 on success)<br>• `--check <state-dir>` → CI pass/fail signal (1 if any FAIL row, 0 otherwise, ≥2 on operational error) |
| `scripts/lint-project-yaml.sh` | Validates a project.yaml's schema (`<path>`) or every standard YAML (`--skill`) |
