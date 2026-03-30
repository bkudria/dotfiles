# Domain Expertise Skill Creation

Build a comprehensive skill that covers an entire domain: a framework, platform, API, language, or tool ecosystem. Use this workflow instead of the standard 5-phase path when the skill requires exhaustive research and lifecycle-oriented design.

## When to Use This Workflow

- The skill covers a broad domain (e.g., "Next.js", "Kubernetes", "BigQuery", "Stripe API")
- Multiple distinct workflows exist within the domain (create, configure, debug, test, deploy)
- Significant reference material is needed (API docs, schemas, patterns, gotchas)
- The standard 5-phase wizard would produce an undercooked result

## Step 1: Identify the Domain

Gather scope and boundaries:

- **Domain name** — the framework, platform, or tool
- **Scope boundaries** — what's in vs. out (e.g., "Next.js App Router only, not Pages Router")
- **Target audience** — skill level assumed (beginner, intermediate, expert)
- **Key pain points** — what problems does the user hit most often in this domain?

Ask the user directly. Don't assume scope — a "Docker" skill could mean just Dockerfile authoring or the full build/run/debug/deploy lifecycle.

## Step 2: Map the Lifecycle

Identify all workflows a user would need across the full lifecycle of working in this domain. Common lifecycle stages:

| Stage | Example workflows |
|-------|-------------------|
| Setup/Init | Create project, configure environment, install dependencies |
| Build/Create | Write code, generate components, scaffold features |
| Configure | Settings, environment variables, plugins, integrations |
| Debug | Diagnose errors, inspect state, trace issues |
| Test | Write tests, run test suites, measure coverage |
| Optimize | Performance tuning, bundle size, query optimization |
| Deploy/Ship | Build for production, deploy, monitor, rollback |

Not every domain needs all stages. Ask the user which stages matter most, then prioritize.

## Step 3: Exhaustive Research

This is what distinguishes domain expertise skills from regular skills. Research must be thorough.

**Minimum research requirements:**
- Perform **5+ web searches** across different aspects of the domain
- Check **official documentation** for the domain
- Search for **common patterns and anti-patterns**
- Capture **exact CLI commands** with flags and options
- Document **API patterns** with real request/response examples
- Identify **gotchas and edge cases** that trip people up

**Verification:**
- Cross-check facts across multiple sources
- Test CLI commands and API calls where possible
- Note version-specific behavior (e.g., "requires v4+")

**Organize research by domain area** as you go — don't dump everything into one file.

## Step 4: Organize into Domain Areas

Group research into logical domain areas. Each area becomes a reference file:

```
references/
├── setup.md          # Installation, configuration, environment
├── core-concepts.md  # Fundamental patterns and architecture
├── api-reference.md  # API endpoints, methods, parameters
├── cli-reference.md  # Commands, flags, common invocations
├── patterns.md       # Best practices, recommended approaches
├── gotchas.md        # Common mistakes, edge cases, workarounds
└── troubleshooting.md # Error messages and fixes
```

**Guidelines for reference files** (see `references/resource-usage.md`):
- One topic per file, max 300 lines per file
- Descriptive filenames (not `ref1.md`)
- Include concrete examples, not abstract descriptions

## Step 5: Create Router SKILL.md

Domain expertise skills should always use the **router pattern** (see `references/router-pattern.md`).

The SKILL.md should contain:
1. **Essential principles** — 3-5 core rules that always apply, regardless of workflow
2. **Intake question** — "What would you like to do?" with numbered options matching lifecycle stages
3. **Routing table** — maps user intent to workflow files
4. **Quick reference** — critical information always worth having visible (e.g., common commands)
5. **Reference index** — table linking all reference files with their purpose
6. **Workflows index** — table linking all workflow files with their purpose

Use `references/skill-templates.md` for the router template structure.

Run `scripts/scaffold.sh` to create the directory structure, then customize.

## Step 6: Write Workflows

Create one workflow file per lifecycle stage. Each workflow should:

- Start with **required reading** — which reference files to load before starting
- Provide **step-by-step instructions** for the workflow
- Include **concrete examples** with realistic inputs and outputs
- End with **success criteria** — how to know the workflow was completed correctly

```
workflows/
├── setup.md
├── create-component.md
├── debug.md
├── test.md
├── optimize.md
└── deploy.md
```

Follow the conventions in `references/writing-style.md`: imperative voice, realistic examples, one idea per paragraph.

## Step 7: Write References

Flesh out the reference files from Step 4 with the research from Step 3. Each reference should:

- Be focused on one domain area
- Include concrete code examples and CLI invocations
- Show expected output where applicable
- Note version requirements or constraints
- Cross-reference other relevant reference files sparingly

## Step 8: Validate Completeness

Before finishing, verify:

- [ ] Every lifecycle stage from Step 2 has a workflow
- [ ] Every workflow's required_reading files exist
- [ ] No orphaned reference files (every reference is mentioned in SKILL.md or a workflow)
- [ ] Router SKILL.md has intake, routing, reference index, and workflows index
- [ ] SKILL.md is under 300 lines (bulk content is in references)
- [ ] At least 3 concrete examples exist across the workflows
- [ ] Essential principles section captures domain-specific rules

**Next**: Proceed to Phase 5 for validation (read `workflows/create-phase3-validate.md`)
