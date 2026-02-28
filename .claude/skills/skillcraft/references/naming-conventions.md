# Skill Naming Conventions

## Skill Name Rules

- Format: **hyphen-case** -- lowercase letters, digits, hyphens only
- Maximum 64 characters
- Directory name = skill name (must match exactly)
- No leading/trailing hyphens, no consecutive hyphens

| Good Names | Bad Names | Why Bad |
|------------|-----------|---------|
| `docker-helper` | `DockerHelper` | No uppercase |
| `pr-review` | `my_skill` | No underscores |
| `api-reference` | `tool` | Too generic |
| `skillcraft` | `helper` | Not descriptive |
| `kdl-syntax-reference` | `PR--Review` | Consecutive hyphens, uppercase |

## Naming Patterns

| Pattern | Example | When to Use |
|---------|---------|-------------|
| `{tool}-reference` | `gum`, `kdl-syntax-reference` | Tool documentation and API surfaces |
| `{action}-{target}` | `skillcraft`, `pr-review` | Workflow automation skills |
| `{domain}-{aspect}` | `api-reference`, `git-workflow` | Domain knowledge skills |
| `{adjective}-{noun}` | `interactive-tmux`, `advanced-ask` | Enhanced or specialized capabilities |

Choose the pattern that most directly communicates what the skill provides. Prefer shorter names when unambiguous.

## Description Construction

**Format:** `"{Purpose sentence}. Use when {trigger1}, {trigger2}, or {trigger3}."`

- Length: 10--1024 characters
- First sentence: what the skill does (verb + noun, active voice)
- Second part: when to activate it (specific scenarios)

**Good:**
```
Reference for using the gum CLI tool. Use when building interactive
prompts, styling terminal output, or creating TUI experiences.
```

```
Creates new Claude Code skills from templates. Use when scaffolding
a skill, writing skill frontmatter, or setting up skill directories.
```

**Bad:**
```
A helpful skill for gum stuff.
```
- "helpful" is filler, "stuff" is vague, no trigger phrases

**Avoid:** vague words (stuff, things, help, misc), redundant phrases ("this skill is"), passive voice ("is used for"), single-word descriptions.

## Trigger Phrase Design

Trigger phrases appear in the description's "Use when" clause. They control when the skill activates.

**Rules:**
- Use verb+noun pairs: "editing Dockerfiles", "debugging containers", "writing SQL queries"
- 3--5 trigger phrases per skill is ideal
- Be specific enough to avoid false positives with unrelated tasks
- Be broad enough to catch legitimate use cases

**Litmus test:** Would a user naturally say this phrase when describing their task?

| Trigger Quality | Example | Problem |
|----------------|---------|---------|
| Too broad | "writing code" | Fires on everything |
| Too narrow | "editing line 42 of Dockerfile.prod" | Almost never fires |
| Right level | "editing Dockerfiles" | Specific domain, natural phrasing |

**Complementary sets** work best -- cover different angles of the same domain:
```
Use when writing Kubernetes manifests, debugging pod failures,
or configuring cluster networking.
```

## Anti-Patterns

| Anti-Pattern | Why It Fails | Better Alternative |
|-------------|-------------|-------------------|
| `helper` | Says nothing about domain | `docker-compose-reference` |
| `tool` | Meaningless qualifier | `api-test-runner` |
| `my-skill` | Not descriptive at all | Name after what it does |
| `v2` suffix | Versioning belongs in git, not names | Update the skill in place |
| `new-thing` | Temporal; "new" becomes stale | Describe the thing itself |
| `misc-utils` | Grab-bag; split into focused skills | One skill per domain |

---

## Claude Search Optimization (CSO)

Optimize skills for discovery by future Claude instances.

### The Description Pitfall

**Critical finding:** Descriptions that summarize a skill's workflow cause Claude to follow the description instead of reading the full skill body.

```yaml
# BAD: Summarizes workflow — Claude may follow this instead of reading skill
description: Use when executing plans — dispatches subagent per task with code review between tasks

# BAD: Too much process detail
description: Use for TDD — write test first, watch it fail, write minimal code, refactor

# GOOD: Just triggering conditions, no workflow summary
description: Use when executing implementation plans with independent tasks in the current session

# GOOD: Triggering conditions only
description: Use when implementing any feature or bugfix, before writing implementation code
```

**Why this matters:** When a description summarizes the skill's workflow, Claude may follow the description instead of reading the full skill content. A description saying "code review between tasks" caused Claude to do ONE review, even though the skill's body clearly showed TWO reviews. When the description was changed to just triggering conditions, Claude correctly read and followed the full body.

**Rule:** Description = when to use. Never what the skill does step-by-step.

### Red-Flag Words in Descriptions

These words in a description suggest workflow summary rather than triggering conditions. Their presence is not always wrong, but warrants review.

| Category | Red-Flag Words | Why Suspicious |
|----------|---------------|----------------|
| Process sequence | "then", "first", "next", "finally" | Implies step ordering |
| Orchestration verbs | "dispatches", "orchestrates", "coordinates", "delegates", "routes" | Describes internal mechanics |
| Step indicators | "step 1", "phase N", "stage N" | Enumerates a procedure |
| Multi-action chains | 3+ verbs joined by commas (e.g., "reads X, transforms Y, and writes Z") | Summarizes what the skill does, not when to use it |

**Note:** Words like "before", "after", "between" are common in legitimate trigger conditions ("Use before deploying", "converting between formats") and are NOT red flags on their own.

**Self-check:** Read the description aloud. If it answers "what does this skill do?" more than "when should Claude load this?", rewrite it.

### Exemplar Descriptions

These real skills demonstrate excellent CSO — study their descriptions as models:

| Skill | Why It Works |
|-------|-------------|
| `jq` | Tool name + action verbs + concrete contexts ("writing jq commands, building jq pipelines") |
| `testing-strategy` | Decision-oriented triggers ("deciding between unit and integration tests, choosing what level to test at") |
| `session-transcripts` | Problem-oriented triggers + IMPORTANT annotation for critical guidance |
| `advanced-ask` | Specific boundary condition ("when AskUserQuestion tool is insufficient") + concrete limits listed |

### Keyword Coverage

Use words Claude would search for when encountering the problem:

| Keyword Type | Examples |
|-------------|----------|
| Error messages | "Hook timed out", "ENOTEMPTY", "race condition" |
| Symptoms | "flaky", "hanging", "zombie", "pollution" |
| Synonyms | "timeout/hang/freeze", "cleanup/teardown/afterEach" |
| Tools | Actual commands, library names, file types |

### Token Efficiency Targets

Every token in a frequently-loaded skill costs context across every conversation.

| Skill Frequency | Target | Rationale |
|----------------|--------|-----------|
| Frequently-loaded / getting-started | <200 words | Loaded in every conversation — minimize |
| Standard skills | <500 words | Loaded on demand — be concise |
| Reference-heavy skills | Unlimited (in references/) | SKILL.md body stays lean, detail in references/ |

**Compression techniques:**
- Move flag/option details to `--help` references instead of documenting inline
- Use cross-references to other skills instead of repeating content
- Compress examples: one realistic example, not three verbose ones
- Eliminate redundancy: don't repeat what cross-referenced skills cover

See `references/writing-style.md` for word count targets by skill type.

### Discovery Workflow

How future Claude finds a skill:

1. **Encounters problem** — "tests are flaky", "need to parse KDL"
2. **Finds skill** — description matches the problem's keywords
3. **Scans overview** — reads title + "When to Use" to confirm relevance
4. **Reads patterns** — quick reference tables for immediate use
5. **Loads detail** — reference files only when implementing

Optimize for this flow: searchable terms early, quick confirmation of relevance, progressive detail.

### Descriptive Naming

**Use active voice, verb-first naming:**
- `creating-skills` not `skill-creation`
- `condition-based-waiting` not `async-test-helpers`
- `flatten-with-flags` not `data-structure-refactoring`

**Gerunds (-ing) work well for processes:**
- `creating-skills`, `testing-skills`, `debugging-with-logs`

---

## Quick Checklist

- [ ] Name is hyphen-case, under 64 characters
- [ ] Name communicates the skill's domain or action
- [ ] Description starts with an active-voice purpose sentence
- [ ] Description includes 3--5 specific trigger phrases
- [ ] Trigger phrases are verb+noun pairs at the right specificity
- [ ] No vague words, no version suffixes, no grab-bag naming
- [ ] Description does NOT summarize the skill's workflow (CSO pitfall)
- [ ] Keywords cover error messages, symptoms, and synonyms
- [ ] Word count within target for skill frequency tier
