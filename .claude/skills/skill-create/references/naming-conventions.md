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
| `skill-create` | `helper` | Not descriptive |
| `kdl-syntax-reference` | `PR--Review` | Consecutive hyphens, uppercase |

## Naming Patterns

| Pattern | Example | When to Use |
|---------|---------|-------------|
| `{tool}-reference` | `gum`, `kdl-syntax-reference` | Tool documentation and API surfaces |
| `{action}-{target}` | `skill-create`, `pr-review` | Workflow automation skills |
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

## Quick Checklist

- [ ] Name is hyphen-case, under 64 characters
- [ ] Name communicates the skill's domain or action
- [ ] Description starts with an active-voice purpose sentence
- [ ] Description includes 3--5 specific trigger phrases
- [ ] Trigger phrases are verb+noun pairs at the right specificity
- [ ] No vague words, no version suffixes, no grab-bag naming
