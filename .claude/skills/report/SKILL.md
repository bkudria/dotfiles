---
name: report
description: "Investigate a scope and produce a numbered findings report. Use when conducting an investigation, performing a review or audit, analyzing a codebase or feature, exploring problems and opportunities, or producing findings for a subsequent iterate pass."
disable-model-invocation: true
argument-hint: "[scope to investigate, e.g. 'this codebase' or 'the auth module']"
---

# Report

Investigate a scope and produce a structured, numbered findings report. Each finding describes an observation, problem, or opportunity — never a solution or fix. Output is designed for consumption by the `iterate` skill.

## When to Use

- Conducting an investigation or analysis of a codebase, feature, or tool
- Reviewing for problems and opportunities for improvement
- Auditing configuration, session transcripts, or documentation
- Exploring a topic where findings need to be actioned later via `/iterate`
- Any task matching: "explore and present findings", "investigate and report", "review in detail"

## Dependencies

Tools used: `AskUserQuestion`, sub-agents, `TaskCreate`, `TaskUpdate`, `Read`, `Grep`, `Glob`, `Bash` (read-only), and `Write` (for the report file only).

---

## Arguments: `$ARGUMENTS`

If arguments were provided, treat them as the investigation scope and proceed to Phase 2.

If no arguments were provided, check the conversation for context that implies a scope. If inferable, confirm it with the user. If not, proceed to Phase 1.

---

## Phase 1: Scope Interview

Determine what to investigate. Use `AskUserQuestion` to gather:

1. **Scope** — What area to investigate (codebase, feature, skill, session, configuration, etc.)
2. **Focus** — What to look for (problems, opportunities, trade-offs, architecture concerns, etc.)
3. **Depth** — How deep to go (quick scan, standard review, comprehensive analysis)

If the scope is broad (entire codebase, "everything"), ask for priority areas or known pain points.

---

## Phase 2: Exploration Planning

Break the scope into areas that can be explored semi-independently and assess complexity. Each area is a facet of the scope, not a disconnected inventory — it should contribute to a cohesive investigation of the overall question.

- **Quick** (1-2 reads) — investigate directly
- **Moderate** (5-10 tool calls) — investigate directly
- **Deep** (extensive exploration) — dispatch a sub-agent

If the investigation has 4+ areas, create a `TaskCreate` for each to track progress.

### Sub-agent strategy

Dispatch sub-agents when 3+ areas need moderate or deep exploration. Each sub-agent explores one area independently. Every sub-agent prompt must include: (1) the observation-only constraint — no fixes or solutions, (2) the output format — numbered observations, each a single paragraph with a short title and specific evidence, and (3) a one-line note of the other areas being investigated.

> Investigate [AREA] within [SCOPE]. Look for problems, inconsistencies, surprising patterns, missing pieces, and opportunities for improvement. Do NOT suggest fixes or solutions — only describe what is found. Report as numbered observations, each a single paragraph with a short title. Include specific evidence (file paths, line numbers, values) in every observation.

For fewer or simpler areas, investigate directly without sub-agents.

---

## Phase 3: Investigation

Execute the exploration plan. For each area:

1. Read relevant files, search for patterns, examine configuration
2. Note concrete observations — always include file paths, line numbers, or specific values
3. Identify relationships between areas (a test gap may relate to a code pattern)
4. Mark area complete via `TaskUpdate` if progress tasks exist

### Investigation discipline

- **Specificity required** — Every finding must reference concrete evidence: a file path, a configuration value, a pattern with examples, or a metric. Findings without evidence are opinions, not findings.
- **Breadth before depth** — Survey all areas at surface level first. Go deep only where initial survey reveals something noteworthy.
- **No solutions** — Record what IS, not what SHOULD BE. Describe the current state and why it is noteworthy. The urge to prescribe a fix means the finding is ready to write down as-is.

### Sub-agent verification

When sub-agents were dispatched, complete these checks before proceeding to Phase 4:

1. **Cross-reference claims between sub-agents** — If one sub-agent's findings depend on or contradict another's, investigate the discrepancy directly.
2. **Spot-check numerical claims** — Counts, frequencies, and statistics are especially error-prone. Run one independent check on the most significant number.
3. **Test tool reliability** — If a sub-agent relied on a tool that could have timed out, truncated, or silently failed, verify the tool produced complete results.

If a check does not apply (e.g., no numerical claims), note why and move on — do not skip silently.

---

## Phase 4: Synthesis and Output

### Organize findings

1. Group related observations into discrete findings
2. **Filter for actionability** — Drop observations that are purely informational (neutral descriptions of working-as-designed behavior, positive observations with no implied problem or opportunity). A finding belongs in the report only if it identifies a problem, a gap, a risk, or a concrete opportunity for improvement. "X works correctly" is not a finding.
3. Order by significance — most impactful first
4. **Merge overlapping observations** — If two findings share a root cause or near-identical concluding clause, they describe the same issue. Keep the stronger framing; fold the other's unique evidence into it.
5. Split compound issues into separate findings

### Write the report

Produce a report with this structure:

```
## Report: [Scope Description]

**Scope**: [what was investigated]
**Areas covered**: [list of areas explored]

## Findings

1. **[Short descriptive title]**

   [Detailed observation. What was found, where (file paths, line numbers),
   current state, and why this is noteworthy. Includes specific evidence.]

2. **[Short descriptive title]**

   [...]

## Summary

[N] findings. [Brief overall assessment.]
```

### Finding format rules

- Each finding is a single numbered item in a flat list under `## Findings`
- Title is bold, on its own line — no em dash, no body text on the title line
- Body is a separate indented paragraph after a blank line — no sub-bullets, no nested structure
- Body includes concrete evidence: file paths, values, patterns observed
- Body describes WHAT and WHY it is noteworthy — never HOW to fix it
- Each finding stands alone without requiring context from other findings

### Red flags — rewrite the finding if it contains any of these

- "Consider..." / "Should..." / "Could..." / "Recommend..."
- "Fix by..." / "Solution:" / "To resolve this..."
- "Migrate to..." / "Replace with..." / "Switch to..."
- Any imperative verb directed at future action

These signal a solution, not a finding. Strip the prescription and keep only the observation.

### Persist and present

1. Write the complete report to `/tmp/report-${CLAUDE_SESSION_ID}.md`
2. Output the full report in the conversation message

State at the end: "Report saved to `/tmp/report-${CLAUDE_SESSION_ID}.md` — run `/iterate` to process findings."

---

## Compaction Recovery

If a compaction occurred and the report needs to be reconstructed, read `/tmp/report-${CLAUDE_SESSION_ID}.md` and output it again in conversation so `iterate` can find it.
