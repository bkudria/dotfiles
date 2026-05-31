export const meta = {
  name: 'project-config-verify',
  description: 'Fan out one verifier sub-agent per pending project-config standard: each reads its rendered prompt file, verifies the standard against the project, and writes a {met,detail} JSON verdict to its response file for run-audit.sh --merge to read.',
  whenToUse: 'Invoked by the project-config audit (workflows/audit.md) after --collect has rendered the pending prompt files. Called once per scope — required, then (if the gate passes) suggested.',
  phases: [
    { title: 'Verify', detail: 'one haiku agent per pending standard, in parallel; each writes its response file' },
  ],
}

// ---- params -----------------------------------------------------------------
// The Workflow harness can deliver `args` as a JSON-encoded string rather than a
// parsed object; normalize both shapes before use.
let P = {}
if (typeof args === 'string') {
  try { P = JSON.parse(args) } catch (e) { P = {} }
} else if (args && typeof args === 'object') {
  P = args
}

const scope = P.scope || 'unspecified'
const pending = Array.isArray(P.pending) ? P.pending : []

if (!pending.length) {
  // Every standard in this scope was deterministic (resolved by --collect with no
  // prompt) — there is nothing to fan out. The main thread proceeds to --merge.
  log('No pending standards for scope "' + scope + '"; nothing to verify.')
  return { scope, requested: 0, dispatched: 0 }
}

// ---- Verify (parallel fan-out, one agent per pending standard) ---------------
phase('Verify')
log('Verifying ' + pending.length + ' pending standard(s) for scope "' + scope + '".')

// Each verifier's COMPLETE instructions live in its rendered prompt file on disk
// (written by run-audit.sh --collect): the project context, the standard's check,
// any maintainer notes, and a final directive to Write {"met","detail"} to its
// response file. The agent's only job is to read that file and obey it. Verdicts
// travel as files — never as the agent's return value — because --merge reads the
// response files and the Workflow completion notification truncates large returns.
const dispatchVerifier = (p) =>
  agent(
    'You are verifying one project-configuration standard. Your COMPLETE instructions ' +
    'live in a file on disk — read it and follow it exactly.\n\n' +
    '1. Use the Read tool to read this file verbatim:\n   ' + p.prompt_path + '\n' +
    '2. That file contains the project context, the standard\'s check, and a final ' +
    'directive to record a one-line JSON verdict. Investigate the project as the check ' +
    'requires (Read, Grep, Glob, and Bash are available), reach a verdict, then use the ' +
    'Write tool to write exactly {"met": true|false, "detail": "<one-line>"} — nothing ' +
    'else, no fenced code block — to this response file:\n   ' + p.response_path + '\n' +
    '3. The response file is the ONLY output that matters; your chat reply is ignored. ' +
    'Do not write the verdict anywhere else, and do not skip the Write.\n\n' +
    'Standard id: ' + p.id,
    { label: 'verify:' + p.id, phase: 'Verify', model: 'haiku' }
  )

const results = await parallel(pending.map((p) => () => dispatchVerifier(p)))
const dispatched = results.filter(Boolean).length
if (dispatched < pending.length) {
  log((pending.length - dispatched) + ' verifier(s) errored; --merge treats any missing ' +
    'response file as FAIL (failure to verify is itself a failure).')
}
log('Dispatched ' + dispatched + '/' + pending.length + ' verifier(s) for scope "' + scope + '".')

return { scope, requested: pending.length, dispatched }
