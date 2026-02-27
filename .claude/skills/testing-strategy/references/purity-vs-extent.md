# Purity vs Extent

Test speed is determined by **purity**, not by **extent**. Understanding this distinction prevents misguided optimization (e.g., mocking internal code to "speed up" tests).

## Purity: IO Involvement

Purity measures how much IO a test performs. Each step up adds roughly 5x runtime.

| Level | Latency | Example |
|-------|---------|---------|
| Pure computation | microseconds | Single-threaded, no IO |
| Filesystem IO | milliseconds | Reading/writing files |
| Process spawning | 10-100ms | IPC, subprocesses |
| Network IO | 100ms+ | External services |

Integration tests that spawn processes are inherently slow. Unit tests that avoid IO are inherently fast, regardless of how much code they exercise.

## Extent: Code Exercised

Extent measures how much code a single test exercises. Extent does **not** affect speed.

A pure test can exercise an entire compilation pipeline in microseconds. A test that calls a single function but hits the network takes 100ms+.

### Common Misconception

Mocking internal code to reduce extent:
- Does not improve speed (purity is unchanged)
- Reduces fidelity (test no longer verifies real behavior)
- Increases coupling (test depends on internal structure)

### Correct Optimization

Reduce purity level instead of extent:
- Replace network calls with in-memory fakes
- Replace filesystem IO with in-memory data structures
- Replace process spawning with direct function calls
- Keep the full code path exercised (high extent, high purity)

**Fakes vs mocks**: Fakes replace external boundaries (network, filesystem) with in-memory implementations that preserve the interface contract. Mocking internal collaborators to isolate units reduces fidelity without improving purity.