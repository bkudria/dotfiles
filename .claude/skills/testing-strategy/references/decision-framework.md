# Test Decision Framework

Detailed guidance for deciding what to test, at what level, and how to evaluate test quality.

## The Refactoring Test

Well-designed tests remain valid across small and medium refactors. Apply this check:

1. Imagine an internal change that preserves all external behavior
2. Run the tests mentally against the refactored code
3. If any test fails, that test is coupled to implementation, not behavior

**Signs of coupling:**
- Tests that verify internal data structures or private methods
- Tests that assert *how* something works rather than *what* it does
- Tests that mock internal collaborators to isolate a single class

**Signs of good tests:**
- Tests that verify public interfaces and contracts
- Tests that assert observable outcomes (return values, side effects, error messages)
- Tests that work against any correct implementation

## The Neural Network Test

Could this test verify a black-box implementation? Replace the code under test with a sufficiently capable neural network. If the test still works, it is testing features. If it fails, it is testing implementation.

**Passes the test (good):**
- "Given input X, output should be Y"
- "Invalid input should produce error message Z"
- "State should not be persisted if the operation fails"

**Fails the test (suspect):**
- "Method A should call method B with argument C"
- "Internal cache should contain key K after operation"
- "The parser should use a recursive descent strategy"

## Behavioral Requirements vs Internal Logic

### Test Behavioral Requirements at Integration Level

Behavioral requirements are observable at the system boundary. Test them there.

**Examples of behavioral requirements:**
- "State should not be persisted if the operation fails" (atomicity)
- "Invalid input should produce an error message"
- "Schema violations should be reported with the path"
- "Output format should match the specification"

These are system behaviors, not internal logic. Testing them at the unit level creates coupling to the mechanism that implements them.

**Anti-pattern:** Writing unit tests for the *mechanism* that implements a behavioral requirement. This tests implementation, not behavior.

### Test Internal Logic at Unit Level

Unit tests are appropriate for internal logic that:
- Has complex algorithms with many edge cases
- Would require many integration tests to exercise all paths
- Involves parsing, transformation, or computation
- Is difficult to observe directly through system-level output

### Decision Flow

When deciding what tests to write for a change:

| Question | Answer | Action |
|----------|--------|--------|
| Does the change add or modify a behavioral requirement? | Yes | Write an integration test |
| Does the change involve complex internal logic? | Yes | Write a unit test |
| Is the change a refactoring that preserves existing behavior? | Yes | Existing tests should suffice |
| None of the above? | -- | Prefer integration level; test at the boundary |

When in doubt, prefer testing at the integration level. Integration tests verify behavior regardless of implementation, enabling refactoring without test changes.

## Test Economy

Integration tests are slow because they involve IO -- this is inherent, not fixable. Balance two concerns:

1. **Minimize impure tests** -- Prefer extending an existing test scenario to creating new test infrastructure
2. **Reduce friction** -- Make adding tests simple and straightforward

### Prefer Extending Over Creating

Adding a new assertion to an existing test scenario:
- Reuses existing setup and teardown
- Adds coverage without adding IO overhead
- Keeps related behaviors documented together

### Create New Tests When

- Extending an existing scenario would make it incoherent or confusing
- The feature requires a fundamentally different setup
- The scenario would no longer serve as good documentation
- The new behavior is independent enough to warrant isolation
