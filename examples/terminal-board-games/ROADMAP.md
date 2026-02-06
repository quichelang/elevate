# Sudoku + Terminal Engine Development Requirements vs Compiler Incomplete Areas

This file maps active compiler-level "Known Incomplete Areas" to what matters for building complete board games (starting with Sudoku).

## Priority Tiers

## Tier 1 (Needed Soon)

1. Ownership lowering planner
- Why: game loops are hot paths; excess auto-clones increase frame latency and memory churn.
- Needed outcome: deterministic and more optimal move/borrow/clone decisions for recurring event/update/render loops.

2. Full pattern parity + richer exhaustiveness
- Why: game input/state dispatch benefits heavily from expressive and verified matching.
- Needed outcome: broader pattern set plus stronger diagnostics for guarded/stateful matches.

3. Object-native standard patterns
- Why: engine/game APIs are best maintained via instance/object-style method surfaces.
- Needed outcome: standardized language/library pattern for object-native APIs (beyond per-project conventions).

## Tier 2 (Useful for Engine Growth)

1. Generic bound enforcement expansion
- Why: reusable engine packages need stronger generic contracts.
- Current: syntax + emission + initial `Clone`/`Copy` checks.
- Needed outcome: broader trait-bound checking parity.

2. Iterator ergonomics completion
- Why: board/state traversal and event pipelines should stay concise and predictable.
- Current: major iterator-method paths are already supported.
- Needed outcome: close remaining edge cases and consistency gaps.

3. Slice completeness
- Why: board operations frequently use windows/slices and pattern destructuring.
- Needed outcome: fuller slice feature parity and ownership-friendly lowering.

## Tier 3 (Interop Hardening)

1. Interop full type compatibility checking
- Why: external integration (system APIs, platform services) needs safer adapter validation.
- Current: declaration shape + arity + lightweight literal type checks.
- Needed outcome: richer static type compatibility across phases.

## Sudoku-Specific Engine Scope (Already Implemented Here)

- Terminal renderer abstraction.
- macOS input abstraction and raw keyboard mode.
- Reusable runtime loop.
- Sudoku board model (fixed cells, conflict checks, completion checks).
- Keyboard-driven game controls and board rendering.

## Next Engine Packages (Suggested)

- `crates/game-loop` (fixed timestep and deterministic simulation helpers).
- `crates/game-ui-term` (widgets: panels, menus, focus, prompts).
- `crates/game-save` (serialization/versioning, replay logs).
- `crates/game-ai` (solver hooks and move hint APIs).
