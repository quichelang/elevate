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
- Host platform input abstraction (`platform-host`) with macOS raw-mode backend.
- Reusable runtime loop.
- Fixed-step planning helpers (`game-loop`).
- Terminal widget layer (`game-ui-term`).
- Snapshot/replay serialization support (`game-save`).
- AI hint traits plus Sudoku backtracking hint solver (`game-ai`).
- Sudoku board model (fixed cells, conflict checks, completion checks).
- Keyboard-driven game controls, board rendering, save/load, and hints.

## Next Engine Packages (Remaining)

- `crates/platform-linux` (raw mode + key decoding backend behind `platform-host`).
- `crates/platform-windows` (console input backend behind `platform-host`).
- `crates/game-scenes` (scene stack/router, transitions, modal layering).
- `crates/game-board` (shared grid/board primitives for multiple board games).
