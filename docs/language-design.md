# Language Design (Single Source of Truth)

Status: Draft v0.4  
Owner: Language team  
Last updated: 2026-02-06

## How To Read This Document

This document is split by decision category:
- `Requires Your Input`: items blocked on product/language decisions.
- `Stable (Unlikely To Change Frequently)`: core constraints and contracts we should avoid changing casually.
- `Informational (Implementation + Progress)`: status, roadmap, and operational notes.

Process note:
- Feedback about team workflow goes in `AGENTS.md`.
- Language and compiler behavior changes go in this document.

## Requires Your Input

### Still Open Decisions

1. **Specialization overflow policy**
- Decide default behavior when specialization budget is exceeded:
- Option A: hard compile error.
- Option B: fallback to boxed/internal representation.
- Current preference: Option A.

### Decided Inputs

1. **Constrained generic syntax**
- Decision: support both Option A and Option C for maximum expressiveness.
- Option A: `fn norm<T where T has { x: f64, y: f64 }>(p: T) -> f64`
- Option C: `fn norm(p: { x: f64, y: f64, .. }) -> f64`
- Note: Option A is preferred when reuse across multiple parameters/returns reduces duplication.
- Note: Option C is preferred for concise single-use shape constraints.

2. **Rust interop trait strategy**
- No traits in Elevate source language.
- MVP mechanism: external wrapper `.rs` files for Rust trait integration.
- MVP+1 mechanism: inline `rust { ... }` escape blocks.

3. **Source extension**
- `.ers` is canonical.

4. **Generic function definitions**
- Required for MVP.

## Stable (Unlikely To Change Frequently)

### Product Direction

We are building a compiled language that transpiles to Rust with strong static typing and automatic compile-time decisions.

Guiding principles:
- Practical over academic.
- Deterministic over clever.
- Clear errors over magical inference.
- Small MVP surface before advanced features.

### Locked MVP Constraints

1. No traits in user language.
2. No references in user language (`&`, `&mut` not exposed).
3. No user-provided mutability hints.
4. Explicit immutability supported via `const` and `static`.
5. `struct` and `enum` are first-class and Rust-aligned in naming.
6. Static typing with local type inference.
7. Compiler makes automatic ownership/memory decisions in lowered Rust.
8. Generics compile by monomorphization with explicit safety limits.
9. Closures are deferred to MVP+1.
10. To support substantial real-world verification and validation, MVP must include:
- Conditional blocks (`if` / `else`).
- Loop constructs.
- Struct functions (associated functions/methods).
- Public/private visibility controls.

### Non-Goals (MVP)

- Reproducing Rust semantics exactly.
- Full trait system.
- Full source-level borrow checker model.
- Macro system.
- Async runtime design.
- Full research-style row-polymorphism implementation.

### Language Contract (Current)

Declarations:
- `const`, `static`, `struct`, `enum`, `fn`, `rust use`.

Expressions/statements:
- Literals, path refs/calls, field access, `match`, postfix `?`, local `const`, `return`.
- Conditionals and loops are required MVP features (implementation pending).
- Required operator/features for substantial programs:
- Ranges (`..`).
- Slices.
- Spread-like expansion (`...`) where applicable by design.
- Logical boolean ops: `and`, `or`, and negation via both `!` and `not`.
- Comparison operators: `==`, `!=`, `<`, `<=`, `>`, `>=`.
- Automatic return of the last value in a block/function when trailing semicolon is omitted (Rust-like tail expression behavior).

Type policy:
- Local inference is enabled.
- Public API boundaries should prefer explicit return types.
- Errors must include concrete expected vs actual details.

String policy:
- Source-level `String` is immutable by default from the language perspective.
- String concatenation (for example `"hello" + "world"`) may be lowered using mutable builder internals for performance.
- Rust interop may use native Rust string operations (`String::new`, `push_str`) inside interoperability boundaries, without exposing general mutable references in Elevate source.

Rust interop policy:
- Interoperability with Rust crates is supported.
- Trait implementations required by Rust frameworks are handled via Rust-side adapters (wrapper `.rs` files in MVP).
- Future direction: optional inline `rust { ... }` escape blocks (MVP+1).

### Core Compiler Architecture

Pipeline contract:
1. Parse source -> untyped AST.
2. Name resolution + symbol table.
3. Type inference/checking -> typed core IR.
4. Ownership decision pass -> ownership-annotated IR.
5. Specialization planning.
6. Lower to Rust-oriented IR.
7. Emit Rust source.

IR boundary contract:
- Typed Core IR: language-centric and type-checked.
- Rust Lowered IR: Rust-close representation with explicit lowered operations.
- No direct AST -> Rust source shortcut.

### Error Model Contract

Minimum standards:
- Include source location.
- Show expected vs actual.
- Explain which automatic decision failed.
- Provide one direct fix hint.

### Change Management Contract

For semantic changes:
1. Update this document first.
2. Add/adjust tests to lock behavior.
3. Implement compiler change.
4. Bump version in this document header.

## Informational (Implementation + Progress)

### Current Implementation Status (As of 2026-02-06)

Implemented:
- Rust compiler project initialized.
- End-to-end pipeline exists: lex -> parse -> typed checks -> lowered Rust IR -> emit.
- Rust emission includes full function bodies (no `todo!` stubs for supported features).
- `Option`/`Result` constructors plus `?` validation.
- Enum `match` expressions.
- Visibility controls for top-level items (`pub` vs private defaults).
- Control-flow statements: `if` / `else` and `while`.
- Struct functions via `impl` blocks (associated methods).
- Logical boolean operators: `and`, `or`, `!`, `not`.
- Comparison operators: `==`, `!=`, `<`, `<=`, `>`, `>=`.
- Tail-expression returns for final function/method expressions.
- Range expressions: `..` and `..=`.
- Tuple literals and local tuple destructuring bindings.
- Expanded `match` patterns: tuple patterns, literal patterns, binding patterns, and nested variant payload patterns.
- `rust use` imports and external Rust path calls.
- Crate build flow for `.ers` projects that transpiles into `target/elevate-gen`.

Quality status:
- Unit tests cover lexer, parser, semantic checks, and codegen behavior.
- Generated Rust is validated via `rustc --crate-type=lib` in tests.

### Command Surface

- Build: `cargo build`
- Test: `cargo test`
- Run compiler: `cargo run -- <input-file>`
- Emit to file: `cargo run -- <input-file> --emit-rust <output-file>`
- Build `.ers` crate (debug): `cargo run -- build <crate-root>`
- Build `.ers` crate (release): `cargo run -- build <crate-root> --release`

### Current Source Extension

- Canonical examples now use `.ers`:
- `examples/point.ers`
- `examples/result_flow.ers`
- `examples/match.ers`

### `.ers` Crate Build Integration

Behavior:
- Input crate is expected to have `Cargo.toml` and `src/`.
- All `.ers` files under `src/` are transpiled to `.rs` under `target/elevate-gen/src/` with identical relative paths.
- Non-`.ers` files under `src/` are copied through unchanged.
- `Cargo.toml` is copied to `target/elevate-gen/Cargo.toml`.
- Build step runs `cargo build --manifest-path <crate>/target/elevate-gen/Cargo.toml`.
- Build step runs cargo with `--target-dir <crate>/target`, so final artifacts land in the standard:
- `<crate>/target/debug`
- `<crate>/target/release`

Safety:
- Path collisions between generated `.rs` outputs and copied files are treated as errors.

### Implementation Plan (Roadmap)

Phase 0: Skeleton
- Parser + AST + module loader.

Phase 1: Types
- Name resolution and typed core IR.
- Inference/checking with diagnostics.

Phase 2: Ownership pass
- Deterministic move/clone policy.

Phase 3: Specialization
- Planner + specialization cache.

Phase 4: Rust lowering/emission hardening
- Stable, compilable Rust output for all MVP features.

Phase 5: Performance hardening
- Compile-time and code-size benchmarking.

### Testing Strategy (Operational)

Must-have categories:
- Parse tests.
- Type inference and type error tests.
- Ownership decision tests.
- Specialization budget tests.
- Codegen snapshot tests.
- End-to-end compile tests.

Quality gates:
- Deterministic output for same input.
- No panic on invalid programs.
- Actionable diagnostics.

### Known Incomplete Areas

- Ownership lowering policy implementation is not complete.
- Generic function definitions and constrained bounds are not complete.
- Additional loop forms beyond `while` are not complete.
- Ranges, slices, and spread-like expansion are not complete.
- Slices are not complete.
- Deep destructuring coverage (all contexts) is not complete.
- Match guards and full exhaustiveness diagnostics are not complete.
- Full tail-expression return behavior across all nested block forms is not complete.
- Borrow/reference features remain intentionally unsupported.
- Closure support is deferred to MVP+1.
