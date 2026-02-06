# Language Design (Single Source of Truth)

Status: Draft v0.1  
Owner: Language team  
Last updated: 2026-02-05

## 1. Purpose

This document defines the language we are building and how we compile it to Rust.

Goals of this doc:
- Be the source of truth for language behavior and compiler decisions.
- Prioritize practical, predictable implementation over theory-heavy design.
- Keep user-facing concepts Rust-aligned where possible (`struct`, `enum`, `const`, `static`).

Process note:
- Feedback about team workflow goes in `AGENTS.md`.
- Language and compiler behavior changes go in this document.

## 2. Product Direction

We are building a compiled language that transpiles to Rust with strong static typing and automatic compile-time decisions.

Guiding principles:
- Practical over academic.
- Deterministic over clever.
- Clear errors over magical inference.
- Small MVP surface before advanced features.

## 3. Locked MVP Constraints

These are intentionally strict for v0:

1. No traits in user language.
2. No references in user language (`&`, `&mut` not exposed).
3. No user-provided mutability hints.
4. Explicit immutability supported via `const` and `static`.
5. `struct` and `enum` are first-class and named like Rust.
6. Static typing with local type inference.
7. Compiler makes automatic ownership/memory decisions for lowered Rust code.
8. Generics compile by monomorphization with explicit safety limits.

## 4. Non-Goals (MVP)

- Reproducing Rust semantics exactly.
- Full trait system.
- Full borrow checker model in source language.
- Macro system.
- Async runtime design.
- Full-blown type-theory row-polymorphism implementation.

## 5. User-Facing Language Model

## 5.1 Declarations

- `const`: immutable local/global bindings.
- `static`: module-level static values.
- `struct`: nominal product types.
- `enum`: tagged unions.
- `fn`: functions with optional type annotations.

## 5.2 Types

Supported in MVP:
- Primitive scalars (`i64`, `f64`, `bool`, `str`/`string` as finalized later).
- Struct types.
- Enum types.
- Function types (as needed for first-order + limited closure support).
- Parametric generics.

## 5.3 Inference Policy

- Local inference is enabled.
- Public API boundaries should prefer explicit return types.
- Type errors must name concrete fields/types and expected vs actual.

## 5.4 Structural Capability Without Traits

We keep `struct` as the user model, but permit constrained generics using field requirements.

Example (illustrative syntax, not final):

```txt
fn norm<T where T has { x: f64, y: f64 }>(p: T) -> f64 {
  sqrt(p.x * p.x + p.y * p.y)
}
```

Notes:
- This is not exposed as a trait system.
- This is a compiler-level capability check against concrete `struct` shapes.
- Syntax can evolve; behavior intent is locked.

## 6. Practical Polymorphism Strategy

We use a practical hybrid:
- Prefer monomorphized specialized code for performance.
- Limit specialization to prevent compile-time/code-size explosions.
- Use controlled fallback representation when limits are exceeded.

Initial limits (tunable):
- Max specializations per function: `16`.
- Max specialization depth chain: `4`.

When limit is exceeded:
- Option A: compile error with actionable message.
- Option B: fallback path using boxed internal representation.

MVP default: start with Option A (simpler, more predictable), add Option B later if needed.

## 7. Ownership and Memory Lowering Policy

Type inference alone does not solve ownership. We define explicit lowering rules.

MVP ownership policy:
1. Values are immutable by default.
2. Lowered Rust uses move semantics by default.
3. Insert `clone` only at predefined boundary cases.
4. Avoid generating borrowed references in emitted Rust unless internal and proven safe.

Clone insertion boundary cases (initial):
- Value reused after move point.
- Captured by closure and also used outside capture path.
- Shared insertion into multiple owning containers.

Fallback strategy:
- If analysis cannot choose safely within rules, choose deterministic safe fallback:
  - Prefer clone.
  - If clone unavailable/too expensive and feature allows: use `Rc`/`Arc` wrapper policy.

This policy must be deterministic and test-covered.

## 8. Compiler Architecture

Pipeline:
1. Parse source -> untyped AST.
2. Name resolution + symbol table.
3. Type inference/checking -> typed core IR.
4. Ownership decision pass -> ownership-annotated IR.
5. Specialization/monomorphization planning.
6. Lower to Rust-oriented IR.
7. Emit Rust source.
8. (Optional) run `rustfmt` and compile checks in toolchain integration mode.

Two core IR layers are required:

1. Typed Core IR
- Language-centric, type-checked.
- Contains struct/enum/function semantics and field constraints.

2. Rust Lowered IR
- Close to Rust constructs.
- Explicit ownership operations (move/clone/wrap) and concrete generic specializations.

Rule: no pass may skip directly from AST to emitted Rust.

## 9. Data Model (Initial)

Required typed IR nodes:
- `Module`
- `StructDef`
- `EnumDef`
- `FnDef`
- `LetConst`
- `StaticDef`
- `Call`
- `FieldGet`
- `Match`
- `If`
- `Literal`
- `VarRef`

Required type nodes:
- `TyPrimitive`
- `TyStruct`
- `TyEnum`
- `TyGenericParam`
- `TyFn`
- `TyApplied`

Required ownership metadata:
- `OwnershipClass` (`Move`, `Copy`, `Clone`, `SharedRc`, `SharedArc`)
- `UseCount`
- `EscapeInfo`

## 10. Error Model

Compiler errors must be practical and fix-oriented.

Minimum standards:
- Point to source span.
- State expected vs actual.
- Explain which automatic decision failed.
- Provide one direct fix hint.

Examples:
- "Function `foo` exceeded specialization budget (16). Add explicit type annotation or simplify call shape."
- "Missing required field `x: f64` for call to `norm`."

## 11. MVP Feature Set

In:
- `const`, `static`
- `struct`, `enum`
- `fn`
- pattern matching on enums
- local type inference
- generic functions
- constrained generics by required fields
- deterministic ownership lowering
- Rust code generation

Out (defer):
- traits
- references/borrowing in source syntax
- mutation syntax
- macros
- async/await
- advanced effect system

## 12. Implementation Plan

Phase 0: Skeleton
- Parser + AST + module loader.
- Golden tests for parsing.

Phase 1: Types
- Name resolution and typed core IR.
- Inference/checking with clear diagnostics.

Phase 2: Ownership pass
- Implement deterministic move/clone policy.
- Add pass-level tests on ownership decisions.

Phase 3: Specialization
- Monomorphization planner + specialization cache.
- Enforce specialization budget.

Phase 4: Rust lowering/emission
- Emit compilable Rust for supported features.
- Snapshot tests from source -> Rust output.

Phase 5: Hardening
- Compile generated Rust in CI.
- Benchmark compile time and code size on sample workloads.

## 13. Testing Strategy

Must-have test categories:
- Parse tests.
- Type inference and type error tests.
- Ownership decision tests.
- Specialization budget tests.
- Codegen snapshot tests.
- End-to-end compile tests (source -> Rust -> binary/library check).

Quality gates before expanding language features:
- Deterministic output for same input.
- No panic on invalid user programs.
- Error messages include actionable hints.

## 14. Open Decisions

1. Exact source syntax for constrained generics (`where T has { ... }` equivalent).
2. String type model and runtime representation.
3. Whether specialization overflow defaults to error forever or gets fallback mode.
4. Closure support scope in MVP (none vs limited).
5. `Rc` vs `Arc` policy for shared fallback in single-thread vs multi-thread targets.

## 15. Change Management

For every semantic change:
1. Update this document first.
2. Add/adjust tests that lock behavior.
3. Implement compiler change.
4. Record version bump in the header.

This file is authoritative for language behavior unless explicitly superseded by a newer version entry.

## 16. Current Implementation Status

Status as of 2026-02-05:
- Rust compiler project initialized in this repository.
- Working pipeline exists: lex -> parse -> typed IR/type checks -> lowered Rust IR -> Rust emission.
- CLI entrypoint reads source and prints or writes generated Rust.
- Unit tests cover lexer, parser, type checks, and codegen behavior.
- Generated Rust is validated in tests via `rustc --crate-type=lib`.

Implemented language surface:
- Top-level items: `rust use`, `struct`, `enum`, `fn`, `const`, `static`.
- Statements: local `const`, `return`, expression statements.
- Expressions: literals, path refs/calls, field access, `match`, postfix `?`.
- Types: named paths + generic arguments (for forms like `Option<T>`, `Result<T, E>`).
- `Option`/`Result` constructor and `?` semantics with compile-time validation.

Current command surface:
- Build: `cargo build`
- Test: `cargo test`
- Run compiler: `cargo run -- <input-file>`
- Emit to file: `cargo run -- <input-file> --emit-rust <output-file>`

What is intentionally incomplete:
- Ownership lowering policy is not implemented yet.
- Generic function definitions and constrained generic bounds are not implemented yet.
- Borrow/reference features remain intentionally unsupported.
