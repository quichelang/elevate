# Language Design (Single Source of Truth)

Status: Draft v0.8  
Owner: Language team  
Last updated: 2026-02-08

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
- Traits are fully supported in Elevate source language (trait declarations, supertraits, and trait unions using `+`).
- Source-level `dyn` syntax is intentionally not exposed.
- The compiler infers trait-object lowering (`dyn` plus required pointer operators) during type/lowering passes.

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

1. Traits are supported in user language, including trait declarations and trait unions (`A + B`).
2. Source-level `dyn` prefix/operator is not supported; trait-object form is inferred by compiler type/lowering.
3. No references in user language (`&`, `&mut` not exposed).
4. No user-provided mutability hints.
5. `const` provides explicit immutable bindings.
6. `static` provides static-lifetime/storage-duration declarations (not a mutability feature).
7. `struct` and `enum` are first-class and Rust-aligned in naming.
8. Static typing with local type inference.
9. Compiler makes automatic ownership/memory decisions in lowered Rust.
10. Generics compile by monomorphization with explicit safety limits.
11. Closures are MVP+1 scope and currently available in compiler preview form.
12. To support substantial real-world verification and validation, MVP must include:
- Conditional blocks (`if` / `else`).
- Loop constructs.
- Struct functions (associated functions/methods).
- Public/private visibility controls.

### Non-Goals (MVP)

- Reproducing Rust semantics exactly.
- Explicit source-level trait-object syntax (`dyn`, `&dyn`, `Box<dyn>`) as user-authored syntax.
- Full source-level borrow checker model.
- Macro system.
- Async runtime design.
- Full research-style row-polymorphism implementation.

### Language Contract (Current)

Declarations:
- `const`, `static`, `struct`, `enum`, `fn`, `use`.
- `static` is source-level static-lifetime storage; mutable statics (`static mut`) are not exposed in Elevate syntax.

Expressions/statements:
- Literals, path refs/calls, field access, `match`, postfix `?`, local `const`, `return`.
- Conditionals and loops are required MVP features.
- Required operator/features for substantial programs:
- Ranges (`..`).
- Slices.
- Logical boolean ops: `and`, `or`, and negation via both `!` and `not`.
- Comparison operators: `==`, `!=`, `<`, `<=`, `>`, `>=`.
- Automatic return of the last value in a block/function when trailing semicolon is omitted (Rust-like tail expression behavior).
- Comments: `//` line comments and `/* ... */` block comments.
- Raw multiline string literals using Rust-style raw syntax (for example `r#"line1\nline2"#` without escape processing).

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
- Control-flow statements: `if` / `else`, `while`, and `loop` (`break` / `continue`).
- `for ... in ...` loops (range, `Vec`/`String`/`Option` iterator-method forms, plus `HashMap`/`BTreeMap` and `HashSet`/`BTreeSet` iterator producers) lower directly to Rust `for`.
- Struct functions via `impl` blocks (associated methods).
- Struct literal expressions (`Type { field: value }`) including support for imported Rust struct paths.
- Logical boolean operators: `and`, `or`, `!`, `not`.
- Comparison operators: `==`, `!=`, `<`, `<=`, `>`, `>=`.
- Tail-expression returns for final function/method expressions.
- Range expressions: `..` and `..=`.
- Arithmetic remainder operator `%`.
- `Vec` indexing and range slicing expressions (`values[i]`, `values[a..b]`).
- Index assignment targets for vectors (`values[i] = v`).
- Slice-adjacent `Vec` methods `first`, `last`, and `get(i)` are typed and lowered.
- `Vec::push` method support with inferred mutable receiver lowering.
- Heterogeneous tuple support with Rust-like semantics (tuple literals, tuple type annotations, and tuple destructuring bindings in const/assignment/`for` contexts).
- Slice destructuring bindings for `Vec` values in `const`/`for` patterns (`[head, ..tail]`, `[left, right]`).
- Generic function definitions with callsite type inference and trait-style bound syntax (for example `fn id<T>(x: T) -> T`, `fn keep<T: Clone + Copy>(x: T) -> T`).
- Trait declarations with supertraits (`trait A: B + C { ... }`).
- Trait-object shorthand types in source (`Trait`, `A + B`) with compiler-inferred lowering to Rust trait objects (`dyn` with inferred pointer/operator context).
- Array/vector literals (`[a, b, c]`) with inferred element type and `Vec` lowering.
- Expanded `match` patterns: tuple patterns, literal patterns, binding patterns, and nested variant payload patterns.
- Struct rest match patterns (`Type { field, .. }`) with diagnostics for missing required fields when rest is omitted.
- Local enum tuple payload variants with multiple fields (for example `Pair(i64, i64)`) across definitions, constructors, and match patterns.
- Match arm block expressions (`pattern => { ... };`).
- Match guards (`pattern if condition => ...`).
- Or-patterns (`p1 | p2`).
- Slice/rest `match` patterns (`[a, ..tail]`, `[.., last]`, `[]`).
- Imported Rust enum variant pattern matching when scrutinee type is known (for example `Ordering::Less`).
- Baseline match exhaustiveness diagnostics for `bool`, finite tuple domains (bool + enum-variant domains, including `Option`/`Result` with total payload patterns), `Option`, `Result`, and known local enums.
- Exhaustiveness analysis is guard-aware for static-true guards (`if true`) while still conservatively excluding other guarded arms.
- Closure expressions and closure calls with typed parameters.
- Comment support (`//` and `/* ... */`) and raw multiline string literals.
- Character literals (`'a'`, escaped forms like `'\n'`).
- `use` imports and external Rust path calls.
- Inline `rust { ... }` escape blocks (top-level and statement position) that pass raw Rust through without Elevate parsing.
- Centralized interop policy registry for clone/borrow/shim behavior.
- Auto-borrow coverage for selected associated Rust calls (String/Option/Result/Vec/HashMap/BTreeMap/HashSet/BTreeSet cases).
- Method-call ownership lowering for known Rust receiver methods:
- borrowed receiver/arg handling for non-consuming calls (for example `len`, `contains`, `contains_key`);
- auto-clone insertion for reused owned receivers on consuming-style calls when clone-safe.
- Owned-operand chain lowering now applies clone insertion for consuming expression operands (for example repeated consuming method chains inside a single expression).
- String interop shims for owned-return helpers (`str::strip_prefix_known`, `str::split_once_known`) with borrow-safe lowering.
- Crate build flow for `.ers` projects that transpiles into `target/elevate-gen`.
- Crate build diagnostics now include line/column output (plus symbol declaration hints when span data is coarse).
- Crate build loop now includes adaptive borrow/clone feedback from Rust diagnostics (retrying transpile/build with inferred interop borrow hints and forced-clone places).
- Interop contract preview via crate-level `elevate.interop` file:
- allow-list validation for `use` imports.
- deterministic generated adapter module (`elevate_interop.rs`) from declared adapter entries.
- adapter module auto-injection into generated crate root (`lib.rs`/`main.rs`) when present.
- contract-declared adapter aliases are automatically rewritten at crate transpile time to generated adapter calls.
- adapter callsites are validated during crate transpile for arity plus lightweight literal argument type compatibility.
- generated interop adapters normalize `&str` parameters to owned `String` wrapper params and borrow internally when calling Rust targets.
- Object-native parser API preview for lexopt via `Parser` impl methods that thread `self`.
- Native test framework pipeline for `.ers` crates:
- discovers `test_*` functions,
- injects generated test wrappers,
- transpiles and runs tests via `cargo test`.
- Native assert function forms in Elevate source (non-macro syntax): `assert(...)`, `assert_eq(...)`, `assert_ne(...)`.
- First-class non-consuming read views via `view(...)` (lowered to Rust borrows).
- CLI support for `test` subcommand and experiment flag toggles.
- Lexopt case study API now centered on `Parser` impl methods (compatibility wrapper surface removed).
- Instance method-call syntax (`value.method(...)`) resolves against user `impl` methods, enabling object-native API style beyond associated-call form.
- Neon Boardwalk migration progress: board parsing/state transitions now run in Elevate (`runtime.ers`), while terminal raw-mode/input rendering remains in a narrow Rust host module (`host.rs`).

Quality status:
- Unit tests cover lexer, parser, semantic checks, and codegen behavior.
- Generated Rust is validated via `rustc --crate-type=lib` in tests.
- Generated condition formatting avoids unnecessary outer parentheses for single-clause `if`/`while` conditions.

### Command Surface

- Build: `cargo build`
- Test: `cargo test`
- Run compiler: `cargo run -- <input-file>`
- Emit Rust to stdout: `cargo run -- <input-file> --emit-rust`
- Emit Rust to file: `cargo run -- <input-file> --emit-rust <output-file>`
- Build `.ers` crate (debug): `cargo run -- build <crate-root>`
- Build `.ers` crate (release): `cargo run -- build <crate-root> --release`
- Test `.ers` crate: `cargo run -- test <crate-root>`

### Current Source Extension

- Canonical examples now use `.ers`:
- `examples/point.ers`
- `examples/result_flow.ers`
- `examples/match.ers`

### `.ers` Crate Build Integration

Behavior:
- Input crate is expected to have `Cargo.toml` and `src/`.
- All `.ers` files under `src/` are transpiled to `.rs` under `target/elevate-gen/src/` with identical relative paths.
- Generated crate output auto-injects missing `mod`/`pub mod` declarations for transpiled `.ers` modules (including nested module trees), so source crates do not need empty `.rs` sibling stubs.
- Non-`.ers` files under `src/` are copied through unchanged.
- `Cargo.toml` is copied to `target/elevate-gen/Cargo.toml`.
- Build step runs `cargo build --manifest-path <crate>/target/elevate-gen/Cargo.toml`.
- Build step runs cargo with `--target-dir <crate>/target`, so final artifacts land in the standard:
- `<crate>/target/debug`
- `<crate>/target/release`
- Compilation failures in `.ers` crate builds are reported with source-relative line/column context.
- Optional interop contract file: `<crate-root>/elevate.interop`.
- `allow` directives gate which `use` imports are permitted.
- `adapter` directives generate deterministic Rust adapter functions in `target/elevate-gen/src/elevate_interop.rs`.
- adapter alias path calls in Elevate source are rewritten to `elevate_interop` generated functions during transpile.

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

- Ownership lowering policy now includes place-level conflict analysis, loop-weighted liveness heuristics, and adaptive borrow feedback from Rust compile diagnostics; a globally optimal whole-program planner is still not complete.
- Borrow-free read ergonomics now include first-class non-consuming read views via `view(...)` (lowered to Rust borrows), but richer view ergonomics and reference-typed inference are still pending.
- Generic constrained bounds now include compile-time enforcement for `Clone`, `Copy`, `Debug`, `Default`, `PartialEq`, `Eq`, `PartialOrd`, `Ord`, and `Hash`; full trait-solver parity and path-qualified trait handling are still pending.
- Full iterator-model ergonomics for `for` loops are not complete beyond current lowering support.
- Slices are not complete (current support focuses on `Vec` literals/index/range expressions and slice-style match patterns).
- Full exhaustiveness diagnostics are not complete (current checks focus on bool/finite-tuple finite domains and enum variants).
- Full Rust-pattern parity for match is not complete (remaining work includes ref/binding-mode patterns and richer guard/exhaustiveness combinations).
- Interop contract signature verification now covers declaration shape, callsite arity, and lightweight literal argument type checks, but not full type-level compatibility across all compiler phases.
- Object-native parser APIs are now language-capable via instance method calls, but a reusable standard-library pattern/module is still pending.

### Experimental Feature Flags (Opt-In)

These are intentionally non-default and must be enabled explicitly.

- `exp_move_mut_args`
  - Ownership move-by-default experiment for mutation-capable call flows.
  - Intended behavior: when a function needs to mutate an argument, the argument is passed by value and ownership is returned explicitly (state-threading style), avoiding source-level reference syntax.
  - Goal: keep user syntax simple while preserving deterministic ownership behavior.

- `exp_infer_local_bidi`
  - Koko-inspired local bidirectional inference mode.
  - Intended behavior: aggressive local inference inside function bodies, with annotations still preferred/required at public API boundaries.
  - Goal: practical inference without unstable global solver complexity.

- `exp_effect_rows_internal`
  - Internal row-like effect/capability tracking metadata (not exposed as source syntax).
  - Intended behavior: compiler tracks operation requirements/effects to improve diagnostics, interop lowering, and specialization planning.
  - Goal: gain row-polymorphism-like utility without exposing a full research-style type/effect surface in MVP.

- `exp_infer_principal_fallback`
  - Principal-type fallback diagnostics mode.
  - Intended behavior: when inference cannot find a stable principal type, emit deterministic guidance for one explicit annotation site rather than cascading errors.
  - Goal: practical developer ergonomics for larger codebases.
