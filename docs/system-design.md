# System Design

Date: 2026-02-11
Scope: Structural organization of Elevate compiler components, their current capabilities, expectations, constraints, and planned evolution.

## Overview (Logistics First)

This document defines the operational shape of the compiler pipeline and a shared vocabulary for design and implementation discussions.

The pipeline is organized as:

1. **Frontend**: source text -> tokens -> AST.
2. **Type-System Passes**: AST -> typed IR, including inference, capability resolution, and diagnostics.
3. **Ownership and Lowering**: typed IR -> Rust lowered IR with borrow/move/clone decisions.
4. **Codegen and Build**: lowered IR -> Rust source -> cargo/rustc build and execution flow.
5. **Verification**: unit/integration tests and benchmark cadence for regressions.

Logistically, each stage has a strict contract:

- A stage must emit either valid output for the next stage or concrete diagnostics.
- Later stages should not re-resolve decisions that earlier stages have already finalized.
- Ownership and performance behavior are validated with explicit benchmark and clone-note checks.

## Terminology (Canonical)

The following terms are canonical and used consistently in this document.

- **AST**: parsed Elevate syntax tree.
- **SemType**: internal semantic type model used during type inference and compatibility checks.
- **Typed IR**: typed intermediate representation produced by type-system passes.
- **Lowered IR**: Rust-oriented intermediate representation used for code generation.
- **Capability**: a resolved operation contract for a type/method/index form (for example direct index or get-like index).
- **Index Metadata**: typed index contract attached to typed index expressions:
  - mode (direct or get-like)
  - key type and value type
  - key passing mode (`Borrowed`, `Owned`, `CloneIfNeeded`)
  - source (`Builtin` or `CustomMethod`)
- **Ownership Plan**: per-expression usage model that drives borrow/move/clone choices during lowering.
- **Constraint Partition**: grouped incompatibility view built from observed types, compatibility links, and union-find partitions.
- **Type-System Mode**: `--exp-type-system` enabled; inference and capability paths are stricter and more complete.
- **Strict Mode**: default mode without `--exp-type-system`.
- **Rustdex Preflight**: metadata readiness check required before type-system mode capability resolution.

## Component Organization

### 1) Frontend (`src/lexer.rs`, `src/parser.rs`, `src/ast.rs`)

Current capabilities:
- Lexing and parsing for Elevate source constructs (functions, impls, traits, enums, match, indexing, etc).
- Produces AST with source spans used by diagnostics.

Expectations and requirements:
- Deterministic parse output for equivalent inputs.
- Stable syntax contracts for downstream type-system behavior.
- Spans should remain precise enough for high-signal diagnostics.

Future plans:
- Continue syntax coverage expansion where it unblocks type-system ergonomics.
- Keep parser complexity bounded; avoid introducing ambiguity that destabilizes diagnostics.

### 2) Type-System Passes (`src/passes.rs`, `src/passes/index_capability.rs`)

Current capabilities:
- Local inference and compatibility checks via `SemType`.
- Capability-first method/index reasoning.
- Typed index metadata now carries resolved capability details; lowering consumes this directly.
- Constraint-partition diagnostics for conflicting inference groups.

Expectations and requirements:
- No lowering-time panic for unresolved index capability paths.
- Diagnostics should be emitted at inference/type stage with actionable mismatch context.
- Index key/value typing must be derived from capability resolution, not ad-hoc lowering logic.

Future plans:
- Move more conflict handling from diagnostics-only partitioning into solver decisions.
- Improve principality handling and deterministic tie-break rules for ambiguous contexts.
- Expand custom indexing contract precision (including richer borrowed-key semantics where source metadata allows it).

### 3) Ownership and Lowering (`src/passes.rs`, `src/ownership_planner.rs`)

Current capabilities:
- Ownership planner tracks expression reuse and conflicting consumers.
- Lowering applies borrow/move/clone decisions using typed metadata and planner state.
- Custom indexing uses `CloneIfNeeded` semantics for key passing; clone inserted only when reuse requires it.
- Builtin map indexing now optimizes return handling: `copied()` for `Copy` values, `cloned()` otherwise.

Expectations and requirements:
- Maintain source-level ergonomics while preserving Rust ownership correctness.
- Keep clone insertion explainable and measurable through ownership notes and benchmarks.
- Avoid duplicate decision systems for ownership (typed metadata + planner should be the single path).

Future plans:
- Improve key passing decisions with richer borrowed/owned signal propagation from resolved signatures.
- Continue reducing unnecessary clones in hot paths without sacrificing correctness.

### 4) Internal Data Module (`src/data/`)

Current capabilities:
- In-house data structures for compiler-oriented usage:
  - hash table, ordered map/set
  - slot map, interner
  - union-find
  - constraint graph
- Used in type-system/diagnostic flows for deterministic and low-overhead operations.

Expectations and requirements:
- Keep dependency surface lean (no new third-party DS crates for core compiler data paths).
- Deterministic iteration/order where diagnostics and generated output depend on ordering.
- Correctness and compatibility verified by dedicated tests.

Future plans:
- Increase direct usage in solver and capability pipelines.
- Continue replacing ad-hoc standard collections where deterministic or performance-sensitive behavior needs stronger control.

### 5) Rustdex Integration (`src/rustdex_backend.rs`)

Current capabilities:
- Preflight check for metadata availability in type-system mode.
- Supports capability cutover and strict metadata-backed behavior for sensitive typing paths.

Expectations and requirements:
- Hard-fail semantics in type-system mode if metadata is unavailable.
- No silent fallback that weakens capability guarantees.

Future plans:
- Expand metadata usage to improve borrowed/owned argument mode precision.
- Keep fallback behavior explicit and diagnosable.

### 6) Codegen and Build (`src/codegen.rs`, `src/crate_builder.rs`)

Current capabilities:
- Emits Rust from lowered IR.
- Integrates with build/test harness paths for end-to-end execution.

Expectations and requirements:
- Generated Rust should remain readable and consistent.
- Type/lowering decisions should be reflected directly in emitted code shape for testability.

Future plans:
- Continue emission simplification as typed/lowering contracts become stricter and less redundant.

### 7) Quality Gates (`tests/`, `docs/bench-latest.csv`)

Current capabilities:
- Extensive unit/integration suites, including type-system stories and DS-focused tests.
- Benchmark cadence tracks median/p95 plus clone/hot-clone counters.

Expectations and requirements:
- For ownership/lowering/interop changes, required cadence:
  - `cargo test -q`
  - `cargo run -q -- bench --iters 20 --warmup 3 --out /Volumes/Dev/code/jagtesh/elevate/docs/bench-latest.csv`
- Regressions must be called out when clone/hot-clone counters rise or latency drifts materially.

Future plans:
- Expand targeted regressions around index inference, borrowed/owned key passing, and principality diagnostics.

## Cross-Cutting Design Rules

1. **Single Source of Truth Per Decision**
- Type decisions occur in type-system passes.
- Ownership decisions occur in lowering with ownership planner inputs.
- Later stages consume metadata; they should not reinterpret contracts.

2. **Diagnostics Before Rustc**
- Errors should surface in Elevate terms before rustc-level failures whenever possible.

3. **Determinism Over Cleverness**
- Stable output ordering and stable diagnostics are preferred over opaque heuristics.

4. **Lean Runtime Surface**
- Internal data structures are preferred when they reduce dependency bloat and preserve compiler-focused behavior.

## Near-Term Plan

1. Integrate constraint graph + union-find deeper into unification decisions (not only diagnostics).
2. Strengthen index key passing inference for borrowed custom key contracts where signature metadata is available.
3. Expand regression coverage for non-primitive/enum key indexing and ownership-sensitive reuse paths.
4. Continue benchmark-gated clone reduction in lowering hot paths.
