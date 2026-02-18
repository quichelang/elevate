# 03. Add Reference-Aware Semantic Types

## Problem

`SemType` has no reference constructor. Borrow behavior is encoded through:
- stringified Rust parameter types,
- call argument modes,
- ad-hoc coercion checks.

This causes fragile compatibility logic and repeated special cases.

## Core Direction

Introduce first-class reference semantics in `SemType`, e.g.:
- `Ref { mutable: bool, inner: Box<SemType> }`.

## Required Refactors

1. Compatibility and coercion:
- make reference compatibility explicit (`T` vs `&T`, `&mut T`).
- remove string-based borrow checks where possible.

2. Method capability expected arguments:
- expected parameter types should include reference forms directly.

3. Lowering:
- lower from semantic reference types into Rust borrow expressions deterministically.

4. Diagnostics:
- report mismatches at semantic type level, not generated Rust string level.

## Migration Strategy

1. Add `Ref` variant and feature-flag internal handling first.
2. Port high-traffic paths:
- call typing,
- method capability matching,
- comparison coercions,
- indexing key handling.
3. Remove redundant reference heuristics after parity tests pass.

## Done When

1. Borrow/reference behavior is determined before lowering.
2. Most "expected `&T`/got `T`" issues are caught in type pass, not Rust backend.
