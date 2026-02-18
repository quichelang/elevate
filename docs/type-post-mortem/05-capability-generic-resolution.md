# 05. Rebuild Method Capability Generic Resolution

## Problem

Generic method typing currently relies on lossy substitutions.
Observed symptom: method calls like `Vec<T>.get(0)` can be resolved as if index arg must be `T`.

Root issue:
- method generic parameters are not represented distinctly enough from receiver/container generics,
- adapter substitution heuristics map uppercase type names positionally.

## Direction

Build a typed capability resolver with explicit generic environments:
- receiver generic bindings,
- method generic bindings,
- trait/inherent context bindings.

## Required Changes

1. Replace positional uppercase mapping heuristics in adapter path.
2. Preserve method signature generic symbols through parse/lookup conversion.
3. Unify capability resolution between:
- rustdex-backed methods,
- builtin overrides,
- local function/method signatures.

4. Introduce safe fallback:
- when generic binding cannot be proven, return `Unknown` with targeted diagnostic,
- never bind to incorrect concrete type.

## Transitional Safeguards

1. Add strict regression tests for:
- `get/get_mut/index` on generic collections,
- iterator predicates with generic captures,
- associated calls with mixed generic families.

2. Keep temporary explicit overrides for critical std APIs until resolver rewrite lands.

## Done When

1. Method/index capability typing no longer depends on fragile string heuristics.
2. `expected T, got i64` class of indexing/call errors disappears for valid code.
