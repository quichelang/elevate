# 06. Install Pass Invariants and Hard Regression Gates

## Problem

Bugs recur because pass boundaries do not enforce semantic invariants.
When one pass shifts behavior, later passes silently adapt or fail in generated Rust.

## Required Invariants

1. Signature stability:
- typed function/method signature contract must remain valid through lowering/codegen.

2. Receiver consistency:
- chosen `self` mode must align with return type and method body semantics.

3. Constraint consistency:
- where-clause and bounds data must be preserved without duplication ambiguity.

4. Reference consistency:
- semantic reference forms and lowered borrow operations must remain aligned.

## Enforcement Mechanisms

1. Add internal verification pass(es) between major stages:
- after type inference,
- after lowering,
- before codegen.

2. Add debug assertions for common drift paths:
- param mode changes,
- return ownership mismatches,
- implicit borrow insertion on reassigned values.

3. Keep adversarial tests as permanent CI targets.

## Test Gate Strategy

1. Tiered test suites:
- parser-edge suite,
- generic capability suite,
- mutability/ownership adversarial suite.

2. Any newly fixed adversarial failure must remain as a non-flaky regression test.

3. Fail fast on invariant violations before generating Rust code.

## Done When

1. Most failures are frontend/type-pass diagnostics, not backend Rust mismatches.
2. Regressions are caught at boundary checks rather than discovered via broad sweeps.
