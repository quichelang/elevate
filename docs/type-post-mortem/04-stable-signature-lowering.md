# 04. Stop Post-Lowering Signature Mutation

## Problem

Current pipeline mutates function/method signatures after lowering (borrow promotion).
That breaks type contract stability and introduces semantic drift:
- owned params become borrowed unexpectedly,
- assignments/returns then fail due to changed types,
- ownership behavior is no longer traceable to source semantics.

## Principle

Once typed IR is produced, function signatures must be treated as stable contracts.

## Required Changes

1. Remove signature-changing borrow promotion from post-lowering.
2. Keep optimization to expression/callsite-level rewrites only, if needed.
3. Move any signature adaptation to a typed phase with explicit proof obligations.

## Short-Term Safety Plan

1. Gate or disable signature mutation path behind strict internal flag.
2. Add assertion pass:
- function params/returns in lowered IR must match typed function signature policy.

3. Convert existing promotion pass into:
- analyzer (collect opportunities),
- non-contract-changing rewriter.

## Long-Term Plan

1. Replace promotion heuristics with explicit borrow planning integrated with type/ownership analysis.
2. Preserve source-level intent and receiver semantics end-to-end.

## Done When

1. No pass mutates parameter ownership kind after typed signatures are fixed.
2. Failures involving `right: Maybe` becoming `&Maybe` disappear as a class.
