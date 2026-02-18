# 01. Define Language Ownership/Mutability Semantics First

## Problem

Multiple failures come from undefined or implicit rules around:
- when values are owned vs borrowed,
- what `self` means by default,
- how `mut` changes behavior,
- how control-flow desugaring should preserve ownership.

Without a single contract, each pass infers behavior independently and diverges.

## What Must Be Specified

1. Receiver semantics:
- `self`, `mut self`, explicit `self: T`, `self: &T`, `self: &mut T`.
- default behavior for impl methods when receiver type is omitted.

2. Local binding mutability:
- `let` vs `let mut` semantics and AST representation.
- assignment legality and rebind behavior.

3. Borrow insertion policy:
- when auto-borrow is allowed,
- when it is forbidden,
- and when ownership must remain explicit.

4. Return ownership rules:
- especially for methods returning `Self` and generic `T`.
- no hidden conversion from owned to borrowed in later passes.

## Required Deliverables

1. A short normative spec section in `docs/language-design.md`.
2. A compiler-facing contract table (syntax -> ownership intent -> typed form).
3. Explicit non-goals for inference heuristics that are currently accidental.

## Implementation Notes

1. Every pass that currently "decides" ownership should consume this contract.
2. Remove implicit defaults that are not documented in the contract.
3. Use this as a blocker before additional feature support on mutability.

## Done When

1. A reviewer can predict receiver and binding ownership without reading compiler code.
2. Failing adversarial tests map to contract violations, not undefined behavior.
