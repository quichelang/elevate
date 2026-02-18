# Type/Ownership Post-Mortem Overview

## Purpose

This post-mortem captures the bug clusters found through adversarial testing and full `cargo test --lib` sweeps, then maps them to architectural causes and required remediation direction.

This is not a patch checklist. It is a design-level correction plan.

## Bug Clusters Found

1. Parser/control-flow grammar gaps:
- `else if let` rejected (`else` only accepts block body).
- Nested `if let` inside `match` arm block delimiter errors.
- `let mut` local declarations not recognized.

2. Method capability and generic substitution failures:
- Generic methods like `Vec<T>::get(0)` mis-typed (`expected T, got i64`).
- Method-level generic parameters conflated with container generics.

3. Ownership and mutability drift after typing:
- Parameters rewritten from owned to borrowed after lowering, causing later semantic mismatches.
- Assignments and returns become invalid due to hidden signature mutation.

4. `self` receiver mode inconsistency:
- Methods lowered to `&self`/`&mut self` while returning owned `Self`.
- Return rewriting is partial and only catches some forms.

5. Bounds/where-clause representation instability:
- Impl-inherited and method-local constraints are merged via ad-hoc mechanisms.
- Tests and parser expectations drift as representation changes.

6. Missing explicit reference semantics in `SemType`:
- Borrow/reference behavior encoded through string types and mode flags rather than first-class semantic types.
- Compatibility and coercion logic must rely on special cases.

## Failure Matrix (Observed)

1. Parser/control-flow:
- `compile_adversarial_edge_else_if_let_chain`:
  parser expects block after `else`, does not accept `else if let`.
- `compile_adversarial_edge_else_if_let_with_struct_literal_tail`:
  same root cause as above.
- `compile_adversarial_edge_match_arm_if_let_chain_block`:
  `match` arm delimiter handling for nested block/if-let shape is brittle.

2. Local mutability parsing:
- `compile_adversarial_generic_impl_any_all_with_mut_capture`:
  `let mut` not parsed as valid local binding form.

3. Capability/generic resolution:
- `compile_adversarial_mut_generic_impl_if_let_and_field_mutation`:
  `Vec<T>.get(0)` mis-resolved as if arg expected `T`.
- `compile_adversarial_inherited_and_method_generics_with_mut_self`:
  same method-generic/index-type resolution defect.

4. Receiver ownership mismatch:
- `compile_adversarial_mut_generic_impl_push_contains_flow`,
  `compile_adversarial_generic_swap_struct_literal_shorthand`,
  `compile_adversarial_generic_impl_mut_self_index_then_push`:
  generated method receiver is borrowed while return type is owned `Self`.

5. Post-lowering borrow drift:
- `compile_adversarial_nested_if_let_chain_and_mut_reassignment`:
  parameter effectively treated as borrowed in lowered code, breaking assignment into owned variable.

6. Constraint representation drift:
- `parser::tests::parse_where_clause_on_impl_method`:
  test assumptions diverged after inherited where-bounds retention change, highlighting non-canonical bound representation.

## Architectural Root Causes

1. Syntax is partially desugared too early in parser code paths.
2. Type/capability resolution depends on lossy generic mapping heuristics.
3. Ownership mode decisions are made after type contracts are assumed stable.
4. Receiver (`self`) semantics are inferred heuristically in multiple places.
5. Constraint/bounds data has no single source of truth through the pipeline.
6. Semantic type model lacks a reference type constructor.

## Direction Documents

1. **Define Language Ownership/Mutability Semantics First**
   See: [01-language-semantics-contract.md](./01-language-semantics-contract.md)

2. **Refactor Parser + AST to Preserve Real Syntax**
   See: [02-parser-ast-structure.md](./02-parser-ast-structure.md)

3. **Add Reference-Aware Semantic Types**
   See: [03-reference-aware-sem-types.md](./03-reference-aware-sem-types.md)

4. **Stop Post-Lowering Signature Mutation**
   See: [04-stable-signature-lowering.md](./04-stable-signature-lowering.md)

5. **Rebuild Method Capability Generic Resolution**
   See: [05-capability-generic-resolution.md](./05-capability-generic-resolution.md)

6. **Install Pass Invariants and Hard Regression Gates**
   See: [06-pass-invariants-and-gates.md](./06-pass-invariants-and-gates.md)

## Recommended Execution Order

1. `01` semantics contract
2. `02` parser/AST reshaping
3. `03` semantic type model upgrade
4. `04` stable lowering/signature discipline
5. `05` capability/generic resolver redesign
6. `06` invariants + regression enforcement

## Success Criteria

1. Adversarial syntax tests pass without parser-specific one-off exceptions.
2. Ownership and receiver mode do not change unexpectedly after typing.
3. Generic method/indexing behavior works without handwritten per-method patches.
4. Full `cargo test --lib` stabilizes with no red clusters recurring in these domains.
