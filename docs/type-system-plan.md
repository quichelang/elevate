# Type System Plan (HM + Practical Extensions)

This plan targets a staged inference system that can be enabled incrementally with experiment flags and folded into the existing type/lowering pipeline.

## Goals

- Keep default behavior deterministic and conservative.
- Make local inference materially stronger when enabled (`exp_infer_local_bidi`).
- Provide stable annotation guidance when inference is ambiguous (`exp_infer_principal_fallback`).
- Avoid introducing whole-program solver complexity in early phases.

## Current Status

- Local inference already exists in expression/typechecking flows.
- Experiment flags already exist in CLI and compile options.
- This pass wires the flags into typechecking behavior and adds local unification for placeholder holes (`_`) inside compatible shapes.
- `exp_infer_local_bidi` now also supports function parameter placeholders (`a: _`) and omitted parameter annotations (`fn f(a, b)`), inferring concrete parameter types from local constraints when possible.

## HM Core (Phase 1)

- Keep public API boundaries explicit by default; allow omitted/placeholder parameter annotations in opt-in bidi mode.
- Infer expression types by constraint collection and unification in a local scope.
- Generalize only at stable `let`/`const` points where no unresolved placeholders remain.
- Instantiate polymorphic function signatures at callsites (already present in the current generic binding path).

## Bidirectional Local Inference (Phase 2)

Source: OCaml/Haskell local bidirectional checking + Koka-style practical locality.

- Use expected type from context (return annotation, declared local type, constructor family) as top-down guidance.
- Use bottom-up inferred type from expression leaves.
- Merge both directions via structural unification, not just compatibility checks.
- Fill holes recursively inside:
  - `Option<_>`, `Result<_, _>`
  - tuples
  - nested containers
  - function and iterator shapes where both sides are structurally known

## Principal Fallback Diagnostics (Phase 3)

- If type remains non-principal (contains unresolved holes), emit one deterministic “best annotation site” diagnostic.
- Priority order:
  1. function/method return type
  2. top-level const type
  3. local binding type (future phase)
- Keep fallback mode opt-in (`exp_infer_principal_fallback`) until noise is proven low.

## Effect/Ownership Interactions (Phase 4)

Source: Koka/Lobster practical effect ownership ideas.

- Keep ownership/move decisions out of HM unifier itself.
- Attach effect/ownership metadata after type inference stabilization.
- Allow experiment composition:
  - `exp_infer_local_bidi` for stronger local type unification
  - `exp_effect_rows` for source-declared effect-row validation
  - `exp_effect_rows_internal` for internal capability metadata
  - `exp_move_mut_args` for ownership lowering policy
- Never let effect metadata destabilize principal type decisions.

## Rollout and Guardrails

- Default remains conservative.
- Each experiment flag should be independently testable and composable.
- Regressions to watch:
  - silent broadening to `_`
  - unstable diagnostics across equivalent programs
  - hidden behavior changes when experiments are disabled

## Test Matrix Focus

- Missing value families (`None`, `Result::Ok`, `Result::Err`).
- Placeholder and omitted parameter type inference in local function bodies.
- Reordered control-flow branches (hole first vs concrete first).
- Nested placeholders (`Option<Result<_, _>>`).
- Tuple and match-arm mixed holes.
- Ambiguous/incompatible families still produce errors.
- Principal fallback emits deterministic annotation guidance.
