# Type Inference Progress Report

Date: 2026-02-11  
Scope: Elevate type-system progress toward OCaml-like inference and algebraic expressiveness.

## Executive Summary

Elevate has made meaningful progress on ergonomic, source-level polymorphism and simplified generic use. In particular, explicit call-site type application now works in expression position (for example `Point<i32>::new(...)`), and a consolidated profile (`--exp-type-system`) now enables a coherent inference bundle (local bidirectional inference, principal fallback diagnostics, numeric coercion, and effect-row flags).

The remaining gap to “OCaml-like by default” is primarily **consistency and completeness**, not baseline capability. The language can already express powerful generic/structural patterns more directly than Rust source in some cases, but behavior is still partially profile-gated and not yet fully principal/predictable in all contexts.

## Goals Recap

Target experience:
1. Generic programming that feels concise and natural (minimal annotation burden).
2. Strong bidirectional inference with stable, predictable outcomes.
3. Algebraic expressiveness (sum/product + polymorphism + structural patterns) with clear diagnostics.
4. Simpler authoring model than Rust while still lowering to Rust safely.

## Progress Snapshot

### Recently Completed

1. Explicit type application in expression paths
- Works for call forms like `id<i64>(x)` and `Type<T>::assoc(...)`.
- Removes a major ergonomics break where code that “looks right” failed at parse stage.

2. Consolidated OCaml-profile experiment flag
- `--exp-type-system` now enables:
  - local bidirectional inference
  - principal fallback guidance
  - numeric coercion
  - effect rows (surface + internal)
- This reduces feature-flag friction and improves repeatability during development.

3. Consolidated inference/generics story suite
- Added integration suite with sectioned checklist-style tests and `r#"..."#` examples:
  - `/Volumes/Dev/code/jagtesh/elevate/tests/ocaml_generics_story.rs`
- Added CLI coverage for profile behavior:
  - `/Volumes/Dev/code/jagtesh/elevate/tests/cli.rs`

4. Structural specialization path improving nested generic cases
- Structural generic nested-object access can now compile via specialization paths in profile mode.

5. Capability cutover for method/index resolution
- Core type/lowering paths now resolve container method/index behavior via capability logic instead of legacy per-method direct-borrow tables.
- Map subscript is now safe-option semantics:
  - `map[key]` lowers to `map.get(key)` style behavior.
  - result type is `Option<V>`.
- Key compatibility for index expressions is now target/capability-derived.
- Capability failures are now reported at Elevate level instead of leaking rustc-only failures in covered paths.

## Capability Cutover (Expectation vs Reality)

### Mode Clarification (Locked)

- strict mode = `--exp-type-system` off.
- type-system mode = `--exp-type-system` on.
- strict mode continues to require robust auto-borrow/auto-clone behavior.
- rustdex hard-fail preflight applies to type-system mode capability resolution only.

### A) Map subscript should be safe and typed, not Vec-only or rustc-fallback

Expectation:
```ers
use std::collections::HashMap;

fn score(scores: HashMap<String, i64>) -> Option<i64> {
  scores["Alice"]
}
```

Previous reality:
- `[]` handling was effectively Vec-centric.
- Non-Vec indexing often failed later in generated Rust, reducing diagnostic quality.

Current reality:
- Map-like `[]` resolves through capability path and returns `Option<V>`.
- Lowering uses `get` semantics, preserving safe access behavior.

Status: **Closed**

---

### B) Method/index typing should not require per-method compiler wiring

Expectation:
- Borrow/move/type behavior should derive from a unified capability model.

Previous reality:
- Several std-container paths were manually hardcoded, which made drift/regressions likely.

Current reality:
- Capability resolution is now the primary method/index typing path.
- Direct hardcoded std-container borrow tables for old method lowering path are removed from the active flow.

Status: **Substantially closed**

---

### C) Failures should be diagnosed in Elevate with actionable context

Expectation:
- Errors should identify capability mismatch/metadata gaps before rustc.

Previous reality:
- Some unsupported paths surfaced only as rustc type errors.

Current reality:
- Capability resolution failures and key-type mismatch cases are emitted directly by Elevate in covered paths.
- Numeric/index mismatch diagnostics are more targeted for vector/key-style misuse.

Status: **Closed for covered capability paths**

## Expectation vs Reality

### 1) “If it looks like Rust generic call syntax, it should compile”

Expectation:
```ers
const p = Point<i32>::new(5, 6);
```

Previous reality:
- Parser rejected this form before inference/typechecking ran.

Current reality:
- Compiles and runs.
- This is now aligned with user expectation.

Status: **Closed**

---

### 2) “One OCaml-like profile should be enough for inference experiments”

Expectation:
- Single switch to enable coherent inference behavior.

Previous reality:
- Multiple interacting flags had to be manually composed.

Current reality:
- `--exp-type-system` exists and bundles key inference/effect features.

Status: **Closed (for profile mode)**

---

### 3) “Structural generic code should stay simple even with nested object fields”

Expectation:
```ers
fn first_value<T>(obj: T) -> i64 {
  obj.child.values[0]
}
```

Previous reality:
- Could fail with unresolved structural parameter / unknown field-type diagnostics.

Current reality:
- Compiles in profile mode through structural specialization paths.
- Still part of an area that needs deeper, default-mode robustness.

Status: **Partially closed**

---

### 4) “OCaml-like inference should be default, principal, and deterministic”

Expectation:
- Minimal annotations in ordinary code.
- Stable principal types without toggles.

Current reality:
- Strong improvements exist, but behavior is still partly experimental/profile-based.
- Some inference outcomes remain context-sensitive and not yet at HM-grade consistency.

Status: **Open**

## Accomplishments Illustrated

## Complex Example A: Generic Geometry Constructor + Display Bound

### Elevate (simple source)

```ers
use std::fmt::Display;

struct Point<T> {
  x: T;
  y: T;
}

impl<T: Display> Point<T> {
  fn new(x: T, y: T) -> Self {
    Point { x: x; y: y; }
  }

  fn render(self) -> String {
    format!("({}, {})", self.x, self.y)
  }
}

fn main() {
  const a = Point<i32>::new(5, 6);
  const b = Point::new(5.1, 6.5);
  println!("{}", a.render());
  println!("{}", b.render());
}
```

### Why this matters
- Explicit type application and inferred polymorphic calls coexist naturally.
- Same implementation supports multiple concrete types without source duplication.
- Source reads like straightforward algebraic modeling, not ownership plumbing.

---

## Complex Example B: Structural-style data access with specialization path

### Elevate (concise source)

```ers
struct Child {
  values: Vec<i64>;
}

struct Parent {
  child: Child;
}

fn first_value<T>(obj: T) -> i64 {
  return obj.child.values[0];
}

fn run() -> i64 {
  const p = Parent { child: Child { values: [3, 9] } };
  return first_value(p);
}
```

### Why this matters
- Author writes one generic function and relies on compiler specialization.
- This style is difficult to model ergonomically in Rust without explicit trait design and impl wiring.
- Demonstrates Elevate’s algebraic/structural direction: simple source, generated specialization where needed.

## Remaining Work (Clear Next Steps)

## Stabilization Checklist (Post-Cutover)

1. Document-level lock-in
- Ensure language spec remains aligned with:
  - map `[]` => `Option<V>` safe semantics
  - capability-first method/index typing
  - rustdex metadata dependency diagnostics

2. Story/integration coverage expansion
- Extend consolidated OCaml story coverage for:
  - map + vector + nested-structure mixed access
  - capability failure diagnostics for custom/non-capability paths
  - mixed inference + ownership + numeric coercion scenarios

3. Regression hygiene
- Keep `cargo test -q` and benchmark cadence mandatory for `passes.rs`/ownership-sensitive changes.
- Watch clone/hot-clone counters and p95 drift after capability refactors.

4. Remaining risk
- rustdex metadata availability remains an operational dependency for capability resolution.
- Structural generic expressiveness still needs further default-mode hardening to match OCaml-like consistency targets.

## Remaining Work (Clear Next Steps)

### P0: Make OCaml-like inference default-quality (not profile-dependent)

1. Principal typing consistency
- Move critical inference behavior from experimental profile into default semantics (after stabilization).
- Ensure comparable principal types across equivalent program shapes.

2. Deterministic numeric inference policy
- Define and enforce one global literal-defaulting + expected-type strategy.
- Minimize case-by-case fallback surprises.

3. Generalization policy (value-restriction equivalent)
- Document and enforce where polymorphism is generalized.
- Prevent “sometimes generic, sometimes monomorphic” confusion.

### P1: Structural algebraic completeness

4. Deep structural inference reliability
- Expand nested structural field/method inference beyond current specialization path success cases.
- Strengthen diagnostics to explain structural requirement derivation.

5. More mixed-mode integration tests
- Add end-to-end cases combining:
  - explicit type application
  - structural generic access
  - match/branch bidirectional inference
  - numeric coercion + effect rows

### P2: Diagnostics and developer confidence

6. Inference failure explainability
- Improve messaging for unsolved type variables and conflicting constraints.
- Provide explicit “why this did not generalize” / “which constraint failed” traces.

7. Bench + quality gates
- Keep inference/structural changes behind benchmark and clone-count checks.
- Track regressions in `docs/bench-latest.csv` on each significant type-system change.

## Suggested Acceptance Criteria for “OCaml-like Inference v1”

1. No feature flag required for standard polymorphic constructor and function call patterns.
2. Explicit type application works for all documented call-site forms.
3. Nested structural generic examples compile in both unit/integration suites with stable generated output shape.
4. Numeric literal behavior is deterministic and documented.
5. Inference diagnostics identify expected type, actual type, and failing constraint source.
6. Full test suite and benchmark cadence pass without clone/hot-clone regressions.

## Current Assessment

- Capability trajectory: **strong**
- Ergonomic trajectory: **strong in profile mode**
- Default-mode parity with OCaml expectations: **not complete yet**
- Risk to product promise (“simpler than Rust” illusion break): **medium until default-mode consistency is hardened**
