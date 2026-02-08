# Quiche Bridge Feedback: Gaps & Proposals

> **Context:** We are converting Quiche `.qrs` files (MetaQuiche) to `.q` files (Quiche) compiled through the Elevate pipeline.
>
> **Test corpus:** `qtest.qrs` -> `qtest.q` (enums, structs, match statements, `strcat`, mutation via `&mut`).
>
> **Status update (2026-02-08):** Gaps **#2, #3, #4 are fixed in Elevate**. Gap **#1 remains**, and the root cause is in Quiche bridge lowering before Elevate codegen sees the arm body statements.

---

## Summary of Gaps

| # | Gap | Impact | Difficulty | Status |
| --- | --- | --- | --- | --- |
| 1 | [Match arms with statements produce empty bodies](#1-match-arm-bodies) | High | Medium | Open (bridge-side) |
| 2 | [`strcat()` emitted as bare function](#2-strcat-codegen) | Medium | Low | Fixed in Elevate |
| 3 | [Numeric type not preserved for same-type ops](#3-numeric-same-type) | Medium | Low | Fixed in Elevate |
| 4 | [Docstrings become dead string expressions](#4-docstring-codegen) | Low | Trivial | Fixed in Elevate |

---

## 1. Match Arm Bodies

### Problem

When a match arm contains multiple statements (assignments, method calls), generated Rust can end up with empty arm bodies (`()`), meaning statements were dropped before final codegen.

### Broken shape

```rust
match result {
    TestResult::Passed => (),
    TestResult::Failed(reason) => (),
    TestResult::Skipped(_) => (),
};
```

### Expected shape

```rust
match result {
    TestResult::Passed => {
        summary.passed = summary.passed + 1;
    }
    TestResult::Failed(reason) => {
        summary.failed = summary.failed + 1;
        summary.failures.push(crate::strcat!(name, ": ", reason));
    }
    TestResult::Skipped(_) => {
        summary.skipped = summary.skipped + 1;
    }
}
```

### Current analysis

- Elevate match arms store a single expression value.
- Elevate parser already handles block arms by desugaring them to an immediately-invoked closure expression per arm.
- Therefore, this gap is fixable if the Quiche bridge preserves arm statement blocks in that same expression form instead of collapsing to unit.

### Required bridge fix

For a statementful arm body, emit an expression equivalent to:

```text
(|| { <stmts>; <tail_expr_or_unit> })()
```

instead of emitting `()`.

### Impact

This is the main blocker for `.qrs` files that rely on side effects inside match arms.

---

## 2. `strcat()` Codegen

### Previous problem

`strcat(a, b, c)` was emitted as a plain function call `strcat(...)`, which failed when runtime only provides macro form.

### Elevate fix (done)

Elevate now treats `strcat` as a special call and lowers it to:

```rust
crate::strcat!(...)
```

### Result

`strcat(...)` now type-checks and codegens correctly through Elevate.

---

## 3. Numeric Same-Type Preservation

### Previous problem

Expressions like `usize + 1` were sometimes promoted to `i64`, causing assignment mismatches.

### Elevate fix (done)

Type inference now preserves the concrete integral type when one operand is an integer literal and the other side is already a concrete integral numeric type.

### Result

`usize + 1` remains `usize` in inferred result type and emitted Rust.

---

## 4. Docstring Codegen

### Previous problem

Docstring-like string statements were emitted as dead runtime expressions (e.g., `String::from("...");`).

### Elevate fix (done)

Standalone string expression statements are now dropped during statement lowering.

### Result

Generated Rust no longer includes dead string-expression statements for docstring-like inputs.

---

## Validation Notes

After implementing #2, #3, #4 in Elevate:

- `cargo test -q` passes.
- Bench run (`--iters 20 --warmup 3`) shows no clone/hot-clone counter regression.
- Median/p95 movement is within normal run-to-run variance.

---

## Priority (Updated)

| Priority | Gap | Blocks | Effort | Status |
| --- | --- | --- | --- | --- |
| **P0** | #1 Match arm bodies | `.q`/`.qrs` with side-effecting match arms | Medium | Open |
| **Done** | #2 `strcat()` codegen | String concat via runtime macro | Low | Fixed |
| **Done** | #3 Numeric same-type preservation | `usize`/integral arithmetic assignment stability | Low | Fixed |
| **Done** | #4 Docstring dead expression emission | Cosmetic noise in generated Rust | Trivial | Fixed |

Fixing #1 in Quiche bridge will unblock the remaining conversion path for match-heavy sources.
