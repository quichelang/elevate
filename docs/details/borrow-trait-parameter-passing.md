# Borrow Trait Parameter Passing

Status: Implemented  
Date: 2026-02-12

## Motivation

Elevate's ownership model automatically decides when to borrow vs clone vs move values at call sites. Previously, all auto-borrowing was done via plain reference operators (`&x`, `&mut x`). While this works for most cases through Rust's deref coercion, it ties the compiler output to a single mechanism and limits flexibility for collection types.

The `std::borrow::Borrow` trait provides a more idiomatic way to express borrowing intent for Rust standard library types. Types like `Vec<T>`, `HashMap<K, V>`, `Option<T>`, and `Result<T, E>` implement `Borrow<Self>`, enabling `.borrow()` calls as a uniform interface. This approach:

- Aligns with Rust standard library conventions for trait-based access
- Enables future extension to custom `Borrow` impls without compiler changes
- Makes the generated code's borrowing intent more explicit

## Design

### IR Variants

Two new `RustExpr` variants were added:

- `BorrowCall(Box<RustExpr>)` - emits `x.borrow()`
- `BorrowMutCall(Box<RustExpr>)` - emits `x.borrow_mut()`

These complement the existing `Borrow(Box<RustExpr>)` (`&x`) and `MutBorrow(Box<RustExpr>)` (`&mut x`) variants.

### Type Qualification via rustdex

The `type_supports_borrow_trait()` helper queries rustdex to determine if a type implements the `Borrow` trait. A type qualifies for `.borrow()` when:

1. It is not a copy primitive (these are passed by value)
2. It is not `String` (excluded - see below)
3. rustdex confirms a `Borrow` implementation exists for the type

### Import Injection

When any `.borrow()` call is emitted, the compiler automatically injects `use std::borrow::Borrow;` into the generated module preamble.

## String Exclusion

`String` is deliberately excluded from `.borrow()` optimization. This is a critical design decision driven by Rust's type system:

`String` has two `Borrow` implementations:
- `impl Borrow<String> for String` (blanket impl for all types)
- `impl Borrow<str> for String` (specialized deref-based impl)

When `String.borrow()` is passed to a function with `Borrow<Q>` generic bounds (e.g., `HashMap::get`, `HashSet::contains`, `str::contains`), Rust cannot determine which `Borrow` impl to use, resulting in E0283 "type annotations needed" ambiguity errors. The plain `&x` form avoids this entirely through deref coercion, which Rust resolves unambiguously.

Additionally, `String.borrow()` cannot satisfy `&dyn Trait` coercion for trait objects. While `&String` can coerce to `&dyn Printable` (when `String: Printable`), `String.borrow()` returns `&String` via `Borrow<String>` and Rust does not attempt `Borrow<dyn Trait>` resolution.

## Affected Types

Types that now use `.borrow()` at call sites:
- `Vec<T>`, `HashMap<K,V>`, `BTreeMap<K,V>`
- `HashSet<T>`, `BTreeSet<T>`
- `Option<T>`, `Result<T,E>`
- Any user-defined type with `Borrow` impl in rustdex

Types that continue to use `&x`:
- `String` (ambiguity avoidance)
- All copy primitives (`i64`, `f64`, `bool`, etc.) - passed by value
- Types not found in rustdex

## Files Changed

| File | Change |
|------|--------|
| `src/ir/lowered.rs` | Added `BorrowCall` and `BorrowMutCall` to `RustExpr` |
| `src/codegen.rs` | Emit `.borrow()` / `.borrow_mut()` method calls; mutation tracking |
| `src/passes.rs` | `type_supports_borrow_trait()` helper, auto-borrow integration, import injection |
| `src/lib.rs` | Updated ~20 test assertions to match new output patterns |

## Verification

All 332 library tests pass. The implementation was validated against the full test suite including:
- Auto-borrow tests for String/Vec/HashMap/HashSet/BTreeMap/BTreeSet
- Heuristic borrow tests for third-party/external calls
- Ownership benchmark regression tests
- End-to-end `rustc` compilation checks (`assert_rust_code_compiles`)
