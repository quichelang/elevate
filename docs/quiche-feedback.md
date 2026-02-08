# Quiche Bridge Feedback: Gaps & Proposals

> **Context:** We built a Quiche → Elevate AST bridge (`desugar.rs`, 1300+ lines) that translates Quiche's
> Python-shaped AST into Elevate's Rust-shaped AST. The desugar works correctly — all constructs
> translate to valid Elevate AST. But Elevate's type checker rejects several patterns that are
> common in real-world Quiche code. This document catalogs those gaps and proposes targeted fixes.
>
> **Test corpus:** `demo.q` (59 lines — structs, impls, HashMap, f-strings, lambdas, list comprehensions)
> and `sudoku.q` (96 lines — nested Vecs, recursion, mutation, type casts).

---

## Summary of Gaps

| # | Gap | Errors | Impact | Difficulty |
|---|-----|--------|--------|------------|
| 1 | [Iterator method chain support](#1-iterator-method-chains) | 6 | High | Medium |
| 2 | [Numeric type coercion](#2-numeric-type-coercion) | 10 | High | Low |
| 3 | [Missing `.to_string()` method](#3-missing-tostring-method) | 1 | Low | Trivial |
| 4 | [Self parameter in method resolution](#4-self-parameter-handling) | 2 | Medium | Low |
| 5 | [`as` cast expression](#5-as-cast-expression) | 3 | Medium | Medium |

**Total: ~22 errors across both test files.** Fixing gaps #1 and #2 alone would resolve ~16 of them.

---

## 1. Iterator Method Chains

### Problem

Elevate has `SemType::Iter(Box<SemType>)` and correctly produces it from `Vec::iter()`, `HashMap::values()`,
etc. But there's **no method resolution for `Iter<T>`** — calling `.map()`, `.filter()`, `.collect()`, or `.fold()`
on an iterator produces "Unsupported method call `map` on type `Iter<Student>`".

### Where It Hits

```python
# demo.q — list comprehension desugars to .iter().map().collect()
doubled = [x * 2 for x in nums]
# → nums.iter().map(|x| x * 2).collect()

# demo.q — dict comprehension with fold
students.values().fold(0, |acc, s| acc + s.age)
```

### Proposed Fix

In `resolve_method_call_type` ([passes.rs:3076](file:///Volumes/Dev/code/jagtesh/elevate/src/passes.rs#L3076)),
add an `Iter` handler before the fallback at line 3302:

```rust
// After the existing Path { path, .. } match block (around line 3302):
if let SemType::Iter(item_ty) = base_ty {
    match method {
        "map" => {
            // .map(|x| expr) → Iter<ReturnTypeOfClosure>
            if let Some(SemType::Fn { ret, .. }) = args.first() {
                return SemType::Iter(ret.clone());
            }
            return SemType::Iter(Box::new(SemType::Unknown));
        }
        "filter" => {
            // .filter(|x| bool) → Iter<same T>
            return SemType::Iter(item_ty.clone());
        }
        "collect" => {
            // .collect() → Vec<T>
            return SemType::Path {
                path: vec!["Vec".to_string()],
                args: vec![*item_ty.clone()],
            };
        }
        "fold" => {
            // .fold(init, |acc, x| expr) → type of init
            if let Some(init_ty) = args.first() {
                return init_ty.clone();
            }
            return SemType::Unknown;
        }
        "enumerate" => {
            // .enumerate() → Iter<(usize, T)>
            return SemType::Iter(Box::new(SemType::Tuple(vec![
                named_type("usize"),
                *item_ty.clone(),
            ])));
        }
        "count" => return named_type("usize"),
        "sum" | "product" => return *item_ty.clone(),
        "any" | "all" => return named_type("bool"),
        "for_each" => return SemType::Unit,
        _ => {}
    }
}
```

### Impact

Resolves **6 errors** in demo.q (`.map()`, `.collect()`, `.fold()`, `.values()`).

---

## 2. Numeric Type Coercion

### Problem

`is_compatible` ([passes.rs:5911](file:///Volumes/Dev/code/jagtesh/elevate/src/passes.rs#L5911)) uses
**strict path equality** — `i32` and `i64` are completely incompatible. This is technically correct for
Rust, but Elevate always infers integer literals as `i64`, so calling a function that takes `i32` with
a literal `0` fails.

### Where It Hits

```python
# sudoku.q — function signatures use i32, literals are i64
def is_valid(board: Vec[Vec[i32]], row: i32, col: i32, num: i32) -> bool:
    if board[row as usize][x as usize] == num:  # comparison: i32 vs i64
        ...

# sudoku.q — Vec[i32] return, but [5, 3, 0, ...] infers as Vec[i64]
def get_board() -> Vec[Vec[i32]]:
    return [[5, 3, 0, ...], ...]  # Return type mismatch: Vec<Vec<i64>> vs Vec<Vec<i32>>
```

### Proposed Fix

Add a `is_numeric_compatible` helper and use it in `is_compatible`:

```rust
fn is_numeric_type(ty: &SemType) -> bool {
    if let SemType::Path { path, args } = ty {
        if args.is_empty() {
            if let Some(name) = path.last() {
                return matches!(
                    name.as_str(),
                    "i8" | "i16" | "i32" | "i64" | "i128" | "isize"
                        | "u8" | "u16" | "u32" | "u64" | "u128" | "usize"
                        | "f32" | "f64"
                );
            }
        }
    }
    false
}

// In is_compatible, before the final `_ => false`:
fn is_compatible(actual: &SemType, expected: &SemType) -> bool {
    match (actual, expected) {
        // ... existing cases ...

        // NEW: Numeric types are compatible (Rust will narrow/widen with `as`)
        (a, b) if is_numeric_type(a) && is_numeric_type(b) => true,

        _ => false,
    }
}
```

> [!NOTE]
> This is intentionally loose — it treats all numeric types as compatible during type checking,
> deferring to Rust's own type checker for the final validation. This matches Quiche's semantics
> where numeric coercion is implicit.

### Impact

Resolves **10 errors** in sudoku.q (3 comparisons, 2 return mismatches, 2 vector indexing, 2 assignment
mismatches, 1 arg mismatch).

---

## 3. Missing `.to_string()` Method

### Problem

The String method table in `resolve_method_call_type`
([passes.rs:3086](file:///Volumes/Dev/code/jagtesh/elevate/src/passes.rs#L3086)) handles `.len()`,
`.push_str()`, `.chars()`, `.is_empty()`, etc., but **`.to_string()` is missing**.

### Where It Hits

```python
# sudoku.q
row_str.push_str(ref(val.to_string()))
```

### Proposed Fix

Add to the String match arm in `resolve_method_call_type`:

```rust
"to_string" => {
    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
    return named_type("String");
}
```

Additionally, `.to_string()` should be a **universal method** — it works on any type that implements
`Display`. Consider adding it to the fallback at
[passes.rs:3407](file:///Volumes/Dev/code/jagtesh/elevate/src/passes.rs#L3407):

```rust
fn infer_external_method_call_type(method: &str, args: &[SemType]) -> SemType {
    // ... existing checks ...

    // NEW: to_string() works on anything that implements Display
    if method == "to_string" {
        return named_type("String");
    }

    // ... rest of function ...
}
```

### Impact

Resolves **1 error** in sudoku.q.

---

## 4. Self Parameter Handling

### Problem

When Quiche defines a method like `def bio(self) -> String`, the desugar emits `self` as an explicit
`Param { name: "self", ty: Type { path: ["Self"] } }`. Elevate counts this as a regular parameter, so
`students[0].bio()` → "Method `Student::bio` expects 1 arg(s), got 0".

The resolution in `resolve_method_call_type`
([passes.rs:3308](file:///Volumes/Dev/code/jagtesh/elevate/src/passes.rs#L3308)) correctly checks
`sig.params.len() == args.len() + 1` for the self-inclusive case, but the **error message path** at
line 3352-3360 doesn't distinguish between "method needs self + N args" and "static function needs
N args". It falls through to the error because the match at line 3309 (`sig.params.len() == args.len() + 1`)
expects `is_compatible(base_ty, &sig.params[0])` to pass — which it does for `Self` types, but the
issue is that `Self` isn't being replaced with the concrete struct type during impl lowering.

### Where It Hits

```python
# demo.q
students[0].bio()      # → expects 1 arg (self), got 0
klass.summary()        # → expects 1 arg (self), got 0
```

### Proposed Fix

Two options (ranked by preference):

**Option A (in Elevate):** During impl block lowering, replace `Self` type in params with the concrete
struct type:

```rust
// In lower_item for Item::Impl (passes.rs, around line 850-870):
for method in &impl_block.methods {
    let mut sig = lower_function_sig(method);
    // Replace Self with concrete type in params
    for param in &mut sig.params {
        if param == &named_type("Self") {
            *param = named_type(&impl_block.target);
        }
    }
    context.functions.insert(format!("{}::{}", impl_block.target, method.name), sig);
}
```

**Option B (in desugar):** Emit the concrete type instead of `Self` in the params. This is a
simpler fix but less correct:

```rust
// In lower_function, when handling SelfKind::Value:
params.push(e::Param {
    name: "self".to_string(),
    ty: e::Type {
        path: vec![struct_name.clone()],  // Use actual struct name, not "Self"
        args: vec![],
    },
});
```

### Impact

Resolves **2 errors** in demo.q (`.bio()`, `.summary()`).

---

## 5. `as` Cast Expression

### Problem

Elevate's AST has no native cast expression. Quiche uses `val as i32`, `row as usize`, etc. extensively.
The desugar currently emits `__quiche_as!(val, i32)` as a macro call, which works for codegen but
Elevate's type checker can't reason about it, so the result type is `Unknown`.

### Where It Hits

```python
# sudoku.q — used 8+ times
board[row as usize][col as usize] == num
total / self.students.len() as f32
```

### Proposed Fix

Add a `Cast` variant to the Elevate AST and handle it in the type checker:

```rust
// ast.rs
pub enum Expr {
    // ... existing variants ...
    Cast {
        expr: Box<Expr>,
        target_type: Type,
    },
}
```

```rust
// In lower_expr (passes.rs), handle Expr::Cast:
Expr::Cast { expr, target_type } => {
    let inner = lower_expr(expr, ...);
    let target = resolve_type(target_type);
    TypedExpr::Cast {
        expr: Box::new(inner),
        ty: target.clone(),
    }
}
```

```rust
// codegen.rs — emit as `(expr as Type)`
TypedExpr::Cast { expr, ty } => {
    format!("({} as {})", emit_expr(expr), emit_type(ty))
}
```

### Impact

Resolves **3 type tracking errors** in sudoku.q (casts to `usize`, `i32`, `f32`) and enables proper
type flow through cast expressions.

---

## Priority Ordering

For maximum impact with minimum effort:

| Priority | Gap | Effort | Errors Fixed |
|----------|-----|--------|-------------|
| **P0** | #2 Numeric coercion | ~10 lines | 10 |
| **P0** | #1 Iterator methods | ~40 lines | 6 |
| **P1** | #3 `.to_string()` | ~5 lines | 1 |
| **P1** | #4 Self param | ~10 lines | 2 |
| **P2** | #5 `as` cast | ~30 lines (ast+passes+codegen) | 3 |

Fixing just **P0** items (two small changes) would resolve **16 of 22 errors** across both test files
and likely unblock full end-to-end compilation of `sudoku.q`.
