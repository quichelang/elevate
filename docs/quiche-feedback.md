# Quiche → Elevate Feedback
*Updated: 2026-02-10*

Issues discovered while testing Quiche's structural polymorphism with Elevate.

---

## ~~Issue 1: Structural Checker Blocks Trait-Provided Methods on Generic Types~~ ✅ RESOLVED

**File**: `src/passes.rs` → `method_is_builtin_for_type`  
**Resolution**: Elevate now auto-inserts trait bounds from method usage.

`str(self.x)` → `self.x.to_string()` → Elevate infers `T: Display` → generates `impl<T: std::fmt::Display> Point<T>`.

Method-to-trait mapping used:

| Method | Required Trait |
|--------|---------------|
| `to_string()` | `std::fmt::Display` |
| `clone()` | `Clone` |
| `into_iter()` | `IntoIterator` |

> **Future**: `match T` type-level dispatch would subsume this entirely.

---

## ~~Issue 2: Specialization Leaks Unresolved Generic Impl Template~~ ✅ RESOLVED

**File**: `src/passes.rs` → `materialize_structural_specializations`  
**Resolution**: Specializations with non-concrete target args are no longer emitted.

**Before** (leaked `impl Point<T>` without `impl<T>`):
```
impl<T> Point<T> { new, to_string }     ← original (label stripped) ✅
impl Point<T> { label }                  ← LEAKED template ❌
impl Point<i32> { label }               ← specialization ✅
impl Point<i64> { label }               ← specialization ✅
```

**After**:
```
impl<T: Display> Point<T> { new, label, to_string }  ← with auto-bound ✅
impl Point<i32> { label }                             ← specialization ✅
impl Point<i64> { label }                             ← specialization ✅
```

---

## Issue 3: Regression Test Coverage

**Priority**: P2

Suggested test cases to guard against regressions:

```python
# Test 1: Trait-aware structural bounds
class Container[T](Struct):
    val: T
    def show(self) -> String:
        return str(self.val)  # should auto-insert T: Display

# Test 2: No leaked unresolved impl blocks
def describe[T](p: T) -> String:
    return p.label()

p: Container[i32] = Container(42)
describe(p)
```

---

## Working: Source Span Reporting ✅

Error messages now show `file:line:col` instead of "location unavailable":

```diff
-Compile error: ... (at traits.q:location unavailable)
+Compile error: ... (at traits.q:11:5-14:1)
```

Change in `quiche/src/parser.rs` — 13 `span: None` sites replaced with real token positions.

---

## Note: `traits.q` Type Mismatch

Not an Elevate bug — `Line[i32]` gets a `Point[i64]` argument. Fix in source.
