# Quiche → Elevate Feedback

Issues discovered while testing Quiche's structural polymorphism features with Elevate.

## Issue 1: `method_is_builtin_for_type` Doesn't Recognize Universal Rust Traits

**File**: `src/passes.rs` → `method_is_builtin_for_type` (line ~5609)

### Problem

Rust has **blanket-implemented traits** that provide methods on virtually all types:
- `ToString::to_string()` (via `Display` blanket impl)
- `Clone::clone()`
- `Default::default()`

Elevate's structural checker treats these as "unknown" for any type not in the explicit builtin list. When a generic impl method like `Point<T>::label` calls `self.x.to_string()`, the structural checker flags `T` as needing a `to_string` method — then rejects it because `to_string` isn't recognized as a builtin for arbitrary types.

### Reproduction

```python
# traits.q
class Point[T](Struct):
    x: T
    y: T

    def label(self) -> String:
        return "Point(" + str(self.x) + ", " + str(self.y) + ")"

    def to_string(self) -> String:
        return self.label()
```

```
$ cargo run -p quiche -- traits.q
Compile error:
`T` passed to `Point::label` does not provide required method `to_string`
with 0 argument(s) for `T` (at traits.q:11:5-14:1)
```

### Suggested Fix

Add a universal catch-all for Rust blanket trait methods:

```diff
 fn method_is_builtin_for_type(type_name: &str, method: &str, arity: usize) -> bool {
     match type_name {
         "String" => ...,
         "Vec" => ...,
         // ...existing arms...
-        _ => false,
+        _ => match method {
+            "to_string" | "clone" => arity == 0,
+            _ => false,
+        },
     }
 }
```

### Considerations

- `to_string` requires `T: Display` and `clone` requires `T: Clone` — both are satisfied by Quiche's `#[derive(Debug, Clone)]` on all structs. If Elevate eventually needs to enforce this more precisely, it should check for the specific trait bound rather than blocking universally.
- An alternative is to suppress structural checking for known Rust trait methods entirely and let `rustc` catch any real violations.

---

## Issue 2: Specialized Impl Methods Duplicate the Generic Template

**File**: `src/passes.rs` → `materialize_structural_specializations` (line ~1836)

### Problem

When a generic inherent method (e.g., `impl<T> Point<T> { fn label }`) triggers structural specialization, Elevate generates **both**:

1. The **specialized** `impl Point<i32> { fn label }` and `impl Point<i64> { fn label }` (correct)
2. An **unparameterized** `impl Point<T> { fn label }` (missing `impl<T>`) leaked from the specialization template

The existing cleanup logic (lines ~1961-1975) correctly strips `label` from the **original** generic `impl<T> Point<T>` block — but a third `impl Point<T>` block (without `<T>` in the `impl` keyword) is emitted as a side effect of the specialization process itself.

### Generated Rust Output

```rust
// ✅ Generic impl — label correctly stripped, new/to_string remain
impl<T> Point<T> {
    pub fn new(x: T, y: T) -> Point<T> { ... }
    pub fn to_string(self: Point<T>) -> String { ... }
}

// ❌ Leaked template — impl Point<T> without impl<T>
impl Point<T> {
    pub fn label(self: Point<T>) -> String { ... }
}

// ✅ Correct specializations
impl Point<i32> {
    pub fn label(self: Point<i32>) -> String { ... }
}
impl Point<i64> {
    pub fn label(self: Point<i64>) -> String { ... }
}
```

This produces `rustc` errors:
- `E0412`: cannot find type `T` in scope (on the unparameterized `impl Point<T>`)
- `E0592`: duplicate definitions for `label`
- `E0034`: multiple applicable items in scope

### Root Cause

The existing cleanup at lines ~1961-1975 checks:
```rust
let method_is_template = impl_has_generics || !method.type_params.is_empty();
!method_is_template || !specialized_templates.contains(&method_key)
```

This correctly strips `label` from the **original generic** `impl<T> Point<T>` (where `impl_has_generics=true`).

But the **specialized** impl blocks emitted by `specialize_impl_method_ast` have `type_params: Vec::new()` — so `impl_has_generics=false`, `method_is_template=false`, and the retain logic says "keep". This is correct for the real `impl Point<i32>` specializations, but must also be happening for an extra unresolved template copy.

### Debug Evidence

```
[DEBUG] impl Point (generics=true, type_params=["T"])
[DEBUG]   method label key=Point::label is_template=true specialized=true keep=false  ← ✅ stripped

[DEBUG] impl Point (generics=false, type_params=[])
[DEBUG]   method label key=Point::label is_template=false specialized=true keep=true  ← ❌ leaked
[DEBUG] impl Point (generics=false, type_params=[])
[DEBUG]   method label key=Point::label is_template=false specialized=true keep=true  ← ✅ i32
[DEBUG] impl Point (generics=false, type_params=[])
[DEBUG]   method label key=Point::label is_template=false specialized=true keep=true  ← ✅ i64
```

There are **3** specialized `impl Point` blocks when there should be only **2** (one per concrete type). The extra one appears to be the template with unresolved `T` in `target_args`.

### Suggested Fix

The cleanup loop should check if a specialized `impl` block's `target_args` still contain unresolved type variables. If `target_args` reference a type param name (like `T`) that isn't a concrete type, the impl block should be stripped:

```rust
// After the existing retain logic, add:
items.retain(|item| {
    let TypedItem::Impl(def) = item else { return true };
    // Keep if it has explicit type params (generic impl<T>)
    if !def.type_params.is_empty() { return true }
    // Strip if target_args contain unresolved type variables
    !def.target_args.iter().any(|arg| is_unresolved_type_var(arg))
});
```

Alternatively, the issue may be in `specialize_impl_method_ast` or `lower_item` creating an extra copy when the binding resolution produces a passthrough (identity substitution).

---

## Issue 3: `Line::new(p1=p, p2=p2)` Mismatched Types

**Not an Elevate bug** — this is a valid `rustc` error in `traits.q`:

```python
p: Point[i32] = Point.new(y=6, x=5)
p2: Point[i64] = Point.new(5, 6)
l: Line[i32] = Line.new(p1=p, p2=p2)  # ← p2 is Point[i64], not Point[i32]
```

`Line[i32]` requires both `p1` and `p2` to be `Point[i32]`, but `p2` is `Point[i64]`. The fix is in the Quiche source file — either make both points the same type or parameterize `Line` with two type params.

---

## Working: Source Span Reporting ✅

Quiche's parser now populates real byte-offset spans on all user-facing AST nodes. Elevate's diagnostic infrastructure correctly resolves them:

```
Before: (at traits.q:location unavailable)
After:  (at traits.q:11:5-14:1)
```

The span wiring was done entirely in `quiche/src/parser.rs` — no Elevate changes needed.
