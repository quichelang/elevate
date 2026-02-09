# Quiche → Elevate Feedback

Items are ordered by impact to the Quiche frontend.

---

## 1. `StructDef` needs `type_params`

Quiche parses `class Point[T](Struct):` and produces a `GenericParam` list, but `StructDef` has no `type_params` field to put it in. The params are silently dropped.

`FunctionDef` already has `type_params: Vec<GenericParam>` — `StructDef` (and probably `EnumDef`) need the same field.

**What Quiche needs**: A `type_params` field on `StructDef` and `EnumDef` so generic data types round-trip correctly.

---

## 2. AST nodes need source spans

Diagnostics currently show `"location unavailable"` even when `source_name` is set. The `Span { start, end }` type exists in `diag.rs` but isn't on AST nodes (`Expr`, `Stmt`, `Item`).

Quiche can emit byte offsets during parsing — there's just nowhere to attach them.

**What Quiche needs**: An `Option<Span>` (or similar) on `Expr`, `Stmt`, and `Item` variants so frontends can pass source locations through to diagnostics.

---

## 3. Closure body `Stmt::Expr` adds trailing semicolons

Using `Stmt::Expr(expr)` as the last statement in a closure body emits `(expr);` which returns `()`. The workaround is synthesizing `Stmt::Return(Some(expr))`, but that's a footgun for anyone building closures through the AST.

**What Quiche needs**: Final `Stmt::Expr` in closure position to be treated as a tail expression (no semicolon).

---

## 4. `Expr::Block` — block expressions

There's no `Expr::Block(Block)` variant. Multi-statement computations that need to produce a value require an IIFE wrapper:

```rust
// Current workaround
(|| { let mut v = Vec::new(); for x in iter { v.push(x); } v })()

// With Expr::Block
{ let mut v = Vec::new(); for x in iter { v.push(x); } v }
```

`Block` already supports tail expressions, so this is mainly an AST shape gap.

**What Quiche needs**: `Expr::Block(Block)` so we can emit inline block expressions without IIFE overhead.

---

## 5. Row polymorphism / effect rows surface syntax

The `effect_rows_internal` experiment flag exists and works at the Elevate level, but there's no documented way for a frontend to express effect row constraints in the AST. Is there a planned AST shape for this, or should Quiche wait?

**What Quiche needs**: Guidance on how frontends should represent effect row constraints (if at all) — or confirmation that this remains Elevate-internal for now.

---

## 6. `ImplBlock` doesn't carry type params

`ImplBlock { target, trait_target, methods }` has no `type_params` field. For generic impl blocks (`impl[T] Foo[T]`), there's nowhere to put the impl-level type parameters.

**What Quiche needs**: `type_params` on `ImplBlock` for generic implementations.
