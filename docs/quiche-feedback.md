# Quiche → Elevate Feedback

## 1. Closure body `Stmt::Expr` adds trailing semicolons

Using `Stmt::Expr(expr)` in a closure body emits `(expr);` which returns `()`. Workaround: `Stmt::Return(Some(expr))`.

Footgun for anyone building closures through the AST.

## 2. `Expr::Block` — block expressions

Elevate has `Block` as a struct but no `Expr::Block(Block)` variant. This means multi-statement computations that need to produce a value require an IIFE:

```rust
// Current: IIFE wrapper
(|| { let mut v = Vec::new(); for x in iter { v.push(x); } v })()

// With Expr::Block:
{ let mut v = Vec::new(); for x in iter { v.push(x); } v }
```

`Block` already has `TailExpr` support, so `Expr::Block` would just expose that as an expression. This would generate cleaner Rust and avoid closure overhead.

## Elevate Response (updated)

You are right: these are AST/codegen shape problems, not inference problems.

### A. Fix closure `Stmt::Expr` return behavior

- Keep statement semantics consistent, but make closure lowering treat final `Stmt::Expr` as value-returning in closure expression position.
- Equivalent lowering target:
  - last `Stmt::Expr(e)` in closure body -> `return e;` (or tail expression without semicolon in emitted Rust block)
- This removes the footgun without forcing all generators to synthesize `Stmt::Return`.

### B. Add `Expr::Block(Block)`

- Add `Expr::Block(Block)` to AST.
- Typecheck path:
  - infer statements in order in a scoped local environment
  - block type = tail expr type if present, else `Unit`
- Lowering/codegen path:
  - emit Rust block expression directly (`{ ... }`) preserving tail expression
- This should replace IIFE patterns for value-producing multi-statement expressions.

### Suggested acceptance tests

1. Closure final expression returns value:
   - closure body with last `Stmt::Expr(x + 1)` infers non-`Unit` return and emits valid Rust without forced explicit `return`.
2. `Expr::Block` as call argument:
   - `foo({ let x = 1; x + 2 })` typechecks and emits inline block.
3. `Expr::Block` in assignment:
   - `let y = { let a = 3; a * 2 };` infers `i64`.
4. No IIFE needed:
   - generated Rust contains block expression and no synthetic closure call for this case.
