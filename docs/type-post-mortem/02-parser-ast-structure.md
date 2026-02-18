# 02. Refactor Parser + AST to Preserve Real Syntax

## Problem

Parser logic currently desugars control flow in-place and restricts grammar in ways that block supported forms:
- `else if let` unsupported,
- nested `if let` in `match` arm blocks brittle,
- `let mut` not represented as a first-class statement/binding property.

This indicates AST is missing syntax-level structures needed for reliable lowering.

## What Must Change

1. Represent syntax directly in AST:
- explicit `if`/`else if` chain structure,
- explicit `let` statement with mutability bit,
- explicit `where` predicates separate from generic param declarations.

2. Delay desugaring:
- keep parser faithful,
- move desugaring to a typed/lowering phase where context is available.

3. Normalize delimiter behavior:
- `match` arm trailing delimiter policy must be uniform for block and expression arms.

## Proposed AST Additions

1. `Stmt::Let { mutable: bool, ... }` or equivalent.
2. `Expr::If { branches, else_branch }` with branch list support.
3. Dedicated `WhereClause` structure on functions/methods/impls.

## Parser Work Plan

1. Extend grammar acceptance:
- `else if ...`,
- `else if let ...`,
- `if let` chains in nested expression/block contexts.

2. Add parser tests for each grammar form before lowering behavior tests.

3. Keep old desugaring path behind temporary compatibility layer until passes migrate.

## Done When

1. Parser accepts Rust-like edge forms already considered in-language.
2. Parser tests fail only for truly unsupported syntax, not for parser-shape limitations.
