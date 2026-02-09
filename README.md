# Elevate

Elevate is a compiler for a Rust-adjacent language with a focus on practical inference, predictable lowering, and ownership-aware code generation.

## Quick Start

```bash
cargo run -q -- path/to/file.ers
```

## Inference Showcase (Experimental)

Enable local bidirectional inference and principal fallback diagnostics:

```bash
cargo run -q -- path/to/file.ers --exp-infer-local-bidi --exp-infer-principal-fallback
```

With `--exp-infer-local-bidi`, Elevate now supports:

- Placeholder parameter types: `a: _`
- Omitted parameter annotations: `fn add(a, b) -> i64`
- Implicit first assignment bindings in function bodies: `x = add(3, 4);`

Example:

```ers
fn add(a, b) -> i64 {
    return a + b;
}

fn main() {
    x = add(3, 4);
    return;
}
```

Compiles (with the inference flag) to a concrete signature and local type:

```rust
fn add(a: i64, b: i64) -> i64 { ... }
fn main() -> () {
    let x: i64 = add(3, 4);
    return;
}
```

## Docs

- Language design: `docs/language-design.md`
- Inference roadmap: `docs/type-inference-plan.md`
