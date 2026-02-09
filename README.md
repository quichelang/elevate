# Elevate

**A compiled language and compiler framework that transpiles to Rust — with zero dependencies, automatic ownership, and practical type inference.**

Elevate is two things: a **language** that lets you write high-level, Rust-adjacent code without manual lifetimes, borrows, or `mut` annotations, and a **framework** that exposes a structured intermediate representation (EIR) that other languages can target. The compiler makes all ownership and mutability decisions automatically, then emits clean, compilable Rust source. No runtime. No GC. No magic.

[Quiche](https://github.com/quichelang/quiche), a Python-inspired compiled language, is built on Elevate's IR — see the Quiche repo for how it uses EIR as its compilation backend.

```rust
struct Point {
    x: i64;
    y: i64;
}

fn manhattan(a: Point, b: Point) -> i64 {
    let dx = a.x - b.x;
    let dy = a.y - b.y;
    if dx < 0 { -dx } else { dx } + if dy < 0 { -dy } else { dy }
}
```

The compiler turns this into idiomatic Rust with correct ownership, borrowing, and mutability — all inferred from how values are actually used.

---

## Why Elevate?

| What you get | How it works |
|---|---|
| **No `&`, `&mut`, or lifetime annotations** | The compiler's ownership planner infers borrows, moves, and clones from usage patterns |
| **No `mut` keyword** | Mutability is detected heuristically — if you reassign or call `.push()`, it becomes `mut` |
| **No `dyn` or `Box<dyn>`** | Trait-object lowering is inferred when you use trait types |
| **Real Rust output** | Every `.ers` file compiles to clean `.rs` — you can read, audit, and ship the generated code |
| **Zero dependencies** | The compiler itself has literally `[dependencies]` empty in `Cargo.toml` |
| **Full interop with Rust crates** | Use `use` imports, call Rust functions, or embed raw Rust with `rust { ... }` blocks |

---

## Quick Start

```bash
# Install (requires Rust 2024 edition / rustc 1.85+)
git clone https://github.com/quichelang/elevate.git
cd elevate
cargo build --release

# Compile a single file → prints generated Rust to stdout
cargo run -q -- examples/point.ers

# Save generated Rust to a file
cargo run -q -- examples/point.ers --emit-rust output.rs

# Create a new Elevate project with transparent bootstrap runner
cargo run -q -- init my-app

# Build an .ers crate project
cargo run -q -- build examples/boardgame-kit

# Run tests for an .ers crate
cargo run -q -- test examples/lexopt-elevate
```

---

## Language Features

### Structs, Enums, and Pattern Matching

```rust
enum Shape {
    Circle(f64);
    Rect(f64, f64);
}

fn area(shape: Shape) -> f64 {
    match shape {
        Shape::Circle(r) => 3.14159 * r * r;
        Shape::Rect(w, h) => w * h;
    }
}
```

Pattern matching supports tuple patterns, literal patterns, binding patterns, nested variant payloads, struct rest patterns (`Type { field, .. }`), or-patterns (`p1 | p2`), match guards (`pattern if condition => ...`), slice/rest patterns (`[a, ..tail]`), and block arm expressions.

Exhaustiveness checking covers `bool`, `Option`, `Result`, finite tuple domains, and known local enums — with guard-aware analysis.

### Generics and Trait Bounds

```rust
fn identity<T>(x: T) -> T {
    x
}

fn keep<T: Clone + Copy>(x: T) -> T {
    x
}

trait Measurable {
    fn measure(self) -> f64;
}

trait Drawable: Measurable {
    fn draw(self) -> String;
}
```

Generics compile by monomorphization with safety limits. Trait declarations support supertraits (`trait A: B + C { ... }`) and trait unions (`A + B`).

### Closures

```rust
fn apply(f: fn(i64) -> i64, x: i64) -> i64 {
    f(x)
}
```

### Control Flow

```rust
fn fizzbuzz(n: i64) -> String {
    if n % 15 == 0 {
        "FizzBuzz"
    } else if n % 3 == 0 {
        "Fizz"
    } else if n % 5 == 0 {
        "Buzz"
    } else {
        format!("{}", n)
    }
}
```

All the essentials: `if`/`else`, `while`, `loop` with `break`/`continue`, `for ... in ...` (ranges, `Vec`, `String`, `Option`, `HashMap`, `BTreeMap`, `HashSet`, `BTreeSet`), and tail-expression returns.

### Impl Blocks and Method Calls

```rust
struct Counter {
    value: i64;
}

impl Counter {
    fn new() -> Self {
        Counter { value: 0 }
    }

    fn increment(self) {
        self.value += 1;
    }

    fn get(self) -> i64 {
        self.value
    }
}

fn main() {
    let c = Counter::new();
    c.increment();
    println!("{}", c.get());
}
```

Instance method-call syntax (`value.method(...)`) also works, resolving against user `impl` methods.

### Result and Option with `?` Propagation

```rust
use std::num::ParseIntError;

fn parse_value(input: String) -> Result<i64, ParseIntError> {
    let parsed = parse_i64(input)?;
    Result::Ok(parsed * 2)
}
```

### Vec, Tuples, and Destructuring

```rust
fn process(values: Vec<i64>) -> i64 {
    let [head, ..tail] = values;
    let (a, b) = (head, tail.len());
    a + b
}
```

Vec indexing, range slicing (`values[a..b]`), `push()`, `first()`, `last()`, `get(i)`, and index assignment (`values[i] = v`) are all supported. Tuples support literals, type annotations, and destructuring bindings.

### Rust Interop

Embed raw Rust directly when you need it:

```rust
rust {
    pub fn fast_hash(data: &[u8]) -> u64 {
        let mut h: u64 = 0;
        for &b in data { h = h.wrapping_mul(31).wrapping_add(b as u64); }
        h
    }
}

fn demo(text: String) -> u64 {
    fast_hash(text.as_bytes())
}
```

For crate-level projects, an `elevate.interop` contract file lets you declare allowed imports and auto-generated adapter functions:

```
allow crossterm
allow std::io

adapter draw_cell = crossterm_helpers::draw_cell
```

### Read Views (Non-Consuming Borrows)

```rust
fn total_length(names: Vec<String>) -> i64 {
    let sum = 0;
    for name in view(names) {
        sum += name.len();
    }
    sum
}
```

`view(...)` lowers to Rust borrows, letting you read without consuming.

---

## Type Inference (Experimental)

Elevate has an experimental bidirectional type inference system inspired by Koka and OCaml. Enable it with flags:

```bash
cargo run -q -- file.ers --exp-infer-local-bidi --exp-infer-principal-fallback
```

### Inferred Parameter Types

```rust
// No type annotations on parameters — inferred from return type + usage
fn add(a, b) -> i64 {
    return a + b;
}

fn main() {
    x = add(3, 4);   // x inferred as i64, no `let` or `const` needed
    return;
}
```

Compiles to:

```rust
fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn main() -> () {
    let x: i64 = add(3, 4);
    return;
}
```

### Placeholder Types

```rust
fn double(n: _) -> i64 {
    n * 2
}
```

The `_` placeholder is resolved from local constraints. When inference can't find a stable principal type, the `--exp-infer-principal-fallback` flag emits a deterministic "add a type annotation here" hint instead of cascading errors.

---

## The Compiler Pipeline

Elevate's compiler is a **complete, 7-stage pipeline** — not a prototype:

```
Source (.ers)
  │
  ├─ 1. Lexer          → Token stream
  ├─ 2. Parser          → Untyped AST
  ├─ 3. Type Checker    → Typed Core IR (name resolution, inference, diagnostics)
  ├─ 4. Ownership Pass  → Ownership-annotated IR (move/clone/borrow decisions)
  ├─ 5. Specialization  → Monomorphized IR with budget limits
  ├─ 6. Rust Lowering   → Rust-oriented IR with explicit Rust operations
  └─ 7. Code Emission   → Clean, compilable .rs source
```

The ownership pass includes:
- **Place-level conflict analysis** — tracks which struct fields are used independently
- **Loop-weighted liveness heuristics** — detects expensive clones in hot loops
- **Adaptive borrow feedback** — retries transpilation with auto-inferred borrow hints from `rustc` diagnostics
- **Disjoint field optimization** — avoids cloning when different struct fields are used in non-overlapping scopes

### Elevate Intermediate Representation (EIR)

Elevate exposes its internal IR as a reusable compilation target. The EIR has two layers:

| Layer | Module | Purpose |
|-------|--------|---------|
| **Typed Core IR** | `TypedModule` | Language-centric, type-checked representation with typed expressions, statements, pattern matching, generics, and traits |
| **Rust Lowered IR** | `RustModule` | Rust-close representation with explicit ownership operations, borrow insertions, clone decisions, and Rust-specific lowering |

Any language that can produce a `TypedModule` gets Elevate's full ownership inference, specialization, and Rust code generation for free. This is exactly how [Quiche](https://github.com/quichelang/quiche) works — its compiler desugars Python-style syntax into EIR's Typed Core IR, then Elevate handles the rest of the pipeline from ownership decisions through Rust emission.

The IR is fully defined in [`src/ir/typed.rs`](src/ir/typed.rs) and [`src/ir/lowered.rs`](src/ir/lowered.rs).

### Frontend Diagnostics Contract

For external frontends, pass source identity into compile options so diagnostics can point to frontend source instead of opaque byte ranges:

```rust
let options = CompileOptions {
    source_name: Some("examples/demo.q".to_string()),
    ..Default::default()
};
```

When source text is available (`compile_source_with_options`), Elevate reports `file:line:col` locations. For internal diagnostics without precise spans, Elevate now reports `location unavailable` explicitly instead of ambiguous `0..0` byte ranges.

Source-location math and rendering are centralized in [`src/source_map.rs`](src/source_map.rs), which is the extension point for richer frontend/backend source-map correlation.

---

## Test Suite: 249+ Tests Across 8 Modules

The compiler is backed by an extensive test suite that validates every layer:

| Module | Tests | What it covers |
|--------|------:|----------------|
| `lib.rs` | 142 | End-to-end compilation, ownership decisions, auto-borrow/clone, method resolution, interop shims, benchmark regression profiles |
| `parser.rs` | 38 | AST structure, operator precedence, error recovery, all syntax forms |
| `crate_builder.rs` | 22 | Multi-file transpilation, mod injection, interop contract validation, path safety |
| `inference_missing_values.rs` | 19 | `Option`/`Result` hole filling, nested placeholders, branch ordering |
| `lexer.rs` | 18 | Token classification, string/char literals, keywords, operators |
| `cli.rs` | 6 | Argument parsing, flag composition, error handling |
| `passes.rs` | 2 | Semantic validation passes |
| `test_runner.rs` | 2 | Native `.ers` test framework discovery and execution |

Tests verify:
- **Deterministic output** — same input always produces same Rust code
- **Generated Rust compiles** — tests run `rustc --crate-type=lib` on output
- **Ownership correctness** — auto-clone insertion, borrow elision, move semantics
- **Error quality** — actionable diagnostics with source location and fix hints
- **No panics on invalid programs** — malformed input produces errors, never crashes

---

## Real-World Example Projects

### `lexopt-elevate` — Full CLI Argument Parser

A complete reimplementation of the `lexopt` CLI parsing library, written entirely in Elevate with Rust interop. Multi-module crate with `Parser`, `Arg`, `ParseError` types, short/long option handling, cluster splitting, value extraction, and error formatting.

**4 source modules, 450+ lines of Elevate code.**

### `boardgame-kit` — Game Engine Rendering Framework

An 839-line game rendering toolkit featuring sprites, tile atlases, animated sprites, scene layout, gradient fills, bevel panels, brick floor patterns, hill wave generation, wood-frame UI, HUD panels with keypad buttons — all written in Elevate and compiled through the crate build system.

### `boardwalk-sudoku` — Interactive Sudoku Game

A working Sudoku game built on `boardgame-kit`, demonstrating Elevate's ability to power real interactive applications with terminal rendering.

---

## CLI Commands

```bash
# Compile single file → stdout
elevate <file.ers>

# Compile → Rust file
elevate <file.ers> --emit-rust [output.rs]

# Build .ers crate (debug/release)
elevate build <crate-root> [--release]

# Test .ers crate (discovers test_* functions)
elevate test <crate-root>

# Benchmark compiler performance
elevate bench [--iters N] [--warmup N] [--out report.csv] [--compare baseline.csv]

# Scaffold new project
elevate init <crate-root>
```

### Experiment Flags

| Flag | Effect |
|------|--------|
| `--exp-infer-local-bidi` | Koka-inspired bidirectional local inference (omitted params, `_` placeholders) |
| `--exp-infer-principal-fallback` | Deterministic "add annotation here" diagnostics when inference stalls |
| `--exp-move-mut-args` | Move-by-default for mutation-capable call flows |
| `--exp-effect-rows-internal` | Internal row-like capability checks (trait-supertrait method capabilities + generic method-use diagnostics) |
| `--exp-numeric-coercion` | Automatic numeric type coercion in expressions |

### Ownership Control Flags

| Flag | Effect |
|------|--------|
| `--fail-on-hot-clone` | Reject expensive clones inside hot loops |
| `--allow-hot-clone-place <place>` | Exempt a specific place from hot-clone policy |
| `--force-clone-place <place>` | Force clone at a specific place |

---

## Editor Support

Syntax highlighting is available for:

- **Visual Studio Code** — TextMate grammar with full keyword, operator, and literal coverage
- **Zed** — Tree-sitter queries with indent rules

See [`docs/editor-extensions.md`](docs/editor-extensions.md) for setup instructions.

---

## Architecture

```
src/
├── main.rs              # CLI entry point (build/test/bench/init commands)
├── lib.rs               # Compiler API + 142 integration tests
├── lexer.rs             # Tokenizer (32K, 18 tests)
├── parser.rs            # Recursive descent parser (70K, 38 tests)
├── ast.rs               # AST node definitions
├── passes.rs            # Semantic analysis and type checking (292K)
├── codegen.rs           # Rust code emission (34K)
├── crate_builder.rs     # Multi-file crate transpilation (88K, 22 tests)
├── ownership_planner.rs # Move/clone/borrow decision engine
├── test_runner.rs       # Native .ers test framework
├── ir/                  # Typed Core IR and Rust Lowered IR
├── source.rs            # Source file loading
├── diag.rs              # Diagnostic types
└── templates/           # Project scaffolding templates
```

Zero external dependencies. The entire compiler is self-contained, built on `std` only.

---

## Building from Source

```bash
git clone https://github.com/quichelang/elevate.git
cd elevate
cargo build --release
cargo test -q
```

Requires **Rust 2024 edition** (rustc 1.85+).

---

## Documentation

- [Language Design](docs/language-design.md) — Full specification, contracts, and implementation status
- [Type Inference Plan](docs/type-inference-plan.md) — HM-style inference + bidirectional extensions roadmap
- [AST Reference](docs/ast.md) — Node structure documentation
- [External Frontend Path](docs/external-frontend-path.md) — Binary AST/EIR integration, terminology, and diagnostics metadata guidance
- [Editor Extensions](docs/editor-extensions.md) — VSCode and Zed setup

---

## Status

Elevate is **v0.4.0** — a fully functional compiler with a broad feature set, not a toy or proof-of-concept. The pipeline compiles real programs, the test suite is comprehensive, and the generated Rust output is clean and correct.

That said, some areas are still evolving:
- Ownership lowering is strong but not globally optimal
- Slice support focuses on `Vec`-based patterns
- Full Rust-pattern parity in `match` is in progress
- The type inference experimental mode is under active development

---

## License

See repository for license details.
