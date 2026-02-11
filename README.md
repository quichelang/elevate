# Elevate

**A compiled language and compiler framework that transpiles to Rust with inferred ownership and no runtime.**

Elevate is a **source-to-source compiler**: it compiles `.ers` into Rust source, then Rust handles final compilation and guarantees.

Elevate is two things:
- A simpler representation of Rust.
- A model of what Rust could look like if the compiler took more decisions automatically for the user.

We think these decisions are good enough for roughly 80% of typical Rust systems-tooling use cases and should not impose a heavy performance burden. Our target is to keep performance within 10-20% of Rust.

Key differences:
- No explicit lifetimes
- No explicit memory-reference access/pointers
- Some match behavior limitations
- Friendlier generics syntax (you can invoke methods on generic objects without explicit definition)
- No unsafe code by default
- No panics by default

Elevate uses Cargo for package management and can interop with crates directly. When direct memory or ownership syntax is required, use a `rust {}` block anywhere in source; it is emitted verbatim. You can also mix `.rs` files with Elevate source in the same crate and add wrappers to expose Rust behavior ergonomically in Elevate.

[Quiche](https://github.com/quichelang/quiche), a Python-inspired compiled language, uses Elevate IR as its backend.

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

The compiler emits idiomatic Rust with ownership, borrowing, and mutability inferred from usage.

---

## Start Here (2 Minutes)

```bash
git clone https://github.com/quichelang/elevate.git
cd elevate
cargo build --release

# Compile an example and print generated Rust
cargo run -q -- examples/point.ers

# Build a real .ers crate
cargo run -q -- build examples/boardgame-kit
```

Then inspect generated Rust with:

```bash
cargo run -q -- examples/point.ers --emit-rust /tmp/point.rs
```

---

## Why Elevate?

| What you get | How it works |
|---|---|
| **No `&`, `&mut`, or lifetime annotations** | The compiler's ownership planner infers borrows, moves, and clones from usage patterns |
| **No `mut` keyword** | Mutability is detected heuristically - if you reassign or call `.push()`, it becomes `mut` |
| **No `dyn` or `Box<dyn>`** | Trait-object lowering is inferred when you use trait types |
| **Real Rust output** | Every `.ers` file compiles to clean `.rs` - you can read, audit, and ship the generated code |
| **Zero dependencies** | The compiler itself has literally `[dependencies]` empty in `Cargo.toml` |
| **Full interop with Rust crates** | Use `use` imports, call Rust functions, or embed raw Rust with `rust { ... }` blocks |

This is the practical value proposition for new users:
- Write expressive source with fewer ownership annotations.
- Keep generated output auditable and deployable as normal Rust.
- Reuse Rust crates instead of rebuilding ecosystem tooling.
- Use the same backend to power new language frontends.

---

## Quick Start CLI

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

Exhaustiveness checking covers `bool`, `Option`, `Result`, finite tuple domains, and known local enums - with guard-aware analysis.

### Generics and Trait Bounds

```rust
struct Box<T> {
    value: T;
}

enum Maybe<T> {
    Some(T);
    None;
}

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

impl<T> Box<T> {
    fn new(value: T) -> Self {
        Box { value: value }
    }

    fn get(self) -> T {
        self.value
    }
}
```

Generics are supported on functions, structs, enums, and impl blocks. Trait declarations support supertraits (`trait A: B + C { ... }`) and trait unions (`A + B`).

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

## Framework Design

A useful mental model:
- **Typed IR** (`src/ir/typed.rs`) is like technical language or writing: language-semantic, type-checked, and close to source intent.
- **Lowered Rust IR** (`src/ir/lowered.rs`) is like legal writing: still structured, but rewritten into Rust-shaped forms needed for deterministic backend emission.

What should you use to design and implement your own language frontend integration?
- Treat Elevate IR as a staged IR family, not a single node format.
- Prefer **Typed IR** for frontend integration.
- Let Elevate handle lowering into **Lowered Rust IR** and code emission.

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
cargo run -q -- file.ers --exp-type-system
```

### Inferred Parameter Types

```rust
// No type annotations on parameters - inferred from return type + usage
fn add(a, b) -> i64 {
    return a + b;
}

fn main() {
    let x = add(3, 4);   // x inferred as i64
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

The `_` placeholder is resolved from local constraints. When inference can't find a stable principal type, the `--exp-type-system` flag emits a deterministic "add a type annotation here" hint instead of cascading errors.

### Effect Rows (Experimental Surface)

Elevate now supports an effect-row surface annotation for function and trait-method signatures:

```rust
fn render_card(card: Card) -> String ![method::render + call::std::mem::drop] {
    const text = card.render();
    std::mem::drop(card);
    text
}
```

- `![cap_a + cap_b]` declares a closed capability row.
- `![..r]` declares an open row tail.
- Enable checks with `--exp-type-system`.

---

## The Compiler Pipeline

Elevate's compiler is a **complete, 7-stage pipeline** - not a prototype:

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
- **Place-level conflict analysis** - tracks which struct fields are used independently
- **Loop-weighted liveness heuristics** - detects expensive clones in hot loops
- **Adaptive borrow feedback** - retries transpilation with auto-inferred borrow hints from `rustc` diagnostics
- **Disjoint field optimization** - avoids cloning when different struct fields are used in non-overlapping scopes

## Build A Language On Elevate

Elevate exposes its IR as a reusable compilation target. This is the path if you want your own syntax/front-end but still want a safe and practical backend.

### EIR Layers

| Layer | Module | Purpose |
|-------|--------|---------|
| **Typed Core IR** | `TypedModule` | Language-centric, type-checked representation with typed expressions, statements, pattern matching, generics, and traits |
| **Rust Lowered IR** | `RustModule` | Rust-close representation with explicit ownership operations, borrow insertions, clone decisions, and Rust-specific lowering |

### Frontend Integration Flow

1. Parse your source language into your own AST.
2. Lower your AST into Elevate `TypedModule`.
3. Hand `TypedModule` to Elevate passes (type checks, ownership planning, specialization, Rust lowering).
4. Emit Rust and build against the normal Rust crate ecosystem.

Any frontend that can emit `TypedModule` inherits Elevate's ownership inference, specialization, and Rust code generation. This is exactly how [Quiche](https://github.com/quichelang/quiche) works.

### Safety And Ecosystem Story

- Elevate does not add a runtime or GC layer between your language and Rust.
- You can keep safety policies in Elevate passes instead of leaking backend hazards to users.
- The output is standard Rust, so existing Rust crates, tooling, and CI remain usable.

The core IR definitions are in [`src/ir/typed.rs`](src/ir/typed.rs) and [`src/ir/lowered.rs`](src/ir/lowered.rs).

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
- **Deterministic output** - same input always produces same Rust code
- **Generated Rust compiles** - tests run `rustc --crate-type=lib` on output
- **Ownership correctness** - auto-clone insertion, borrow elision, move semantics
- **Error quality** - actionable diagnostics with source location and fix hints
- **No panics on invalid programs** - malformed input produces errors, never crashes

---

## Real-World Example Projects

### `lexopt-elevate` - Full CLI Argument Parser

A complete reimplementation of the `lexopt` CLI parsing library, written entirely in Elevate with Rust interop. Multi-module crate with `Parser`, `Arg`, `ParseError` types, short/long option handling, cluster splitting, value extraction, and error formatting.

**4 source modules, 450+ lines of Elevate code.**

### `boardgame-kit` - Game Engine Rendering Framework

An 839-line game rendering toolkit featuring sprites, tile atlases, animated sprites, scene layout, gradient fills, bevel panels, brick floor patterns, hill wave generation, wood-frame UI, HUD panels with keypad buttons - all written in Elevate and compiled through the crate build system.

**Linking default for SDL2/OpenGL examples**

`boardgame-kit`-based examples (`boardwalk-sudoku`, `prosemaster`) default to:

```toml
sdl2 = { version = "0.38", default-features = false, features = ["bundled", "static-link"] }
```

This avoids macOS runtime loader failures like missing `@rpath/libSDL2-2.0.0.dylib` and gives a more reliable "run immediately after build" experience.

If you want dynamic linking instead:

1. Change dependency to dynamic:
   `sdl2 = { version = "0.38", features = ["bundled"] }`
2. Ensure your binary can locate `libSDL2-2.0.0.dylib` at runtime:
   add an `LC_RPATH` (for example `@executable_path` or `@loader_path/../Frameworks`) and place the dylib there.
3. Verify with:
   `otool -L ./target/debug/<your-binary>`

### `boardwalk-sudoku` - Interactive Sudoku Game

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
| `--exp-type-system` | Unified inference/type-system preview: bidirectional local inference, literal steering, principal fallback diagnostics, numeric coercion, and effect-row checking (surface + internal) |
| `--exp-move-mut-args` | Move-by-default for mutation-capable call flows |

### Ownership Control Flags

| Flag | Effect |
|------|--------|
| `--fail-on-hot-clone` | Reject expensive clones inside hot loops |
| `--allow-hot-clone-place <place>` | Exempt a specific place from hot-clone policy |
| `--force-clone-place <place>` | Force clone at a specific place |

---

## Editor Support

Syntax highlighting is available for:

- **Visual Studio Code** - TextMate grammar with full keyword, operator, and literal coverage
- **Zed** - Tree-sitter queries with indent rules

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

- [Language Design](docs/language-design.md) - Full specification, contracts, and implementation status
- [Type Inference Plan](docs/type-inference-plan.md) - HM-style inference + bidirectional extensions roadmap
- [AST Reference](docs/ast.md) - Node structure documentation
- [External Frontend Path](docs/external-frontend-path.md) - Binary AST/EIR integration, terminology, and diagnostics metadata guidance
- [Editor Extensions](docs/editor-extensions.md) - VSCode and Zed setup

---

## Status

Elevate is **v0.4.0** - a fully functional compiler with a broad feature set, not a toy or proof-of-concept. The pipeline compiles real programs, the test suite is comprehensive, and the generated Rust output is clean and correct.

That said, some areas are still evolving:
- Ownership lowering is strong but not globally optimal
- Slice support focuses on `Vec`-based patterns
- Full Rust-pattern parity in `match` is in progress
- The type inference experimental mode is under active development

---

## License

See repository for license details.
