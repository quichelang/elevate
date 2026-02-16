# FAQ

## Mutability and Borrow Inference

### Q: Does Elevate (compiler) prefer mutability, or immutability? 🤔

A: Elevate doesn’t force a strict “mutable-first” or “immutable-first” style in source; it’s inference-first.

- In practice, you write `let`, and the compiler infers mutability from usage (reassignment, mutating calls).
- Use `const` when you want explicit immutability.

So the design leans toward ergonomic defaults with compiler-managed ownership/mutability, rather than making you choose up front.

(See `docs/language-design.md` and `README.md`.)

---

### Q: What about function passing, does the compiler infer that based on what is being done with the object in the function? 🧠

A: Yes — Elevate infers how parameters are passed (owned / borrowed / mutably‑borrowed) from how they’re *used* (both inside the callee and at the call site).

- Source-level code never writes `&` / `&mut` — the compiler decides for you.
- The compiler analyzes the function body (ownership plan + mutation analysis) and:
  - promotes params to `&mut T` when they are mutated but not consumed,
  - marks read‑only params as borrowed,
  - leaves parameters as owned when the callee consumes/returns them.
- At call sites the lowering pass inserts `&` / `&mut` / `.borrow()` or auto‑clone as needed (heuristics + known signatures).

(See `docs/details/borrow-trait-parameter-passing.md`, `src/passes.rs`, `src/borrow_promotion.rs`.)

---

### Q: What if we have deep nested structure of function calls, and the only mutable operation happening on the object, is 5 levels deep. Can the compiler infer the intent? Is there a hard-limit? ❓

A: Yes — Elevate will infer mutability through deep, nested call chains (there is no small fixed depth limit).

- Ownership/borrow analysis and borrow‑promotion run across the whole lowered module, so a mutation several levels deep can cause an earlier parameter to be treated as `&mut` (or trigger clones/owned passing if appropriate).

Practical limits / caveats:
- There is **no hard-coded "max depth"** in the compiler source — inference is transitive and module‑wide.
- Inference requires visibility: functions must be in the analyzed module/crate or have signature/interop metadata available; opaque/FFI/trait-object boundaries may force conservative (owned/clone) behavior.
- Generics / unknown param types or external/opaque calls can make the pass conservative.
- Recursive/self calls and a few edge cases are handled specially by the promotion logic.

(See `src/passes.rs`, `src/borrow_promotion.rs`, `src/ownership_planner.rs`.)

---

### Where to read in the code/docs 📚
- Design: `docs/language-design.md`
- Borrow/parameter rules: `docs/details/borrow-trait-parameter-passing.md`
- Implementation & heuristics: `src/passes.rs`, `src/borrow_promotion.rs`, `src/ownership_planner.rs`

---

### Q: Do I need Rust nightly? How do I install it and how is it enforced?

A: Yes — Elevate/Quiche rely on `rustdex`, which requires rustdoc JSON features present only on Rust **nightly**. Each repository that depends on `rustdex` includes a `rust-toolchain.toml` that requests `nightly` plus the `rust-docs-json` component; Cargo invoked in that repository will automatically use the specified toolchain (or prompt rustup to install it).

Install nightly + rust-docs-json manually:

```bash
rustup toolchain install nightly
rustup component add rust-docs-json --toolchain nightly
# or rely on the repo's rust-toolchain.toml and run `cargo build` inside the repo
```

This enforces the requirement at the toolchain level and avoids surprising behavior when building.
