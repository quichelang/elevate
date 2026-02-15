Here is a significantly improved, highly structured version of your prompt. I have rewritten it to act as a comprehensive **System Design Prompt** that you can feed to an AI assistant or use as an architectural brief for an engineering team.

It explicitly integrates the terminology and architecture from the Elevate documentation you provided (like the 3-layer IR, `AstEnvelope`, and Ownership Planner).

Following the rewritten prompt, I have included a section of **Additional Thoughts & Architectural Pointers** that provides concrete implementation strategies based on your codebase's current state.

---

### 📋 The Rewritten Prompt (Ready to Copy/Paste)

**Role & Objective:**
Act as a Principal Compiler Engineer specializing in language design, diagnostics, and developer experience (DX). I need you to architect a **Unified Diagnostics and Error Translation System** for the **Elevate** compiler infrastructure.

The primary goal is to create a "one-stop shop" diagnostics engine that guarantees users only ever see errors in the context of the programming language they actually wrote—whether that is Elevate (`.ers`) or a higher-level frontend language like **Quiche** (`.q`).

**Background Context:**

* **Elevate:** A language and transpiler with a 3-layer IR pipeline (AST  Typed IR  Lowered Rust IR) that generates Rust code and compiles it via `cargo build`.
* **Quiche:** An external frontend that compiles down to Elevate's Layer 1 AST and passes it to Elevate via a serialized, versioned `AstEnvelope`.

**Core System Requirements:**

1. **Language-Native Error Context (The Source of Truth)**
* Error messages must strictly use the user's source language as the reference point.
* If a user authored Quiche, the file names, line numbers, variable names, problem descriptions, and *suggested solutions* must be expressed in Quiche semantics. If they wrote Elevate, it must use Elevate semantics.
* We need a robust Source Map architecture that threads span data bidirectionally from the generated Rust back up to the Elevate IR, and finally back to the frontend AST.


2. **`rustc` Interception & Translation**
* All `rustc` and `cargo` warnings/errors generated during the final backend build step must be automatically intercepted.
* Raw `rustc` output should be suppressed by default and made available only via an opt-in verbose flag.
* The system must translate these Rust-level borrow, type, or syntax errors back into the conceptual framework of the user's frontend language.


3. **Compiler Bug Isolation (ICE Detection Heuristic)**
* We need a deterministic heuristic to classify errors and prevent blaming the user for compiler flaws.
* **The Rule:** If the user's source code has no "smells"—meaning it successfully passes the frontend's syntactic checks and Elevate's internal type/capability checks—but `rustc` subsequently fails to compile the generated Rust, **this must be explicitly flagged as an Elevate Compiler Bug** (e.g., an Internal Compiler Error / ICE, invalid lowering, or a missing feature).
* The system should gracefully catch this, apologize to the user, and dump an ICE crash report rather than displaying a confusing Rust error.


4. **Structured Error Catalogs**
* We need a comprehensive, structured registry of error messages segregated by language (Elevate will have its own catalog; Quiche will have its own).
* *For this task, we will focus solely on architecting the catalog schema and defining the initial error catalog for **Elevate**.*



**Your Task:**
Based on these requirements, please provide:

1. **The Source Mapping Architecture:** Detail how we thread spans from `rustc` diagnostics back through the 3-layer IR to the frontend AST using Elevate's existing `AstEnvelope`, `FrontendMeta`, and `src/source_map.rs`.
2. **The Interceptor & ICE Logic:** Define the exact pipeline boundaries and heuristics needed to confidently parse `cargo` errors, translate them, and classify a failure as an ICE versus a user error.
3. **Error Catalog Schema:** Propose a data structure (e.g., Rust `enum` or JSON schema) for defining language-specific errors that adheres to Elevate's diagnostic contract: *Source Location, Expected vs. Actual, Explanation of failed decision, and a Direct Fix Hint.*
4. **Draft Elevate Errors:** Provide a foundational list of 5–10 structured Elevate error codes covering Syntax, Type/Capability mismatches, Ownership limits, and ICEs.

---

### 💡 Additional Thoughts & Architectural Pointers

Based on the extensive Elevate documentation you provided, you actually have a massive head start on building this. Here are specific technical pointers to guide your implementation:

#### 1. You already have the infrastructure to intercept `rustc`

Your documentation states: *"Crate build loop now includes adaptive borrow/clone feedback from Rust diagnostics (retrying transpile/build with inferred interop borrow hints...)"*.

* **The Implementation Strategy:** Because Elevate is *already* parsing `rustc` diagnostics to dynamically insert `.clone()` or `&` during the build loop, you simply need to hook into this exact same flow *after* the adaptive loop exhausts its retries. Ensure you are invoking `cargo build --message-format=json` to get highly structured JSON containing the exact Rust file paths, lines, columns, and error codes (like `E0382`).

#### 2. The ICE Boundary is mathematically crisp

In a transpiled language like Elevate, detecting an Internal Compiler Error (ICE) doesn't require complex guesswork. Elevate has strict pipeline contracts:

* **Layer 1 & 2:** If the user writes bad Quiche or Elevate code, it should be caught and failed in Elevate's `passes.rs` (Type & Capability System).
* **Layer 3:** If the code reaches the Rust lowering and emission phase, Elevate's internal checks have essentially signed a "Contract of Correctness."
* **The Heuristic:** If `rustc` subsequently throws a type error or borrow checker error, **Elevate's ownership planner broke its contract**. For example, if `rustc` throws a lifetime error or complains about a missing trait, the user did nothing wrong. You can confidently swallow the `rustc` error and output: `[ICE-E9001]: Elevate Ownership Planner failed to safely lower this expression. Valid source resulted in invalid Rust code.`

#### 3. Threading the Source Map mechanically

To achieve the "Language-Native Reference Point," you need a strict chain of custody for `Span` data:

* **Frontend Duty:** As mentioned in your docs, Quiche **must** populate the `AstEnvelope` with accurate byte-span ranges and the `FrontendMeta` (`source_map_id` and `source_path`).
* **Compiler Duty:** As Elevate transforms Layer 1  Layer 2  Layer 3, every IR node must retain a reference to its original AST span.
* **Emission Duty:** When `codegen.rs` writes the final `.rs` file, it must maintain a sidecar interval tree (or inject hidden comments) that maps `generated_rust_file.rs:Line:Col` to the original `AST_Span`.
* **The Resolution:** When `rustc` JSON reports an error on Line 45, your engine looks up Line 45 in the sidecar map, finds the Quiche AST Span, checks `FrontendMeta.language`, and queries the Quiche Error Catalog for that specific AST node type.

#### 4. Formatting Elevate Errors (Hiding Rust-isms)

Because Elevate has strict language constraints (e.g., no explicit `dyn`, no `&` or `&mut` exposed to the user, `let` vs `const` inferred mutability), your Elevate error catalog **must aggressively sanitize Rust terminology**.

* *Bad Error:* "Cannot borrow `x` as mutable because it is behind a shared reference." (The Elevate/Quiche user doesn't know what a Rust reference is).
* *Good Elevate Error:* "Cannot mutate `x` here. `x` was passed as a read-only view (`view(...)`). Consider passing it by value."
* Look into standard Rust diagnostic rendering crates like [`miette`](https://www.google.com/search?q=%5Bhttps://docs.rs/miette/latest/miette/%5D(https://docs.rs/miette/latest/miette/)) or [`ariadne`](https://www.google.com/search?q=%5Bhttps://docs.rs/ariadne/latest/ariadne/%5D(https://docs.rs/ariadne/latest/ariadne/)). They handle the heavy lifting of drawing beautiful terminal output with code snippets and underlines based on the spans you feed them.