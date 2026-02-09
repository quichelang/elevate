# External Frontend Path (Binary AST / EIR)

This document focuses on integrating non-Elevate frontends (for example Quiche) with Elevate as a backend.

## Terminology (corrected)

- `AST` in this repository means the untyped source-shaped model in `/Volumes/Dev/code/jagtesh/elevate/src/ast.rs`.
- `Typed IR` means typechecked IR in `/Volumes/Dev/code/jagtesh/elevate/src/ir/typed.rs`.
- `Lowered IR` means Rust-oriented ownership-resolved IR in `/Volumes/Dev/code/jagtesh/elevate/src/ir/lowered.rs`.
- `EIR` (Elevate IR) is commonly used to refer to this whole pipeline, but external frontends should usually produce AST (Layer 1), not Typed/Lowered IR.
- `Source map` here means metadata that maps diagnostics/generated code back to frontend source files and spans.

## Current integration path (today)

External frontends integrate through:

- `compile_ast(module: &Module)`
- `compile_ast_with_options(module: &Module, options: &CompileOptions)`

Relevant option for diagnostics:

- `CompileOptions.source_name: Option<String>`
  - Set this to original frontend file path (for example `src/demo.q`).
  - Elevate includes it in diagnostics.

## Diagnostics metadata path

When compiling from raw source (`compile_source_with_options`), Elevate has:

- source text
- source name (if provided)

This allows `file:line:col` rendering.  
When only AST is provided (`compile_ast_with_options`), Elevate currently has:

- source name (optional)
- no source text by default

So diagnostics can still show file identity, but line/column quality depends on spans already carried by AST nodes/diagnostics.

## Recommended binary payload shape

For decoupled frontend/backend workflows, use a versioned envelope:

```rust
struct AstEnvelope {
    schema_version: u32,
    module: Module,
    meta: Option<FrontendMeta>,
}

struct FrontendMeta {
    language: String,                 // "quiche", etc.
    compiler_version: String,         // frontend version
    source_path: Option<String>,      // original path, for diagnostics
    source_map_id: Option<String>,    // ID/key to fetch richer mapping
}
```

## What AST/IR nodes should include for richer diagnostics?

### Minimum

- keep using `Span { start, end }` byte offsets in frontend source text
- set `CompileOptions.source_name`

### Better

- include per-node spans consistently across all emitted AST expressions/statements
- ensure spans point to frontend source (not generated/transpiled Rust)

### AST fields frontend authors should use (current)

- Generic data types:
  - `StructDef.type_params`
  - `EnumDef.type_params`
- Generic impls:
  - `ImplBlock.type_params`
  - `ImplBlock.target_args`
  - `ImplBlock.trait_target` for trait impls
- Effect rows (experiment):
  - `FunctionDef.effect_row`
  - `TraitMethodSig.effect_row`
  - surface syntax in Elevate source is `![cap::path + ..r]`

### Best (future)

- include a sidecar source-map table keyed by node IDs
- let diagnostics carry stable node IDs and resolve to:
  - file path
  - line/column range
  - optional snippet
  - optional function/symbol context

## Suggested implementation plan

1. Frontend always sets `CompileOptions.source_name`.
2. Frontend emits accurate byte-span ranges in AST nodes.
3. Add serialized `AstEnvelope` CLI ingest (`--from-ir`) for binary/JSON payloads.
4. Add optional source-map sidecar resolution in Elevate (via `source_map_id`).
5. Upgrade diagnostics to include function/symbol context when available.

## Practical guidance for frontend authors

- Prefer preserving original source offsets during lowering/desugaring.
- If you synthesize nodes, either:
  - derive spans from nearest original tokens, or
  - mark as synthetic and expect `location unavailable` fallback.
- Keep source-path normalization stable (relative project paths are usually better than absolute machine-local paths).

## Current status summary

- Source-aware diagnostic rendering is now centralized in `/Volumes/Dev/code/jagtesh/elevate/src/source_map.rs`.
- `CompileOptions.source_name` is supported and used by compiler/CLI pathways.
- Generic structs/enums/impl blocks are supported in AST + parser + lowering.
- Effect-row surface syntax is available behind `--exp-effect-rows`.
- Full external binary AST ingest and full source-map sidecar resolution are still design/next-step work.
