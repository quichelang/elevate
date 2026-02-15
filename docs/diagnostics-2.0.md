# Diagnostics 2.0 (Unified Translation + ICE Boundary)

This document defines the Diagnostics 2.0 architecture and maps directly to the implementation in:

- `src/source_map.rs`
- `src/crate_builder.rs`
- `src/backend_diagnostics.rs`
- `src/diagnostics_catalog.rs`

## 1) Source-Mapping Architecture

### Source of truth

- Frontend-authored spans remain the source of truth (`Span { start, end }` semantics).
- Frontend identity remains attached via compile options and envelope metadata (`source_path`, `source_map_id`).
- Diagnostic rendering remains centralized in `src/source_map.rs` for file/line/column rendering.

### Mapping chain (implemented foundation)

1. Frontend (Elevate or external) emits AST spans in source-language coordinates.
2. Elevate type/capability diagnostics preserve those spans and render with source identity.
3. During crate transpilation (`src/crate_builder.rs`), each generated `.rs` file now records a `GeneratedSourceLink`:
   - `generated_path` (in `target/elevate-gen/src/...`)
   - `source_path` (original `.ers` input)
4. Backend (`cargo`/`rustc`) diagnostics are translated via this generated-to-source link table.

### Mapping chain (next phase)

- For precise non-line-preserving transforms, add sidecar span segments:
  - `(generated_line/col range) -> (frontend span)`
- `source_map_id` from `FrontendMeta` can index those sidecars per module/file.
- Resolution order for backend diagnostics:
  1. exact sidecar segment,
  2. generated-file to source-file fallback,
  3. location unavailable.

## 2) Interceptor Pipeline + ICE Logic

## Pipeline boundaries

- Frontend/type/capability phase completes in Elevate passes.
- Rust lowering/codegen emits generated crate.
- `cargo build` runs on generated crate.
- Backend diagnostics are intercepted and translated in `src/backend_diagnostics.rs`.

### Interception behavior

- Raw `rustc` output is suppressed by default.
- New flag: `--verbose-backend-diagnostics` to include raw backend output in the final message.
- Translation maps common rustc error codes to Elevate catalog entries and renders:
  - source-language location,
  - expected vs actual,
  - explanation,
  - direct fix hint,
  - backend detail.

### ICE heuristic

`cargo` failure is classified as ICE (`E9001`) when all error diagnostics that have locations point to generated files that map back to transpiled frontend source.

Interpretation:

- Frontend + Elevate internal checks succeeded.
- Generated Rust then failed rustc checks.
- Failure is therefore treated as backend contract break (lowering/planner/codegen issue), not user blame.

If mapping is missing/ambiguous, fallback to `E9002` (translation gap) and suggest verbose mode/reporting.

## 3) Error Catalog Schema

The catalog is implemented in `src/diagnostics_catalog.rs`.

```rust
pub struct ErrorCatalogEntry {
    pub language: &'static str,
    pub code: ElevateErrorCode,
    pub severity: DiagnosticSeverity,
    pub title: &'static str,
    pub explanation: &'static str,
    pub expected: &'static str,
    pub actual: &'static str,
    pub direct_fix_hint: &'static str,
}
```

This satisfies the diagnostics contract:

- Source Location: resolved by translator/rendering layer
- Expected vs Actual: first-class fields
- Failed decision explanation: `explanation`
- Direct fix hint: `direct_fix_hint`

Language segregation is by catalog namespace (`language` + language-specific code enum / builder).

## 4) Initial Elevate Error Set

### Syntax

- `E1001`: Unexpected token
- `E1002`: Missing declaration

### Type / Capability

- `E2001`: Type mismatch
- `E2002`: Capability mismatch

### Ownership

- `E3001`: Value moved and then reused
- `E3002`: Conflicting borrows (mutable vs shared)
- `E3003`: Multiple mutable borrows
- `E3004`: Mutating read-only value

### ICE / Translation

- `E9001`: Invalid backend lowering (ICE)
- `E9002`: Backend translation gap

## Notes for External Frontends (Quiche)

- Continue emitting accurate AST spans in frontend source space.
- Populate envelope metadata (`language`, `source_path`, `source_map_id`).
- Prefer stable source IDs so sidecar span maps can be resolved consistently.

### Quiche call path (verified)

Quiche currently integrates by calling `elevate::compile_ast_with_options` (from `quiche/src/lib.rs`).

Diagnostics 2.0 now supports this path directly:

- `render_compile_error_for_frontend(error, profile)` maps Elevate compile errors into frontend-facing grouped/optional semantics.
- `translate_backend_build_failure(..., profile, include_raw)` maps backend rustc/cargo failures into structured frontend-facing diagnostics.

### Optional/grouped semantics model

Frontends that simplify semantics (for example, no explicit type surface) can avoid re-implementing diagnostics by supplying a `FrontendDiagnosticProfile`:

- map many Elevate codes into one frontend code (for example `E2001|E2002|E300* -> Q-SEM-001`),
- keep passthrough for unmapped codes (`passthrough_unmapped = true`), or
- hide unmapped backend details (`passthrough_unmapped = false`).

This gives frontends control over language wording while Elevate remains the diagnostics engine.

## CLI Surface

- `--verbose-backend-diagnostics`:
  - Off (default): translated user-language diagnostics only.
  - On: include raw `cargo`/`rustc` output footer for debugging and bug reports.
