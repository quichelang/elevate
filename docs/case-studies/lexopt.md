# Lexopt Case Study

Status: In Progress (Milestone 5)
Date: 2026-02-06
Target: `examples/lexopt-elevate`
Reference: `https://github.com/blyxxyz/lexopt`

## Goal

Recreate lexopt semantics in Elevate, then compare generated Rust and behavior against upstream lexopt.

## Current Scope (Implemented So Far)

- Create an Elevate-first public surface (`.ers`) for core parser operations.
- Keep a compatibility runtime in Rust only for parser-state storage and low-level host integration.
- Add behavior tests for a minimal but real slice:
- short options (`-n`)
- long options (`--name`)
- long options with equals value (`--name=value`)
- combined short options (`-abc`)
- error when value is left unconsumed and parsing continues

### Refactor Note (Current)

- Parser control flow and decision logic now live in `examples/lexopt-elevate/src/parser.ers`.
- Thin `runtime.ers` wrappers have been removed; parser now calls `runtime_core` directly.
- Rust compatibility core remains in `examples/lexopt-elevate/src/runtime_core.rs` for:
- parser-state registry/storage
- host argument ingestion (`from_env`, `from_raw`)
- one low-level helper (`drop_first_char_known`)
- constructor-style helpers previously in Rust have been migrated into native Elevate via struct literals.
- Parser API now includes a preview host-handle wrapper (`ParserHandle`) while preserving existing `i64` entry points for compatibility.
- Interop gating is now explicit via `examples/lexopt-elevate/elevate.interop`.

## Current Deviations From Upstream lexopt

- Values are modeled as `String`, not `OsString`.
- Public parser handle is `i64` (registry-backed), not a struct API yet.
- Input helper `from_raw` splits by whitespace (shell quoting is not modeled yet).
- `values()` currently returns one value per call, not an iterator object.
- Runtime still depends on Rust-side state management (`runtime_core`) rather than a pure Elevate runtime model.

## Next Milestones

1. API parity pass:
- model `Parser` and `Arg` ergonomics closer to lexopt surface
- add `unexpected()` helper semantics
- replace handle-based API with a parser-typed user-facing object

2. Runtime reduction pass:
- migrate remaining Rust runtime responsibilities where feasible
- keep Rust code only for unavoidable host interop/performance boundaries

3. Comparison harness:
- run identical fixture vectors through upstream lexopt and Elevate implementation
- output a behavior diff report under `docs/case-studies/lexopt-report.md`
