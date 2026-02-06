# Lexopt Case Study

Status: In Progress (Baseline)
Date: 2026-02-06
Target: `examples/lexopt-elevate`
Reference: `https://github.com/blyxxyz/lexopt`

## Goal

Recreate lexopt semantics in Elevate, then compare generated Rust and behavior against upstream lexopt.

## Baseline Scope (This Milestone)

- Create an Elevate-first public surface (`.ers`) for core parser operations.
- Implement a compatibility runtime in Rust for features not yet expressible in Elevate.
- Add behavior tests for a minimal but real slice:
- short options (`-n`)
- long options (`--name`)
- long options with equals value (`--name=value`)
- combined short options (`-abc`)
- error when value is left unconsumed and parsing continues

## Current Deviations From Upstream lexopt

- Values are modeled as `String`, not `OsString`.
- Public parser handle is `i64` (registry-backed), not a struct API yet.
- Input helper `from_raw` splits by whitespace (shell quoting is not modeled yet).
- `values()` currently returns one value per call, not an iterator object.

## Next Milestones

1. Expand syntax parity:
- `--` end-of-options marker
- `-o=value`
- attached short arg parsing behavior (`-ovalue`)

2. API parity pass:
- model `Parser` and `Arg` ergonomics closer to lexopt surface
- add `unexpected()` helper semantics

3. Comparison harness:
- run identical fixture vectors through upstream lexopt and Elevate implementation
- output a behavior diff report under `docs/case-studies/lexopt-report.md`

