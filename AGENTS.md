# Elevate Working Notes

## Benchmark Cadence
- For every change touching ownership, lowering, or interop behavior (`/Volumes/Dev/code/jagtesh/elevate/src/passes.rs`, `/Volumes/Dev/code/jagtesh/elevate/src/ownership_planner.rs`, `/Volumes/Dev/code/jagtesh/elevate/src/crate_builder.rs`), run:
  - `cargo test -q`
  - `cargo run -q -- bench --iters 20 --warmup 3 --out /Volumes/Dev/code/jagtesh/elevate/docs/bench-latest.csv`
- Call out regressions when clone/hot-clone counters increase or median/p95 times drift up materially.

## Example Runtime Checks
- When modifying Neon runtime/rendering, also run:
  - `cargo run -q -- build /Volumes/Dev/code/jagtesh/elevate/examples/neon-boardwalk`
  - `cargo run -q -- build /Volumes/Dev/code/jagtesh/elevate/examples/boardgame-kit`
