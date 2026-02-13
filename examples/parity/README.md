# Parity Examples

These examples test **behavioral parity** between Elevate and Python — specifically
around mutability. Each example is built in both languages using the same architecture,
the same variable names, and the same lifecycle checkpoints.

## Why?

Elevate infers mutability automatically — you never write `mut` or `&mut`. The question
is: does this produce the same "it just works" feel that Python developers expect?

## How

Each example supports two modes:

1. **Interactive** — run it, use the TUI, verify it feels the same.
2. **Script** — feed a sequence of events via `--script events.txt`, and both
   versions emit structured snapshots after each event. Diff the output to
   find any behavioral divergence.

```bash
cd fuzzy_finder
bash compare.sh .   # runs both, diffs snapshots
```

## Snapshot Format

After each scripted event, both versions emit:

```
EVENT=type:h | query=h | cursor=0 | matches=12 | scroll=0 | selected=hello.py
```

Variables are shared across both implementations: `query`, `cursor`, `matches`,
`scroll`, `selected`, `visible`.
