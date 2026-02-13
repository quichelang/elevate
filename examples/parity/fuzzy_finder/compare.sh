#!/usr/bin/env bash
# compare.sh — Run both fuzzy finder versions in script mode and diff snapshots.
# Usage: bash compare.sh [directory]

set -euo pipefail

DIR="${1:-.}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EVENTS="$SCRIPT_DIR/events.txt"

echo "=== Parity Test: Fuzzy Finder ==="
echo "Directory: $DIR"
echo "Events:    $EVENTS"
echo ""

# --- Python ---
echo "[1/3] Running Python version..."
PYTHON_OUT=$(python3 "$SCRIPT_DIR/python/fuzzy.py" --script "$EVENTS" "$DIR")

# --- Elevate ---
echo "[2/3] Running Elevate version..."
ELEVATE_BIN="$SCRIPT_DIR/elevate/target/debug/fuzzy-finder"
if [ ! -f "$ELEVATE_BIN" ]; then
    echo "  Building Elevate version first..."
    (cd "$SCRIPT_DIR/elevate" && cargo run --bin build-fuzzy 2>/dev/null)
fi
ELEVATE_OUT=$("$ELEVATE_BIN" --script "$EVENTS" "$DIR")

# --- Compare ---
echo "[3/3] Comparing snapshots..."
echo ""

DIFF=$(diff <(echo "$PYTHON_OUT") <(echo "$ELEVATE_OUT") || true)

if [ -z "$DIFF" ]; then
    echo "✓ Snapshots match — 0 differences"
    echo ""
    echo "Output:"
    echo "$PYTHON_OUT"
else
    echo "✗ Snapshots differ:"
    echo ""
    echo "--- Python"
    echo "+++ Elevate"
    echo "$DIFF"
    exit 1
fi
