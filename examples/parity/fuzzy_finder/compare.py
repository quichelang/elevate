#!/usr/bin/env python3
"""Cross-runtime parity harness for fuzzy_finder.

Builds the Elevate fuzzy finder once, then executes Python and Rust/Elevate
implementations in --script mode across fixture cases and compares snapshots.
"""

from __future__ import annotations

import argparse
import difflib
import json
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


@dataclass
class Case:
    name: str
    description: str
    files: list[str]
    events: list[str]


def load_cases(path: Path) -> list[Case]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    out: list[Case] = []
    for raw in payload.get("cases", []):
        out.append(
            Case(
                name=raw["name"],
                description=raw.get("description", ""),
                files=list(raw.get("files", [])),
                events=list(raw.get("events", [])),
            )
        )
    return out


def repo_root_from(script_dir: Path) -> Path:
    current = script_dir
    while current != current.parent:
        cargo = current / "Cargo.toml"
        if cargo.exists() and 'name = "elevate"' in cargo.read_text(encoding="utf-8"):
            return current
        current = current.parent
    raise RuntimeError("Could not locate elevate repo root from script location")


def run_checked(cmd: list[str], cwd: Path) -> str:
    proc = subprocess.run(
        cmd,
        cwd=str(cwd),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
    )
    if proc.returncode != 0:
        raise RuntimeError(
            f"Command failed ({proc.returncode}): {' '.join(cmd)}\n"
            f"--- stdout ---\n{proc.stdout}\n"
            f"--- stderr ---\n{proc.stderr}"
        )
    return proc.stdout


def ensure_elevate_binary(repo_root: Path, elevate_example_root: Path) -> Path:
    run_checked(
        ["cargo", "run", "-r", "--", "build", str(elevate_example_root)],
        cwd=repo_root,
    )
    name = "fuzzy-finder.exe" if sys.platform.startswith("win") else "fuzzy-finder"
    binary = elevate_example_root / "target" / "debug" / name
    if not binary.exists():
        raise RuntimeError(f"Expected generated binary at {binary}")
    return binary


def write_case_fixture(case: Case, root: Path) -> tuple[Path, Path]:
    corpus = root / "corpus"
    corpus.mkdir(parents=True, exist_ok=True)
    for rel in case.files:
        file_path = corpus / rel
        file_path.parent.mkdir(parents=True, exist_ok=True)
        file_path.write_text(f"fixture:{case.name}:{rel}\n", encoding="utf-8")

    events = root / "events.txt"
    events.write_text("\n".join(case.events) + "\n", encoding="utf-8")
    return corpus, events


def normalize_lines(text: str) -> list[str]:
    return [line.rstrip("\r") for line in text.strip().splitlines() if line.strip()]


def run_case(
    case: Case,
    script_dir: Path,
    repo_root: Path,
    elevate_binary: Path,
    keep_temp: bool,
) -> tuple[bool, list[str], list[str]]:
    tmp_ctx = tempfile.TemporaryDirectory(prefix=f"fuzzy-parity-{case.name}-")
    tmp_root = Path(tmp_ctx.name)

    try:
        corpus_root, events_path = write_case_fixture(case, tmp_root)

        py_cmd = [
            sys.executable,
            str(script_dir / "python" / "fuzzy.py"),
            "--script",
            str(events_path),
            str(corpus_root),
        ]
        py_out = run_checked(py_cmd, cwd=repo_root)

        rust_cmd = [
            str(elevate_binary),
            "--script",
            str(events_path),
            str(corpus_root),
        ]
        rust_out = run_checked(rust_cmd, cwd=repo_root)

        py_lines = normalize_lines(py_out)
        rust_lines = normalize_lines(rust_out)
        matched = py_lines == rust_lines

        if keep_temp:
            preserved = script_dir / ".tmp" / case.name
            if preserved.exists():
                shutil.rmtree(preserved)
            preserved.parent.mkdir(parents=True, exist_ok=True)
            shutil.copytree(tmp_root, preserved)

        return matched, py_lines, rust_lines
    finally:
        tmp_ctx.cleanup()


def filter_cases(cases: Iterable[Case], selected: list[str]) -> list[Case]:
    if not selected:
        return list(cases)
    selected_set = set(selected)
    return [case for case in cases if case.name in selected_set]


def main() -> int:
    parser = argparse.ArgumentParser(description="Run fuzzy finder parity fixtures")
    parser.add_argument(
        "--cases",
        default="fixtures/cases.json",
        help="Path to fixture cases JSON (relative to this script directory)",
    )
    parser.add_argument(
        "--case",
        action="append",
        dest="selected_cases",
        default=[],
        help="Run only specific case name (repeatable)",
    )
    parser.add_argument(
        "--keep-temp",
        action="store_true",
        help="Keep generated fixture temp directories under .tmp/",
    )
    args = parser.parse_args()

    script_dir = Path(__file__).resolve().parent
    repo_root = repo_root_from(script_dir)
    elevate_example_root = script_dir / "elevate"
    cases_path = (script_dir / args.cases).resolve()

    cases = load_cases(cases_path)
    cases = filter_cases(cases, args.selected_cases)
    if not cases:
        print("No matching cases to run.")
        return 2

    print(f"Using fixtures: {cases_path}")
    print(f"Repo root:      {repo_root}")
    print(f"Cases:          {', '.join(case.name for case in cases)}")

    elevate_binary = ensure_elevate_binary(repo_root, elevate_example_root)
    print(f"Elevate binary: {elevate_binary}")

    failures = 0
    for case in cases:
        print(f"\n=== CASE: {case.name} ===")
        if case.description:
            print(case.description)
        matched, py_lines, rust_lines = run_case(
            case,
            script_dir,
            repo_root,
            elevate_binary,
            keep_temp=args.keep_temp,
        )
        if matched:
            print("PASS: outputs match")
            continue

        failures += 1
        print("FAIL: outputs differ")
        diff = difflib.unified_diff(
            py_lines,
            rust_lines,
            fromfile="python",
            tofile="elevate",
            lineterm="",
        )
        for line in diff:
            print(line)

    print("\n=== SUMMARY ===")
    print(f"Total cases: {len(cases)}")
    print(f"Failed:      {failures}")
    print(f"Passed:      {len(cases) - failures}")

    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
