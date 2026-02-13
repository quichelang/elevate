#!/usr/bin/env python3
"""Fuzzy file finder — Python version.

Usage:
    python fuzzy.py [directory]                  # Interactive mode
    python fuzzy.py --script events.txt [dir]    # Scripted snapshot mode
"""

import os
import sys
import subprocess


# ---------------------------------------------------------------------------
# Directory scanning
# ---------------------------------------------------------------------------

def scan_dir(root):
    """Recursively collect relative file paths under root."""
    files = []
    for dirpath, dirnames, filenames in os.walk(root):
        # Skip hidden directories and common noise
        dirnames[:] = [d for d in dirnames if not d.startswith(".") and d != "node_modules" and d != "target"]
        for name in filenames:
            if name.startswith("."):
                continue
            rel = os.path.relpath(os.path.join(dirpath, name), root)
            files.append(rel)
    files.sort()
    return files


# ---------------------------------------------------------------------------
# Fuzzy matching
# ---------------------------------------------------------------------------

def fuzzy_match(name, query):
    """Case-insensitive substring match."""
    return query.lower() in name.lower()


# ---------------------------------------------------------------------------
# Terminal helpers (raw mode via stty, ANSI escapes)
# ---------------------------------------------------------------------------

def term_init():
    """Enter raw mode and alternate screen."""
    saved = subprocess.check_output(["stty", "-f", "/dev/tty", "-g"]).decode().strip()
    subprocess.run(["stty", "-f", "/dev/tty", "-echo", "-icanon", "min", "1", "time", "0"],
                   check=True)
    sys.stdout.write("\x1b[?1049h\x1b[2J\x1b[H\x1b[?25l")
    sys.stdout.flush()
    return saved


def term_cleanup(saved):
    """Restore terminal state."""
    sys.stdout.write("\x1b[?25h\x1b[?1049l")
    sys.stdout.flush()
    subprocess.run(["stty", "-f", "/dev/tty", saved], check=True)


def term_read_key():
    """Read a single key event from stdin. Returns a string tag."""
    b = os.read(sys.stdin.fileno(), 1)
    if not b:
        return None
    ch = b[0]
    if ch == 0x1b:  # Escape sequence
        seq = os.read(sys.stdin.fileno(), 2)
        if len(seq) == 2 and seq[0] == 0x5b:  # ESC [ X
            if seq[1] == 0x41:
                return "up"
            if seq[1] == 0x42:
                return "down"
            if seq[1] == 0x43:
                return "right"
            if seq[1] == 0x44:
                return "left"
        return "escape"
    if ch == 0x03:  # Ctrl-C
        return "escape"
    if ch == 0x7f or ch == 0x08:  # Backspace
        return "backspace"
    if ch == 0x0d or ch == 0x0a:  # Enter
        return "enter"
    if 0x20 <= ch < 0x7f:
        return "char:" + chr(ch)
    return None


def term_height():
    """Get terminal height."""
    try:
        rows = os.get_terminal_size().lines
        return rows
    except OSError:
        return 24


def term_draw(frame):
    """Write a full frame to stdout."""
    sys.stdout.write(frame)
    sys.stdout.flush()


# ---------------------------------------------------------------------------
# Rendering
# ---------------------------------------------------------------------------

def render(query, results, cursor, scroll, height):
    """Build an ANSI frame string."""
    lines = []
    # Header: query line
    lines.append("\x1b[2J\x1b[H")  # Clear + home
    lines.append(f"\x1b[1m> {query}\x1b[0m\x1b[K\n")
    lines.append(f"  {len(results)} matches\x1b[K\n")
    lines.append("\x1b[K\n")

    # File list
    visible_count = height - 4
    for i in range(visible_count):
        idx = scroll + i
        if idx < len(results):
            name = results[idx]
            if idx == cursor:
                lines.append(f"\x1b[7m  {name}\x1b[0m\x1b[K\n")
            else:
                lines.append(f"  {name}\x1b[K\n")
        else:
            lines.append("\x1b[K\n")

    return "".join(lines)


# ---------------------------------------------------------------------------
# Snapshot (shared variable format)
# ---------------------------------------------------------------------------

def snapshot(event, query, cursor, matches, scroll, results, height):
    """Emit one snapshot line for scripted comparison."""
    visible_count = height - 4
    visible = results[scroll:scroll + visible_count]
    selected = results[cursor] if cursor < len(results) else ""
    visible_str = ",".join(visible[:5])  # Cap at 5 for readability
    return (
        f"EVENT={event} | query={query} | cursor={cursor} | "
        f"matches={matches} | scroll={scroll} | selected={selected}"
    )


# ---------------------------------------------------------------------------
# Event processing (shared logic)
# ---------------------------------------------------------------------------

def process_event(event, query, cursor, scroll, files, height):
    """Process one event and return updated state."""
    if event.startswith("char:"):
        query = query + event[5:]
    elif event == "backspace":
        query = query[:-1] if len(query) > 0 else query
    elif event == "type:" and len(event) > 5:
        query = query + event[5:]

    # Re-filter
    results = [f for f in files if fuzzy_match(f, query)]
    matches = len(results)

    if event == "up":
        cursor = cursor - 1
    elif event == "down":
        cursor = cursor + 1

    # Clamp cursor
    if cursor < 0:
        cursor = 0
    if cursor >= matches:
        cursor = matches - 1 if matches > 0 else 0

    # Scroll
    visible_count = height - 4
    if cursor < scroll:
        scroll = cursor
    if cursor >= scroll + visible_count:
        scroll = cursor - visible_count + 1
    if scroll < 0:
        scroll = 0

    return query, cursor, scroll, results, matches


# ---------------------------------------------------------------------------
# Script mode
# ---------------------------------------------------------------------------

def run_script(events_path, root):
    """Run scripted events and emit snapshots."""
    files = scan_dir(root)
    height = 24  # Fixed height for reproducibility

    query = ""
    cursor = 0
    scroll = 0
    results = list(files)
    matches = len(results)

    with open(events_path) as f:
        for line in f:
            event = line.strip()
            if not event:
                continue

            # Normalize "type:x" events
            if event.startswith("type:"):
                char = event[5:]
                query, cursor, scroll, results, matches = process_event(
                    "char:" + char, query, cursor, scroll, files, height
                )
                event_label = "type:" + char
            else:
                query, cursor, scroll, results, matches = process_event(
                    event, query, cursor, scroll, files, height
                )
                event_label = event

            if event == "enter":
                selected = results[cursor] if cursor < len(results) else ""
                print(snapshot(event_label, query, cursor, matches, scroll, results, height))
                return
            if event == "escape":
                print(snapshot(event_label, query, cursor, matches, scroll, results, height))
                return

            print(snapshot(event_label, query, cursor, matches, scroll, results, height))


# ---------------------------------------------------------------------------
# Interactive mode
# ---------------------------------------------------------------------------

def run_interactive(root):
    """Run interactive fuzzy finder."""
    files = scan_dir(root)
    height = term_height()

    query = ""
    cursor = 0
    scroll = 0
    results = list(files)
    matches = len(results)

    saved = term_init()
    try:
        term_draw(render(query, results, cursor, scroll, height))

        while True:
            key = term_read_key()
            if key is None:
                continue

            if key == "escape":
                break

            if key == "enter":
                if cursor < len(results):
                    selected = results[cursor]
                    term_cleanup(saved)
                    print(selected)
                    return
                break

            query, cursor, scroll, results, matches = process_event(
                key, query, cursor, scroll, files, height
            )
            term_draw(render(query, results, cursor, scroll, height))
    finally:
        try:
            term_cleanup(saved)
        except Exception:
            pass


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    args = sys.argv[1:]

    if "--script" in args:
        idx = args.index("--script")
        events_path = args[idx + 1]
        root = args[idx + 2] if idx + 2 < len(args) else "."
        run_script(events_path, root)
    else:
        root = args[0] if args else "."
        run_interactive(root)


if __name__ == "__main__":
    main()
