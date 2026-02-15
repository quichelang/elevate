#!/usr/bin/env python3
"""Fuzzy file finder — Python version.

Usage:
    python fuzzy.py [directory]                  # Interactive mode
    python fuzzy.py --script events.txt [dir]    # Scripted snapshot mode
"""

import os
import sys
import subprocess


if os.name == "nt":
    import msvcrt
    import ctypes
    from ctypes import wintypes


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
    if os.name == "nt":
        state = _win_console_state_save_and_enable_vt()
        sys.stdout.write("\x1b[?1049h\x1b[2J\x1b[H\x1b[?25l")
        sys.stdout.flush()
        return state

    saved = _run_stty(["-g"]) or ""
    _run_stty(["-echo", "-icanon", "min", "1", "time", "0"])
    sys.stdout.write("\x1b[?1049h\x1b[2J\x1b[H\x1b[?25l")
    sys.stdout.flush()
    return saved


def term_cleanup(saved):
    """Restore terminal state."""
    sys.stdout.write("\x1b[?25h\x1b[?1049l")
    sys.stdout.flush()
    if os.name == "nt":
        _win_console_state_restore(saved)
        return
    if saved:
        _run_stty([saved])


def term_read_key():
    """Read a single key event from stdin. Returns a string tag."""
    if os.name == "nt":
        ch = msvcrt.getwch()
        if not ch:
            return None
        if ch in ("\x00", "\xe0"):
            ext = msvcrt.getwch()
            if ext == "H":
                return "up"
            if ext == "P":
                return "down"
            if ext == "M":
                return "right"
            if ext == "K":
                return "left"
            return None
        if ch == "\x03":
            return "escape"
        if ch == "\x08":
            return "backspace"
        if ch in ("\r", "\n"):
            return "enter"
        if ch == "\x1b":
            return "escape"
        if " " <= ch <= "~":
            return "char:" + ch
        return None

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


def _run_stty(args):
    for prefix in (["stty", "-f", "/dev/tty"], ["stty"]):
        try:
            output = subprocess.check_output(prefix + args, stderr=subprocess.DEVNULL)
            return output.decode().strip()
        except Exception:
            continue
    return None


def _win_console_state_save_and_enable_vt():
    kernel32 = ctypes.windll.kernel32
    stdin = kernel32.GetStdHandle(-10)   # STD_INPUT_HANDLE
    stdout = kernel32.GetStdHandle(-11)  # STD_OUTPUT_HANDLE

    in_mode = wintypes.DWORD()
    out_mode = wintypes.DWORD()
    kernel32.GetConsoleMode(stdin, ctypes.byref(in_mode))
    kernel32.GetConsoleMode(stdout, ctypes.byref(out_mode))

    ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004
    ENABLE_PROCESSED_INPUT = 0x0001
    ENABLE_LINE_INPUT = 0x0002
    ENABLE_ECHO_INPUT = 0x0004

    new_out_mode = wintypes.DWORD(out_mode.value | ENABLE_VIRTUAL_TERMINAL_PROCESSING)
    kernel32.SetConsoleMode(stdout, new_out_mode)

    new_in_mode = wintypes.DWORD(
        (in_mode.value | ENABLE_PROCESSED_INPUT) & ~(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT)
    )
    kernel32.SetConsoleMode(stdin, new_in_mode)

    return f"{in_mode.value}:{out_mode.value}"


def _win_console_state_restore(saved):
    try:
        in_raw, out_raw = saved.split(":", 1)
        in_mode = int(in_raw)
        out_mode = int(out_raw)
    except Exception:
        return

    kernel32 = ctypes.windll.kernel32
    stdin = kernel32.GetStdHandle(-10)
    stdout = kernel32.GetStdHandle(-11)
    kernel32.SetConsoleMode(stdin, in_mode)
    kernel32.SetConsoleMode(stdout, out_mode)


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
    lines.append("  Enter: select | Esc: quit\x1b[K\n")
    lines.append("\x1b[K\n")

    # File list
    visible_count = height - 5
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
    visible_count = height - 5
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
    visible_count = height - 5
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
