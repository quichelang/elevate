use std::collections::HashMap;
use std::io::{self, Read, Write};
use std::process::Command;
use std::sync::mpsc::{self, Receiver, RecvTimeoutError, Sender, TryRecvError};
use std::sync::{Mutex, OnceLock};
use std::thread;
use std::thread::JoinHandle;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use font8x8::{BASIC_FONTS, UnicodeFonts};
use minifb::{Key, KeyRepeat, Window, WindowOptions};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KeyEvent {
    Up,
    Down,
    Left,
    Right,
    Check,
    Next,
    Reset,
    ToggleRenderer,
    ToggleDebug,
    Hint,
    Save,
    Load,
    Clear,
    Digit(i64),
    Quit,
}

#[derive(Debug, Clone, Default)]
struct RenderPacket {
    cells: Vec<i64>,
    fixed: Vec<bool>,
    cursor_row: i64,
    cursor_col: i64,
    message: String,
    vga: bool,
    debug_enabled: bool,
    fps: i64,
}

#[derive(Debug)]
struct WindowState {
    key_rx: Receiver<KeyEvent>,
    render_tx: Sender<RenderPacket>,
    shutdown_tx: Sender<()>,
    thread_handle: JoinHandle<()>,
}

#[derive(Debug)]
struct TerminalState {
    _raw: RawModeGuard,
    rx: Receiver<KeyEvent>,
    _reader_handle: JoinHandle<()>,
}

#[derive(Debug)]
enum RuntimeBackend {
    Window(WindowState),
    Terminal(TerminalState),
}

#[derive(Debug, Default)]
struct RuntimeRegistry {
    next_id: i64,
    backends: HashMap<i64, RuntimeBackend>,
}

static RUNTIMES: OnceLock<Mutex<RuntimeRegistry>> = OnceLock::new();

pub fn runtime_start() -> i64 {
    let backend = start_window_runtime()
        .map(RuntimeBackend::Window)
        .unwrap_or_else(|| RuntimeBackend::Terminal(start_terminal_runtime()));

    let mut registry = runtimes().lock().expect("runtime registry mutex poisoned");
    registry.next_id += 1;
    let id = registry.next_id;
    registry.backends.insert(id, backend);
    id
}

pub fn runtime_poll_key(handle: i64) -> Option<KeyEvent> {
    let mut registry = runtimes().lock().expect("runtime registry mutex poisoned");
    let backend = registry.backends.get_mut(&handle)?;
    match backend {
        RuntimeBackend::Window(state) => poll_key_channel(&mut state.key_rx),
        RuntimeBackend::Terminal(state) => poll_key_channel(&mut state.rx),
    }
}

pub fn runtime_shutdown(handle: i64) {
    let backend = {
        let mut registry = runtimes().lock().expect("runtime registry mutex poisoned");
        registry.backends.remove(&handle)
    };

    if let Some(backend) = backend {
        match backend {
            RuntimeBackend::Window(state) => {
                let _ = state.shutdown_tx.send(());
                let _ = state.thread_handle.join();
            }
            RuntimeBackend::Terminal(_) => {
                let mut out = io::stdout();
                let _ = write!(out, "\x1b[0m\x1b[?25h\x1b[?1049l");
                let _ = out.flush();
            }
        }
    }
}

pub fn runtime_draw(frame: String) {
    let mut out = io::stdout();
    let _ = out.write_all(frame.as_bytes());
    let _ = out.flush();
}

pub fn runtime_draw_scene(
    cells: &[i64],
    fixed: &[bool],
    cursor_row: i64,
    cursor_col: i64,
    message: String,
    vga: bool,
    debug_enabled: bool,
    fps: i64,
) -> bool {
    let mut rendered = false;
    let mut packet: Option<RenderPacket> = None;
    let mut registry = runtimes().lock().expect("runtime registry mutex poisoned");
    for backend in registry.backends.values_mut() {
        if let RuntimeBackend::Window(state) = backend {
            if packet.is_none() {
                packet = Some(RenderPacket {
                    cells: cells.to_vec(),
                    fixed: fixed.to_vec(),
                    cursor_row,
                    cursor_col,
                    message: message.clone(),
                    vga,
                    debug_enabled,
                    fps,
                });
            }
            if state
                .render_tx
                .send(packet.as_ref().expect("packet initialized").clone())
                .is_ok()
            {
                rendered = true;
            }
        }
    }
    rendered
}

pub fn runtime_now_millis() -> i64 {
    let Ok(elapsed) = SystemTime::now().duration_since(UNIX_EPOCH) else {
        return 0;
    };
    elapsed.as_millis() as i64
}

fn start_window_runtime() -> Option<WindowState> {
    let (key_tx, key_rx) = mpsc::channel::<KeyEvent>();
    let (render_tx, render_rx) = mpsc::channel::<RenderPacket>();
    let (shutdown_tx, shutdown_rx) = mpsc::channel::<()>();
    let (init_tx, init_rx) = mpsc::channel::<bool>();

    let handle = thread::spawn(move || {
        run_window_loop(key_tx, render_rx, shutdown_rx, init_tx);
    });

    match init_rx.recv_timeout(Duration::from_secs(2)) {
        Ok(true) => Some(WindowState {
            key_rx,
            render_tx,
            shutdown_tx,
            thread_handle: handle,
        }),
        _ => {
            let _ = shutdown_tx.send(());
            let _ = handle.join();
            None
        }
    }
}

fn run_window_loop(
    key_tx: Sender<KeyEvent>,
    render_rx: Receiver<RenderPacket>,
    shutdown_rx: Receiver<()>,
    init_tx: Sender<bool>,
) {
    const WIDTH: usize = 1280;
    const HEIGHT: usize = 840;

    let mut window = match Window::new(
        "Neon Boardwalk - Arcade Renderer",
        WIDTH,
        HEIGHT,
        WindowOptions {
            resize: false,
            ..WindowOptions::default()
        },
    ) {
        Ok(window) => window,
        Err(_) => {
            let _ = init_tx.send(false);
            return;
        }
    };

    window.set_target_fps(120);
    let _ = init_tx.send(true);

    let mut frame = RenderPacket::default();
    let mut buffer = vec![0u32; WIDTH * HEIGHT];

    while window.is_open() {
        if shutdown_rx.try_recv().is_ok() {
            break;
        }

        while let Ok(next) = render_rx.try_recv() {
            frame = next;
        }

        pump_window_keys(&window, &key_tx);
        draw_scene(&mut buffer, WIDTH as i32, HEIGHT as i32, &frame);

        if window.update_with_buffer(&buffer, WIDTH, HEIGHT).is_err() {
            break;
        }
    }

    let _ = key_tx.send(KeyEvent::Quit);
}

fn pump_window_keys(window: &Window, key_tx: &Sender<KeyEvent>) {
    for key in window.get_keys_pressed(KeyRepeat::Yes) {
        let mapped = match key {
            Key::Up | Key::W => Some(KeyEvent::Up),
            Key::Down | Key::S => Some(KeyEvent::Down),
            Key::Left | Key::A => Some(KeyEvent::Left),
            Key::Right | Key::D => Some(KeyEvent::Right),
            Key::C => Some(KeyEvent::Check),
            Key::N => Some(KeyEvent::Next),
            Key::R => Some(KeyEvent::Reset),
            Key::V => Some(KeyEvent::ToggleRenderer),
            Key::G => Some(KeyEvent::ToggleDebug),
            Key::H => Some(KeyEvent::Hint),
            Key::P => Some(KeyEvent::Save),
            Key::O => Some(KeyEvent::Load),
            Key::Space | Key::Backspace | Key::Delete | Key::Key0 => Some(KeyEvent::Clear),
            Key::Key1 => Some(KeyEvent::Digit(1)),
            Key::Key2 => Some(KeyEvent::Digit(2)),
            Key::Key3 => Some(KeyEvent::Digit(3)),
            Key::Key4 => Some(KeyEvent::Digit(4)),
            Key::Key5 => Some(KeyEvent::Digit(5)),
            Key::Key6 => Some(KeyEvent::Digit(6)),
            Key::Key7 => Some(KeyEvent::Digit(7)),
            Key::Key8 => Some(KeyEvent::Digit(8)),
            Key::Key9 => Some(KeyEvent::Digit(9)),
            Key::Escape | Key::Q => Some(KeyEvent::Quit),
            _ => None,
        };
        if let Some(event) = mapped {
            let _ = key_tx.send(event);
        }
    }
}

fn draw_scene(buffer: &mut [u32], width: i32, height: i32, frame: &RenderPacket) {
    draw_vertical_gradient(buffer, width, height, 0, 0, width, height / 2, rgb(36, 102, 183), rgb(129, 194, 255));
    draw_vertical_gradient(buffer, width, height, 0, height / 2, width, height / 2, rgb(74, 57, 36), rgb(112, 86, 48));

    draw_hills(buffer, width, height);
    draw_floor_tiles(buffer, width, height);

    let board_x = 150;
    let board_y = 130;
    let cell = 52;
    let board_size = cell * 9;

    draw_shadow_panel(buffer, width, height, board_x - 34, board_y - 34, board_size + 68, board_size + 68);
    draw_wood_panel(buffer, width, height, board_x - 28, board_y - 28, board_size + 56, board_size + 56);
    draw_parchment_grid(
        buffer,
        width,
        height,
        board_x,
        board_y,
        cell,
        &frame.cells,
        &frame.fixed,
        frame.cursor_row,
        frame.cursor_col,
        frame.vga,
    );

    draw_hud_panel(buffer, width, height, frame);

    draw_label(buffer, width, height, 70, 52, 3, rgb(244, 237, 205), "PLAYER 1");
    draw_label(buffer, width, height, 315, 52, 3, rgb(244, 237, 205), "CPU");
    draw_label(buffer, width, height, 90, 86, 4, rgb(255, 248, 208), "34");
    draw_label(buffer, width, height, 332, 86, 4, rgb(255, 248, 208), "28");

    draw_bottom_bar(buffer, width, height, frame);

    if frame.debug_enabled {
        let fps_text = format!("FPS {:>3}", frame.fps);
        draw_label(buffer, width, height, width - 220, 20, 2, rgb(145, 255, 162), &fps_text);
    }
}

fn draw_parchment_grid(
    buffer: &mut [u32],
    width: i32,
    height: i32,
    board_x: i32,
    board_y: i32,
    cell: i32,
    cells: &[i64],
    fixed: &[bool],
    cursor_row: i64,
    cursor_col: i64,
    vga: bool,
) {
    let parchment = if vga { rgb(231, 201, 142) } else { rgb(233, 206, 150) };
    let parchment_dark = if vga { rgb(210, 174, 114) } else { rgb(212, 178, 120) };

    fill_rect(buffer, width, height, board_x, board_y, cell * 9, cell * 9, parchment);

    for row in 0..9 {
        for col in 0..9 {
            if ((row + col) & 1) == 0 {
                fill_rect(
                    buffer,
                    width,
                    height,
                    board_x + col * cell,
                    board_y + row * cell,
                    cell,
                    cell,
                    parchment_dark,
                );
            }
        }
    }

    for row in 0..9 {
        for col in 0..9 {
            let is_cursor = row as i64 == cursor_row && col as i64 == cursor_col;
            if is_cursor {
                draw_rect_border(
                    buffer,
                    width,
                    height,
                    board_x + col * cell,
                    board_y + row * cell,
                    cell,
                    cell,
                    4,
                    rgb(255, 214, 73),
                );
            }

            let idx = row * 9 + col;
            let value = cells.get(idx as usize).copied().unwrap_or(0);
            if value > 0 {
                let is_fixed = fixed.get(idx as usize).copied().unwrap_or(false);
                let color = if is_fixed { rgb(17, 48, 126) } else { rgb(37, 82, 170) };
                let text = value.to_string();
                let tx = board_x + col * cell + (cell / 2) - 10;
                let ty = board_y + row * cell + (cell / 2) - 16;
                draw_label(buffer, width, height, tx, ty, 4, color, &text);
            }
        }
    }

    for i in 0..=9 {
        let thick = if i % 3 == 0 { 5 } else { 2 };
        let color = if i % 3 == 0 { rgb(108, 64, 34) } else { rgb(135, 95, 58) };
        let x = board_x + i * cell;
        fill_rect(
            buffer,
            width,
            height,
            x - (thick / 2),
            board_y,
            thick,
            cell * 9,
            color,
        );
        let y = board_y + i * cell;
        fill_rect(
            buffer,
            width,
            height,
            board_x,
            y - (thick / 2),
            cell * 9,
            thick,
            color,
        );
    }
}

fn draw_hud_panel(buffer: &mut [u32], width: i32, height: i32, frame: &RenderPacket) {
    let panel_x = 700;
    let panel_y = 150;
    let panel_w = 420;
    let panel_h = 500;

    draw_shadow_panel(buffer, width, height, panel_x + 8, panel_y + 8, panel_w, panel_h);
    draw_wood_panel(buffer, width, height, panel_x, panel_y, panel_w, panel_h);

    draw_bevel_box(buffer, width, height, panel_x + 26, panel_y + 26, 368, 95, rgb(57, 33, 17), rgb(80, 48, 24), rgb(43, 22, 11));
    draw_label(buffer, width, height, panel_x + 48, panel_y + 42, 3, rgb(255, 245, 180), "TIME:");
    draw_label(buffer, width, height, panel_x + 52, panel_y + 80, 4, rgb(255, 248, 212), "02:15");

    draw_bevel_box(buffer, width, height, panel_x + 26, panel_y + 138, 368, 95, rgb(57, 33, 17), rgb(80, 48, 24), rgb(43, 22, 11));
    draw_label(buffer, width, height, panel_x + 48, panel_y + 154, 3, rgb(255, 245, 180), "LEVEL:");
    draw_label(buffer, width, height, panel_x + 48, panel_y + 192, 4, rgb(255, 248, 212), "MEDIUM");

    let button_w = 104;
    let button_h = 74;
    let keypad_x = panel_x + 26;
    let keypad_y = panel_y + 252;
    for row in 0..3 {
        for col in 0..3 {
            let bx = keypad_x + col * (button_w + 9);
            let by = keypad_y + row * (button_h + 10);
            draw_bevel_box(buffer, width, height, bx, by, button_w, button_h, rgb(62, 41, 24), rgb(90, 63, 38), rgb(45, 27, 15));
            let number = (row * 3 + col + 1).to_string();
            draw_label(buffer, width, height, bx + 34, by + 22, 4, rgb(253, 236, 145), &number);
        }
    }

    let erase_color = rgb(188, 48, 40);
    let hint_color = rgb(37, 150, 63);
    draw_bevel_box(buffer, width, height, panel_x + 26, panel_y + 486, 180, 60, rgb(255, 140, 140), erase_color, rgb(118, 21, 18));
    draw_bevel_box(buffer, width, height, panel_x + 214, panel_y + 486, 180, 60, rgb(158, 255, 170), hint_color, rgb(16, 90, 35));
    draw_label(buffer, width, height, panel_x + 66, panel_y + 506, 3, rgb(255, 248, 233), "ERASE");
    draw_label(buffer, width, height, panel_x + 266, panel_y + 506, 3, rgb(240, 255, 240), "HINT");

    let mode_text = if frame.vga { "MODE: VGA" } else { "MODE: NEON" };
    draw_label(buffer, width, height, panel_x + 30, panel_y + 570, 2, rgb(255, 236, 170), mode_text);
}

fn draw_bottom_bar(buffer: &mut [u32], width: i32, height: i32, frame: &RenderPacket) {
    draw_bevel_box(buffer, width, height, 42, height - 108, width - 84, 74, rgb(89, 64, 38), rgb(66, 44, 23), rgb(46, 30, 17));
    draw_label(buffer, width, height, 70, height - 84, 2, rgb(233, 235, 220), "Status:");
    draw_label(
        buffer,
        width,
        height,
        184,
        height - 84,
        2,
        rgb(247, 247, 230),
        &frame.message,
    );
}

fn draw_hills(buffer: &mut [u32], width: i32, height: i32) {
    let horizon = (height as f32 * 0.45) as i32;
    for x in 0..width {
        let xf = x as f32;
        let y = horizon as f32
            + (xf / 70.0).sin() * 28.0
            + (xf / 39.0).sin() * 16.0
            + (xf / 23.0).cos() * 8.0;
        let start = y as i32;
        for yy in start..(horizon + 90) {
            let shade = ((yy - start).max(0) * 2).min(80) as u8;
            let color = rgb(54, 136u8.saturating_add(shade / 3), 66);
            put_pixel(buffer, width, height, x, yy, color);
        }
    }
}

fn draw_floor_tiles(buffer: &mut [u32], width: i32, height: i32) {
    let floor_y = (height as f32 * 0.72) as i32;
    for y in floor_y..height {
        for x in 0..width {
            let tile_x = x / 56;
            let tile_y = (y - floor_y) / 32;
            let mut color = match (tile_x + tile_y) % 4 {
                0 => rgb(143, 103, 71),
                1 => rgb(156, 121, 87),
                2 => rgb(131, 95, 63),
                _ => rgb(118, 86, 58),
            };
            let mortar = x % 56 < 2 || (y - floor_y) % 32 < 2;
            if mortar {
                color = rgb(78, 60, 44);
            }
            put_pixel(buffer, width, height, x, y, color);
        }
    }
}

fn draw_shadow_panel(buffer: &mut [u32], width: i32, height: i32, x: i32, y: i32, w: i32, h: i32) {
    fill_rect(buffer, width, height, x + 8, y + 8, w, h, rgba_dim(rgb(23, 18, 12), 0.4));
}

fn draw_wood_panel(buffer: &mut [u32], width: i32, height: i32, x: i32, y: i32, w: i32, h: i32) {
    fill_rect(buffer, width, height, x, y, w, h, rgb(91, 62, 35));
    fill_rect(buffer, width, height, x + 8, y + 8, w - 16, h - 16, rgb(126, 87, 51));
    for yy in (y + 8)..(y + h - 8) {
        for xx in (x + 8)..(x + w - 8) {
            let stripe = ((xx * 13 + yy * 7) % 19) as i32 - 9;
            let base = if (yy / 9) % 2 == 0 { 0 } else { -6 };
            let tone = (base + stripe / 5) as i16;
            put_pixel(buffer, width, height, xx, yy, tint(rgb(126, 87, 51), tone));
        }
    }
    draw_rect_border(buffer, width, height, x, y, w, h, 5, rgb(62, 40, 21));
}

fn draw_bevel_box(
    buffer: &mut [u32],
    width: i32,
    height: i32,
    x: i32,
    y: i32,
    w: i32,
    h: i32,
    light: u32,
    fill: u32,
    dark: u32,
) {
    fill_rect(buffer, width, height, x, y, w, h, fill);
    fill_rect(buffer, width, height, x, y, w, 3, light);
    fill_rect(buffer, width, height, x, y, 3, h, light);
    fill_rect(buffer, width, height, x + w - 3, y, 3, h, dark);
    fill_rect(buffer, width, height, x, y + h - 3, w, 3, dark);
}

fn draw_vertical_gradient(
    buffer: &mut [u32],
    width: i32,
    height: i32,
    x: i32,
    y: i32,
    w: i32,
    h: i32,
    top: u32,
    bottom: u32,
) {
    let (tr, tg, tb) = unpack(top);
    let (br, bg, bb) = unpack(bottom);
    let denom = (h - 1).max(1) as f32;
    for row in 0..h {
        let t = row as f32 / denom;
        let r = (tr as f32 + (br as f32 - tr as f32) * t) as u8;
        let g = (tg as f32 + (bg as f32 - tg as f32) * t) as u8;
        let b = (tb as f32 + (bb as f32 - tb as f32) * t) as u8;
        fill_rect(buffer, width, height, x, y + row, w, 1, rgb(r, g, b));
    }
}

fn draw_label(buffer: &mut [u32], width: i32, height: i32, x: i32, y: i32, scale: i32, color: u32, text: &str) {
    let mut cx = x;
    for ch in text.chars() {
        if let Some(glyph) = BASIC_FONTS.get(ch) {
            for (row, bits) in glyph.iter().enumerate() {
                for col in 0..8 {
                    if (bits >> col) & 1 == 1 {
                        fill_rect(
                            buffer,
                            width,
                            height,
                            cx + col as i32 * scale,
                            y + row as i32 * scale,
                            scale,
                            scale,
                            color,
                        );
                    }
                }
            }
        }
        cx += 8 * scale;
    }
}

fn fill_rect(buffer: &mut [u32], width: i32, height: i32, x: i32, y: i32, w: i32, h: i32, color: u32) {
    let x0 = x.max(0);
    let y0 = y.max(0);
    let x1 = (x + w).min(width);
    let y1 = (y + h).min(height);
    if x0 >= x1 || y0 >= y1 {
        return;
    }
    for yy in y0..y1 {
        let row = yy as usize * width as usize;
        for xx in x0..x1 {
            buffer[row + xx as usize] = color;
        }
    }
}

fn draw_rect_border(
    buffer: &mut [u32],
    width: i32,
    height: i32,
    x: i32,
    y: i32,
    w: i32,
    h: i32,
    thick: i32,
    color: u32,
) {
    fill_rect(buffer, width, height, x, y, w, thick, color);
    fill_rect(buffer, width, height, x, y + h - thick, w, thick, color);
    fill_rect(buffer, width, height, x, y, thick, h, color);
    fill_rect(buffer, width, height, x + w - thick, y, thick, h, color);
}

fn put_pixel(buffer: &mut [u32], width: i32, height: i32, x: i32, y: i32, color: u32) {
    if x < 0 || y < 0 || x >= width || y >= height {
        return;
    }
    buffer[y as usize * width as usize + x as usize] = color;
}

fn poll_key_channel(rx: &mut Receiver<KeyEvent>) -> Option<KeyEvent> {
    let first = match rx.recv_timeout(Duration::from_millis(16)) {
        Ok(key) => key,
        Err(RecvTimeoutError::Timeout) => return None,
        Err(RecvTimeoutError::Disconnected) => return Some(KeyEvent::Quit),
    };

    let mut latest = first;
    loop {
        match rx.try_recv() {
            Ok(key) => latest = key,
            Err(TryRecvError::Empty) => return Some(latest),
            Err(TryRecvError::Disconnected) => return Some(KeyEvent::Quit),
        }
    }
}

fn start_terminal_runtime() -> TerminalState {
    let mut out = io::stdout();
    let _ = write!(out, "\x1b[?1049h\x1b[2J\x1b[H\x1b[?25l");
    let _ = out.flush();

    let raw = RawModeGuard::new().expect("failed to enter raw mode");
    let (tx, rx) = mpsc::channel::<KeyEvent>();

    let handle = thread::spawn(move || {
        let stdin = io::stdin();
        let mut lock = stdin.lock();
        let mut byte = [0u8; 1];

        loop {
            match lock.read(&mut byte) {
                Ok(0) => continue,
                Ok(_) => {
                    if let Some(key) = decode_key(byte[0], &mut lock)
                        && tx.send(key).is_err()
                    {
                        break;
                    }
                }
                Err(err) if err.kind() == io::ErrorKind::Interrupted => continue,
                Err(_) => break,
            }
        }
    });

    TerminalState {
        _raw: raw,
        rx,
        _reader_handle: handle,
    }
}

fn decode_key(first: u8, lock: &mut dyn Read) -> Option<KeyEvent> {
    if first == 0x03 {
        return Some(KeyEvent::Quit);
    }
    if first == 0x7f {
        return Some(KeyEvent::Clear);
    }
    if first == 0x1b {
        let mut second = [0u8; 1];
        let mut third = [0u8; 1];

        if lock.read(&mut second).ok()? == 1
            && second[0] == b'['
            && lock.read(&mut third).ok()? == 1
        {
            return match third[0] {
                b'A' => Some(KeyEvent::Up),
                b'B' => Some(KeyEvent::Down),
                b'C' => Some(KeyEvent::Right),
                b'D' => Some(KeyEvent::Left),
                _ => Some(KeyEvent::Quit),
            };
        }
        return Some(KeyEvent::Quit);
    }

    if first.is_ascii_digit() {
        if first == b'0' {
            return Some(KeyEvent::Clear);
        }
        return Some(KeyEvent::Digit(i64::from(first - b'0')));
    }

    match first as char {
        'w' | 'k' => Some(KeyEvent::Up),
        's' | 'j' => Some(KeyEvent::Down),
        'a' | 'h' => Some(KeyEvent::Left),
        'd' | 'l' => Some(KeyEvent::Right),
        ' ' => Some(KeyEvent::Clear),
        'c' => Some(KeyEvent::Check),
        'n' => Some(KeyEvent::Next),
        'r' => Some(KeyEvent::Reset),
        'v' => Some(KeyEvent::ToggleRenderer),
        'g' => Some(KeyEvent::ToggleDebug),
        'q' => Some(KeyEvent::Quit),
        'H' => Some(KeyEvent::Hint),
        'p' => Some(KeyEvent::Save),
        'o' => Some(KeyEvent::Load),
        _ => None,
    }
}

#[derive(Debug)]
struct RawModeGuard {
    original_state: String,
}

impl RawModeGuard {
    fn new() -> io::Result<Self> {
        let original = run_stty(&["-f", "/dev/tty", "-g"])?;
        run_stty(&["-f", "/dev/tty", "-echo", "-icanon", "min", "0", "time", "1"])?;
        Ok(Self {
            original_state: original.trim().to_string(),
        })
    }
}

impl Drop for RawModeGuard {
    fn drop(&mut self) {
        let _ = run_stty(&["-f", "/dev/tty", &self.original_state]);
    }
}

fn run_stty(args: &[&str]) -> io::Result<String> {
    let output = Command::new("stty").args(args).output()?;
    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    } else {
        Err(io::Error::other(format!(
            "stty failed: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        )))
    }
}

fn runtimes() -> &'static Mutex<RuntimeRegistry> {
    RUNTIMES.get_or_init(|| Mutex::new(RuntimeRegistry::default()))
}

fn rgb(r: u8, g: u8, b: u8) -> u32 {
    ((r as u32) << 16) | ((g as u32) << 8) | b as u32
}

fn unpack(color: u32) -> (u8, u8, u8) {
    (
        ((color >> 16) & 0xFF) as u8,
        ((color >> 8) & 0xFF) as u8,
        (color & 0xFF) as u8,
    )
}

fn tint(color: u32, amount: i16) -> u32 {
    let (r, g, b) = unpack(color);
    let rr = (r as i16 + amount).clamp(0, 255) as u8;
    let gg = (g as i16 + amount).clamp(0, 255) as u8;
    let bb = (b as i16 + amount).clamp(0, 255) as u8;
    rgb(rr, gg, bb)
}

fn rgba_dim(color: u32, alpha: f32) -> u32 {
    let (r, g, b) = unpack(color);
    rgb(
        (r as f32 * alpha) as u8,
        (g as f32 * alpha) as u8,
        (b as f32 * alpha) as u8,
    )
}
