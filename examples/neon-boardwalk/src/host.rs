use std::collections::{HashMap, VecDeque};
use std::cell::RefCell;
use std::io::{self, Read, Write};
use std::process::Command;
use std::sync::mpsc::{self, Receiver, RecvTimeoutError, Sender, TryRecvError};
use std::sync::OnceLock;
use std::thread;
use std::thread::JoinHandle;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use ab_glyph::{point, Font, FontArc, PxScale, ScaleFont};
use font8x8::{BASIC_FONTS, UnicodeFonts};
use minifb::{Key as MinifbKey, KeyRepeat, Window as MinifbWindow, WindowOptions};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;

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

struct SdlMainState {
    _sdl: sdl2::Sdl,
    event_pump: sdl2::EventPump,
    canvas: sdl2::render::Canvas<sdl2::video::Window>,
    texture_creator: sdl2::render::TextureCreator<sdl2::video::WindowContext>,
    buffer: Vec<u32>,
    key_queue: VecDeque<KeyEvent>,
}

#[derive(Clone)]
struct Sprite {
    width: i32,
    height: i32,
    pixels: Vec<Option<u32>>,
}

struct SpritePack {
    player: Sprite,
    cpu: Sprite,
}

#[derive(Debug)]
struct TerminalState {
    _raw: Option<RawModeGuard>,
    rx: Receiver<KeyEvent>,
    _reader_handle: Option<JoinHandle<()>>,
}

enum RuntimeBackend {
    Window(WindowState),
    SdlMain(SdlMainState),
    Terminal(TerminalState),
}

#[derive(Default)]
struct RuntimeRegistry {
    next_id: i64,
    backends: HashMap<i64, RuntimeBackend>,
}

thread_local! {
    static RUNTIMES: RefCell<RuntimeRegistry> = RefCell::new(RuntimeRegistry::default());
}

static SPRITES: OnceLock<SpritePack> = OnceLock::new();
static UI_FONT: OnceLock<Option<FontArc>> = OnceLock::new();

pub fn runtime_start() -> i64 {
    // Window mode is opt-in while runtime/thread-safety work is in progress.
    // If window init fails, we transparently fall back to terminal mode.
    let window_opt_in = std::env::var("ELEVATE_NEON_WINDOW")
        .ok()
        .map(|value| value == "1")
        .unwrap_or(false);
    let enable_window_backend = window_opt_in;
    let backend = if enable_window_backend {
        start_window_runtime()
            .unwrap_or_else(|| RuntimeBackend::Terminal(start_terminal_runtime()))
    } else {
        RuntimeBackend::Terminal(start_terminal_runtime())
    };

    with_registry(|registry| {
        registry.next_id += 1;
        let id = registry.next_id;
        registry.backends.insert(id, backend);
        id
    })
}

pub fn runtime_poll_key(handle: i64) -> Option<KeyEvent> {
    with_registry(|registry| {
        let backend = registry.backends.get_mut(&handle)?;
        match backend {
            RuntimeBackend::Window(state) => poll_key_channel(&mut state.key_rx),
            RuntimeBackend::SdlMain(state) => {
                pump_sdl_events(state);
                let next = state.key_queue.pop_front();
                if next.is_none() {
                    thread::sleep(Duration::from_millis(8));
                }
                next
            }
            RuntimeBackend::Terminal(state) => poll_key_channel(&mut state.rx),
        }
    })
}

pub fn runtime_shutdown(handle: i64) {
    let backend = with_registry(|registry| registry.backends.remove(&handle));

    if let Some(backend) = backend {
        match backend {
            RuntimeBackend::Window(state) => {
                let _ = state.shutdown_tx.send(());
                let _ = state.thread_handle.join();
            }
            RuntimeBackend::SdlMain(_) => {}
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
    with_registry(|registry| {
        let mut rendered = false;
        let mut packet: Option<RenderPacket> = None;
        for backend in registry.backends.values_mut() {
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

            match backend {
                RuntimeBackend::Window(state) => {
                    if state
                        .render_tx
                        .send(packet.as_ref().expect("packet initialized").clone())
                        .is_ok()
                    {
                        rendered = true;
                    }
                }
                RuntimeBackend::SdlMain(state) => {
                    rendered = render_sdl_frame(state, packet.as_ref().expect("packet initialized"))
                        || rendered;
                }
                RuntimeBackend::Terminal(_) => {}
            }
        }
        rendered
    })
}

pub fn runtime_now_millis() -> i64 {
    let Ok(elapsed) = SystemTime::now().duration_since(UNIX_EPOCH) else {
        return 0;
    };
    elapsed.as_millis() as i64
}

fn start_window_runtime() -> Option<RuntimeBackend> {
    let preferred = std::env::var("ELEVATE_NEON_BACKEND")
        .ok()
        .map(|value| value.to_ascii_lowercase())
        .unwrap_or_else(|| "sdl".to_string());

    if preferred == "minifb" {
        start_minifb_window_runtime()
            .map(RuntimeBackend::Window)
            .or_else(|| start_sdl_window_runtime().map(RuntimeBackend::SdlMain))
    } else {
        start_sdl_window_runtime()
            .map(RuntimeBackend::SdlMain)
            .or_else(|| start_minifb_window_runtime().map(RuntimeBackend::Window))
    }
}

fn start_minifb_window_runtime() -> Option<WindowState> {
    let (key_tx, key_rx) = mpsc::channel::<KeyEvent>();
    let (render_tx, render_rx) = mpsc::channel::<RenderPacket>();
    let (shutdown_tx, shutdown_rx) = mpsc::channel::<()>();
    let (init_tx, init_rx) = mpsc::channel::<bool>();

    let handle = thread::spawn(move || {
        run_minifb_window_loop(key_tx, render_rx, shutdown_rx, init_tx);
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

fn start_sdl_window_runtime() -> Option<SdlMainState> {
    const WIDTH: u32 = 1280;
    const HEIGHT: u32 = 840;

    let sdl = sdl2::init().ok()?;
    let video = sdl.video().ok()?;
    let window = video
        .window("Neon Boardwalk - SDL/OpenGL Renderer", WIDTH, HEIGHT)
        .position_centered()
        .allow_highdpi()
        .opengl()
        .build()
        .ok()?;
    let canvas = window.into_canvas().accelerated().present_vsync().build().ok()?;
    let texture_creator = canvas.texture_creator();
    let event_pump = sdl.event_pump().ok()?;

    Some(SdlMainState {
        _sdl: sdl,
        event_pump,
        canvas,
        texture_creator,
        buffer: vec![0u32; WIDTH as usize * HEIGHT as usize],
        key_queue: VecDeque::new(),
    })
}

fn run_minifb_window_loop(
    key_tx: Sender<KeyEvent>,
    render_rx: Receiver<RenderPacket>,
    shutdown_rx: Receiver<()>,
    init_tx: Sender<bool>,
) {
    const WIDTH: usize = 1280;
    const HEIGHT: usize = 840;

    let mut window = match MinifbWindow::new(
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

fn pump_sdl_events(state: &mut SdlMainState) {
    for event in state.event_pump.poll_iter() {
        match event {
            Event::Quit { .. } => state.key_queue.push_back(KeyEvent::Quit),
            Event::KeyDown {
                keycode: Some(keycode),
                ..
            } => {
                if let Some(mapped) = map_sdl_key(keycode) {
                    state.key_queue.push_back(mapped);
                }
            }
            _ => {}
        }
    }
}

fn render_sdl_frame(state: &mut SdlMainState, frame: &RenderPacket) -> bool {
    const WIDTH: u32 = 1280;
    const HEIGHT: u32 = 840;

    pump_sdl_events(state);
    draw_scene(&mut state.buffer, WIDTH as i32, HEIGHT as i32, frame);

    let mut texture = match state
        .texture_creator
        .create_texture_streaming(PixelFormatEnum::RGBA32, WIDTH, HEIGHT)
    {
        Ok(texture) => texture,
        Err(_) => return false,
    };
    if texture
        .with_lock(None, |pixels: &mut [u8], pitch: usize| {
            for y in 0..HEIGHT as usize {
                let src = y * WIDTH as usize;
                let dst = y * pitch;
                for x in 0..WIDTH as usize {
                    let color = state.buffer[src + x];
                    let offset = dst + x * 4;
                    pixels[offset] = ((color >> 16) & 0xFF) as u8;
                    pixels[offset + 1] = ((color >> 8) & 0xFF) as u8;
                    pixels[offset + 2] = (color & 0xFF) as u8;
                    pixels[offset + 3] = 0xFF;
                }
            }
        })
        .is_err()
    {
        return false;
    }

    state.canvas.clear();
    if state.canvas.copy(&texture, None, None).is_err() {
        return false;
    }
    state.canvas.present();
    true
}

fn map_sdl_key(key: Keycode) -> Option<KeyEvent> {
    match key {
        Keycode::Up | Keycode::W => Some(KeyEvent::Up),
        Keycode::Down | Keycode::S => Some(KeyEvent::Down),
        Keycode::Left | Keycode::A => Some(KeyEvent::Left),
        Keycode::Right | Keycode::D => Some(KeyEvent::Right),
        Keycode::C => Some(KeyEvent::Check),
        Keycode::N => Some(KeyEvent::Next),
        Keycode::R => Some(KeyEvent::Reset),
        Keycode::V => Some(KeyEvent::ToggleRenderer),
        Keycode::G => Some(KeyEvent::ToggleDebug),
        Keycode::H => Some(KeyEvent::Hint),
        Keycode::P => Some(KeyEvent::Save),
        Keycode::O => Some(KeyEvent::Load),
        Keycode::Space | Keycode::Backspace | Keycode::Delete | Keycode::Num0 => {
            Some(KeyEvent::Clear)
        }
        Keycode::Num1 | Keycode::Kp1 => Some(KeyEvent::Digit(1)),
        Keycode::Num2 | Keycode::Kp2 => Some(KeyEvent::Digit(2)),
        Keycode::Num3 | Keycode::Kp3 => Some(KeyEvent::Digit(3)),
        Keycode::Num4 | Keycode::Kp4 => Some(KeyEvent::Digit(4)),
        Keycode::Num5 | Keycode::Kp5 => Some(KeyEvent::Digit(5)),
        Keycode::Num6 | Keycode::Kp6 => Some(KeyEvent::Digit(6)),
        Keycode::Num7 | Keycode::Kp7 => Some(KeyEvent::Digit(7)),
        Keycode::Num8 | Keycode::Kp8 => Some(KeyEvent::Digit(8)),
        Keycode::Num9 | Keycode::Kp9 => Some(KeyEvent::Digit(9)),
        Keycode::Escape | Keycode::Q => Some(KeyEvent::Quit),
        _ => None,
    }
}

fn pump_window_keys(window: &MinifbWindow, key_tx: &Sender<KeyEvent>) {
    for key in window.get_keys_pressed(KeyRepeat::Yes) {
        let mapped = match key {
            MinifbKey::Up | MinifbKey::W => Some(KeyEvent::Up),
            MinifbKey::Down | MinifbKey::S => Some(KeyEvent::Down),
            MinifbKey::Left | MinifbKey::A => Some(KeyEvent::Left),
            MinifbKey::Right | MinifbKey::D => Some(KeyEvent::Right),
            MinifbKey::C => Some(KeyEvent::Check),
            MinifbKey::N => Some(KeyEvent::Next),
            MinifbKey::R => Some(KeyEvent::Reset),
            MinifbKey::V => Some(KeyEvent::ToggleRenderer),
            MinifbKey::G => Some(KeyEvent::ToggleDebug),
            MinifbKey::H => Some(KeyEvent::Hint),
            MinifbKey::P => Some(KeyEvent::Save),
            MinifbKey::O => Some(KeyEvent::Load),
            MinifbKey::Space | MinifbKey::Backspace | MinifbKey::Delete | MinifbKey::Key0 => {
                Some(KeyEvent::Clear)
            }
            MinifbKey::Key1 => Some(KeyEvent::Digit(1)),
            MinifbKey::Key2 => Some(KeyEvent::Digit(2)),
            MinifbKey::Key3 => Some(KeyEvent::Digit(3)),
            MinifbKey::Key4 => Some(KeyEvent::Digit(4)),
            MinifbKey::Key5 => Some(KeyEvent::Digit(5)),
            MinifbKey::Key6 => Some(KeyEvent::Digit(6)),
            MinifbKey::Key7 => Some(KeyEvent::Digit(7)),
            MinifbKey::Key8 => Some(KeyEvent::Digit(8)),
            MinifbKey::Key9 => Some(KeyEvent::Digit(9)),
            MinifbKey::Escape | MinifbKey::Q => Some(KeyEvent::Quit),
            _ => None,
        };
        if let Some(event) = mapped {
            let _ = key_tx.send(event);
        }
    }
}

fn draw_scene(buffer: &mut [u32], width: i32, height: i32, frame: &RenderPacket) {
    draw_vertical_gradient(
        buffer,
        width,
        height,
        0,
        0,
        width,
        height / 2,
        rgb(72, 134, 214),
        rgb(187, 225, 255),
    );
    draw_vertical_gradient(
        buffer,
        width,
        height,
        0,
        height / 2,
        width,
        height / 2,
        rgb(99, 74, 44),
        rgb(73, 54, 37),
    );

    draw_sun_glow(buffer, width, height, width - 170, 90, 165);
    draw_cloud_bands(buffer, width, height);
    draw_hills(buffer, width, height);
    draw_floor_tiles(buffer, width, height);

    let board_x = 102;
    let board_y = 168;
    let cell = 56;
    let board_size = cell * 9;

    draw_shadow_panel(
        buffer,
        width,
        height,
        board_x - 44,
        board_y - 24,
        board_size + 84,
        board_size + 62,
    );
    draw_wood_panel(
        buffer,
        width,
        height,
        board_x - 36,
        board_y - 18,
        board_size + 72,
        board_size + 52,
    );
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

    draw_wood_panel(buffer, width, height, 56, 18, 250, 126);
    draw_wood_panel(buffer, width, height, 328, 18, 250, 126);
    draw_portrait_slot(buffer, width, height, 70, 60, true);
    draw_portrait_slot(buffer, width, height, 478, 60, false);
    draw_label_ornate(
        buffer,
        width,
        height,
        84,
        38,
        3,
        rgb(255, 246, 201),
        rgb(54, 30, 16),
        rgb(255, 255, 240),
        "PLAYER 1",
    );
    draw_label_ornate(
        buffer,
        width,
        height,
        368,
        38,
        3,
        rgb(255, 246, 201),
        rgb(54, 30, 16),
        rgb(255, 255, 240),
        "CPU",
    );
    draw_label_ornate(
        buffer,
        width,
        height,
        174,
        84,
        4,
        rgb(255, 251, 227),
        rgb(49, 30, 13),
        rgb(255, 255, 245),
        "34",
    );
    draw_label_ornate(
        buffer,
        width,
        height,
        400,
        84,
        4,
        rgb(255, 251, 227),
        rgb(49, 30, 13),
        rgb(255, 255, 245),
        "28",
    );

    draw_bottom_bar(buffer, width, height, frame);
    apply_retro_post_fx(buffer, width, height, frame.vga);

    if frame.debug_enabled {
        let fps_text = format!("FPS {:>3}", frame.fps);
        draw_bevel_box(
            buffer,
            width,
            height,
            width - 210,
            16,
            170,
            36,
            rgb(145, 171, 102),
            rgb(57, 67, 35),
            rgb(28, 34, 18),
        );
        draw_label_ornate(
            buffer,
            width,
            height,
            width - 188,
            26,
            2,
            rgb(220, 255, 184),
            rgb(17, 24, 11),
            rgb(241, 255, 216),
            &fps_text,
        );
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
    let parchment = if vga { rgb(227, 194, 132) } else { rgb(232, 205, 151) };
    let parchment_dark = if vga { rgb(204, 164, 104) } else { rgb(209, 174, 118) };
    let board_w = cell * 9;
    let board_h = cell * 9;

    fill_rect(buffer, width, height, board_x, board_y, board_w, board_h, parchment);
    fill_rect_grain(
        buffer,
        width,
        height,
        board_x,
        board_y,
        board_w,
        board_h,
        parchment,
        0x9c4d_u32,
        7,
    );

    for row in 0..9 {
        for col in 0..9 {
            if ((row + col) & 1) == 0 {
                fill_rect_grain(
                    buffer,
                    width,
                    height,
                    board_x + col * cell,
                    board_y + row * cell,
                    cell,
                    cell,
                    parchment_dark,
                    0x1839_u32 + (row * 17 + col * 23) as u32,
                    5,
                );
            }
        }
    }

    fill_rect(
        buffer,
        width,
        height,
        board_x + cell * 3,
        board_y + cell * 3,
        cell * 3,
        cell * 3,
        tint(parchment_dark, -12),
    );

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
                    5,
                    rgb(255, 231, 118),
                );
                draw_rect_border(
                    buffer,
                    width,
                    height,
                    board_x + col * cell + 4,
                    board_y + row * cell + 4,
                    cell - 8,
                    cell - 8,
                    2,
                    rgb(255, 214, 73),
                );
            }

            let idx = row * 9 + col;
            let value = cells.get(idx as usize).copied().unwrap_or(0);
            if value > 0 {
                let is_fixed = fixed.get(idx as usize).copied().unwrap_or(false);
                let color = if is_fixed { rgb(17, 48, 126) } else { rgb(37, 82, 170) };
                let text = value.to_string();
                let tx = board_x + col * cell + (cell / 2) - 11;
                let ty = board_y + row * cell + (cell / 2) - 17;
                draw_label(
                    buffer,
                    width,
                    height,
                    tx + 2,
                    ty + 2,
                    4,
                    rgba_dim(rgb(17, 20, 28), 0.45),
                    &text,
                );
                draw_label(buffer, width, height, tx, ty, 4, color, &text);
            }
        }
    }

    for i in 0..=9 {
        let thick = if i % 3 == 0 { 6 } else { 2 };
        let color = if i % 3 == 0 {
            rgb(97, 57, 31)
        } else {
            rgb(132, 91, 57)
        };
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

    draw_rect_border(
        buffer,
        width,
        height,
        board_x - 5,
        board_y - 5,
        board_w + 10,
        board_h + 10,
        5,
        rgb(82, 49, 29),
    );
}

fn draw_hud_panel(buffer: &mut [u32], width: i32, height: i32, frame: &RenderPacket) {
    let panel_x = 700;
    let panel_y = 178;
    let panel_w = 440;
    let panel_h = 452;

    draw_shadow_panel(buffer, width, height, panel_x + 8, panel_y + 8, panel_w, panel_h);
    draw_wood_panel(buffer, width, height, panel_x, panel_y, panel_w, panel_h);

    draw_bevel_box(
        buffer,
        width,
        height,
        panel_x + 28,
        panel_y + 28,
        384,
        102,
        rgb(63, 40, 21),
        rgb(50, 35, 21),
        rgb(28, 18, 11),
    );
    draw_label_ornate(
        buffer,
        width,
        height,
        panel_x + 54,
        panel_y + 46,
        3,
        rgb(255, 246, 191),
        rgb(46, 31, 19),
        rgb(255, 255, 230),
        "TIME:",
    );
    draw_label_ornate(
        buffer,
        width,
        height,
        panel_x + 54,
        panel_y + 86,
        4,
        rgb(255, 250, 224),
        rgb(40, 26, 16),
        rgb(255, 255, 235),
        "02:15",
    );

    draw_bevel_box(
        buffer,
        width,
        height,
        panel_x + 28,
        panel_y + 146,
        384,
        102,
        rgb(63, 40, 21),
        rgb(50, 35, 21),
        rgb(28, 18, 11),
    );
    draw_label_ornate(
        buffer,
        width,
        height,
        panel_x + 54,
        panel_y + 164,
        3,
        rgb(255, 246, 191),
        rgb(46, 31, 19),
        rgb(255, 255, 230),
        "LEVEL:",
    );
    draw_label_ornate(
        buffer,
        width,
        height,
        panel_x + 54,
        panel_y + 204,
        4,
        rgb(255, 250, 224),
        rgb(40, 26, 16),
        rgb(255, 255, 235),
        "MEDIUM",
    );

    let button_w = 86;
    let button_h = 62;
    let keypad_x = panel_x + 54;
    let keypad_y = panel_y + 264;
    for row in 0..3 {
        for col in 0..3 {
            let bx = keypad_x + col * (button_w + 9);
            let by = keypad_y + row * (button_h + 10);
            draw_keypad_button(buffer, width, height, bx, by, button_w, button_h);
            let number = (row * 3 + col + 1).to_string();
            draw_label_ornate(
                buffer,
                width,
                height,
                bx + 27,
                by + 17,
                3,
                rgb(255, 229, 128),
                rgb(24, 14, 8),
                rgb(255, 246, 179),
                &number,
            );
        }
    }

    let erase_color = rgb(188, 48, 40);
    let hint_color = rgb(37, 150, 63);
    draw_bevel_box(
        buffer,
        width,
        height,
        panel_x + 54,
        panel_y + 392,
        144,
        52,
        rgb(255, 158, 143),
        erase_color,
        rgb(110, 18, 18),
    );
    draw_bevel_box(
        buffer,
        width,
        height,
        panel_x + 214,
        panel_y + 392,
        144,
        52,
        rgb(168, 255, 176),
        hint_color,
        rgb(13, 83, 35),
    );
    draw_label_ornate(
        buffer,
        width,
        height,
        panel_x + 85,
        panel_y + 410,
        3,
        rgb(255, 248, 233),
        rgb(79, 20, 17),
        rgb(255, 255, 245),
        "ERASE",
    );
    draw_label_ornate(
        buffer,
        width,
        height,
        panel_x + 249,
        panel_y + 410,
        3,
        rgb(240, 255, 240),
        rgb(10, 64, 28),
        rgb(248, 255, 248),
        "HINT",
    );

    let mode_text = if frame.vga { "MODE: VGA" } else { "MODE: NEON" };
    draw_label(
        buffer,
        width,
        height,
        panel_x + 34,
        panel_y + panel_h - 18,
        2,
        rgb(255, 236, 170),
        mode_text,
    );
}

fn draw_keypad_button(
    buffer: &mut [u32],
    width: i32,
    height: i32,
    x: i32,
    y: i32,
    w: i32,
    h: i32,
) {
    draw_bevel_box(
        buffer,
        width,
        height,
        x,
        y,
        w,
        h,
        rgb(163, 129, 81),
        rgb(104, 70, 41),
        rgb(46, 29, 17),
    );
    draw_bevel_box(
        buffer,
        width,
        height,
        x + 6,
        y + 6,
        w - 12,
        h - 12,
        rgb(52, 34, 20),
        rgb(26, 17, 10),
        rgb(16, 10, 7),
    );
}

fn draw_bottom_bar(buffer: &mut [u32], width: i32, height: i32, frame: &RenderPacket) {
    draw_bevel_box(
        buffer,
        width,
        height,
        34,
        height - 112,
        width - 68,
        78,
        rgb(139, 102, 58),
        rgb(92, 61, 35),
        rgb(43, 28, 18),
    );
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

fn draw_portrait_slot(
    buffer: &mut [u32],
    width: i32,
    height: i32,
    x: i32,
    y: i32,
    is_player: bool,
) {
    let sprite = if is_player {
        &sprites().player
    } else {
        &sprites().cpu
    };
    let slot_w = 74;
    let slot_h = 74;
    let inner_pad = 6;
    let avail_w = slot_w - inner_pad * 2;
    let avail_h = slot_h - inner_pad * 2;
    let mut scale = (avail_w / sprite.width).min(avail_h / sprite.height);
    if scale < 1 {
        scale = 1;
    }
    let sprite_w = sprite.width * scale;
    let sprite_h = sprite.height * scale;
    let sprite_x = x + (slot_w - sprite_w) / 2;
    let sprite_y = y + (slot_h - sprite_h) / 2;

    draw_bevel_box(
        buffer,
        width,
        height,
        x - 6,
        y - 6,
        slot_w + 12,
        slot_h + 12,
        rgb(173, 136, 86),
        rgb(92, 61, 37),
        rgb(45, 27, 15),
    );
    fill_rect(buffer, width, height, x, y, slot_w, slot_h, rgb(59, 35, 24));
    draw_sprite(buffer, width, height, sprite_x, sprite_y, scale, sprite);
}

fn apply_retro_post_fx(buffer: &mut [u32], width: i32, height: i32, vga_mode: bool) {
    let center_x = width as f32 * 0.5;
    let center_y = height as f32 * 0.52;
    let inv_x = 1.0 / (center_x * center_x);
    let inv_y = 1.0 / (center_y * center_y);
    for y in 0..height {
        for x in 0..width {
            let idx = y as usize * width as usize + x as usize;
            let mut color = buffer[idx];
            let dx = x as f32 - center_x;
            let dy = y as f32 - center_y;
            let radial = (dx * dx) * inv_x + (dy * dy) * inv_y;
            let vignette = (radial * if vga_mode { 18.0 } else { 11.0 }) as i16;
            let scan = if vga_mode {
                if y & 1 == 0 { -2 } else { 0 }
            } else if y & 1 == 0 {
                -1
            } else {
                0
            };
            let grain = if vga_mode {
                (hash_noise(x, y, 0x9137_u32) % 5) as i16 - 2
            } else {
                (hash_noise(x, y, 0x9137_u32) % 3) as i16 - 1
            };
            color = tint(color, scan + grain - vignette);
            if vga_mode {
                let (r, g, b) = unpack(color);
                color = rgb(
                    ((r as u16 / 12) * 12) as u8,
                    ((g as u16 / 12) * 12) as u8,
                    ((b as u16 / 12) * 12) as u8,
                );
            }
            buffer[idx] = color;
        }
    }
}

fn draw_hills(buffer: &mut [u32], width: i32, height: i32) {
    let horizon = (height as f32 * 0.44) as i32;
    for layer in 0..3 {
        let base = horizon + layer * 24;
        let depth = layer as f32 + 1.0;
        let mut ridge = vec![0i32; width as usize];
        for x in 0..width {
            let xf = x as f32;
            let y = base as f32
                + (xf / (92.0 - depth * 10.0)).sin() * (26.0 - depth * 4.0)
                + (xf / (53.0 - depth * 5.0)).cos() * (15.0 - depth * 2.0)
                + (xf / (27.0 - depth * 2.0)).sin() * (7.0 - depth);
            ridge[x as usize] = y as i32;
        }

        for x in 0..width {
            let start = ridge[x as usize];
            for yy in start..(horizon + 120 + layer * 12) {
                let shade = ((yy - start).max(0) * 2).min(90) as i16;
                let base_color = match layer {
                    0 => rgb(88, 170, 106),
                    1 => rgb(66, 149, 84),
                    _ => rgb(51, 126, 69),
                };
                let noise = hash_noise(x, yy, 0xB17B_u32 + layer as u32) % 5;
                let color = tint(base_color, -shade / 4 + noise as i16);
                put_pixel(buffer, width, height, x, yy, color);
            }
        }
    }
}

fn draw_floor_tiles(buffer: &mut [u32], width: i32, height: i32) {
    let floor_y = (height as f32 * 0.73) as i32;
    for y in floor_y..height {
        let depth = ((y - floor_y) as f32 / (height - floor_y).max(1) as f32).clamp(0.0, 1.0);
        let tile_w = (64.0 - depth * 30.0).max(26.0) as i32;
        let tile_h = (34.0 - depth * 16.0).max(14.0) as i32;
        for x in 0..width {
            let tile_x = x / tile_w.max(1);
            let tile_y = (y - floor_y) / tile_h.max(1);
            let mut color = match (tile_x + tile_y) % 4 {
                0 => rgb(148, 108, 73),
                1 => rgb(168, 130, 94),
                2 => rgb(137, 98, 66),
                _ => rgb(122, 89, 59),
            };
            let mortar = x % tile_w.max(1) < 2 || (y - floor_y) % tile_h.max(1) < 2;
            if mortar {
                color = rgb(86, 66, 47);
            }
            let noise = hash_noise(x, y, 0x77a1_u32) % 5;
            color = tint(color, noise as i16 - 2);
            put_pixel(buffer, width, height, x, y, color);
        }
    }
}

fn draw_shadow_panel(buffer: &mut [u32], width: i32, height: i32, x: i32, y: i32, w: i32, h: i32) {
    fill_rect(buffer, width, height, x + 8, y + 8, w, h, rgba_dim(rgb(23, 18, 12), 0.4));
}

fn draw_wood_panel(buffer: &mut [u32], width: i32, height: i32, x: i32, y: i32, w: i32, h: i32) {
    fill_rect(buffer, width, height, x, y, w, h, rgb(88, 58, 34));
    fill_rect(buffer, width, height, x + 8, y + 8, w - 16, h - 16, rgb(126, 87, 52));
    for yy in (y + 8)..(y + h - 8) {
        for xx in (x + 8)..(x + w - 8) {
            let stripe = ((xx * 11 + yy * 5) % 21) as i32 - 10;
            let wave = (((xx / 23) + (yy / 17)) % 7 - 3) as i16;
            let tone = (stripe / 5) as i16 + wave;
            let grain = hash_noise(xx, yy, 0x4412_u32) % 6;
            put_pixel(
                buffer,
                width,
                height,
                xx,
                yy,
                tint(rgb(126, 87, 52), tone + grain as i16 - 3),
            );
        }
    }
    draw_rect_border(buffer, width, height, x, y, w, h, 4, rgb(62, 40, 21));
    draw_rect_border(
        buffer,
        width,
        height,
        x + 5,
        y + 5,
        w - 10,
        h - 10,
        2,
        rgb(151, 113, 71),
    );
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

fn draw_sun_glow(buffer: &mut [u32], width: i32, height: i32, cx: i32, cy: i32, radius: i32) {
    let r2 = (radius * radius) as i64;
    for y in (cy - radius).max(0)..(cy + radius).min(height) {
        for x in (cx - radius).max(0)..(cx + radius).min(width) {
            let dx = (x - cx) as i64;
            let dy = (y - cy) as i64;
            let d2 = dx * dx + dy * dy;
            if d2 <= r2 {
                let t = 1.0 - (d2 as f32 / r2 as f32);
                let glow = rgb(
                    (245.0 + 10.0 * t) as u8,
                    (221.0 + 28.0 * t) as u8,
                    (156.0 + 26.0 * t) as u8,
                );
                let mixed = blend(buffer[y as usize * width as usize + x as usize], glow, t * 0.35);
                put_pixel(buffer, width, height, x, y, mixed);
            }
        }
    }
}

fn draw_cloud_bands(buffer: &mut [u32], width: i32, height: i32) {
    let top = 40;
    let bottom = (height as f32 * 0.33) as i32;
    for y in top..bottom {
        let band = ((y - top) / 22) % 3;
        for x in 0..width {
            let wave = ((x / 33 + y / 21 + band * 3) % 11) - 5;
            if wave > 1 {
                let alpha = 0.06 + (wave as f32 * 0.018);
                let cloud = match band {
                    0 => rgb(237, 246, 255),
                    1 => rgb(225, 239, 252),
                    _ => rgb(213, 232, 249),
                };
                let idx = y as usize * width as usize + x as usize;
                let mixed = blend(buffer[idx], cloud, alpha);
                buffer[idx] = mixed;
            }
        }
    }
}

fn fill_rect_grain(
    buffer: &mut [u32],
    width: i32,
    height: i32,
    x: i32,
    y: i32,
    w: i32,
    h: i32,
    color: u32,
    seed: u32,
    intensity: i16,
) {
    let x0 = x.max(0);
    let y0 = y.max(0);
    let x1 = (x + w).min(width);
    let y1 = (y + h).min(height);
    if x0 >= x1 || y0 >= y1 {
        return;
    }
    for yy in y0..y1 {
        for xx in x0..x1 {
            let n = (hash_noise(xx, yy, seed) % (intensity as i32 * 2 + 1)) - intensity as i32;
            put_pixel(buffer, width, height, xx, yy, tint(color, n as i16));
        }
    }
}

fn hash_noise(x: i32, y: i32, seed: u32) -> i32 {
    let mut v = (x as u32).wrapping_mul(0x9E37_79B9)
        ^ (y as u32).wrapping_mul(0x85EB_CA6B)
        ^ seed.wrapping_mul(0xC2B2_AE35);
    v ^= v >> 15;
    v = v.wrapping_mul(0x2C1B_3C6D);
    v ^= v >> 12;
    (v & 0xFF) as i32
}

fn blend(base: u32, top: u32, alpha: f32) -> u32 {
    let a = alpha.clamp(0.0, 1.0);
    let (br, bg, bb) = unpack(base);
    let (tr, tg, tb) = unpack(top);
    rgb(
        (br as f32 * (1.0 - a) + tr as f32 * a) as u8,
        (bg as f32 * (1.0 - a) + tg as f32 * a) as u8,
        (bb as f32 * (1.0 - a) + tb as f32 * a) as u8,
    )
}

fn draw_sprite(
    buffer: &mut [u32],
    width: i32,
    height: i32,
    x: i32,
    y: i32,
    scale: i32,
    sprite: &Sprite,
) {
    for sy in 0..sprite.height {
        for sx in 0..sprite.width {
            let idx = sy as usize * sprite.width as usize + sx as usize;
            if let Some(color) = sprite.pixels[idx] {
                fill_rect(
                    buffer,
                    width,
                    height,
                    x + sx * scale,
                    y + sy * scale,
                    scale,
                    scale,
                    color,
                );
            }
        }
    }
}

fn sprites() -> &'static SpritePack {
    SPRITES.get_or_init(|| SpritePack {
        player: parse_sprite(
            include_str!("assets/portrait_player.sprite"),
            &sprite_palette(true),
        ),
        cpu: parse_sprite(
            include_str!("assets/portrait_cpu.sprite"),
            &sprite_palette(false),
        ),
    })
}

fn sprite_palette(is_player: bool) -> Vec<(char, u32)> {
    let skin = if is_player {
        rgb(242, 184, 140)
    } else {
        rgb(235, 188, 159)
    };
    let shirt = if is_player {
        rgb(24, 86, 195)
    } else {
        rgb(127, 63, 162)
    };
    vec![
        (' ', 0),
        ('.', 0),
        ('#', rgb(22, 18, 24)),
        ('w', rgb(255, 255, 255)),
        ('s', skin),
        ('h', rgb(99, 54, 29)),
        ('r', rgb(198, 53, 47)),
        ('b', shirt),
        ('e', rgb(37, 97, 170)),
        ('g', rgb(54, 150, 82)),
        ('y', rgb(248, 224, 109)),
        ('k', rgb(13, 11, 16)),
    ]
}

fn parse_sprite(source: &str, palette: &[(char, u32)]) -> Sprite {
    let rows = source
        .lines()
        .filter(|line| !line.trim().is_empty())
        .collect::<Vec<_>>();
    let height = rows.len() as i32;
    let width = rows
        .iter()
        .map(|line| line.chars().count() as i32)
        .max()
        .unwrap_or(0);
    let mut pixels = vec![None; (width * height).max(0) as usize];
    for (y, row) in rows.iter().enumerate() {
        for (x, ch) in row.chars().enumerate() {
            let mapped = palette
                .iter()
                .find(|(key, _)| *key == ch)
                .map(|(_, color)| *color)
                .unwrap_or_else(|| rgb(255, 0, 255));
            if mapped != 0 {
                pixels[y * width as usize + x] = Some(mapped);
            }
        }
    }
    Sprite {
        width,
        height,
        pixels,
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

fn ui_font() -> Option<&'static FontArc> {
    UI_FONT
        .get_or_init(|| {
            FontArc::try_from_slice(include_bytes!("assets/fonts/Bungee-Regular.ttf")).ok()
        })
        .as_ref()
}

fn draw_label_ttf(
    buffer: &mut [u32],
    width: i32,
    height: i32,
    x: i32,
    y: i32,
    scale: i32,
    color: u32,
    text: &str,
) -> bool {
    let Some(font) = ui_font() else {
        return false;
    };

    let px = (scale.max(1) as f32) * 11.5;
    let glyph_scale = PxScale::from(px);
    let scaled_font = font.as_scaled(glyph_scale);
    let mut caret = point(x as f32, y as f32 + scaled_font.ascent());

    for ch in text.chars() {
        if ch == '\n' {
            caret.x = x as f32;
            caret.y += scaled_font.height() * 1.1;
            continue;
        }

        let glyph_id = scaled_font.glyph_id(ch);
        let glyph = glyph_id.with_scale_and_position(glyph_scale, caret);
        if let Some(outlined) = font.outline_glyph(glyph) {
            let bounds = outlined.px_bounds();
            outlined.draw(|gx, gy, coverage| {
                let px = bounds.min.x as i32 + gx as i32;
                let py = bounds.min.y as i32 + gy as i32;
                if px < 0 || py < 0 || px >= width || py >= height {
                    return;
                }
                let idx = py as usize * width as usize + px as usize;
                buffer[idx] = blend(buffer[idx], color, coverage);
            });
        }
        caret.x += scaled_font.h_advance(glyph_id);
    }
    true
}

fn draw_label_ornate(
    buffer: &mut [u32],
    width: i32,
    height: i32,
    x: i32,
    y: i32,
    scale: i32,
    color: u32,
    shadow: u32,
    highlight: u32,
    text: &str,
) {
    if draw_label_ttf(
        buffer,
        width,
        height,
        x + scale / 2 + 1,
        y + scale / 2 + 1,
        scale,
        shadow,
        text,
    ) {
        let _ = draw_label_ttf(
            buffer,
            width,
            height,
            x - 1,
            y - 1,
            scale,
            highlight,
            text,
        );
        let _ = draw_label_ttf(buffer, width, height, x, y, scale, color, text);
        return;
    }

    draw_label(
        buffer,
        width,
        height,
        x + scale / 2 + 1,
        y + scale / 2 + 1,
        scale,
        shadow,
        text,
    );
    draw_label(buffer, width, height, x - 1, y - 1, scale, highlight, text);
    draw_label(buffer, width, height, x, y, scale, color, text);
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

    let raw = RawModeGuard::new().ok();
    let (tx, rx) = mpsc::channel::<KeyEvent>();

    let handle = thread::spawn(move || {
        let stdin = io::stdin();
        let mut lock = stdin.lock();
        let mut byte = [0u8; 1];

        loop {
            match lock.read(&mut byte) {
                Ok(0) => {
                    thread::sleep(Duration::from_millis(8));
                    continue;
                }
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
        _reader_handle: Some(handle),
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

fn with_registry<R>(f: impl FnOnce(&mut RuntimeRegistry) -> R) -> R {
    RUNTIMES.with(|registry| {
        let mut borrowed = registry.borrow_mut();
        f(&mut borrowed)
    })
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
