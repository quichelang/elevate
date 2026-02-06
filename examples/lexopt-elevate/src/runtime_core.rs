use std::collections::HashMap;

#[derive(Debug, Clone)]
struct ParserState {
    args: Vec<String>,
    index: usize,
    pending_value: Option<String>,
    short_cluster: Option<String>,
    finished_opts: bool,
    last_option: Option<String>,
}

#[derive(Debug, Default)]
pub(crate) struct Registry {
    next_id: i64,
    states: HashMap<i64, ParserState>,
}

pub fn from_env() -> i64 {
    let args = std::env::args().skip(1).collect::<Vec<_>>();
    insert_state(args)
}

pub fn from_raw(raw: String) -> i64 {
    let args = raw
        .split_whitespace()
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();
    insert_state(args)
}

pub fn reset(handle: i64) {
    with_state_mut(handle, |state| {
        state.index = 0;
        state.pending_value = None;
        state.short_cluster = None;
        state.finished_opts = false;
        state.last_option = None;
    });
}

pub fn take_pending_value(handle: i64) -> Option<String> {
    with_state_mut(handle, |state| state.pending_value.take()).flatten()
}

pub fn set_pending_value(handle: i64, value: String) {
    with_state_mut(handle, |state| {
        state.pending_value = Some(value);
    });
}

pub fn take_short_cluster(handle: i64) -> Option<String> {
    with_state_mut(handle, |state| state.short_cluster.take()).flatten()
}

pub fn set_short_cluster(handle: i64, cluster: String) {
    with_state_mut(handle, |state| {
        state.short_cluster = if cluster.is_empty() { None } else { Some(cluster) };
    });
}

pub fn take_next_arg(handle: i64) -> Option<String> {
    with_state_mut(handle, |state| {
        let arg = state.args.get(state.index).cloned();
        if arg.is_some() {
            state.index += 1;
        }
        arg
    })
    .flatten()
}

pub fn peek_next_arg(handle: i64) -> Option<String> {
    with_state_mut(handle, |state| state.args.get(state.index).cloned()).flatten()
}

pub fn is_finished_opts(handle: i64) -> bool {
    with_state_mut(handle, |state| state.finished_opts).unwrap_or(false)
}

pub fn set_finished_opts(handle: i64) {
    with_state_mut(handle, |state| {
        state.finished_opts = true;
    });
}

pub fn clear_last_option(handle: i64) {
    with_state_mut(handle, |state| {
        state.last_option = None;
    });
}

pub fn set_last_short_option(handle: i64, short: String) {
    with_state_mut(handle, |state| {
        state.last_option = Some(format!("-{short}"));
    });
}

pub fn set_last_long_option(handle: i64, name: String) {
    with_state_mut(handle, |state| {
        state.last_option = Some(format!("--{name}"));
    });
}

pub fn last_option(handle: i64) -> Option<String> {
    with_state_mut(handle, |state| state.last_option.clone()).flatten()
}

pub fn drop_first_char_known(text: String) -> (String, String) {
    let mut chars = text.chars();
    let first = chars
        .next()
        .expect("text must be non-empty before dropping first char");
    (first.to_string(), chars.collect::<String>())
}

fn insert_state(args: Vec<String>) -> i64 {
    let mut registry = crate::__elevate_interop::registry()
        .lock()
        .expect("registry mutex poisoned");
    registry.next_id += 1;
    let handle = registry.next_id;
    registry.states.insert(
        handle,
        ParserState {
            args,
            index: 0,
            pending_value: None,
            short_cluster: None,
            finished_opts: false,
            last_option: None,
        },
    );
    handle
}

fn with_state_mut<T>(handle: i64, f: impl FnOnce(&mut ParserState) -> T) -> Option<T> {
    let mut registry = crate::__elevate_interop::registry()
        .lock()
        .expect("registry mutex poisoned");
    let state = registry.states.get_mut(&handle)?;
    Some(f(state))
}
