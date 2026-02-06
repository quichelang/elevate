use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};

use crate::model::Arg;
use crate::parser::{
    ClusterParts, ClusterSplit, LongEqSplit, LongPrefixSplit, LongWithValue, MaybeString,
    ShortPrefixSplit,
};

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
struct Registry {
    next_id: i64,
    states: HashMap<i64, ParserState>,
}

static REGISTRY: OnceLock<Mutex<Registry>> = OnceLock::new();

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

pub fn take_pending_value(handle: i64) -> MaybeString {
    from_option(with_state_mut(handle, |state| state.pending_value.take()).flatten())
}

pub fn set_pending_value(handle: i64, value: String) {
    with_state_mut(handle, |state| {
        state.pending_value = Some(value);
    });
}

pub fn take_short_cluster(handle: i64) -> MaybeString {
    from_option(with_state_mut(handle, |state| state.short_cluster.take()).flatten())
}

pub fn set_short_cluster(handle: i64, cluster: String) {
    with_state_mut(handle, |state| {
        state.short_cluster = if cluster.is_empty() { None } else { Some(cluster) };
    });
}

pub fn take_next_arg(handle: i64) -> MaybeString {
    let value = with_state_mut(handle, |state| {
        let arg = state.args.get(state.index).cloned();
        if arg.is_some() {
            state.index += 1;
        }
        arg
    })
    .flatten();
    from_option(value)
}

pub fn peek_next_arg(handle: i64) -> MaybeString {
    let value = with_state_mut(handle, |state| state.args.get(state.index).cloned()).flatten();
    from_option(value)
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

pub fn finalize_short(handle: i64, short: String) -> Arg {
    with_state_mut(handle, |state| {
        state.last_option = Some(format!("-{short}"));
    });
    Arg::Short(short)
}

pub fn finalize_long(handle: i64, name: String) -> Arg {
    with_state_mut(handle, |state| {
        state.last_option = Some(format!("--{name}"));
    });
    Arg::Long(name)
}

pub fn finalize_long_with_value(handle: i64, payload: LongWithValue) -> Arg {
    with_state_mut(handle, |state| {
        state.last_option = Some(format!("--{}", payload.name));
        state.pending_value = Some(payload.value);
    });
    Arg::Long(payload.name)
}

pub fn format_missing_value_for_last(handle: i64) -> String {
    with_state_mut(handle, |state| match &state.last_option {
        Some(option) => format!("missing value for option {option}"),
        None => "missing value".to_string(),
    })
    .unwrap_or_else(|| "missing value".to_string())
}

pub fn format_unexpected_value_for_last(handle: i64, value: String) -> String {
    with_state_mut(handle, |state| {
        let option = state
            .last_option
            .clone()
            .unwrap_or_else(|| "<unknown-option>".to_string());
        format!("unexpected argument for option '{}': {}", option, value)
    })
    .unwrap_or_else(|| format!("unexpected argument: {value}"))
}

pub fn split_long_prefix(arg: String) -> LongPrefixSplit {
    if let Some(rest) = arg.strip_prefix("--") {
        return LongPrefixSplit::Long(rest.to_string());
    }
    LongPrefixSplit::Other(arg)
}

pub fn nonempty_text(text: String) -> MaybeString {
    if text.is_empty() {
        MaybeString::None
    } else {
        MaybeString::Some(text)
    }
}

pub fn split_long_eq(text: String) -> LongEqSplit {
    if let Some((name, value)) = text.split_once('=') {
        return LongEqSplit::WithValue(LongWithValue {
            name: name.to_string(),
            value: value.to_string(),
        });
    }
    LongEqSplit::Plain(text)
}

pub fn split_short_prefix(arg: String) -> ShortPrefixSplit {
    if arg.starts_with('-') && arg != "-" {
        let cluster = arg.chars().skip(1).collect::<String>();
        if !cluster.is_empty() {
            return ShortPrefixSplit::ShortCluster(cluster);
        }
    }
    ShortPrefixSplit::Value(arg)
}

pub fn split_cluster(cluster: String) -> ClusterSplit {
    if cluster.is_empty() {
        return ClusterSplit::End;
    }

    if let Some(value) = cluster.strip_prefix('=') {
        return ClusterSplit::UnexpectedValue(value.to_string());
    }

    let mut chars = cluster.chars();
    let short = chars.next().expect("cluster is known non-empty").to_string();
    let rest = chars.collect::<String>();
    ClusterSplit::Short(ClusterParts { short, rest })
}

pub fn cluster_optional_value(cluster: String) -> String {
    if let Some(value) = cluster.strip_prefix('=') {
        return value.to_string();
    }
    cluster
}

fn from_option(value: Option<String>) -> MaybeString {
    match value {
        Some(value) => MaybeString::Some(value),
        None => MaybeString::None,
    }
}

fn insert_state(args: Vec<String>) -> i64 {
    let mut registry = registry().lock().expect("registry mutex poisoned");
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
    let mut registry = registry().lock().expect("registry mutex poisoned");
    let state = registry.states.get_mut(&handle)?;
    Some(f(state))
}

fn registry() -> &'static Mutex<Registry> {
    REGISTRY.get_or_init(|| Mutex::new(Registry::default()))
}
