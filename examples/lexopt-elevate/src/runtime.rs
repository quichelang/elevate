use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};

use crate::model::{Arg, ParseError};

#[derive(Debug, Clone)]
struct ParserState {
    args: Vec<String>,
    index: usize,
    pending_value: Option<String>,
    short_cluster: Option<(Vec<char>, usize)>,
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
    let mut registry = registry().lock().expect("registry mutex poisoned");
    if let Some(state) = registry.states.get_mut(&handle) {
        state.index = 0;
        state.pending_value = None;
        state.short_cluster = None;
        state.finished_opts = false;
        state.last_option = None;
    }
}

pub fn next(handle: i64) -> Result<Option<Arg>, ParseError> {
    let mut registry = registry().lock().expect("registry mutex poisoned");
    let state = lookup_state(&mut registry, handle)?;
    next_impl(state)
}

pub fn value(handle: i64) -> Result<String, ParseError> {
    let mut registry = registry().lock().expect("registry mutex poisoned");
    let state = lookup_state(&mut registry, handle)?;

    if let Some(value) = take_optional_value(state) {
        return Ok(value);
    }

    if let Some(value) = state.args.get(state.index).cloned() {
        state.index += 1;
        return Ok(value);
    }

    Err(ParseError::MissingValue(format_missing_value(
        state.last_option.as_ref(),
    )))
}

pub fn optional_value(handle: i64) -> Option<String> {
    let mut registry = registry().lock().expect("registry mutex poisoned");
    let state = registry.states.get_mut(&handle)?;
    take_optional_value(state)
}

pub fn values(handle: i64) -> Result<Option<String>, ParseError> {
    let mut registry = registry().lock().expect("registry mutex poisoned");
    let state = lookup_state(&mut registry, handle)?;

    if let Some(value) = take_optional_value(state) {
        return Ok(Some(value));
    }

    if let Some(value) = state.args.get(state.index).cloned() {
        if state.finished_opts || value == "-" || !value.starts_with('-') {
            state.index += 1;
            return Ok(Some(value));
        }
    }

    Err(ParseError::MissingValue(format_missing_value(
        state.last_option.as_ref(),
    )))
}

fn next_impl(state: &mut ParserState) -> Result<Option<Arg>, ParseError> {
    loop {
        if let Some(value) = state.pending_value.take() {
            let option = state
                .last_option
                .clone()
                .unwrap_or_else(|| "<unknown-option>".to_string());
            return Err(ParseError::UnexpectedValue(format!(
                "unexpected argument for option '{}': {}",
                option, value
            )));
        }

        if let Some((cluster, mut pos)) = state.short_cluster.take() {
            if pos >= cluster.len() {
                continue;
            }

            if cluster[pos] == '=' && pos > 1 {
                pos += 1;
                let value = cluster[pos..].iter().collect::<String>();
                let option = state
                    .last_option
                    .clone()
                    .unwrap_or_else(|| "<unknown-option>".to_string());
                return Err(ParseError::UnexpectedValue(format!(
                    "unexpected argument for option '{}': {}",
                    option, value
                )));
            }

            let short = cluster[pos].to_string();
            pos += 1;
            if pos < cluster.len() {
                state.short_cluster = Some((cluster, pos));
            }
            state.last_option = Some(format!("-{short}"));
            return Ok(Some(Arg::Short(short)));
        }

        let current = match state.args.get(state.index).cloned() {
            Some(arg) => arg,
            None => return Ok(None),
        };
        state.index += 1;

        if state.finished_opts {
            state.last_option = None;
            return Ok(Some(Arg::Value(current)));
        }

        if current == "--" {
            state.finished_opts = true;
            continue;
        }

        if let Some(rest) = current.strip_prefix("--") {
            if let Some((name, value)) = rest.split_once('=') {
                state.pending_value = Some(value.to_string());
                state.last_option = Some(format!("--{name}"));
                return Ok(Some(Arg::Long(name.to_string())));
            }
            state.last_option = Some(format!("--{rest}"));
            return Ok(Some(Arg::Long(rest.to_string())));
        }

        if current.starts_with('-') && current != "-" {
            let chars = current.chars().collect::<Vec<_>>();
            if chars.len() > 1 {
                state.short_cluster = Some((chars, 1));
                continue;
            }
        }

        state.last_option = None;
        return Ok(Some(Arg::Value(current)));
    }
}

fn take_optional_value(state: &mut ParserState) -> Option<String> {
    if let Some(value) = state.pending_value.take() {
        return Some(value);
    }

    if let Some((cluster, mut pos)) = state.short_cluster.take() {
        if pos >= cluster.len() {
            return None;
        }
        if cluster[pos] == '=' {
            pos += 1;
        }
        return Some(cluster[pos..].iter().collect::<String>());
    }

    None
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

fn lookup_state(registry: &mut Registry, handle: i64) -> Result<&mut ParserState, ParseError> {
    registry.states.get_mut(&handle).ok_or_else(|| {
        ParseError::UnexpectedArgument(format!("invalid parser handle: {handle}"))
    })
}

fn format_missing_value(last_option: Option<&String>) -> String {
    match last_option {
        Some(option) => format!("missing value for option {option}"),
        None => "missing value".to_string(),
    }
}

fn registry() -> &'static Mutex<Registry> {
    REGISTRY.get_or_init(|| Mutex::new(Registry::default()))
}
