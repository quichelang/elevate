use std::path::PathBuf;
use std::process::Command;
use std::sync::{Mutex, OnceLock};

use rustdex::{IndexBuilder, StdIndex};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitMethodSignature {
    pub param_rust_types: Vec<String>,
    pub return_rust_type: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BackendPreference {
    Direct,
    Cli,
}

static BACKEND_PREFERENCE: OnceLock<BackendPreference> = OnceLock::new();
static DIRECT_INDEX: OnceLock<Option<StdIndex>> = OnceLock::new();
static CLI_BIN_PATH: OnceLock<Option<PathBuf>> = OnceLock::new();
static TYPE_TRAIT_CACHE: OnceLock<Mutex<std::collections::HashMap<(String, String), bool>>> =
    OnceLock::new();
static TYPE_METHOD_CACHE: OnceLock<Mutex<std::collections::HashMap<(String, String), bool>>> =
    OnceLock::new();

pub fn type_implements(type_name: &str, trait_name: &str) -> Option<bool> {
    let key = (type_name.to_string(), trait_name.to_string());
    if let Some(cache) = TYPE_TRAIT_CACHE.get()
        && let Some(value) = cache
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .get(&key)
    {
        return Some(*value);
    }
    match backend_preference() {
        BackendPreference::Cli => cli_type_implements(type_name, trait_name),
        BackendPreference::Direct => direct_type_implements(type_name, trait_name)
            .or_else(|| cli_type_implements(type_name, trait_name)),
    }
    .inspect(|value| {
        let cache = TYPE_TRAIT_CACHE.get_or_init(|| Mutex::new(std::collections::HashMap::new()));
        cache
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .insert(key, *value);
    })
}

pub fn type_has_associated_method(type_name: &str, method: &str) -> Option<bool> {
    let key = (type_name.to_string(), method.to_string());
    if let Some(cache) = TYPE_METHOD_CACHE.get()
        && let Some(value) = cache
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .get(&key)
    {
        return Some(*value);
    }
    match backend_preference() {
        BackendPreference::Cli => cli_type_has_associated_method(type_name, method),
        BackendPreference::Direct => direct_type_has_associated_method(type_name, method)
            .or_else(|| cli_type_has_associated_method(type_name, method)),
    }
    .inspect(|value| {
        let cache = TYPE_METHOD_CACHE.get_or_init(|| Mutex::new(std::collections::HashMap::new()));
        cache
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .insert(key, *value);
    })
}

pub fn trait_method_signature(trait_name: &str, method_name: &str) -> Option<TraitMethodSignature> {
    match backend_preference() {
        BackendPreference::Cli => cli_trait_method_signature(trait_name, method_name),
        BackendPreference::Direct => {
            direct_trait_method_signature(trait_name, method_name)
                .or_else(|| cli_trait_method_signature(trait_name, method_name))
        }
    }
}

fn backend_preference() -> BackendPreference {
    *BACKEND_PREFERENCE.get_or_init(|| {
        if let Ok(value) = std::env::var("ELEVATE_RUSTDEX_BACKEND") {
            return match value.trim().to_ascii_lowercase().as_str() {
                "cli" => BackendPreference::Cli,
                "direct" => BackendPreference::Direct,
                _ => BackendPreference::Direct,
            };
        }
        if std::env::var_os("ELEVATE_RUSTDEX_BIN").is_some() {
            return BackendPreference::Cli;
        }
        BackendPreference::Direct
    })
}

fn direct_type_implements(type_name: &str, trait_name: &str) -> Option<bool> {
    let index = direct_index()?;
    Some(index.implements(type_name, trait_name))
}

fn direct_type_has_associated_method(type_name: &str, method: &str) -> Option<bool> {
    let index = direct_index()?;
    let implemented = index.find_impls_for(type_name);
    Some(
        implemented
            .iter()
            .any(|imp| imp.methods.iter().any(|candidate| candidate == method)),
    )
}

fn direct_trait_method_signature(_trait_name: &str, _method_name: &str) -> Option<TraitMethodSignature> {
    None
}

fn direct_index() -> Option<&'static StdIndex> {
    DIRECT_INDEX
        .get_or_init(|| {
            if let Ok(index) = StdIndex::load_from_sysroot() {
                return Some(index);
            }
            let builder = IndexBuilder::from_sysroot().ok()?;
            let index = builder.build().ok()?;
            let _ = index.save_to_sysroot();
            Some(index)
        })
        .as_ref()
}

fn cli_type_implements(type_name: &str, trait_name: &str) -> Option<bool> {
    let mut attempted_build = false;
    loop {
        let output = cli_command_output(&["check", type_name, trait_name])?;
        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            if stdout.contains(" does NOT implement ") {
                return Some(false);
            }
            if stdout.contains(" implements ") {
                return Some(true);
            }
            return None;
        }
        if attempted_build || !cli_build_index() {
            return None;
        }
        attempted_build = true;
    }
}

fn cli_type_has_associated_method(type_name: &str, method: &str) -> Option<bool> {
    let mut attempted_build = false;
    loop {
        let output = cli_command_output(&["query", type_name])?;
        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            for line in stdout.lines() {
                let Some(open) = line.find('[') else {
                    continue;
                };
                let Some(close) = line[open + 1..].find(']') else {
                    continue;
                };
                let methods = &line[open + 1..open + 1 + close];
                let hit = methods
                    .split(',')
                    .map(|part| part.trim())
                    .any(|candidate| candidate == method);
                if hit {
                    return Some(true);
                }
            }
            return Some(false);
        }
        if attempted_build || !cli_build_index() {
            return None;
        }
        attempted_build = true;
    }
}

fn cli_trait_method_signature(trait_name: &str, method_name: &str) -> Option<TraitMethodSignature> {
    let output = cli_command_output(&["trait-method-signature", trait_name, method_name])?;
    if !output.status.success() {
        return None;
    }
    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut params_text = None::<&str>;
    let mut return_text = None::<&str>;
    for line in stdout.lines() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("params:") {
            params_text = Some(rest.trim());
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("return:") {
            return_text = Some(rest.trim());
        }
    }
    let params_text = params_text?;
    let return_text = return_text?;
    let param_rust_types = split_rust_signature_params(params_text)
        .into_iter()
        .map(|item| normalize_rust_signature_type(&item))
        .filter(|item| !item.is_empty())
        .collect::<Vec<_>>();
    if param_rust_types.is_empty() {
        return None;
    }
    let return_rust_type = normalize_rust_signature_type(return_text);
    if return_rust_type.is_empty() {
        return None;
    }
    Some(TraitMethodSignature {
        param_rust_types,
        return_rust_type,
    })
}

fn cli_build_index() -> bool {
    cli_command_output(&["build"])
        .map(|output| output.status.success())
        .unwrap_or(false)
}

fn cli_command_output(args: &[&str]) -> Option<std::process::Output> {
    let bin = cli_bin_path()?;
    Command::new(bin).args(args).output().ok()
}

fn cli_bin_path() -> Option<&'static PathBuf> {
    CLI_BIN_PATH
        .get_or_init(|| {
            if let Some(bin_override) = std::env::var_os("ELEVATE_RUSTDEX_BIN") {
                let path = PathBuf::from(bin_override);
                if path.is_file() {
                    return Some(path);
                }
            }

            let local =
                PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../rustdex/target/debug/rustdex");
            if local.is_file() {
                return Some(local);
            }

            let output = Command::new("rustdex").arg("help").output().ok()?;
            if output.status.success() {
                return Some(PathBuf::from("rustdex"));
            }
            None
        })
        .as_ref()
}

fn normalize_rust_signature_type(raw: &str) -> String {
    let mut ty = raw.trim();
    if let Some((_, rhs)) = ty.split_once(':') {
        ty = rhs.trim();
    }
    ty.to_string()
}

fn split_rust_signature_params(text: &str) -> Vec<String> {
    let mut items = Vec::new();
    let mut current = String::new();
    let mut depth = 0usize;
    for ch in text.chars() {
        match ch {
            '<' | '(' | '[' => {
                depth += 1;
                current.push(ch);
            }
            '>' | ')' | ']' => {
                depth = depth.saturating_sub(1);
                current.push(ch);
            }
            ',' if depth == 0 => {
                let trimmed = current.trim();
                if !trimmed.is_empty() {
                    items.push(trimmed.to_string());
                }
                current.clear();
            }
            _ => current.push(ch),
        }
    }
    let trimmed = current.trim();
    if !trimmed.is_empty() {
        items.push(trimmed.to_string());
    }
    items
}
