use std::path::PathBuf;
use std::process::Command;
use std::sync::{Mutex, OnceLock};

use rustdex::{IndexBuilder, StdIndex};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitMethodSignature {
    pub param_rust_types: Vec<String>,
    pub return_rust_type: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustdexError {
    IndexUnavailable {
        backend: &'static str,
        detail: String,
    },
    CliUnavailable {
        detail: String,
    },
    BackendQueryFailed {
        backend: &'static str,
        query: String,
        detail: String,
    },
    SignatureUnavailable {
        trait_name: String,
        method_name: String,
    },
    InvalidBackendConfig {
        value: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustdexSession {
    pub backend: &'static str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BackendPreference {
    Direct,
    Cli,
}

impl BackendPreference {
    fn as_str(self) -> &'static str {
        match self {
            BackendPreference::Direct => "direct",
            BackendPreference::Cli => "cli",
        }
    }
}

static DIRECT_INDEX: OnceLock<Result<StdIndex, RustdexError>> = OnceLock::new();
static TYPE_TRAIT_CACHE: OnceLock<Mutex<std::collections::HashMap<(String, String), bool>>> =
    OnceLock::new();
static TYPE_METHOD_CACHE: OnceLock<Mutex<std::collections::HashMap<(String, String), bool>>> =
    OnceLock::new();

pub fn preflight_required() -> Result<RustdexSession, RustdexError> {
    let backend = backend_preference()?;
    match backend {
        BackendPreference::Direct => {
            let _ = direct_index()?;
        }
        BackendPreference::Cli => {
            let _ = cli_bin_path()?;
            cli_build_index()?;
        }
    }
    Ok(RustdexSession {
        backend: backend.as_str(),
    })
}

pub fn type_implements(type_name: &str, trait_name: &str) -> Result<bool, RustdexError> {
    let key = (type_name.to_string(), trait_name.to_string());
    if let Some(cache) = TYPE_TRAIT_CACHE.get()
        && let Some(value) = cache
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .get(&key)
    {
        return Ok(*value);
    }
    let value = match backend_preference()? {
        BackendPreference::Cli => cli_type_implements(type_name, trait_name)?,
        BackendPreference::Direct => direct_type_implements(type_name, trait_name)?,
    };
    let cache = TYPE_TRAIT_CACHE.get_or_init(|| Mutex::new(std::collections::HashMap::new()));
    cache
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .insert(key, value);
    Ok(value)
}

pub fn type_has_associated_method(type_name: &str, method: &str) -> Result<bool, RustdexError> {
    let key = (type_name.to_string(), method.to_string());
    if let Some(cache) = TYPE_METHOD_CACHE.get()
        && let Some(value) = cache
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .get(&key)
    {
        return Ok(*value);
    }
    let value = match backend_preference()? {
        BackendPreference::Cli => cli_type_has_associated_method(type_name, method)?,
        BackendPreference::Direct => direct_type_has_associated_method(type_name, method)?,
    };
    let cache = TYPE_METHOD_CACHE.get_or_init(|| Mutex::new(std::collections::HashMap::new()));
    cache
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .insert(key, value);
    Ok(value)
}

pub fn trait_method_signature(
    trait_name: &str,
    method_name: &str,
) -> Result<TraitMethodSignature, RustdexError> {
    match backend_preference()? {
        BackendPreference::Cli => cli_trait_method_signature(trait_name, method_name),
        BackendPreference::Direct => direct_trait_method_signature(trait_name, method_name),
    }
}

fn backend_preference() -> Result<BackendPreference, RustdexError> {
    if let Ok(value) = std::env::var("ELEVATE_RUSTDEX_BACKEND") {
        return match value.trim().to_ascii_lowercase().as_str() {
            "cli" => Ok(BackendPreference::Cli),
            "direct" => Ok(BackendPreference::Direct),
            other => Err(RustdexError::InvalidBackendConfig {
                value: other.to_string(),
            }),
        };
    }
    if std::env::var_os("ELEVATE_RUSTDEX_BIN").is_some() {
        return Ok(BackendPreference::Cli);
    }
    Ok(BackendPreference::Direct)
}

fn direct_type_implements(type_name: &str, trait_name: &str) -> Result<bool, RustdexError> {
    let index = direct_index()?;
    Ok(index.implements(type_name, trait_name))
}

fn direct_type_has_associated_method(type_name: &str, method: &str) -> Result<bool, RustdexError> {
    let index = direct_index()?;
    let implemented = index.find_impls_for(type_name);
    Ok(implemented
        .iter()
        .any(|imp| imp.methods.iter().any(|candidate| candidate == method)))
}

fn direct_trait_method_signature(
    trait_name: &str,
    method_name: &str,
) -> Result<TraitMethodSignature, RustdexError> {
    let _ = direct_index()?;
    Err(RustdexError::SignatureUnavailable {
        trait_name: trait_name.to_string(),
        method_name: method_name.to_string(),
    })
}

fn direct_index() -> Result<&'static StdIndex, RustdexError> {
    DIRECT_INDEX
        .get_or_init(|| {
            if let Ok(index) = StdIndex::load_from_sysroot() {
                return Ok(index);
            }
            let builder = IndexBuilder::from_sysroot().map_err(|err| RustdexError::IndexUnavailable {
                backend: "direct",
                detail: format!("failed to initialize index builder from sysroot: {err}"),
            })?;
            let index = builder.build().map_err(|err| RustdexError::IndexUnavailable {
                backend: "direct",
                detail: format!("failed to build std index from sysroot: {err}"),
            })?;
            let _ = index.save_to_sysroot();
            Ok(index)
        })
        .as_ref()
        .map_err(|err| err.clone())
}

fn cli_type_implements(type_name: &str, trait_name: &str) -> Result<bool, RustdexError> {
    let mut attempted_build = false;
    loop {
        let output = cli_command_output(&["check", type_name, trait_name])?;
        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            if stdout.contains(" does NOT implement ") {
                return Ok(false);
            }
            if stdout.contains(" implements ") {
                return Ok(true);
            }
            return Err(RustdexError::BackendQueryFailed {
                backend: "cli",
                query: format!("check {type_name} {trait_name}"),
                detail: "unable to parse rustdex check output".to_string(),
            });
        }
        if attempted_build {
            return Err(RustdexError::BackendQueryFailed {
                backend: "cli",
                query: format!("check {type_name} {trait_name}"),
                detail: String::from_utf8_lossy(&output.stderr).trim().to_string(),
            });
        }
        cli_build_index()?;
        attempted_build = true;
    }
}

fn cli_type_has_associated_method(type_name: &str, method: &str) -> Result<bool, RustdexError> {
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
                    return Ok(true);
                }
            }
            return Ok(false);
        }
        if attempted_build {
            return Err(RustdexError::BackendQueryFailed {
                backend: "cli",
                query: format!("query {type_name}"),
                detail: String::from_utf8_lossy(&output.stderr).trim().to_string(),
            });
        }
        cli_build_index()?;
        attempted_build = true;
    }
}

fn cli_trait_method_signature(
    trait_name: &str,
    method_name: &str,
) -> Result<TraitMethodSignature, RustdexError> {
    let output = cli_command_output(&["trait-method-signature", trait_name, method_name])?;
    if !output.status.success() {
        return Err(RustdexError::SignatureUnavailable {
            trait_name: trait_name.to_string(),
            method_name: method_name.to_string(),
        });
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
    let params_text = params_text.ok_or_else(|| RustdexError::SignatureUnavailable {
        trait_name: trait_name.to_string(),
        method_name: method_name.to_string(),
    })?;
    let return_text = return_text.ok_or_else(|| RustdexError::SignatureUnavailable {
        trait_name: trait_name.to_string(),
        method_name: method_name.to_string(),
    })?;
    let param_rust_types = split_rust_signature_params(params_text)
        .into_iter()
        .map(|item| normalize_rust_signature_type(&item))
        .filter(|item| !item.is_empty())
        .collect::<Vec<_>>();
    if param_rust_types.is_empty() {
        return Err(RustdexError::SignatureUnavailable {
            trait_name: trait_name.to_string(),
            method_name: method_name.to_string(),
        });
    }
    let return_rust_type = normalize_rust_signature_type(return_text);
    if return_rust_type.is_empty() {
        return Err(RustdexError::SignatureUnavailable {
            trait_name: trait_name.to_string(),
            method_name: method_name.to_string(),
        });
    }
    Ok(TraitMethodSignature {
        param_rust_types,
        return_rust_type,
    })
}

fn cli_build_index() -> Result<(), RustdexError> {
    let output = cli_command_output(&["build"])?;
    if output.status.success() {
        return Ok(());
    }
    Err(RustdexError::BackendQueryFailed {
        backend: "cli",
        query: "build".to_string(),
        detail: String::from_utf8_lossy(&output.stderr).trim().to_string(),
    })
}

fn cli_command_output(args: &[&str]) -> Result<std::process::Output, RustdexError> {
    let bin = cli_bin_path()?;
    Command::new(&bin)
        .args(args)
        .output()
        .map_err(|err| RustdexError::BackendQueryFailed {
            backend: "cli",
            query: args.join(" "),
            detail: err.to_string(),
        })
}

fn cli_bin_path() -> Result<PathBuf, RustdexError> {
    if let Some(bin_override) = std::env::var_os("ELEVATE_RUSTDEX_BIN") {
        let path = PathBuf::from(bin_override);
        if path.is_file() {
            return Ok(path);
        }
        return Err(RustdexError::CliUnavailable {
            detail: format!(
                "configured rustdex binary path does not exist: {}",
                path.display()
            ),
        });
    }

    let local = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../rustdex/target/debug/rustdex");
    if local.is_file() {
        return Ok(local);
    }

    let output = Command::new("rustdex")
        .arg("help")
        .output()
        .map_err(|err| RustdexError::CliUnavailable {
            detail: format!("failed to invoke rustdex from PATH: {err}"),
        })?;
    if output.status.success() {
        return Ok(PathBuf::from("rustdex"));
    }
    Err(RustdexError::CliUnavailable {
        detail: "rustdex binary was not found in PATH".to_string(),
    })
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
