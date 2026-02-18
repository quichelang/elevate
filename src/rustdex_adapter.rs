//! Adapter layer converting rustdex types → Elevate AST types.
//!
//! This module provides a clean boundary between rustdex's data model
//! (`MethodSig`, `ParamSig`, `ReceiverMode`) and Elevate's semantic types
//! (`SemType`, `CallArgMode`, `MethodCapability`, `TraitMethodSignatureOverride`).
//!
//! Two main entry points:
//! - [`method_sig_to_capability`] – inherent methods (e.g. `Vec::push`)
//! - [`trait_method_sig_to_override`] – trait methods (e.g. `Display::fmt`)

use crate::passes::{
    CallArgMode, CapabilityReceiverMode, MethodCapability, SemType, TraitMethodSignatureOverride,
    named_type, option_type, rust_owned_type_string,
};
use std::collections::{BTreeSet, HashMap, HashSet};

// ─── Inherent method conversion ────────────────────────────────────────

/// Convert a rustdex `MethodSig` (from an inherent impl) into an Elevate
/// `MethodCapability`.
///
/// `generic_args` are the caller's concrete type arguments (e.g. for
/// `Vec<i64>`, `generic_args = [SemType::Path(["i64"])]`).
pub(crate) fn method_sig_to_capability(
    sig: &rustdex::MethodSig,
    type_name: &str,
    generic_args: &[SemType],
) -> (MethodCapability, Vec<String>) {
    let receiver_mode = convert_receiver(&sig.receiver);
    let mut unresolved = BTreeSet::new();
    let env = build_method_type_env(sig, type_name, generic_args);

    let mut arg_modes = Vec::with_capacity(sig.params.len());
    let mut expected_args = Vec::with_capacity(sig.params.len());

    for param in &sig.params {
        arg_modes.push(convert_param_mode(param));
        expected_args.push(parse_rustdoc_type_with_env(&param.ty, &env, &mut unresolved));
    }

    let return_ty = parse_rustdoc_type_with_env(&sig.return_type, &env, &mut unresolved);

    (
        MethodCapability {
            receiver_mode,
            arg_modes,
            expected_args,
            return_ty,
        },
        unresolved.into_iter().collect(),
    )
}

// ─── Trait method conversion ───────────────────────────────────────────

/// Context for resolving a trait method signature.
pub(crate) struct TraitMethodContext<'a> {
    /// Full trait path from rustdex (e.g. `"core::fmt::Display"`).
    pub trait_path: &'a str,
    /// Concrete impl target name (e.g. `"Token"`).
    pub impl_target: &'a str,
    /// Semantic type of the impl target.
    pub impl_target_sem: &'a SemType,
}

/// Convert a rustdex `MethodSig` (from a trait impl) into an Elevate
/// `TraitMethodSignatureOverride`.
///
/// This handles:
/// - Including the receiver as the first parameter
/// - Substituting `Self` → concrete target type
/// - Qualifying short type names using the trait's module path
/// - Normalizing `core::` → `std::`
pub(crate) fn trait_method_sig_to_override(
    sig: &rustdex::MethodSig,
    ctx: &TraitMethodContext<'_>,
) -> (TraitMethodSignatureOverride, Vec<String>) {
    let impl_target_rust = rust_owned_type_string(ctx.impl_target_sem);
    let mut unresolved = BTreeSet::new();
    let env = build_trait_method_type_env(sig, ctx);

    // Build the raw Rust type strings including receiver
    let mut param_rust_types = build_param_rust_types(sig, ctx);

    // Normalize all param types: Self→target, core::→std::, qualify short names
    for ty in &mut param_rust_types {
        *ty = normalize_type_string(ty, &impl_target_rust, ctx.trait_path);
    }

    // Build semantic types from normalized strings
    let param_sem_types: Vec<SemType> = param_rust_types
        .iter()
        .map(|ty| parse_rustdoc_type_with_env(ty, &env, &mut unresolved))
        .collect();

    // Normalize return type
    let mut return_rust_type = sig.return_type.clone();
    return_rust_type = normalize_type_string(&return_rust_type, &impl_target_rust, ctx.trait_path);
    let return_sem_type = parse_rustdoc_type_with_env(&return_rust_type, &env, &mut unresolved);

    (
        TraitMethodSignatureOverride {
            param_sem_types,
            return_sem_type,
            param_rust_types,
            return_rust_type,
        },
        unresolved.into_iter().collect(),
    )
}

// ─── Internal: enum conversion ─────────────────────────────────────────

/// Map rustdex `ReceiverMode` → Elevate `CapabilityReceiverMode`.
fn convert_receiver(mode: &rustdex::ReceiverMode) -> CapabilityReceiverMode {
    match mode {
        rustdex::ReceiverMode::Ref => CapabilityReceiverMode::Borrowed,
        rustdex::ReceiverMode::RefMut => CapabilityReceiverMode::MutBorrowed,
        rustdex::ReceiverMode::Owned | rustdex::ReceiverMode::None => CapabilityReceiverMode::Owned,
    }
}

/// Map a rustdex `ParamSig` → Elevate `CallArgMode`.
fn convert_param_mode(param: &rustdex::ParamSig) -> CallArgMode {
    if param.is_mut_ref {
        CallArgMode::MutBorrowed
    } else if param.is_ref {
        CallArgMode::Borrowed
    } else {
        CallArgMode::Owned
    }
}

// ─── Internal: type string builders ────────────────────────────────────

/// Build the list of Rust type strings for a trait method's parameters,
/// including the receiver as the first entry.
fn build_param_rust_types(sig: &rustdex::MethodSig, ctx: &TraitMethodContext<'_>) -> Vec<String> {
    let mut types = Vec::with_capacity(1 + sig.params.len());

    // Receiver → first param with Self placeholder
    match sig.receiver {
        rustdex::ReceiverMode::Ref => types.push("&Self".to_string()),
        rustdex::ReceiverMode::RefMut => types.push("&mut Self".to_string()),
        rustdex::ReceiverMode::Owned => types.push("Self".to_string()),
        rustdex::ReceiverMode::None => {
            // Static method — include target type if params exist
            if !sig.params.is_empty() {
                types.push(ctx.impl_target.to_string());
            }
        }
    }

    for p in &sig.params {
        types.push(p.ty.clone());
    }

    types
}

#[derive(Debug, Clone, Default)]
struct RustdocTypeBindingEnv {
    self_type: Option<SemType>,
    container_bindings: HashMap<String, SemType>,
    method_bindings: HashMap<String, SemType>,
    trait_bindings: HashMap<String, SemType>,
    known_symbols: HashSet<String>,
}

fn build_method_type_env(
    sig: &rustdex::MethodSig,
    type_name: &str,
    generic_args: &[SemType],
) -> RustdocTypeBindingEnv {
    let mut env = RustdocTypeBindingEnv {
        self_type: Some(path_with_generic_args(type_name, generic_args)),
        ..RustdocTypeBindingEnv::default()
    };
    bind_generic_symbols(
        &sig.impl_type_params,
        generic_args,
        &mut env.container_bindings,
        &mut env.known_symbols,
    );
    for param in &sig.method_type_params {
        if let Some(symbol) = generic_param_symbol(param) {
            env.known_symbols.insert(symbol);
        }
    }
    env
}

fn build_trait_method_type_env(
    sig: &rustdex::MethodSig,
    ctx: &TraitMethodContext<'_>,
) -> RustdocTypeBindingEnv {
    let mut env = RustdocTypeBindingEnv {
        self_type: Some(ctx.impl_target_sem.clone()),
        ..RustdocTypeBindingEnv::default()
    };
    if let SemType::Path { args, .. } = ctx.impl_target_sem {
        bind_generic_symbols(
            &sig.impl_type_params,
            args,
            &mut env.container_bindings,
            &mut env.known_symbols,
        );
    } else {
        for param in &sig.impl_type_params {
            if let Some(symbol) = generic_param_symbol(param) {
                env.known_symbols.insert(symbol);
            }
        }
    }
    for param in &sig.method_type_params {
        if let Some(symbol) = generic_param_symbol(param) {
            env.known_symbols.insert(symbol);
        }
    }
    env
}

fn path_with_generic_args(type_name: &str, generic_args: &[SemType]) -> SemType {
    let path = type_name
        .split("::")
        .filter(|segment| !segment.is_empty())
        .map(|segment| segment.to_string())
        .collect::<Vec<_>>();
    if path.is_empty() {
        return SemType::Unknown;
    }
    SemType::Path {
        path,
        args: generic_args.to_vec(),
    }
}

fn bind_generic_symbols(
    params: &[String],
    args: &[SemType],
    bindings: &mut HashMap<String, SemType>,
    known_symbols: &mut HashSet<String>,
) {
    for (index, param) in params.iter().enumerate() {
        let Some(symbol) = generic_param_symbol(param) else {
            continue;
        };
        known_symbols.insert(symbol.clone());
        if let Some(arg) = args.get(index) {
            bindings.insert(symbol, arg.clone());
        }
    }
}

fn generic_param_symbol(param: &str) -> Option<String> {
    let trimmed = param.trim();
    if trimmed.is_empty() {
        return None;
    }
    let trimmed = trimmed.strip_prefix("const ").unwrap_or(trimmed);
    let stop = trimmed
        .find(|ch: char| ch == ':' || ch == '=' || ch.is_whitespace())
        .unwrap_or(trimmed.len());
    let symbol = trimmed[..stop].trim();
    if symbol.is_empty() {
        None
    } else {
        Some(symbol.to_string())
    }
}

/// Normalize a raw Rust type string from rustdex:
/// - Replace `Self` with the actual impl target type name
/// - Normalize `core::` prefixes to `std::`
/// - Qualify unqualified type names using the trait's module path
///
/// For example, given `trait_path = "core::fmt::Display"` and
/// `impl_target = "Token"`:
///   `"&Self"` → `"&Token"`
///   `"Formatter<'_>"` → `"std::fmt::Formatter<'_>"`
///   `"core::fmt::Result"` → `"std::fmt::Result"`
pub(crate) fn normalize_type_string(ty: &str, impl_target: &str, trait_path: &str) -> String {
    let mut result = ty.to_string();

    // Step 1: Replace Self with the concrete target type
    result = result.replace("Self", impl_target);

    // Step 2: Normalize core:: to std:: (rustdoc uses core:: internally)
    result = result.replace("core::", "std::");

    // Step 3: Qualify unqualified type names using the trait's module prefix
    if let Some(module) = trait_module_prefix(trait_path) {
        let module = module.replace("core::", "std::");
        result = qualify_unqualified_types(&result, &module, impl_target);
    }

    result
}

/// Extract the module prefix from a trait path.
/// e.g. `"std::fmt::Display"` → `"std::fmt"`
fn trait_module_prefix(trait_path: &str) -> Option<&str> {
    trait_path.rsplit_once("::").map(|(prefix, _)| prefix)
}

/// Walk a type string and qualify capitalized identifiers that aren't
/// already path-qualified and aren't the impl target type.
fn qualify_unqualified_types(ty: &str, module: &str, impl_target: &str) -> String {
    let mut result = String::with_capacity(ty.len());
    let mut chars = ty.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch.is_ascii_uppercase() {
            // Collect the full identifier
            let mut ident = String::new();
            ident.push(ch);
            while let Some(&next) = chars.peek() {
                if next.is_ascii_alphanumeric() || next == '_' {
                    ident.push(next);
                    chars.next();
                } else {
                    break;
                }
            }

            // Don't qualify if already path-qualified or is the target type
            let already_qualified = result.ends_with("::");
            if !already_qualified && ident != impl_target {
                result.push_str(&format!("{module}::{ident}"));
            } else {
                result.push_str(&ident);
            }
        } else {
            result.push(ch);
        }
    }

    result
}

// ─── Internal: type string → SemType conversion ───────────────────────

fn parse_rustdoc_type_with_env(
    s: &str,
    env: &RustdocTypeBindingEnv,
    unresolved: &mut BTreeSet<String>,
) -> SemType {
    let s = s.trim();
    if s.is_empty() || s == "_" {
        return SemType::Unknown;
    }
    if s == "()" {
        return SemType::Unit;
    }
    if s == "self" || s == "Self" {
        return env.self_type.clone().unwrap_or(SemType::Unknown);
    }
    if let Some((mutable, rest)) = parse_reference_prefix(s) {
        return SemType::Ref {
            mutable,
            inner: Box::new(parse_rustdoc_type_with_env(rest, env, unresolved)),
        };
    }

    if let Some(tuple_items) = parse_tuple_type_items(s) {
        return SemType::Tuple(
            tuple_items
                .into_iter()
                .map(|item| parse_rustdoc_type_with_env(item, env, unresolved))
                .collect(),
        );
    }

    if let Some(inner) = s.strip_prefix("Option<").and_then(|rest| rest.strip_suffix('>')) {
        return option_type(parse_rustdoc_type_with_env(inner, env, unresolved));
    }

    if let Some((head, generic_body)) = split_type_head_and_generic_body(s) {
        let path = head
            .split("::")
            .filter(|segment| !segment.is_empty())
            .map(|segment| segment.to_string())
            .collect::<Vec<_>>();
        if path.is_empty() {
            return SemType::Unknown;
        }
        let args = split_top_level_commas(generic_body)
            .into_iter()
            .map(|item| parse_rustdoc_type_with_env(item, env, unresolved))
            .collect();
        return SemType::Path { path, args };
    }

    if let Some(bound) = resolve_generic_symbol(s, env, unresolved) {
        return bound;
    }

    // Primitives
    match s {
        "bool" => return named_type("bool"),
        "usize" | "isize" | "u8" | "u16" | "u32" | "u64" | "u128" | "i8" | "i16" | "i32"
        | "i64" | "i128" | "f32" | "f64" => return named_type(s),
        "char" => return named_type("char"),
        "String" => return named_type("String"),
        "str" => return named_type("str"),
        _ => {}
    }

    if s.contains("::") && !s.starts_with('&') {
        return SemType::Path {
            path: s
                .split("::")
                .filter(|segment| !segment.is_empty())
                .map(|segment| segment.to_string())
                .collect(),
            args: Vec::new(),
        };
    }

    named_type(s)
}

fn resolve_generic_symbol(
    symbol: &str,
    env: &RustdocTypeBindingEnv,
    unresolved: &mut BTreeSet<String>,
) -> Option<SemType> {
    if let Some(bound) = env.method_bindings.get(symbol) {
        return Some(bound.clone());
    }
    if let Some(bound) = env.container_bindings.get(symbol) {
        return Some(bound.clone());
    }
    if let Some(bound) = env.trait_bindings.get(symbol) {
        return Some(bound.clone());
    }
    if env.known_symbols.contains(symbol) || is_probable_generic_symbol(symbol) {
        unresolved.insert(symbol.to_string());
        return Some(SemType::Unknown);
    }
    None
}

fn is_probable_generic_symbol(symbol: &str) -> bool {
    symbol.len() == 1
        && symbol
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_uppercase())
}

fn split_type_head_and_generic_body(ty: &str) -> Option<(&str, &str)> {
    let start = ty.find('<')?;
    if !ty.ends_with('>') || start == 0 {
        return None;
    }
    let head = ty[..start].trim();
    let body = &ty[start + 1..ty.len() - 1];
    if head.is_empty() || body.trim().is_empty() {
        return None;
    }
    Some((head, body))
}

fn parse_tuple_type_items(ty: &str) -> Option<Vec<&str>> {
    let ty = ty.trim();
    if !ty.starts_with('(') || !ty.ends_with(')') {
        return None;
    }
    let inner = &ty[1..ty.len() - 1];
    if inner.trim().is_empty() {
        return Some(Vec::new());
    }
    Some(split_top_level_commas(inner))
}

fn split_top_level_commas(input: &str) -> Vec<&str> {
    let mut items = Vec::new();
    let mut depth_angle = 0usize;
    let mut depth_paren = 0usize;
    let mut start = 0usize;

    for (idx, ch) in input.char_indices() {
        match ch {
            '<' => depth_angle += 1,
            '>' => depth_angle = depth_angle.saturating_sub(1),
            '(' => depth_paren += 1,
            ')' => depth_paren = depth_paren.saturating_sub(1),
            ',' if depth_angle == 0 && depth_paren == 0 => {
                let segment = input[start..idx].trim();
                if !segment.is_empty() {
                    items.push(segment);
                }
                start = idx + 1;
            }
            _ => {}
        }
    }

    let tail = input[start..].trim();
    if !tail.is_empty() {
        items.push(tail);
    }
    items
}

/// Convert a rustdoc type string to `SemType`, substituting generic type
/// parameters with concrete types from the caller's context.
///
/// Used for inherent methods where `generic_args` are the actual type
/// arguments (e.g. for `Vec<i32>`, `generic_args = [SemType for i32]`).
pub(crate) fn parse_rustdoc_type_str(
    s: &str,
    generic_args: &[SemType],
    type_name: &str,
) -> SemType {
    let env = RustdocTypeBindingEnv {
        self_type: Some(path_with_generic_args(type_name, generic_args)),
        ..RustdocTypeBindingEnv::default()
    };
    let mut unresolved = BTreeSet::new();
    parse_rustdoc_type_with_env(s, &env, &mut unresolved)
}

fn parse_reference_prefix(ty: &str) -> Option<(bool, &str)> {
    let rest = ty.strip_prefix('&')?.trim_start();
    let mut rest = rest;
    if rest.starts_with('\'') {
        if let Some(space_idx) = rest.find(char::is_whitespace) {
            rest = rest[space_idx..].trim_start();
        } else {
            return Some((false, "_"));
        }
    }
    if let Some(rest_mut) = rest.strip_prefix("mut ") {
        Some((true, rest_mut.trim_start()))
    } else {
        Some((false, rest))
    }
}

// ─── Tests ─────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normalize_replaces_self_with_target() {
        assert_eq!(normalize_type_string("&Self", "Token", ""), "&Token");
    }

    #[test]
    fn normalize_replaces_core_with_std() {
        assert_eq!(
            normalize_type_string("core::fmt::Result", "Token", ""),
            "std::fmt::Result"
        );
    }

    #[test]
    fn normalize_qualifies_formatter() {
        assert_eq!(
            normalize_type_string("&mut Formatter<'_>", "Token", "std::fmt::Display"),
            "&mut std::fmt::Formatter<'_>"
        );
    }

    #[test]
    fn normalize_does_not_qualify_impl_target() {
        assert_eq!(
            normalize_type_string("&Token", "Token", "std::fmt::Display"),
            "&Token"
        );
    }

    #[test]
    fn normalize_combined_display_fmt_params() {
        // Simulates what rustdex returns for Display::fmt after Self→target
        assert_eq!(
            normalize_type_string("&Self", "Token", "std::fmt::Display"),
            "&Token"
        );
        assert_eq!(
            normalize_type_string("Result", "Token", "core::fmt::Display"),
            "std::fmt::Result"
        );
    }

    #[test]
    fn convert_receiver_modes() {
        assert_eq!(
            convert_receiver(&rustdex::ReceiverMode::Ref),
            CapabilityReceiverMode::Borrowed
        );
        assert_eq!(
            convert_receiver(&rustdex::ReceiverMode::RefMut),
            CapabilityReceiverMode::MutBorrowed
        );
        assert_eq!(
            convert_receiver(&rustdex::ReceiverMode::Owned),
            CapabilityReceiverMode::Owned
        );
        assert_eq!(
            convert_receiver(&rustdex::ReceiverMode::None),
            CapabilityReceiverMode::Owned
        );
    }

    #[test]
    fn convert_param_modes() {
        let ref_param = rustdex::ParamSig {
            name: "x".to_string(),
            ty: "&str".to_string(),
            is_ref: true,
            is_mut_ref: false,
        };
        assert_eq!(convert_param_mode(&ref_param), CallArgMode::Borrowed);

        let owned_param = rustdex::ParamSig {
            name: "x".to_string(),
            ty: "String".to_string(),
            is_ref: false,
            is_mut_ref: false,
        };
        assert_eq!(convert_param_mode(&owned_param), CallArgMode::Owned);

        let mut_ref_param = rustdex::ParamSig {
            name: "x".to_string(),
            ty: "&mut Vec<i64>".to_string(),
            is_ref: false,
            is_mut_ref: true,
        };
        assert_eq!(convert_param_mode(&mut_ref_param), CallArgMode::MutBorrowed);
    }

    #[test]
    fn parse_primitives() {
        assert_eq!(
            parse_rustdoc_type_str("bool", &[], "Vec"),
            named_type("bool")
        );
        assert_eq!(parse_rustdoc_type_str("i64", &[], "Vec"), named_type("i64"));
        assert_eq!(parse_rustdoc_type_str("()", &[], "Vec"), SemType::Unit);
    }

    #[test]
    fn parse_generics() {
        let args = vec![named_type("i64")];
        assert_eq!(parse_rustdoc_type_str("T", &args, "Vec"), SemType::Unknown);
        assert_eq!(parse_rustdoc_type_str("T", &[], "Vec"), SemType::Unknown);
        assert_eq!(parse_rustdoc_type_str("I", &args, "Vec"), SemType::Unknown);
    }

    #[test]
    fn method_capability_binds_container_generic_by_name() {
        let sig = rustdex::MethodSig {
            name: "first".to_string(),
            receiver: rustdex::ReceiverMode::Ref,
            impl_type_params: vec!["T".to_string()],
            method_type_params: Vec::new(),
            method_where_predicates: Vec::new(),
            params: Vec::new(),
            return_type: "Option<&T>".to_string(),
        };
        let (capability, unresolved) = method_sig_to_capability(&sig, "Vec", &[named_type("i64")]);
        assert!(unresolved.is_empty());
        assert_eq!(
            capability.return_ty,
            option_type(SemType::Ref {
                mutable: false,
                inner: Box::new(named_type("i64"))
            })
        );
    }

    #[test]
    fn method_capability_reports_unresolved_method_generic() {
        let sig = rustdex::MethodSig {
            name: "get".to_string(),
            receiver: rustdex::ReceiverMode::Ref,
            impl_type_params: vec!["T".to_string()],
            method_type_params: vec!["I: SliceIndex<[T]>".to_string()],
            method_where_predicates: Vec::new(),
            params: vec![rustdex::ParamSig {
                name: "index".to_string(),
                ty: "I".to_string(),
                is_ref: false,
                is_mut_ref: false,
            }],
            return_type: "Option<&I>".to_string(),
        };
        let (_capability, unresolved) =
            method_sig_to_capability(&sig, "Vec", &[named_type("i64")]);
        assert!(unresolved.contains(&"I".to_string()));
    }

    #[test]
    fn parse_option() {
        assert_eq!(
            parse_rustdoc_type_str("Option<T>", &[named_type("i64")], "Vec"),
            option_type(SemType::Unknown)
        );
    }

    #[test]
    fn parse_references_preserves_ref_semantics() {
        assert_eq!(
            parse_rustdoc_type_str("&T", &[named_type("i64")], "Vec"),
            SemType::Ref {
                mutable: false,
                inner: Box::new(SemType::Unknown),
            }
        );
        assert_eq!(
            parse_rustdoc_type_str("&mut T", &[named_type("i64")], "Vec"),
            SemType::Ref {
                mutable: true,
                inner: Box::new(SemType::Unknown),
            }
        );
        assert_eq!(
            parse_rustdoc_type_str("&str", &[named_type("i64")], "Vec"),
            SemType::Ref {
                mutable: false,
                inner: Box::new(named_type("str")),
            }
        );
    }
}
