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
) -> MethodCapability {
    let receiver_mode = convert_receiver(&sig.receiver);

    let mut arg_modes = Vec::with_capacity(sig.params.len());
    let mut expected_args = Vec::with_capacity(sig.params.len());

    for param in &sig.params {
        arg_modes.push(convert_param_mode(param));
        expected_args.push(parse_rustdoc_type_str(&param.ty, generic_args, type_name));
    }

    let return_ty = parse_rustdoc_type_str(&sig.return_type, generic_args, type_name);

    MethodCapability {
        receiver_mode,
        arg_modes,
        expected_args,
        return_ty,
    }
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
) -> TraitMethodSignatureOverride {
    let impl_target_rust = rust_owned_type_string(ctx.impl_target_sem);

    // Build the raw Rust type strings including receiver
    let mut param_rust_types = build_param_rust_types(sig, ctx);

    // Normalize all param types: Self→target, core::→std::, qualify short names
    for ty in &mut param_rust_types {
        *ty = normalize_type_string(ty, &impl_target_rust, ctx.trait_path);
    }

    // Build semantic types from normalized strings
    let param_sem_types: Vec<SemType> = param_rust_types
        .iter()
        .map(|ty| sem_type_from_rust_type(ty, ctx.impl_target_sem))
        .collect();

    // Normalize return type
    let mut return_rust_type = sig.return_type.clone();
    return_rust_type = normalize_type_string(&return_rust_type, &impl_target_rust, ctx.trait_path);
    let return_sem_type = sem_type_from_rust_type(&return_rust_type, ctx.impl_target_sem);

    TraitMethodSignatureOverride {
        param_sem_types,
        return_sem_type,
        param_rust_types,
        return_rust_type,
    }
}

// ─── Internal: enum conversion ─────────────────────────────────────────

/// Map rustdex `ReceiverMode` → Elevate `CapabilityReceiverMode`.
fn convert_receiver(mode: &rustdex::ReceiverMode) -> CapabilityReceiverMode {
    match mode {
        rustdex::ReceiverMode::Ref | rustdex::ReceiverMode::RefMut => {
            CapabilityReceiverMode::Borrowed
        }
        rustdex::ReceiverMode::Owned | rustdex::ReceiverMode::None => CapabilityReceiverMode::Owned,
    }
}

/// Map a rustdex `ParamSig` → Elevate `CallArgMode`.
fn convert_param_mode(param: &rustdex::ParamSig) -> CallArgMode {
    if param.is_ref || param.is_mut_ref {
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

/// Convert a raw Rust type string into an Elevate `SemType`.
///
/// Handles references, `Self`, generics, primitives, and qualified paths.
/// Used for trait method signatures where the type strings have already
/// been normalized.
fn sem_type_from_rust_type(ty: &str, impl_target: &SemType) -> SemType {
    let mut inner = ty.trim();

    // "self" → target type
    if inner == "self" {
        return impl_target.clone();
    }

    // Strip reference qualifiers: &, &mut
    while let Some(stripped) = inner.strip_prefix('&') {
        inner = stripped.trim_start();
        if let Some(stripped_mut) = inner.strip_prefix("mut ") {
            inner = stripped_mut.trim_start();
        }
    }
    if let Some(stripped_mut) = inner.strip_prefix("mut ") {
        inner = stripped_mut.trim_start();
    }

    // Strip surrounding parens (e.g. `(Self)`)
    inner = inner.trim_matches(|ch: char| ch == '(' || ch == ')');

    if inner == "Self" {
        return impl_target.clone();
    }
    if inner == "_" {
        return SemType::Unknown;
    }
    if inner == "()" {
        return SemType::Unit;
    }

    // Split at first '<' to get the base type name
    let without_generics = inner.split('<').next().unwrap_or(inner).trim();
    if without_generics.is_empty() {
        return SemType::Unknown;
    }

    SemType::Path {
        path: without_generics
            .split("::")
            .map(|s| s.to_string())
            .collect(),
        args: Vec::new(),
    }
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
    let s = s.trim();
    if s == "()" {
        return SemType::Unit;
    }
    if s == "Self" {
        return named_type(type_name);
    }

    // Single-letter generic params: map T→first, K→first, V→second
    if s.len() == 1 && s.chars().next().map_or(false, |c| c.is_ascii_uppercase()) {
        let idx = match s {
            "K" => 0,
            "V" => 1,
            _ => 0, // T, A, etc. → first
        };
        return generic_args.get(idx).cloned().unwrap_or(SemType::Unknown);
    }

    // Primitives
    match s {
        "bool" => return named_type("bool"),
        "usize" | "isize" | "u8" | "u16" | "u32" | "u64" | "u128" | "i8" | "i16" | "i32"
        | "i64" | "i128" | "f32" | "f64" => return named_type(s),
        "char" => return named_type("char"),
        "String" | "str" => return named_type("String"),
        _ => {}
    }

    // Handle Option<...>, Vec<...>, etc.
    if let Some(inner_start) = s.find('<') {
        let outer = &s[..inner_start];
        let inner_str = &s[inner_start + 1..s.len().saturating_sub(1)]; // strip < and >

        match outer {
            "Option" => {
                let inner = parse_rustdoc_type_str(inner_str, generic_args, type_name);
                return option_type(inner);
            }
            "Vec" => {
                let inner = parse_rustdoc_type_str(inner_str, generic_args, type_name);
                return SemType::Path {
                    path: vec!["Vec".to_string()],
                    args: vec![inner],
                };
            }
            _ => {
                return named_type(outer);
            }
        }
    }

    // Reference types: &str, &T, &mut T
    if let Some(rest) = s.strip_prefix("&mut ") {
        return parse_rustdoc_type_str(rest, generic_args, type_name);
    }
    if let Some(rest) = s.strip_prefix('&') {
        return parse_rustdoc_type_str(rest, generic_args, type_name);
    }

    named_type(s)
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
            CapabilityReceiverMode::Borrowed
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
        assert_eq!(parse_rustdoc_type_str("T", &args, "Vec"), named_type("i64"));
        assert_eq!(parse_rustdoc_type_str("T", &[], "Vec"), SemType::Unknown);
    }

    #[test]
    fn parse_option() {
        let args = vec![named_type("i64")];
        assert_eq!(
            parse_rustdoc_type_str("Option<T>", &args, "Vec"),
            option_type(named_type("i64"))
        );
    }
}
