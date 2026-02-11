use super::{
    Context, Diagnostic, HashMap, HashSet, SemType, named_type, rustdex_has_method_strict,
    substitute_generic_type,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum CapabilityIndexMode {
    DirectIndex,
    GetLikeOption,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct IndexCapability {
    pub(super) mode: CapabilityIndexMode,
    pub(super) key_ty: SemType,
    pub(super) value_ty: SemType,
}

pub(super) fn resolve_index_capability(
    base_ty: &SemType,
    context: Option<&Context>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<IndexCapability> {
    let SemType::Path { path, args } = base_ty else {
        return None;
    };
    let type_name = path
        .last()
        .map(|segment| segment.as_str())
        .unwrap_or_default();
    let builtin = match type_name {
        "Vec" => Some(IndexCapability {
            mode: CapabilityIndexMode::DirectIndex,
            key_ty: named_type("usize"),
            value_ty: args.first().cloned().unwrap_or(SemType::Unknown),
        }),
        "HashMap" | "BTreeMap" => {
            if rustdex_has_method_strict(type_name, "get", diagnostics).is_err() {
                return None;
            }
            Some(IndexCapability {
                mode: CapabilityIndexMode::GetLikeOption,
                key_ty: args.first().cloned().unwrap_or(SemType::Unknown),
                value_ty: args.get(1).cloned().unwrap_or(SemType::Unknown),
            })
        }
        _ => None,
    };
    if builtin.is_some() {
        return builtin;
    }
    resolve_custom_index_capability(type_name, args, context)
}

fn resolve_custom_index_capability(
    type_name: &str,
    receiver_args: &[SemType],
    context: Option<&Context>,
) -> Option<IndexCapability> {
    let context = context?;
    let instantiate = |ty: &SemType| -> SemType {
        let Some(param_names) = context.struct_type_params.get(type_name) else {
            return ty.clone();
        };
        let type_param_set = param_names.iter().cloned().collect::<HashSet<_>>();
        let bindings = param_names
            .iter()
            .zip(receiver_args.iter())
            .map(|(name, sem)| (name.clone(), sem.clone()))
            .collect::<HashMap<_, _>>();
        substitute_generic_type(ty, &type_param_set, &bindings)
    };

    let get_name = format!("{type_name}::get");
    if let Some(sig) = context.functions.get(&get_name)
        && let Some(key_param) = index_key_param(sig.params.as_slice())
    {
        let key_ty = instantiate(key_param);
        let return_ty = instantiate(&sig.return_type);
        if let SemType::Path { path, args } = return_ty
            && path.last().is_some_and(|segment| segment == "Option")
            && args.len() == 1
        {
            return Some(IndexCapability {
                mode: CapabilityIndexMode::GetLikeOption,
                key_ty,
                value_ty: args[0].clone(),
            });
        }
    }

    let index_name = format!("{type_name}::index");
    if let Some(sig) = context.functions.get(&index_name)
        && let Some(key_param) = index_key_param(sig.params.as_slice())
    {
        return Some(IndexCapability {
            mode: CapabilityIndexMode::DirectIndex,
            key_ty: instantiate(key_param),
            value_ty: instantiate(&sig.return_type),
        });
    }
    None
}

fn index_key_param(params: &[SemType]) -> Option<&SemType> {
    match params.len() {
        1 => params.first(),
        2 => params.get(1),
        _ => None,
    }
}
