use std::collections::{BTreeSet, HashMap, HashSet};

use crate::ast::{
    AssignOp, AssignTarget, BinaryOp, Block, DestructurePattern, Expr, Item, Module, Pattern,
    Stmt, StructLiteralField, Type, UnaryOp, Visibility,
};
use crate::diag::{Diagnostic, Span};
use crate::ir::lowered::{
    RustAssignOp, RustAssignTarget, RustBinaryOp, RustConst, RustDestructurePattern, RustEnum,
    RustExpr, RustField, RustFunction, RustImpl, RustItem, RustMatchArm, RustModule, RustParam,
    RustPattern, RustPatternField, RustStatic, RustStmt, RustStruct, RustStructLiteralField,
    RustTypeParam, RustUnaryOp, RustUse,
    RustVariant,
};
use crate::ir::typed::{
    TypedAssignOp, TypedAssignTarget, TypedBinaryOp, TypedConst, TypedDestructurePattern,
    TypedEnum, TypedExpr, TypedExprKind, TypedField, TypedFunction, TypedImpl, TypedItem,
    TypedMatchArm, TypedModule, TypedParam, TypedPattern, TypedRustUse, TypedStatic, TypedStmt,
    TypedStruct, TypedStructLiteralField, TypedTypeParam, TypedUnaryOp, TypedVariant,
    TypedPatternField,
};
use crate::DirectBorrowHint;

#[derive(Debug, Clone, PartialEq, Eq)]
enum SemType {
    Path {
        path: Vec<String>,
        args: Vec<SemType>,
    },
    Tuple(Vec<SemType>),
    Fn {
        params: Vec<SemType>,
        ret: Box<SemType>,
    },
    Iter(Box<SemType>),
    Unit,
    Unknown,
}

#[derive(Debug, Clone)]
struct FunctionSig {
    type_params: Vec<String>,
    type_param_bounds: HashMap<String, Vec<SemType>>,
    params: Vec<SemType>,
    return_type: SemType,
}

#[derive(Debug, Clone)]
struct Context {
    functions: HashMap<String, FunctionSig>,
    structs: HashMap<String, HashMap<String, SemType>>,
    enums: HashMap<String, HashMap<String, Vec<SemType>>>,
    globals: HashMap<String, SemType>,
}

pub fn lower_to_typed(module: &Module) -> Result<TypedModule, Vec<Diagnostic>> {
    let mut diagnostics = Vec::new();
    let mut context = Context {
        functions: HashMap::new(),
        structs: HashMap::new(),
        enums: HashMap::new(),
        globals: HashMap::new(),
    };

    collect_definitions(module, &mut context, &mut diagnostics);
    let items = module
        .items
        .iter()
        .filter_map(|item| lower_item(item, &mut context, &mut diagnostics))
        .collect::<Vec<_>>();

    if diagnostics.is_empty() {
        Ok(TypedModule { items })
    } else {
        Err(diagnostics)
    }
}

pub fn lower_to_rust(module: &TypedModule) -> RustModule {
    lower_to_rust_with_hints(module, &[])
}

pub fn lower_to_rust_with_hints(module: &TypedModule, hints: &[DirectBorrowHint]) -> RustModule {
    let mut state = LoweringState::from_module(module);
    for hint in hints {
        state
            .registry
            .register_direct_borrow_policy(&hint.path, &hint.borrowed_arg_indexes);
    }
    let mut items = Vec::new();
    for item in &module.items {
        let lowered = match item {
            TypedItem::RustUse(def) => RustItem::Use(RustUse {
                path: def.path.clone(),
            }),
            TypedItem::RustBlock(code) => RustItem::Raw(code.clone()),
            TypedItem::Struct(def) => RustItem::Struct(RustStruct {
                is_public: def.is_public,
                name: def.name.clone(),
                fields: def
                    .fields
                    .iter()
                    .map(|field| RustField {
                        name: field.name.clone(),
                        ty: field.ty.clone(),
                    })
                    .collect(),
            }),
            TypedItem::Enum(def) => RustItem::Enum(RustEnum {
                is_public: def.is_public,
                name: def.name.clone(),
                variants: def
                    .variants
                    .iter()
                    .map(|variant| RustVariant {
                        name: variant.name.clone(),
                        payload: variant.payload.clone(),
                    })
                    .collect(),
            }),
            TypedItem::Impl(def) => RustItem::Impl(RustImpl {
                target: def.target.clone(),
                methods: def
                    .methods
                    .iter()
                    .map(|method| lower_function(method, &mut state))
                    .collect(),
            }),
            TypedItem::Function(def) => RustItem::Function(lower_function(def, &mut state)),
            TypedItem::Const(def) => RustItem::Const(RustConst {
                is_public: def.is_public,
                name: def.name.clone(),
                ty: def.ty.clone(),
                value: lower_expr_with_context(
                    &def.value,
                    &mut LoweringContext::default(),
                    ExprPosition::Value,
                    &mut state,
                ),
            }),
            TypedItem::Static(def) => RustItem::Static(RustStatic {
                is_public: def.is_public,
                name: def.name.clone(),
                ty: def.ty.clone(),
                value: lower_expr_with_context(
                    &def.value,
                    &mut LoweringContext::default(),
                    ExprPosition::Value,
                    &mut state,
                ),
            }),
        };
        items.push(lowered);
    }
    for shim_name in &state.used_shims {
        if let Some(shim) = state.registry.resolve_shim_by_name(shim_name) {
            items.push(RustItem::Function(lower_interop_shim(shim)));
        }
    }
    RustModule {
        items,
        ownership_notes: state.ownership_notes.clone(),
    }
}

#[derive(Debug, Default)]
struct LoweringContext {
    ownership_plan: OwnershipPlan,
    scope_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct OwnershipPlace {
    root: String,
    projections: Vec<OwnershipProjection>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum OwnershipProjection {
    Field(String),
    Index,
}

impl OwnershipPlace {
    fn from_root(root: &str) -> Self {
        Self {
            root: root.to_string(),
            projections: Vec::new(),
        }
    }

    fn with_projection(mut self, projection: OwnershipProjection) -> Self {
        self.projections.push(projection);
        self
    }

    fn display_name(&self) -> String {
        let mut out = self.root.clone();
        for segment in &self.projections {
            match segment {
                OwnershipProjection::Field(field) => {
                    out.push('.');
                    out.push_str(field);
                }
                OwnershipProjection::Index => out.push_str("[_]"),
            }
        }
        out
    }
}

#[derive(Debug, Default, Clone)]
struct OwnershipPlan {
    remaining_place_uses: HashMap<OwnershipPlace, usize>,
}

impl OwnershipPlan {
    fn from_stmts(stmts: &[TypedStmt]) -> Self {
        Self {
            remaining_place_uses: collect_place_uses_in_stmts(stmts),
        }
    }

    fn remaining_for_place(&self, place: &OwnershipPlace) -> usize {
        self.remaining_place_uses.get(place).copied().unwrap_or(0)
    }

    fn remaining_conflicting_for_expr(&self, expr: &TypedExpr) -> usize {
        let Some(place) = place_for_expr(expr) else {
            return 0;
        };
        self.remaining_place_uses
            .iter()
            .filter_map(|(other, remaining)| {
                if *remaining > 0 && ownership_places_conflict(&place, other) {
                    Some(*remaining)
                } else {
                    None
                }
            })
            .sum()
    }

    fn consume_expr(&mut self, expr: &TypedExpr) -> usize {
        let Some(place) = place_for_expr(expr) else {
            return 0;
        };
        let remaining = self.remaining_for_place(&place);
        if remaining > 0 {
            self.remaining_place_uses.insert(place, remaining - 1);
        }
        remaining
    }
}

fn ownership_places_conflict(a: &OwnershipPlace, b: &OwnershipPlace) -> bool {
    a.root == b.root
        && (ownership_projection_prefix(&a.projections, &b.projections)
            || ownership_projection_prefix(&b.projections, &a.projections))
}

fn ownership_projection_prefix(
    prefix: &[OwnershipProjection],
    candidate: &[OwnershipProjection],
) -> bool {
    candidate.starts_with(prefix)
}

#[derive(Debug, Default)]
struct LoweringState {
    used_shims: BTreeSet<String>,
    imported_rust_paths: HashMap<String, Vec<Vec<String>>>,
    ownership_notes: Vec<String>,
    ownership_note_keys: BTreeSet<String>,
    registry: InteropPolicyRegistry,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprPosition {
    Value,
    CallArgOwned,
    ProjectionBase,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CallArgMode {
    Owned,
    Borrowed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MethodCallModes {
    receiver: CallArgMode,
    args: Vec<CallArgMode>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct InteropShimDef {
    name: String,
    target_path: Vec<String>,
    param_types: Vec<String>,
    return_type: String,
    borrowed_arg_indexes: Vec<usize>,
    body: InteropShimBody,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InteropShimBody {
    DirectCall,
    StrStripPrefixKnown,
    StrSplitOnceKnown,
}

#[derive(Debug, Default)]
struct InteropPolicyRegistry {
    shim_policies: HashMap<String, String>,
    shim_defs: HashMap<String, InteropShimDef>,
    direct_borrow_policies: HashMap<String, Vec<usize>>,
    cloneable_types: BTreeSet<String>,
    observed_import_paths: BTreeSet<String>,
}

impl LoweringState {
    fn from_module(module: &TypedModule) -> Self {
        let mut state = Self {
            registry: InteropPolicyRegistry::with_builtins(),
            ..Self::default()
        };
        for item in &module.items {
            match item {
                TypedItem::RustUse(def) => {
                    if let Some(name) = def.path.last() {
                        state
                            .imported_rust_paths
                            .entry(name.clone())
                            .or_default()
                            .push(def.path.clone());
                    }
                    state.registry.observe_import_path(&def.path);
                }
                TypedItem::Struct(def) => {
                    state.registry.register_user_cloneable_type(&def.name);
                }
                TypedItem::Enum(def) => {
                    state.registry.register_user_cloneable_type(&def.name);
                }
                TypedItem::RustBlock(_) => {}
                _ => {}
            }
        }
        state
    }

    fn resolve_callee_path(&self, callee_path: &[String]) -> Vec<String> {
        if callee_path.len() != 1 {
            return callee_path.to_vec();
        }
        let Some(candidates) = self.imported_rust_paths.get(&callee_path[0]) else {
            return callee_path.to_vec();
        };
        if candidates.len() == 1 {
            return candidates[0].clone();
        }
        callee_path.to_vec()
    }

    fn push_ownership_note(&mut self, note: String) {
        if self.ownership_note_keys.insert(note.clone()) {
            self.ownership_notes.push(note);
        }
    }
}

impl InteropPolicyRegistry {
    fn with_builtins() -> Self {
        let mut registry = Self::default();
        registry.register_builtin_shims();
        registry.register_builtin_direct_borrows();
        registry.register_builtin_cloneable_types();
        registry
    }

    fn register_builtin_shims(&mut self) {
        self.register_function_shim(
            &["str::len", "std::str::len"],
            "__elevate_shim_str_len",
            &["str", "len"],
            &["&str"],
            "usize",
            &[0],
        );
        self.register_function_shim(
            &["str::is_empty", "std::str::is_empty"],
            "__elevate_shim_str_is_empty",
            &["str", "is_empty"],
            &["&str"],
            "bool",
            &[0],
        );
        self.register_function_shim(
            &["str::contains", "std::str::contains"],
            "__elevate_shim_str_contains",
            &["str", "contains"],
            &["&str", "&str"],
            "bool",
            &[0, 1],
        );
        self.register_function_shim(
            &["str::starts_with", "std::str::starts_with"],
            "__elevate_shim_str_starts_with",
            &["str", "starts_with"],
            &["&str", "&str"],
            "bool",
            &[0, 1],
        );
        self.register_function_shim(
            &["str::ends_with", "std::str::ends_with"],
            "__elevate_shim_str_ends_with",
            &["str", "ends_with"],
            &["&str", "&str"],
            "bool",
            &[0, 1],
        );
        self.register_custom_shim(
            &[
                "str::strip_prefix_known",
                "std::str::strip_prefix_known",
                "str::strip_prefix_or_panic",
                "std::str::strip_prefix_or_panic",
            ],
            "__elevate_shim_str_strip_prefix_known",
            &["str", "strip_prefix"],
            &["&str", "&str"],
            "String",
            &[0, 1],
            InteropShimBody::StrStripPrefixKnown,
        );
        self.register_custom_shim(
            &[
                "str::split_once_known",
                "std::str::split_once_known",
                "str::split_once_or_panic",
                "std::str::split_once_or_panic",
            ],
            "__elevate_shim_str_split_once_known",
            &["str", "split_once"],
            &["&str", "&str"],
            "(String, String)",
            &[0, 1],
            InteropShimBody::StrSplitOnceKnown,
        );
    }

    fn register_builtin_direct_borrows(&mut self) {
        for path in [
            "String::len",
            "String::is_empty",
            "std::string::String::len",
            "std::string::String::is_empty",
            "alloc::string::String::len",
            "alloc::string::String::is_empty",
            "Option::is_some",
            "Option::is_none",
            "std::option::Option::is_some",
            "std::option::Option::is_none",
            "core::option::Option::is_some",
            "core::option::Option::is_none",
            "Result::is_ok",
            "Result::is_err",
            "std::result::Result::is_ok",
            "std::result::Result::is_err",
            "core::result::Result::is_ok",
            "core::result::Result::is_err",
            "Vec::len",
            "Vec::is_empty",
            "std::vec::Vec::len",
            "std::vec::Vec::is_empty",
            "alloc::vec::Vec::len",
            "alloc::vec::Vec::is_empty",
            "HashMap::len",
            "HashMap::is_empty",
            "std::collections::HashMap::len",
            "std::collections::HashMap::is_empty",
            "BTreeMap::len",
            "BTreeMap::is_empty",
            "std::collections::BTreeMap::len",
            "std::collections::BTreeMap::is_empty",
        ] {
            self.register_direct_borrow_policy(path, &[0]);
        }
        for path in [
            "HashMap::contains_key",
            "std::collections::HashMap::contains_key",
            "BTreeMap::contains_key",
            "std::collections::BTreeMap::contains_key",
            "HashSet::contains",
            "std::collections::HashSet::contains",
            "BTreeSet::contains",
            "std::collections::BTreeSet::contains",
        ] {
            self.register_direct_borrow_policy(path, &[0, 1]);
        }
    }

    fn register_builtin_cloneable_types(&mut self) {
        self.cloneable_types.insert("String".to_string());
    }

    fn register_function_shim(
        &mut self,
        source_paths: &[&str],
        shim_name: &str,
        target_path: &[&str],
        param_types: &[&str],
        return_type: &str,
        borrowed_arg_indexes: &[usize],
    ) {
        let def = InteropShimDef {
            name: shim_name.to_string(),
            target_path: target_path.iter().map(|part| (*part).to_string()).collect(),
            param_types: param_types.iter().map(|ty| (*ty).to_string()).collect(),
            return_type: return_type.to_string(),
            borrowed_arg_indexes: borrowed_arg_indexes.to_vec(),
            body: InteropShimBody::DirectCall,
        };
        self.shim_defs.insert(shim_name.to_string(), def);
        for path in source_paths {
            self.shim_policies
                .insert((*path).to_string(), shim_name.to_string());
        }
    }

    fn register_custom_shim(
        &mut self,
        source_paths: &[&str],
        shim_name: &str,
        target_path: &[&str],
        param_types: &[&str],
        return_type: &str,
        borrowed_arg_indexes: &[usize],
        body: InteropShimBody,
    ) {
        let def = InteropShimDef {
            name: shim_name.to_string(),
            target_path: target_path.iter().map(|part| (*part).to_string()).collect(),
            param_types: param_types.iter().map(|ty| (*ty).to_string()).collect(),
            return_type: return_type.to_string(),
            borrowed_arg_indexes: borrowed_arg_indexes.to_vec(),
            body,
        };
        self.shim_defs.insert(shim_name.to_string(), def);
        for path in source_paths {
            self.shim_policies
                .insert((*path).to_string(), shim_name.to_string());
        }
    }

    fn register_direct_borrow_policy(&mut self, path: &str, borrowed_indexes: &[usize]) {
        self.direct_borrow_policies
            .insert(path.to_string(), borrowed_indexes.to_vec());
    }

    fn register_user_cloneable_type(&mut self, type_name: &str) {
        self.cloneable_types.insert(type_name.to_string());
    }

    fn observe_import_path(&mut self, path: &[String]) {
        self.observed_import_paths.insert(path.join("::"));
    }

    fn resolve_shim(&self, path: &[String]) -> Option<&InteropShimDef> {
        let shim_name = self.shim_policies.get(&path.join("::"))?;
        self.shim_defs.get(shim_name)
    }

    fn resolve_shim_by_name(&self, name: &str) -> Option<&InteropShimDef> {
        self.shim_defs.get(name)
    }

    fn resolve_direct_borrow_modes(
        &self,
        path: &[String],
        arg_count: usize,
    ) -> Option<Vec<CallArgMode>> {
        let borrowed_indexes = self.direct_borrow_policies.get(&path.join("::"))?;
        let mut modes = vec![CallArgMode::Owned; arg_count];
        for index in borrowed_indexes {
            set_borrowed(&mut modes, *index);
        }
        Some(modes)
    }

    fn is_clone_candidate(&self, ty: &str) -> bool {
        let ty = ty.trim();
        if ty == "_" {
            return false;
        }
        let (head, args) = split_type_head_and_args(ty);
        if args.is_empty() && is_copy_primitive_type(head) {
            return false;
        }
        if let Some(items) = parse_tuple_items(ty) {
            return items
                .iter()
                .all(|item| self.is_clone_candidate_or_copy(item));
        }

        if self.cloneable_types.contains(head)
            || self.cloneable_types.contains(last_path_segment(head))
        {
            return args.iter().all(|arg| self.is_clone_candidate_or_copy(arg));
        }

        if is_known_clone_container(head) {
            return args.iter().all(|arg| self.is_clone_candidate_or_copy(arg));
        }

        // Imported nominal types are treated as optimistic clone candidates so
        // lowering can preserve source ergonomics for repeated by-value use.
        let head_last = last_path_segment(head);
        if is_probably_nominal_type(head_last)
            && self.import_looks_like_type(head_last)
        {
            return args.iter().all(|arg| self.is_clone_candidate_or_copy(arg));
        }

        false
    }

    fn is_clone_candidate_or_copy(&self, ty: &str) -> bool {
        let (head, args) = split_type_head_and_args(ty.trim());
        if args.is_empty() && is_copy_primitive_type(head) {
            return true;
        }
        self.is_clone_candidate(ty)
    }

    fn import_looks_like_type(&self, type_name: &str) -> bool {
        self.observed_import_paths
            .iter()
            .any(|path| last_path_segment(path) == type_name)
    }
}

fn lower_function(def: &TypedFunction, state: &mut LoweringState) -> RustFunction {
    let mut context = LoweringContext {
        ownership_plan: OwnershipPlan::from_stmts(&def.body),
        scope_name: def.name.clone(),
    };
    RustFunction {
        is_public: def.is_public,
        name: def.name.clone(),
        type_params: def
            .type_params
            .iter()
            .map(|param| RustTypeParam {
                name: param.name.clone(),
                bounds: param.bounds.clone(),
            })
            .collect(),
        params: def
            .params
            .iter()
            .map(|param| RustParam {
                name: param.name.clone(),
                ty: param.ty.clone(),
            })
            .collect(),
        return_type: def.return_type.clone(),
        body: def
            .body
            .iter()
            .map(|stmt| lower_stmt_with_context(stmt, &mut context, state))
            .collect(),
    }
}

fn collect_definitions(module: &Module, context: &mut Context, _diagnostics: &mut Vec<Diagnostic>) {
    for item in &module.items {
        match item {
            Item::Struct(def) => {
                let fields = def
                    .fields
                    .iter()
                    .map(|field| (field.name.clone(), type_from_ast(&field.ty)))
                    .collect::<HashMap<_, _>>();
                context.structs.insert(def.name.clone(), fields);
            }
            Item::Enum(def) => {
                let variants = def
                    .variants
                    .iter()
                    .map(|variant| {
                        (
                            variant.name.clone(),
                            variant.payload.iter().map(type_from_ast).collect::<Vec<_>>(),
                        )
                    })
                    .collect::<HashMap<_, _>>();
                context.enums.insert(def.name.clone(), variants);
            }
            Item::Function(def) => {
                let sig = FunctionSig {
                    type_params: def.type_params.iter().map(|param| param.name.clone()).collect(),
                    type_param_bounds: def
                        .type_params
                        .iter()
                        .map(|param| {
                            (
                                param.name.clone(),
                                param.bounds.iter().map(type_from_ast).collect::<Vec<_>>(),
                            )
                        })
                        .collect(),
                    params: def
                        .params
                        .iter()
                        .map(|param| type_from_ast(&param.ty))
                        .collect(),
                    return_type: def
                        .return_type
                        .as_ref()
                        .map(type_from_ast)
                        .unwrap_or(SemType::Unit),
                };
                context.functions.insert(def.name.clone(), sig);
            }
            Item::Impl(def) => {
                for method in &def.methods {
                    let sig = FunctionSig {
                        type_params: method
                            .type_params
                            .iter()
                            .map(|param| param.name.clone())
                            .collect(),
                        type_param_bounds: method
                            .type_params
                            .iter()
                            .map(|param| {
                                (
                                    param.name.clone(),
                                    param.bounds
                                        .iter()
                                        .map(|bound| type_from_ast_with_impl_self(bound, &def.target))
                                        .collect::<Vec<_>>(),
                                )
                            })
                            .collect(),
                        params: method
                            .params
                            .iter()
                            .map(|param| type_from_ast_with_impl_self(&param.ty, &def.target))
                            .collect(),
                        return_type: method
                            .return_type
                            .as_ref()
                            .map(|ty| type_from_ast_with_impl_self(ty, &def.target))
                            .unwrap_or(SemType::Unit),
                    };
                    context
                        .functions
                        .insert(format!("{}::{}", def.target, method.name), sig);
                }
            }
            Item::Const(def) => {
                let ty = def
                    .ty
                    .as_ref()
                    .map(type_from_ast)
                    .unwrap_or(SemType::Unknown);
                context.globals.insert(def.name.clone(), ty);
            }
            Item::Static(def) => {
                context
                    .globals
                    .insert(def.name.clone(), type_from_ast(&def.ty));
            }
            Item::RustUse(_) => {}
            Item::RustBlock(_) => {}
        }
    }
}

fn lower_item(
    item: &Item,
    context: &mut Context,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<TypedItem> {
    match item {
        Item::RustUse(def) => Some(TypedItem::RustUse(TypedRustUse {
            path: def.path.clone(),
        })),
        Item::RustBlock(code) => Some(TypedItem::RustBlock(code.clone())),
        Item::Struct(def) => Some(TypedItem::Struct(TypedStruct {
            is_public: def.visibility == Visibility::Public,
            name: def.name.clone(),
            fields: def
                .fields
                .iter()
                .map(|field| TypedField {
                    name: field.name.clone(),
                    ty: type_to_string(&type_from_ast(&field.ty)),
                })
                .collect(),
        })),
        Item::Enum(def) => Some(TypedItem::Enum(TypedEnum {
            is_public: def.visibility == Visibility::Public,
            name: def.name.clone(),
            variants: def
                .variants
                .iter()
                .map(|variant| TypedVariant {
                    name: variant.name.clone(),
                    payload: variant
                        .payload
                        .iter()
                        .map(type_from_ast)
                        .map(|ty| type_to_string(&ty))
                        .collect(),
                })
                .collect(),
        })),
        Item::Impl(def) => {
            let mut methods = Vec::new();
            for method in &def.methods {
                let mut locals = HashMap::new();
                let mut immutable_locals = HashSet::new();
                for param in &method.params {
                    locals.insert(
                        param.name.clone(),
                        type_from_ast_with_impl_self(&param.ty, &def.target),
                    );
                }
                let declared_return_ty = method
                    .return_type
                    .as_ref()
                    .map(|ty| type_from_ast_with_impl_self(ty, &def.target));
                let provisional_return_ty = declared_return_ty.clone().unwrap_or(SemType::Unknown);
                let mut body = Vec::new();
                let mut inferred_returns = Vec::new();
                for (index, statement) in method.body.statements.iter().enumerate() {
                    let is_last = index + 1 == method.body.statements.len();
                    if is_last && let Stmt::TailExpr(expr) = statement {
                        let (typed_expr, inferred) = infer_expr(
                            expr,
                            context,
                            &mut locals,
                            &provisional_return_ty,
                            diagnostics,
                        );
                        inferred_returns.push(inferred.clone());
                        if provisional_return_ty != SemType::Unknown
                            && !is_compatible(&inferred, &provisional_return_ty)
                        {
                            diagnostics.push(Diagnostic::new(
                                format!(
                                    "Return type mismatch: expected `{}`, got `{}`",
                                    type_to_string(&provisional_return_ty),
                                    type_to_string(&inferred)
                                ),
                                Span::new(0, 0),
                            ));
                        }
                        body.push(TypedStmt::Return(Some(typed_expr)));
                        continue;
                    }
                    if let Some(stmt) = lower_stmt_with_types(
                        statement,
                        context,
                        &mut locals,
                        &mut immutable_locals,
                        &provisional_return_ty,
                        &mut inferred_returns,
                        diagnostics,
                    ) {
                        body.push(stmt);
                    }
                }
                let final_return_ty = declared_return_ty.unwrap_or_else(|| {
                    infer_return_type(&inferred_returns, diagnostics, &method.name)
                });
                if contains_unknown(&final_return_ty) {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Method `{}` return type could not be fully inferred; add an explicit return type",
                            method.name
                        ),
                        Span::new(0, 0),
                    ));
                }
                if final_return_ty != SemType::Unit && inferred_returns.is_empty() {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Method `{}` must explicitly return `{}`",
                            method.name,
                            type_to_string(&final_return_ty)
                        ),
                        Span::new(0, 0),
                    ));
                }
                methods.push(TypedFunction {
                    is_public: method.visibility == Visibility::Public,
                    name: method.name.clone(),
                    type_params: method
                        .type_params
                        .iter()
                        .map(|param| TypedTypeParam {
                            name: param.name.clone(),
                            bounds: param
                                .bounds
                                .iter()
                                .map(|bound| type_to_string(&type_from_ast_with_impl_self(bound, &def.target)))
                                .collect(),
                        })
                        .collect(),
                    params: method
                        .params
                        .iter()
                        .map(|param| TypedParam {
                            name: param.name.clone(),
                            ty: type_to_string(&type_from_ast_with_impl_self(&param.ty, &def.target)),
                        })
                        .collect(),
                    return_type: type_to_string(&final_return_ty),
                    body,
                });
            }
            Some(TypedItem::Impl(TypedImpl {
                target: def.target.clone(),
                methods,
            }))
        }
        Item::Const(def) => {
            let mut locals = HashMap::new();
            let (typed_value, inferred) = infer_expr(
                &def.value,
                context,
                &mut locals,
                &SemType::Unit,
                diagnostics,
            );
            let declared = def.ty.as_ref().map(type_from_ast);
            let final_ty = if let Some(declared_ty) = declared {
                if !is_compatible(&inferred, &declared_ty) {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Const `{}` type mismatch: expected `{}`, got `{}`",
                            def.name,
                            type_to_string(&declared_ty),
                            type_to_string(&inferred)
                        ),
                        Span::new(0, 0),
                    ));
                }
                declared_ty
            } else {
                inferred
            };
            if contains_unknown(&final_ty) {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Const `{}` type could not be fully inferred; add an explicit type",
                        def.name
                    ),
                    Span::new(0, 0),
                ));
            }
            Some(TypedItem::Const(TypedConst {
                is_public: def.visibility == Visibility::Public,
                name: def.name.clone(),
                ty: type_to_string(&final_ty),
                value: typed_value,
            }))
        }
        Item::Static(def) => {
            let mut locals = HashMap::new();
            let (typed_value, inferred) = infer_expr(
                &def.value,
                context,
                &mut locals,
                &SemType::Unit,
                diagnostics,
            );
            let declared = type_from_ast(&def.ty);
            if !is_compatible(&inferred, &declared) {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Static `{}` type mismatch: expected `{}`, got `{}`",
                        def.name,
                        type_to_string(&declared),
                        type_to_string(&inferred)
                    ),
                    Span::new(0, 0),
                ));
            }
            if contains_unknown(&declared) {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Static `{}` type must be concrete and cannot include `_`",
                        def.name
                    ),
                    Span::new(0, 0),
                ));
            }
            Some(TypedItem::Static(TypedStatic {
                is_public: def.visibility == Visibility::Public,
                name: def.name.clone(),
                ty: type_to_string(&declared),
                value: typed_value,
            }))
        }
        Item::Function(def) => {
            let mut locals = HashMap::new();
            let mut immutable_locals = HashSet::new();
            for param in &def.params {
                locals.insert(param.name.clone(), type_from_ast(&param.ty));
            }
            let declared_return_ty = def.return_type.as_ref().map(type_from_ast);
            let provisional_return_ty = declared_return_ty.clone().unwrap_or(SemType::Unknown);
            let mut body = Vec::new();
            let mut inferred_returns = Vec::new();
            for (index, statement) in def.body.statements.iter().enumerate() {
                let is_last = index + 1 == def.body.statements.len();
                if is_last && let Stmt::TailExpr(expr) = statement {
                    let (typed_expr, inferred) = infer_expr(
                        expr,
                        context,
                        &mut locals,
                        &provisional_return_ty,
                        diagnostics,
                    );
                    inferred_returns.push(inferred.clone());
                    if provisional_return_ty != SemType::Unknown
                        && !is_compatible(&inferred, &provisional_return_ty)
                    {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "Return type mismatch: expected `{}`, got `{}`",
                                type_to_string(&provisional_return_ty),
                                type_to_string(&inferred)
                            ),
                            Span::new(0, 0),
                        ));
                    }
                    body.push(TypedStmt::Return(Some(typed_expr)));
                    continue;
                }
                if let Some(stmt) = lower_stmt_with_types(
                    statement,
                    context,
                    &mut locals,
                    &mut immutable_locals,
                    &provisional_return_ty,
                    &mut inferred_returns,
                    diagnostics,
                ) {
                    body.push(stmt);
                }
            }
            let final_return_ty = declared_return_ty
                .unwrap_or_else(|| infer_return_type(&inferred_returns, diagnostics, &def.name));
            if contains_unknown(&final_return_ty) {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Function `{}` return type could not be fully inferred; add an explicit return type",
                        def.name
                    ),
                    Span::new(0, 0),
                ));
            }
            if final_return_ty != SemType::Unit && inferred_returns.is_empty() {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Function `{}` must explicitly return `{}`",
                        def.name,
                        type_to_string(&final_return_ty)
                    ),
                    Span::new(0, 0),
                ));
            }

            Some(TypedItem::Function(TypedFunction {
                is_public: def.visibility == Visibility::Public,
                name: def.name.clone(),
                type_params: def
                    .type_params
                    .iter()
                    .map(|param| TypedTypeParam {
                        name: param.name.clone(),
                        bounds: param
                            .bounds
                            .iter()
                            .map(type_from_ast)
                            .map(|bound| type_to_string(&bound))
                            .collect(),
                    })
                    .collect(),
                params: def
                    .params
                    .iter()
                    .map(|param| TypedParam {
                        name: param.name.clone(),
                        ty: type_to_string(&type_from_ast(&param.ty)),
                    })
                    .collect(),
                return_type: type_to_string(&final_return_ty),
                body,
            }))
        }
    }
}

fn lower_stmt_with_types(
    stmt: &Stmt,
    context: &Context,
    locals: &mut HashMap<String, SemType>,
    immutable_locals: &mut HashSet<String>,
    return_ty: &SemType,
    inferred_returns: &mut Vec<SemType>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<TypedStmt> {
    match stmt {
        Stmt::Const(def) => {
            let (typed_value, inferred) =
                infer_expr(&def.value, context, locals, return_ty, diagnostics);
            let declared = def.ty.as_ref().map(type_from_ast);
            let final_ty = if let Some(declared_ty) = declared {
                if !is_compatible(&inferred, &declared_ty) {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Const `{}` type mismatch: expected `{}`, got `{}`",
                            def.name,
                            type_to_string(&declared_ty),
                            type_to_string(&inferred)
                        ),
                        Span::new(0, 0),
                    ));
                }
                declared_ty
            } else {
                inferred
            };
            locals.insert(def.name.clone(), final_ty.clone());
            immutable_locals.insert(def.name.clone());
            Some(TypedStmt::Const(TypedConst {
                is_public: false,
                name: def.name.clone(),
                ty: type_to_string(&final_ty),
                value: typed_value,
            }))
        }
        Stmt::DestructureConst { pattern, value } => {
            let (typed_value, value_ty) =
                infer_expr(value, context, locals, return_ty, diagnostics);
            bind_destructure_pattern(pattern, &value_ty, locals, diagnostics);
            collect_destructure_pattern_names(pattern, immutable_locals);
            Some(TypedStmt::DestructureConst {
                pattern: lower_destructure_pattern_typed(pattern),
                value: typed_value,
            })
        }
        Stmt::Assign { target, op, value } => {
            let (typed_target, target_ty) = infer_assign_target(
                target,
                context,
                locals,
                immutable_locals,
                return_ty,
                diagnostics,
            )?;
            let (typed_value, value_ty) = infer_expr(value, context, locals, return_ty, diagnostics);
            match op {
                AssignOp::Assign => {
                    if !is_compatible(&value_ty, &target_ty) {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "Assignment type mismatch: expected `{}`, got `{}`",
                                type_to_string(&target_ty),
                                type_to_string(&value_ty)
                            ),
                            Span::new(0, 0),
                        ));
                    }
                }
                AssignOp::AddAssign => {
                    if matches!(typed_target, TypedAssignTarget::Tuple(_)) {
                        diagnostics.push(Diagnostic::new(
                            "`+=` is not supported on tuple assignment targets",
                            Span::new(0, 0),
                        ));
                    }
                    let sum_ty = resolve_add_type(&target_ty, &value_ty, diagnostics);
                    if !is_compatible(&sum_ty, &target_ty) {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "`+=` result type `{}` does not match target `{}`",
                                type_to_string(&sum_ty),
                                type_to_string(&target_ty)
                            ),
                            Span::new(0, 0),
                        ));
                    }
                }
            }
            Some(TypedStmt::Assign {
                target: typed_target,
                op: match op {
                    AssignOp::Assign => TypedAssignOp::Assign,
                    AssignOp::AddAssign => TypedAssignOp::AddAssign,
                },
                value: typed_value,
            })
        }
        Stmt::Return(value) => {
            if let Some(expr) = value {
                let (typed_expr, inferred) =
                    infer_expr(expr, context, locals, return_ty, diagnostics);
                inferred_returns.push(inferred.clone());
                if *return_ty != SemType::Unknown && !is_compatible(&inferred, return_ty) {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Return type mismatch: expected `{}`, got `{}`",
                            type_to_string(return_ty),
                            type_to_string(&inferred)
                        ),
                        Span::new(0, 0),
                    ));
                }
                Some(TypedStmt::Return(Some(typed_expr)))
            } else {
                inferred_returns.push(SemType::Unit);
                if *return_ty != SemType::Unknown && *return_ty != SemType::Unit {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Return type mismatch: expected `{}`, got `()`",
                            type_to_string(return_ty)
                        ),
                        Span::new(0, 0),
                    ));
                }
                Some(TypedStmt::Return(None))
            }
        }
        Stmt::Expr(expr) => {
            let (typed_expr, _) = infer_expr(expr, context, locals, return_ty, diagnostics);
            Some(TypedStmt::Expr(typed_expr))
        }
        Stmt::TailExpr(expr) => {
            let (typed_expr, _) = infer_expr(expr, context, locals, return_ty, diagnostics);
            Some(TypedStmt::Expr(typed_expr))
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            let (typed_condition, condition_ty) =
                infer_expr(condition, context, locals, return_ty, diagnostics);
            if !is_compatible(&condition_ty, &named_type("bool")) {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "`if` condition must be `bool`, got `{}`",
                        type_to_string(&condition_ty)
                    ),
                    Span::new(0, 0),
                ));
            }

            let then_body = lower_block_with_types(
                then_block,
                context,
                locals,
                immutable_locals,
                return_ty,
                inferred_returns,
                diagnostics,
            );
            let else_body = else_block.as_ref().map(|block| {
                lower_block_with_types(
                    block,
                    context,
                    locals,
                    immutable_locals,
                    return_ty,
                    inferred_returns,
                    diagnostics,
                )
            });

            Some(TypedStmt::If {
                condition: typed_condition,
                then_body,
                else_body,
            })
        }
        Stmt::While { condition, body } => {
            let (typed_condition, condition_ty) =
                infer_expr(condition, context, locals, return_ty, diagnostics);
            if !is_compatible(&condition_ty, &named_type("bool")) {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "`while` condition must be `bool`, got `{}`",
                        type_to_string(&condition_ty)
                    ),
                    Span::new(0, 0),
                ));
            }
            let typed_body = lower_block_with_types(
                body,
                context,
                locals,
                immutable_locals,
                return_ty,
                inferred_returns,
                diagnostics,
            );
            Some(TypedStmt::While {
                condition: typed_condition,
                body: typed_body,
            })
        }
        Stmt::For {
            binding,
            iter,
            body,
        } => {
            let (typed_iter, iter_ty) = infer_expr(iter, context, locals, return_ty, diagnostics);
            let item_ty = infer_for_item_type(&typed_iter, &iter_ty);
            let mut loop_locals = locals.clone();
            let mut loop_immutable_locals = immutable_locals.clone();
            bind_destructure_pattern(binding, &item_ty, &mut loop_locals, diagnostics);
            collect_destructure_pattern_names(binding, &mut loop_immutable_locals);
            let typed_body = lower_block_with_types(
                body,
                context,
                &loop_locals,
                &loop_immutable_locals,
                return_ty,
                inferred_returns,
                diagnostics,
            );
            Some(TypedStmt::For {
                binding: lower_destructure_pattern_typed(binding),
                iter: typed_iter,
                body: typed_body,
            })
        }
        Stmt::Loop { body } => {
            let typed_body = lower_block_with_types(
                body,
                context,
                locals,
                immutable_locals,
                return_ty,
                inferred_returns,
                diagnostics,
            );
            Some(TypedStmt::Loop { body: typed_body })
        }
        Stmt::Break => Some(TypedStmt::Break),
        Stmt::Continue => Some(TypedStmt::Continue),
        Stmt::RustBlock(code) => Some(TypedStmt::RustBlock(code.clone())),
    }
}

fn infer_assign_target(
    target: &AssignTarget,
    context: &Context,
    locals: &mut HashMap<String, SemType>,
    immutable_locals: &HashSet<String>,
    return_ty: &SemType,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<(TypedAssignTarget, SemType)> {
    match target {
        AssignTarget::Path(name) => {
            if immutable_locals.contains(name) {
                diagnostics.push(Diagnostic::new(
                    format!("Cannot assign to immutable const `{name}`"),
                    Span::new(0, 0),
                ));
            }
            if context.globals.contains_key(name) {
                diagnostics.push(Diagnostic::new(
                    format!("Cannot assign to global item `{name}`"),
                    Span::new(0, 0),
                ));
            }
            let ty = locals.get(name).cloned().or_else(|| context.globals.get(name).cloned());
            if let Some(ty) = ty {
                Some((TypedAssignTarget::Path(name.clone()), ty))
            } else {
                diagnostics.push(Diagnostic::new(
                    format!("Unknown assignment target `{name}`"),
                    Span::new(0, 0),
                ));
                None
            }
        }
        AssignTarget::Field { base, field } => {
            let (typed_base, base_ty) = infer_expr(base, context, locals, return_ty, diagnostics);
            let resolved_ty = resolve_field_type(&base_ty, field, context, diagnostics);
            Some((
                TypedAssignTarget::Field {
                    base: typed_base,
                    field: field.clone(),
                },
                resolved_ty,
            ))
        }
        AssignTarget::Index { base, index } => {
            let (typed_base, base_ty) = infer_expr(base, context, locals, return_ty, diagnostics);
            let (typed_index, index_ty) =
                infer_expr(index, context, locals, return_ty, diagnostics);
            let resolved_ty = match &base_ty {
                SemType::Path { path, args }
                    if path.len() == 1 && path[0] == "Vec" && args.len() == 1 =>
                {
                    if is_compatible(&index_ty, &named_type("i64")) {
                        args[0].clone()
                    } else {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "Vector index assignment expects `i64` index, got `{}`",
                                type_to_string(&index_ty)
                            ),
                            Span::new(0, 0),
                        ));
                        SemType::Unknown
                    }
                }
                _ => {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Index assignment target expects `Vec<T>`, got `{}`",
                            type_to_string(&base_ty)
                        ),
                        Span::new(0, 0),
                    ));
                    SemType::Unknown
                }
            };
            Some((
                TypedAssignTarget::Index {
                    base: typed_base,
                    index: typed_index,
                },
                resolved_ty,
            ))
        }
        AssignTarget::Tuple(items) => {
            let mut typed_items = Vec::with_capacity(items.len());
            let mut tys = Vec::with_capacity(items.len());
            for item in items {
                let (typed_item, ty) = infer_assign_target(
                    item,
                    context,
                    locals,
                    immutable_locals,
                    return_ty,
                    diagnostics,
                )?;
                typed_items.push(typed_item);
                tys.push(ty);
            }
            Some((TypedAssignTarget::Tuple(typed_items), SemType::Tuple(tys)))
        }
    }
}

fn infer_expr(
    expr: &Expr,
    context: &Context,
    locals: &mut HashMap<String, SemType>,
    return_ty: &SemType,
    diagnostics: &mut Vec<Diagnostic>,
) -> (TypedExpr, SemType) {
    match expr {
        Expr::Int(value) => (
            TypedExpr {
                kind: TypedExprKind::Int(*value),
                ty: "i64".to_string(),
            },
            named_type("i64"),
        ),
        Expr::Bool(value) => (
            TypedExpr {
                kind: TypedExprKind::Bool(*value),
                ty: "bool".to_string(),
            },
            named_type("bool"),
        ),
        Expr::Char(value) => (
            TypedExpr {
                kind: TypedExprKind::Char(*value),
                ty: "char".to_string(),
            },
            named_type("char"),
        ),
        Expr::String(value) => (
            TypedExpr {
                kind: TypedExprKind::String(value.clone()),
                ty: "String".to_string(),
            },
            named_type("String"),
        ),
        Expr::Path(path) => {
            let ty = if path.len() == 1 {
                locals
                    .get(&path[0])
                    .cloned()
                    .or_else(|| context.globals.get(&path[0]).cloned())
                    .or_else(|| resolve_path_value(path, context))
                    .unwrap_or(SemType::Unknown)
            } else {
                resolve_path_value(path, context).unwrap_or(SemType::Unknown)
            };
            (
                TypedExpr {
                    kind: TypedExprKind::Path(path.clone()),
                    ty: type_to_string(&ty),
                },
                ty,
            )
        }
        Expr::StructLiteral { path, fields } => {
            let mut typed_fields = Vec::new();
            let mut seen = HashSet::new();
            let mut resolved_ty = SemType::Path {
                path: path.clone(),
                args: Vec::new(),
            };
            let mut expected_fields: Option<&HashMap<String, SemType>> = None;

            if let Some(struct_name) = path.last() {
                if let Some(struct_fields) = context.structs.get(struct_name) {
                    expected_fields = Some(struct_fields);
                }
            } else {
                resolved_ty = SemType::Unknown;
            }

            for StructLiteralField { name, value } in fields {
                let (typed_value, value_ty) =
                    infer_expr(value, context, locals, return_ty, diagnostics);
                if !seen.insert(name.clone()) {
                    diagnostics.push(Diagnostic::new(
                        format!("Duplicate struct literal field `{name}`"),
                        Span::new(0, 0),
                    ));
                }
                if let Some(expected) = expected_fields.and_then(|map| map.get(name)) {
                    if !is_compatible(&value_ty, expected) {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "Struct field `{name}` expected `{}`, got `{}`",
                                type_to_string(expected),
                                type_to_string(&value_ty)
                            ),
                            Span::new(0, 0),
                        ));
                    }
                } else if expected_fields.is_some() {
                    diagnostics.push(Diagnostic::new(
                        format!("Unknown struct field `{name}` in literal"),
                        Span::new(0, 0),
                    ));
                }
                typed_fields.push(TypedStructLiteralField {
                    name: name.clone(),
                    value: typed_value,
                });
            }

            if let Some(struct_fields) = expected_fields {
                for field_name in struct_fields.keys() {
                    if !seen.contains(field_name) {
                        diagnostics.push(Diagnostic::new(
                            format!("Missing struct literal field `{field_name}`"),
                            Span::new(0, 0),
                        ));
                    }
                }
            }

            (
                TypedExpr {
                    kind: TypedExprKind::StructLiteral {
                        path: path.clone(),
                        fields: typed_fields,
                    },
                    ty: type_to_string(&resolved_ty),
                },
                resolved_ty,
            )
        }
        Expr::Field { base, field } => {
            let (typed_base, base_ty) = infer_expr(base, context, locals, return_ty, diagnostics);
            let resolved = resolve_field_type(&base_ty, field, context, diagnostics);
            (
                TypedExpr {
                    kind: TypedExprKind::Field {
                        base: Box::new(typed_base),
                        field: field.clone(),
                    },
                    ty: type_to_string(&resolved),
                },
                resolved,
            )
        }
        Expr::Call { callee, args } => {
            let mut typed_args = Vec::new();
            let mut arg_types = Vec::new();
            for arg in args {
                let (typed_arg, ty) = infer_expr(arg, context, locals, return_ty, diagnostics);
                typed_args.push(typed_arg);
                arg_types.push(ty);
            }

            if let Expr::Field { base, field } = callee.as_ref() {
                let (typed_base, base_ty) = infer_expr(base, context, locals, return_ty, diagnostics);
                let resolved =
                    resolve_method_call_type(&base_ty, field, &arg_types, context, diagnostics);
                let typed_callee = TypedExpr {
                    kind: TypedExprKind::Field {
                        base: Box::new(typed_base),
                        field: field.clone(),
                    },
                    ty: "_".to_string(),
                };
                return (
                    TypedExpr {
                        kind: TypedExprKind::Call {
                            callee: Box::new(typed_callee),
                            args: typed_args,
                        },
                        ty: type_to_string(&resolved),
                    },
                    resolved,
                );
            }

            let (typed_callee, callee_ty) =
                infer_expr(callee, context, locals, return_ty, diagnostics);
            let resolved = resolve_call_type(&typed_callee, &callee_ty, &arg_types, context, diagnostics);
            (
                TypedExpr {
                    kind: TypedExprKind::Call {
                        callee: Box::new(typed_callee),
                        args: typed_args,
                    },
                    ty: type_to_string(&resolved),
                },
                resolved,
            )
        }
        Expr::MacroCall { path, args } => {
            let mut typed_args = Vec::new();
            for arg in args {
                let (typed_arg, _) = infer_expr(arg, context, locals, return_ty, diagnostics);
                typed_args.push(typed_arg);
            }
            let resolved = if path.len() == 1 && path[0] == "format" {
                named_type("String")
            } else {
                SemType::Unknown
            };
            (
                TypedExpr {
                    kind: TypedExprKind::MacroCall {
                        path: path.clone(),
                        args: typed_args,
                    },
                    ty: type_to_string(&resolved),
                },
                resolved,
            )
        }
        Expr::Index { base, index } => {
            let (typed_base, base_ty) = infer_expr(base, context, locals, return_ty, diagnostics);
            let (typed_index, index_ty) =
                infer_expr(index, context, locals, return_ty, diagnostics);

            let resolved = match &base_ty {
                SemType::Path { path, args } if path.len() == 1 && path[0] == "Vec" && args.len() == 1 => {
                    if matches!(typed_index.kind, TypedExprKind::Range { .. }) {
                        SemType::Path {
                            path: vec!["Vec".to_string()],
                            args: vec![args[0].clone()],
                        }
                    } else if is_compatible(&index_ty, &named_type("i64")) {
                        args[0].clone()
                    } else if index_ty == SemType::Unknown {
                        SemType::Unknown
                    } else {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "Vector indexing expects `i64` index or range, got `{}`",
                                type_to_string(&index_ty)
                            ),
                            Span::new(0, 0),
                        ));
                        SemType::Unknown
                    }
                }
                SemType::Unknown => SemType::Unknown,
                _ => {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Indexing requires `Vec<T>` value, got `{}`",
                            type_to_string(&base_ty)
                        ),
                        Span::new(0, 0),
                    ));
                    SemType::Unknown
                }
            };

            (
                TypedExpr {
                    kind: TypedExprKind::Index {
                        base: Box::new(typed_base),
                        index: Box::new(typed_index),
                    },
                    ty: type_to_string(&resolved),
                },
                resolved,
            )
        }
        Expr::Match { scrutinee, arms } => {
            let (typed_scrutinee, scrutinee_ty) =
                infer_expr(scrutinee, context, locals, return_ty, diagnostics);
            let mut typed_arms = Vec::new();
            let mut resolved_arm_ty: Option<SemType> = None;
            for arm in arms {
                let mut arm_locals = locals.clone();
                let typed_pattern = lower_pattern(
                    &arm.pattern,
                    &scrutinee_ty,
                    context,
                    &mut arm_locals,
                    diagnostics,
                );
                let typed_guard = arm.guard.as_ref().map(|guard| {
                    let (typed_guard, guard_ty) =
                        infer_expr(guard, context, &mut arm_locals, return_ty, diagnostics);
                    if !is_compatible(&guard_ty, &named_type("bool")) {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "Match guard must be `bool`, got `{}`",
                                type_to_string(&guard_ty)
                            ),
                            Span::new(0, 0),
                        ));
                    }
                    typed_guard
                });
                let (typed_value, arm_ty) =
                    infer_expr(&arm.value, context, &mut arm_locals, return_ty, diagnostics);
                resolved_arm_ty = match resolved_arm_ty {
                    Some(existing) => Some(unify_types(existing, arm_ty, diagnostics)),
                    None => Some(arm_ty),
                };
                typed_arms.push(TypedMatchArm {
                    pattern: typed_pattern,
                    guard: typed_guard,
                    value: typed_value,
                });
            }
            check_match_exhaustiveness(&scrutinee_ty, &typed_arms, context, diagnostics);

            let final_ty = resolved_arm_ty.unwrap_or_else(|| {
                diagnostics.push(Diagnostic::new(
                    "Match expression must have at least one arm",
                    Span::new(0, 0),
                ));
                SemType::Unknown
            });

            (
                TypedExpr {
                    kind: TypedExprKind::Match {
                        scrutinee: Box::new(typed_scrutinee),
                        arms: typed_arms,
                    },
                    ty: type_to_string(&final_ty),
                },
                final_ty,
            )
        }
        Expr::Unary { op, expr } => {
            let (typed_expr, expr_ty) = infer_expr(expr, context, locals, return_ty, diagnostics);
            match op {
                UnaryOp::Not => {
                    if !is_compatible(&expr_ty, &named_type("bool")) {
                        diagnostics.push(Diagnostic::new(
                            format!("`not` expects `bool`, got `{}`", type_to_string(&expr_ty)),
                            Span::new(0, 0),
                        ));
                    }
                    let out_ty = named_type("bool");
                    (
                        TypedExpr {
                            kind: TypedExprKind::Unary {
                                op: TypedUnaryOp::Not,
                                expr: Box::new(typed_expr),
                            },
                            ty: type_to_string(&out_ty),
                        },
                        out_ty,
                    )
                }
                UnaryOp::Neg => {
                    let out_ty = resolve_unary_neg_type(&expr_ty, diagnostics);
                    (
                        TypedExpr {
                            kind: TypedExprKind::Unary {
                                op: TypedUnaryOp::Neg,
                                expr: Box::new(typed_expr),
                            },
                            ty: type_to_string(&out_ty),
                        },
                        out_ty,
                    )
                }
            }
        }
        Expr::Binary { op, left, right } => {
            let (typed_left, left_ty) = infer_expr(left, context, locals, return_ty, diagnostics);
            let (typed_right, right_ty) =
                infer_expr(right, context, locals, return_ty, diagnostics);
            let (typed_op, out_ty) = match op {
                BinaryOp::Add => {
                    let out_ty = resolve_add_type(&left_ty, &right_ty, diagnostics);
                    (TypedBinaryOp::Add, out_ty)
                }
                BinaryOp::Sub => {
                    let out_ty = resolve_numeric_binary_type("-", &left_ty, &right_ty, diagnostics);
                    (TypedBinaryOp::Sub, out_ty)
                }
                BinaryOp::Mul => {
                    let out_ty = resolve_numeric_binary_type("*", &left_ty, &right_ty, diagnostics);
                    (TypedBinaryOp::Mul, out_ty)
                }
                BinaryOp::Div => {
                    let out_ty = resolve_numeric_binary_type("/", &left_ty, &right_ty, diagnostics);
                    (TypedBinaryOp::Div, out_ty)
                }
                BinaryOp::Rem => {
                    let out_ty = resolve_numeric_binary_type("%", &left_ty, &right_ty, diagnostics);
                    (TypedBinaryOp::Rem, out_ty)
                }
                BinaryOp::And | BinaryOp::Or => {
                    if !is_compatible(&left_ty, &named_type("bool"))
                        || !is_compatible(&right_ty, &named_type("bool"))
                    {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "logical op expects `bool` operands, got `{}` and `{}`",
                                type_to_string(&left_ty),
                                type_to_string(&right_ty)
                            ),
                            Span::new(0, 0),
                        ));
                    }
                    (
                        if matches!(op, BinaryOp::And) {
                            TypedBinaryOp::And
                        } else {
                            TypedBinaryOp::Or
                        },
                        named_type("bool"),
                    )
                }
                BinaryOp::Eq
                | BinaryOp::Ne
                | BinaryOp::Lt
                | BinaryOp::Le
                | BinaryOp::Gt
                | BinaryOp::Ge => {
                    if !is_compatible(&left_ty, &right_ty) {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "comparison operands must be compatible, got `{}` and `{}`",
                                type_to_string(&left_ty),
                                type_to_string(&right_ty)
                            ),
                            Span::new(0, 0),
                        ));
                    }
                    (
                        match op {
                            BinaryOp::Eq => TypedBinaryOp::Eq,
                            BinaryOp::Ne => TypedBinaryOp::Ne,
                            BinaryOp::Lt => TypedBinaryOp::Lt,
                            BinaryOp::Le => TypedBinaryOp::Le,
                            BinaryOp::Gt => TypedBinaryOp::Gt,
                            BinaryOp::Ge => TypedBinaryOp::Ge,
                            _ => unreachable!(),
                        },
                        named_type("bool"),
                    )
                }
            };
            (
                TypedExpr {
                    kind: TypedExprKind::Binary {
                        op: typed_op,
                        left: Box::new(typed_left),
                        right: Box::new(typed_right),
                    },
                    ty: type_to_string(&out_ty),
                },
                out_ty,
            )
        }
        Expr::Array(items) => {
            let mut typed_items = Vec::new();
            let mut item_types = Vec::new();
            for item in items {
                let (typed_item, ty) = infer_expr(item, context, locals, return_ty, diagnostics);
                typed_items.push(typed_item);
                item_types.push(ty);
            }

            let mut elem_ty = SemType::Unknown;
            for ty in &item_types {
                if *ty == SemType::Unknown {
                    continue;
                }
                if elem_ty == SemType::Unknown {
                    elem_ty = ty.clone();
                    continue;
                }
                if !is_compatible(ty, &elem_ty) || !is_compatible(&elem_ty, ty) {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Array literal element type mismatch: expected `{}`, got `{}`",
                            type_to_string(&elem_ty),
                            type_to_string(ty)
                        ),
                        Span::new(0, 0),
                    ));
                }
            }

            let out_ty = SemType::Path {
                path: vec!["Vec".to_string()],
                args: vec![elem_ty],
            };
            (
                TypedExpr {
                    kind: TypedExprKind::Array(typed_items),
                    ty: type_to_string(&out_ty),
                },
                out_ty,
            )
        }
        Expr::Tuple(items) => {
            let mut typed_items = Vec::new();
            let mut item_types = Vec::new();
            for item in items {
                let (typed_item, ty) = infer_expr(item, context, locals, return_ty, diagnostics);
                typed_items.push(typed_item);
                item_types.push(ty);
            }
            let out_ty = if item_types.is_empty() {
                SemType::Unit
            } else {
                SemType::Tuple(item_types)
            };
            (
                TypedExpr {
                    kind: TypedExprKind::Tuple(typed_items),
                    ty: type_to_string(&out_ty),
                },
                out_ty,
            )
        }
        Expr::Closure {
            params,
            return_type,
            body,
        } => {
            let mut closure_locals = locals.clone();
            let mut closure_immutable_locals = HashSet::new();
            let param_types = params
                .iter()
                .map(|p| type_from_ast(&p.ty))
                .collect::<Vec<_>>();
            for (param, ty) in params.iter().zip(param_types.iter()) {
                closure_locals.insert(param.name.clone(), ty.clone());
            }

            let declared_return_ty = return_type.as_ref().map(type_from_ast);
            let provisional_return_ty = declared_return_ty.clone().unwrap_or(SemType::Unknown);
            let mut typed_body = Vec::new();
            let mut inferred_returns = Vec::new();
            for (index, statement) in body.statements.iter().enumerate() {
                let is_last = index + 1 == body.statements.len();
                if is_last && let Stmt::TailExpr(expr) = statement {
                    let (typed_expr, inferred) = infer_expr(
                        expr,
                        context,
                        &mut closure_locals,
                        &provisional_return_ty,
                        diagnostics,
                    );
                    inferred_returns.push(inferred.clone());
                    if provisional_return_ty != SemType::Unknown
                        && !is_compatible(&inferred, &provisional_return_ty)
                    {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "Closure return type mismatch: expected `{}`, got `{}`",
                                type_to_string(&provisional_return_ty),
                                type_to_string(&inferred)
                            ),
                            Span::new(0, 0),
                        ));
                    }
                    typed_body.push(TypedStmt::Return(Some(typed_expr)));
                    continue;
                }
                if let Some(stmt) = lower_stmt_with_types(
                    statement,
                    context,
                    &mut closure_locals,
                    &mut closure_immutable_locals,
                    &provisional_return_ty,
                    &mut inferred_returns,
                    diagnostics,
                ) {
                    typed_body.push(stmt);
                }
            }
            let final_return_ty = declared_return_ty
                .unwrap_or_else(|| infer_return_type(&inferred_returns, diagnostics, "closure"));
            if final_return_ty != SemType::Unit && inferred_returns.is_empty() {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Closure must explicitly return `{}`",
                        type_to_string(&final_return_ty)
                    ),
                    Span::new(0, 0),
                ));
            }

            let fn_ty = SemType::Fn {
                params: param_types.clone(),
                ret: Box::new(final_return_ty.clone()),
            };
            (
                TypedExpr {
                    kind: TypedExprKind::Closure {
                        params: params
                            .iter()
                            .zip(param_types.iter())
                            .map(|(param, ty)| TypedParam {
                                name: param.name.clone(),
                                ty: type_to_string(ty),
                            })
                            .collect(),
                        return_type: type_to_string(&final_return_ty),
                        body: typed_body,
                    },
                    ty: type_to_string(&fn_ty),
                },
                fn_ty,
            )
        }
        Expr::Range {
            start,
            end,
            inclusive,
        } => {
            let typed_start = start
                .as_ref()
                .map(|expr| infer_expr(expr, context, locals, return_ty, diagnostics).0);
            let typed_end = end
                .as_ref()
                .map(|expr| infer_expr(expr, context, locals, return_ty, diagnostics).0);
            (
                TypedExpr {
                    kind: TypedExprKind::Range {
                        start: typed_start.map(Box::new),
                        end: typed_end.map(Box::new),
                        inclusive: *inclusive,
                    },
                    ty: "_".to_string(),
                },
                SemType::Unknown,
            )
        }
        Expr::Try(inner) => {
            let (typed_inner, inner_ty) =
                infer_expr(inner, context, locals, return_ty, diagnostics);
            let resolved = resolve_try_type(&inner_ty, return_ty, diagnostics);
            (
                TypedExpr {
                    kind: TypedExprKind::Try(Box::new(typed_inner)),
                    ty: type_to_string(&resolved),
                },
                resolved,
            )
        }
    }
}

fn check_match_exhaustiveness(
    scrutinee_ty: &SemType,
    arms: &[TypedMatchArm],
    context: &Context,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if match_has_total_pattern(arms) {
        return;
    }

    if is_concrete_named_type(scrutinee_ty, "bool") {
        let mut has_true = false;
        let mut has_false = false;
        for arm in arms {
            if !guard_counts_for_exhaustiveness(arm.guard.as_ref()) {
                continue;
            }
            collect_bool_coverage(&arm.pattern, &mut has_true, &mut has_false);
        }
        if !(has_true && has_false) {
            diagnostics.push(Diagnostic::new(
                "Non-exhaustive match on `bool` (missing `true` or `false`)",
                Span::new(0, 0),
            ));
        }
        return;
    }

    if let Some(arity) = bool_tuple_arity(scrutinee_ty) {
        let mut covered = HashSet::new();
        for arm in arms {
            if !guard_counts_for_exhaustiveness(arm.guard.as_ref()) {
                continue;
            }
            collect_bool_tuple_coverage(&arm.pattern, arity, &mut covered);
        }
        let total = 1usize << arity;
        if covered.len() != total {
            let mut missing = Vec::new();
            for mask in 0..total {
                if !covered.contains(&mask) {
                    missing.push(format_bool_tuple_mask(mask, arity));
                }
            }
            diagnostics.push(Diagnostic::new(
                format!(
                    "Non-exhaustive match on tuple bool pattern; missing: {}",
                    missing.join(", ")
                ),
                Span::new(0, 0),
            ));
        }
        return;
    }

    if let Some(domains) = finite_tuple_domains(scrutinee_ty, context) {
        let total = domains
            .iter()
            .map(|domain| domain.len())
            .try_fold(1usize, |acc, len| acc.checked_mul(len))
            .unwrap_or(usize::MAX);
        if total == 0 || total > 256 {
            return;
        }

        let mut covered = HashSet::new();
        for arm in arms {
            if !guard_counts_for_exhaustiveness(arm.guard.as_ref()) {
                continue;
            }
            collect_finite_tuple_coverage(&arm.pattern, &domains, &mut covered);
        }

        if covered.len() != total {
            let mut missing = Vec::new();
            for combo in enumerate_finite_tuple_combos(&domains) {
                let key = finite_tuple_combo_key(&combo);
                if !covered.contains(&key) {
                    missing.push(format!("({})", combo.join(", ")));
                }
            }
            diagnostics.push(Diagnostic::new(
                format!(
                    "Non-exhaustive match on finite tuple domain; missing: {}",
                    missing.join(", ")
                ),
                Span::new(0, 0),
            ));
        }
        return;
    }

    let Some(enum_name) = enum_name_from_type(scrutinee_ty) else {
        return;
    };
    let expected_variants = expected_enum_variants(&enum_name, context);
    if expected_variants.is_empty() {
        return;
    }

    let mut covered = HashSet::new();
    for arm in arms {
        if !guard_counts_for_exhaustiveness(arm.guard.as_ref()) {
            continue;
        }
        collect_enum_coverage(&arm.pattern, &enum_name, &mut covered);
    }

    if covered.len() != expected_variants.len()
        || expected_variants.iter().any(|variant| !covered.contains(variant))
    {
        let missing = expected_variants
            .iter()
            .filter(|variant| !covered.contains(*variant))
            .cloned()
            .collect::<Vec<_>>()
            .join(", ");
        diagnostics.push(Diagnostic::new(
            format!("Non-exhaustive match on `{enum_name}`; missing variants: {missing}"),
            Span::new(0, 0),
        ));
    }
}

fn bool_tuple_arity(ty: &SemType) -> Option<usize> {
    let SemType::Tuple(items) = ty else {
        return None;
    };
    if items.is_empty() || items.iter().any(|item| !is_concrete_named_type(item, "bool")) {
        return None;
    }
    Some(items.len())
}

fn collect_bool_tuple_coverage(pattern: &TypedPattern, arity: usize, covered: &mut HashSet<usize>) {
    match pattern {
        TypedPattern::Wildcard | TypedPattern::Binding(_) => {
            let total = 1usize << arity;
            for mask in 0..total {
                covered.insert(mask);
            }
        }
        TypedPattern::BindingAt { pattern, .. } => {
            collect_bool_tuple_coverage(pattern, arity, covered);
        }
        TypedPattern::Or(items) => {
            for item in items {
                collect_bool_tuple_coverage(item, arity, covered);
            }
        }
        TypedPattern::Tuple(items) if items.len() == arity => {
            let choices = items
                .iter()
                .map(bool_pattern_values)
                .collect::<Option<Vec<_>>>();
            let Some(choices) = choices else {
                return;
            };
            let mut masks = vec![0usize];
            for (index, values) in choices.iter().enumerate() {
                let bit = arity - index - 1;
                let mut next = Vec::new();
                for mask in &masks {
                    for value in values {
                        let updated = if *value {
                            *mask | (1usize << bit)
                        } else {
                            *mask
                        };
                        next.push(updated);
                    }
                }
                masks = next;
            }
            for mask in masks {
                covered.insert(mask);
            }
        }
        _ => {}
    }
}

fn bool_pattern_values(pattern: &TypedPattern) -> Option<Vec<bool>> {
    match pattern {
        TypedPattern::Bool(value) => Some(vec![*value]),
        TypedPattern::Wildcard | TypedPattern::Binding(_) => Some(vec![false, true]),
        TypedPattern::BindingAt { pattern, .. } => bool_pattern_values(pattern),
        TypedPattern::Or(items) => {
            let mut out = Vec::new();
            for item in items {
                for value in bool_pattern_values(item)? {
                    if !out.contains(&value) {
                        out.push(value);
                    }
                }
            }
            Some(out)
        }
        _ => None,
    }
}

fn format_bool_tuple_mask(mask: usize, arity: usize) -> String {
    let mut items = Vec::new();
    for index in 0..arity {
        let bit = arity - index - 1;
        let value = ((mask >> bit) & 1usize) == 1usize;
        items.push(if value { "true" } else { "false" });
    }
    format!("({})", items.join(", "))
}

fn finite_tuple_domains(scrutinee_ty: &SemType, context: &Context) -> Option<Vec<Vec<String>>> {
    let SemType::Tuple(items) = scrutinee_ty else {
        return None;
    };
    if items.is_empty() {
        return None;
    }
    let mut domains = Vec::new();
    for item in items {
        if is_concrete_named_type(item, "bool") {
            domains.push(vec!["false".to_string(), "true".to_string()]);
            continue;
        }
        let enum_name = enum_name_from_type(item)?;
        let variants = finite_enum_variants_for_tuple_domain(&enum_name, context)?;
        if variants.is_empty() {
            return None;
        }
        domains.push(
            variants
                .into_iter()
                .map(|variant| format!("{enum_name}::{variant}"))
                .collect(),
        );
    }
    Some(domains)
}

fn finite_enum_variants_for_tuple_domain(enum_name: &str, context: &Context) -> Option<Vec<String>> {
    if enum_name == "Option" {
        return Some(vec!["Some".to_string(), "None".to_string()]);
    }
    if enum_name == "Result" {
        return Some(vec!["Ok".to_string(), "Err".to_string()]);
    }
    context
        .enums
        .get(enum_name)
        .map(|variants| variants.keys().cloned().collect())
}

fn collect_finite_tuple_coverage(
    pattern: &TypedPattern,
    domains: &[Vec<String>],
    covered: &mut HashSet<String>,
) {
    match pattern {
        TypedPattern::Wildcard | TypedPattern::Binding(_) => {
            for combo in enumerate_finite_tuple_combos(domains) {
                covered.insert(finite_tuple_combo_key(&combo));
            }
        }
        TypedPattern::BindingAt { pattern, .. } => {
            collect_finite_tuple_coverage(pattern, domains, covered);
        }
        TypedPattern::Or(items) => {
            for item in items {
                collect_finite_tuple_coverage(item, domains, covered);
            }
        }
        TypedPattern::Tuple(items) if items.len() == domains.len() => {
            let mut choices = Vec::new();
            for (item, domain) in items.iter().zip(domains.iter()) {
                let Some(values) = finite_tuple_pattern_values(item, domain) else {
                    return;
                };
                choices.push(values);
            }
            for combo in enumerate_finite_tuple_combos(&choices) {
                covered.insert(finite_tuple_combo_key(&combo));
            }
        }
        _ => {}
    }
}

fn finite_tuple_pattern_values(pattern: &TypedPattern, domain: &[String]) -> Option<Vec<String>> {
    match pattern {
        TypedPattern::Wildcard | TypedPattern::Binding(_) => Some(domain.to_vec()),
        TypedPattern::BindingAt { pattern, .. } => finite_tuple_pattern_values(pattern, domain),
        TypedPattern::Or(items) => {
            let mut out = Vec::new();
            for item in items {
                for value in finite_tuple_pattern_values(item, domain)? {
                    if !out.contains(&value) {
                        out.push(value);
                    }
                }
            }
            Some(out)
        }
        TypedPattern::Bool(value) => {
            let label = if *value { "true" } else { "false" };
            if domain.iter().any(|item| item == label) {
                Some(vec![label.to_string()])
            } else {
                None
            }
        }
        TypedPattern::Variant { path, payload } => {
            if let Some(payload_pattern) = payload
                && !pattern_is_total(payload_pattern)
            {
                return None;
            }
            let selected = match path.as_slice() {
                [enum_name, variant, ..] => Some(format!("{enum_name}::{variant}")),
                [variant] => {
                    let suffix = format!("::{variant}");
                    let matches = domain
                        .iter()
                        .filter(|item| item.ends_with(&suffix))
                        .cloned()
                        .collect::<Vec<_>>();
                    if matches.len() == 1 {
                        matches.first().cloned()
                    } else {
                        None
                    }
                }
                _ => None,
            }?;
            if domain.iter().any(|item| item == &selected) {
                Some(vec![selected])
            } else {
                None
            }
        }
        _ => None,
    }
}

fn enumerate_finite_tuple_combos(domains: &[Vec<String>]) -> Vec<Vec<String>> {
    fn walk(
        domains: &[Vec<String>],
        index: usize,
        current: &mut Vec<String>,
        out: &mut Vec<Vec<String>>,
    ) {
        if index == domains.len() {
            out.push(current.clone());
            return;
        }
        for value in &domains[index] {
            current.push(value.clone());
            walk(domains, index + 1, current, out);
            current.pop();
        }
    }

    let mut out = Vec::new();
    walk(domains, 0, &mut Vec::new(), &mut out);
    out
}

fn finite_tuple_combo_key(combo: &[String]) -> String {
    combo.join("\u{1f}")
}

fn match_has_total_pattern(arms: &[TypedMatchArm]) -> bool {
    arms.iter()
        .any(|arm| guard_counts_for_exhaustiveness(arm.guard.as_ref()) && pattern_is_total(&arm.pattern))
}

fn guard_counts_for_exhaustiveness(guard: Option<&TypedExpr>) -> bool {
    match guard.map(|expr| &expr.kind) {
        None => true,
        Some(TypedExprKind::Bool(true)) => true,
        _ => false,
    }
}

fn pattern_is_total(pattern: &TypedPattern) -> bool {
    match pattern {
        TypedPattern::Wildcard | TypedPattern::Binding(_) => true,
        TypedPattern::BindingAt { pattern, .. } => pattern_is_total(pattern),
        TypedPattern::Or(items) => items.iter().any(pattern_is_total),
        _ => false,
    }
}

fn collect_bool_coverage(pattern: &TypedPattern, has_true: &mut bool, has_false: &mut bool) {
    match pattern {
        TypedPattern::Bool(true) => *has_true = true,
        TypedPattern::Bool(false) => *has_false = true,
        TypedPattern::Or(items) => {
            for item in items {
                collect_bool_coverage(item, has_true, has_false);
            }
        }
        TypedPattern::BindingAt { pattern, .. } => collect_bool_coverage(pattern, has_true, has_false),
        TypedPattern::Wildcard | TypedPattern::Binding(_) => {
            *has_true = true;
            *has_false = true;
        }
        _ => {}
    }
}

fn enum_name_from_type(ty: &SemType) -> Option<String> {
    match ty {
        SemType::Path { path, .. } => path.last().cloned(),
        _ => None,
    }
}

fn is_concrete_named_type(ty: &SemType, name: &str) -> bool {
    matches!(
        ty,
        SemType::Path { path, args }
            if path.len() == 1 && path[0] == name && args.is_empty()
    )
}

fn expected_enum_variants(enum_name: &str, context: &Context) -> Vec<String> {
    if enum_name == "Option" {
        return vec!["Some".to_string(), "None".to_string()];
    }
    if enum_name == "Result" {
        return vec!["Ok".to_string(), "Err".to_string()];
    }
    context
        .enums
        .get(enum_name)
        .map(|variants| variants.keys().cloned().collect())
        .unwrap_or_default()
}

fn collect_enum_coverage(pattern: &TypedPattern, enum_name: &str, covered: &mut HashSet<String>) {
    match pattern {
        TypedPattern::Variant { path, .. } if path.len() >= 2 && path[0] == enum_name => {
            covered.insert(path[1].clone());
        }
        TypedPattern::Variant { path, .. } if path.len() == 2 => {
            if path[0] == enum_name {
                covered.insert(path[1].clone());
            }
        }
        TypedPattern::Or(items) => {
            for item in items {
                collect_enum_coverage(item, enum_name, covered);
            }
        }
        TypedPattern::BindingAt { pattern, .. } => collect_enum_coverage(pattern, enum_name, covered),
        TypedPattern::Wildcard | TypedPattern::Binding(_) => {}
        _ => {}
    }
}

fn resolve_field_type(
    base_ty: &SemType,
    field: &str,
    context: &Context,
    diagnostics: &mut Vec<Diagnostic>,
) -> SemType {
    if let SemType::Path { path, .. } = base_ty {
        if let Some(struct_name) = path.last() {
            if let Some(fields) = context.structs.get(struct_name) {
                if let Some(field_ty) = fields.get(field) {
                    return field_ty.clone();
                }
                diagnostics.push(Diagnostic::new(
                    format!("Struct `{}` has no field `{field}`", struct_name),
                    Span::new(0, 0),
                ));
                return SemType::Unknown;
            }
            // For non-Elevate structs (including imported Rust types), defer
            // member validation so method-call lowering can apply ownership policies.
            return SemType::Unknown;
        }
    }

    diagnostics.push(Diagnostic::new(
        "Field access requires a known struct type",
        Span::new(0, 0),
    ));
    SemType::Unknown
}

fn resolve_call_type(
    callee: &TypedExpr,
    callee_ty: &SemType,
    args: &[SemType],
    context: &Context,
    diagnostics: &mut Vec<Diagnostic>,
) -> SemType {
    if let SemType::Fn { params, ret } = callee_ty {
        if params.len() != args.len() {
            diagnostics.push(Diagnostic::new(
                format!(
                    "Closure/function value expects {} args, got {}",
                    params.len(),
                    args.len()
                ),
                Span::new(0, 0),
            ));
            return *ret.clone();
        }
        for (index, (actual, expected)) in args.iter().zip(params.iter()).enumerate() {
            if !is_compatible(actual, expected) {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Arg {} mismatch: expected `{}`, got `{}`",
                        index + 1,
                        type_to_string(expected),
                        type_to_string(actual)
                    ),
                    Span::new(0, 0),
                ));
            }
        }
        return *ret.clone();
    }

    if let TypedExprKind::Path(path) = &callee.kind {
        if let Some(ty) = resolve_builtin_assert_call(path, args, diagnostics) {
            return ty;
        }

        let lookup_name = path.join("::");
        if let Some(sig) = context.functions.get(&lookup_name) {
            return resolve_named_function_call(sig, &lookup_name, args, diagnostics);
        }

        if path.len() == 1 {
            let name = &path[0];
            if let Some(sig) = context.functions.get(name) {
                return resolve_named_function_call(sig, name, args, diagnostics);
            }
        }

        if let Some(resolved) = resolve_constructor_call(path, args, context, diagnostics) {
            return resolved;
        }

        return SemType::Unknown;
    }

    diagnostics.push(Diagnostic::new("Unsupported call target", Span::new(0, 0)));
    SemType::Unknown
}

fn resolve_named_function_call(
    sig: &FunctionSig,
    name: &str,
    args: &[SemType],
    diagnostics: &mut Vec<Diagnostic>,
) -> SemType {
    if sig.params.len() != args.len() {
        diagnostics.push(Diagnostic::new(
            format!(
                "Function `{name}` expects {} args, got {}",
                sig.params.len(),
                args.len()
            ),
            Span::new(0, 0),
        ));
        return sig.return_type.clone();
    }

    let type_param_set = sig.type_params.iter().cloned().collect::<HashSet<_>>();
    let mut generic_bindings = HashMap::new();

    for (index, (actual, expected)) in args.iter().zip(&sig.params).enumerate() {
        if !bind_generic_params(expected, actual, &type_param_set, &mut generic_bindings)
            && !is_compatible(actual, expected)
        {
            diagnostics.push(Diagnostic::new(
                format!(
                    "Arg {} for `{name}`: expected `{}`, got `{}`",
                    index + 1,
                    type_to_string(expected),
                    type_to_string(actual)
                ),
                Span::new(0, 0),
            ));
        }
    }

    for type_param in &sig.type_params {
        let Some(actual_ty) = generic_bindings.get(type_param) else {
            continue;
        };
        let Some(bounds) = sig.type_param_bounds.get(type_param) else {
            continue;
        };
        for bound in bounds {
            if !type_satisfies_bound(actual_ty, bound) {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Type `{}` does not satisfy bound `{}` for `{}` in function `{}`",
                        type_to_string(actual_ty),
                        type_to_string(bound),
                        type_param,
                        name
                    ),
                    Span::new(0, 0),
                ));
            }
        }
    }

    substitute_generic_type(&sig.return_type, &type_param_set, &generic_bindings)
}

fn resolve_builtin_assert_call(
    path: &[String],
    args: &[SemType],
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<SemType> {
    if path.len() != 1 {
        return None;
    }
    let name = path[0].as_str();
    match name {
        "assert" => {
            if args.len() != 1 {
                diagnostics.push(Diagnostic::new(
                    "assert(...) expects exactly one boolean argument",
                    Span::new(0, 0),
                ));
                return Some(SemType::Unit);
            }
            if !is_compatible(&args[0], &named_type("bool")) {
                diagnostics.push(Diagnostic::new(
                    format!("assert(...) expects `bool`, got `{}`", type_to_string(&args[0])),
                    Span::new(0, 0),
                ));
            }
            Some(SemType::Unit)
        }
        "assert_eq" | "assert_ne" => {
            if args.len() != 2 {
                diagnostics.push(Diagnostic::new(
                    format!("{name}(...) expects exactly two arguments"),
                    Span::new(0, 0),
                ));
                return Some(SemType::Unit);
            }
            if !is_compatible(&args[0], &args[1]) && !is_compatible(&args[1], &args[0]) {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "{name}(...) arguments must have compatible types, got `{}` and `{}`",
                        type_to_string(&args[0]),
                        type_to_string(&args[1])
                    ),
                    Span::new(0, 0),
                ));
            }
            Some(SemType::Unit)
        }
        _ => None,
    }
}

fn resolve_method_call_type(
    base_ty: &SemType,
    method: &str,
    args: &[SemType],
    context: &Context,
    diagnostics: &mut Vec<Diagnostic>,
) -> SemType {
    if let SemType::Path { path, .. } = base_ty {
        let type_name = path.last().map(|s| s.as_str()).unwrap_or("");
        match type_name {
            "String" => match method {
                "len" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    return named_type("usize");
                }
                "is_empty" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    return named_type("bool");
                }
                "starts_with" | "contains" | "ends_with" | "strip_prefix" | "split_once" => {
                    expect_method_arity(type_name, method, args.len(), 1, diagnostics);
                    return if method == "strip_prefix" {
                        option_type(named_type("String"))
                    } else if method == "split_once" {
                        option_type(SemType::Tuple(vec![named_type("String"), named_type("String")]))
                    } else {
                        named_type("bool")
                    };
                }
                "into_bytes" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    return SemType::Path {
                        path: vec!["Vec".to_string()],
                        args: vec![named_type("u8")],
                    };
                }
                "chars" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    return SemType::Iter(Box::new(named_type("char")));
                }
                _ => {}
            },
            "Vec" => match method {
                "len" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    return named_type("usize");
                }
                "is_empty" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    return named_type("bool");
                }
                "contains" => {
                    expect_method_arity(type_name, method, args.len(), 1, diagnostics);
                    return named_type("bool");
                }
                "push" => {
                    expect_method_arity(type_name, method, args.len(), 1, diagnostics);
                    if let SemType::Path { args: item_args, .. } = base_ty
                        && let (Some(item_ty), Some(arg_ty)) = (item_args.first(), args.first())
                        && !is_compatible(arg_ty, item_ty)
                    {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "Arg 1 for method `Vec::push`: expected `{}`, got `{}`",
                                type_to_string(item_ty),
                                type_to_string(arg_ty)
                            ),
                            Span::new(0, 0),
                        ));
                    }
                    return SemType::Unit;
                }
                "first" | "last" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    if let SemType::Path { args, .. } = base_ty
                        && let Some(item_ty) = args.first()
                    {
                        return option_type(item_ty.clone());
                    }
                    return option_type(SemType::Unknown);
                }
                "get" => {
                    expect_method_arity(type_name, method, args.len(), 1, diagnostics);
                    if args.len() == 1 && !is_compatible(&args[0], &named_type("i64")) {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "Method `Vec::get` expects `i64` index, got `{}`",
                                type_to_string(&args[0])
                            ),
                            Span::new(0, 0),
                        ));
                    }
                    if let SemType::Path { args, .. } = base_ty
                        && let Some(item_ty) = args.first()
                    {
                        return option_type(item_ty.clone());
                    }
                    return option_type(SemType::Unknown);
                }
                "into_iter" | "iter" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    if let SemType::Path { args, .. } = base_ty
                        && let Some(item_ty) = args.first()
                    {
                        return SemType::Iter(Box::new(item_ty.clone()));
                    }
                    return SemType::Iter(Box::new(SemType::Unknown));
                }
                _ => {}
            },
            "Option" => match method {
                "is_some" | "is_none" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    return named_type("bool");
                }
                "into_iter" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    if let SemType::Path { args, .. } = base_ty
                        && let Some(item_ty) = args.first()
                    {
                        return SemType::Iter(Box::new(item_ty.clone()));
                    }
                    return SemType::Iter(Box::new(SemType::Unknown));
                }
                _ => {}
            },
            "Result" => match method {
                "is_ok" | "is_err" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    return named_type("bool");
                }
                "into_iter" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    if let SemType::Path { args, .. } = base_ty
                        && let Some(ok_ty) = args.first()
                    {
                        return SemType::Iter(Box::new(ok_ty.clone()));
                    }
                    return SemType::Iter(Box::new(SemType::Unknown));
                }
                _ => {}
            },
            "HashMap" | "BTreeMap" => match method {
                "len" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    return named_type("usize");
                }
                "is_empty" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    return named_type("bool");
                }
                "contains_key" => {
                    expect_method_arity(type_name, method, args.len(), 1, diagnostics);
                    return named_type("bool");
                }
                "keys" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    if let SemType::Path { args, .. } = base_ty
                        && let Some(key_ty) = args.first()
                    {
                        return SemType::Iter(Box::new(key_ty.clone()));
                    }
                    return SemType::Iter(Box::new(SemType::Unknown));
                }
                "values" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    if let SemType::Path { args, .. } = base_ty
                        && let Some(value_ty) = args.get(1)
                    {
                        return SemType::Iter(Box::new(value_ty.clone()));
                    }
                    return SemType::Iter(Box::new(SemType::Unknown));
                }
                "iter" | "into_iter" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    if let SemType::Path { args, .. } = base_ty
                        && args.len() >= 2
                    {
                        return SemType::Iter(Box::new(SemType::Tuple(vec![
                            args[0].clone(),
                            args[1].clone(),
                        ])));
                    }
                    return SemType::Iter(Box::new(SemType::Unknown));
                }
                _ => {}
            },
            "HashSet" | "BTreeSet" => match method {
                "len" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    return named_type("usize");
                }
                "is_empty" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    return named_type("bool");
                }
                "contains" => {
                    expect_method_arity(type_name, method, args.len(), 1, diagnostics);
                    return named_type("bool");
                }
                "iter" | "into_iter" => {
                    expect_method_arity(type_name, method, args.len(), 0, diagnostics);
                    if let SemType::Path { args, .. } = base_ty
                        && let Some(item_ty) = args.first()
                    {
                        return SemType::Iter(Box::new(item_ty.clone()));
                    }
                    return SemType::Iter(Box::new(SemType::Unknown));
                }
                _ => {}
            },
            _ => {}
        }
    }

    if let SemType::Path { path, .. } = base_ty
        && let Some(type_name) = path.last()
    {
        let lookup = format!("{type_name}::{method}");
        if let Some(sig) = context.functions.get(&lookup) {
            if sig.params.len() == args.len() + 1 {
                if !is_compatible(base_ty, &sig.params[0]) {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Method `{lookup}` receiver mismatch: expected `{}`, got `{}`",
                            type_to_string(&sig.params[0]),
                            type_to_string(base_ty)
                        ),
                        Span::new(0, 0),
                    ));
                }
                for (index, (actual, expected)) in args.iter().zip(sig.params.iter().skip(1)).enumerate()
                {
                    if !is_compatible(actual, expected) {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "Arg {} for method `{lookup}`: expected `{}`, got `{}`",
                                index + 1,
                                type_to_string(expected),
                                type_to_string(actual)
                            ),
                            Span::new(0, 0),
                        ));
                    }
                }
                return sig.return_type.clone();
            }
            if sig.params.len() == args.len() {
                for (index, (actual, expected)) in args.iter().zip(&sig.params).enumerate() {
                    if !is_compatible(actual, expected) {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "Arg {} for method `{lookup}`: expected `{}`, got `{}`",
                                index + 1,
                                type_to_string(expected),
                                type_to_string(actual)
                            ),
                            Span::new(0, 0),
                        ));
                    }
                }
                return sig.return_type.clone();
            }
            diagnostics.push(Diagnostic::new(
                format!(
                    "Method `{lookup}` expects {} arg(s), got {}",
                    sig.params.len().saturating_sub(1),
                    args.len()
                ),
                Span::new(0, 0),
            ));
            return sig.return_type.clone();
        }
    }

    if is_external_method_candidate(base_ty, context) {
        return infer_external_method_call_type(method, args);
    }

    diagnostics.push(Diagnostic::new(
        format!(
            "Unsupported method call `{}` on type `{}`",
            method,
            type_to_string(base_ty)
        ),
        Span::new(0, 0),
    ));
    SemType::Unknown
}

fn is_external_method_candidate(base_ty: &SemType, context: &Context) -> bool {
    let SemType::Path { path, .. } = base_ty else {
        return false;
    };
    let Some(type_name) = path.last() else {
        return false;
    };
    let type_name = type_name.as_str();
    if is_builtin_method_carrier(type_name) {
        return false;
    }
    !context.structs.contains_key(type_name) && !context.enums.contains_key(type_name)
}

fn is_builtin_method_carrier(type_name: &str) -> bool {
    matches!(
        type_name,
        "String"
            | "Vec"
            | "Option"
            | "Result"
            | "HashMap"
            | "BTreeMap"
            | "HashSet"
            | "BTreeSet"
    )
}

fn infer_external_method_call_type(method: &str, args: &[SemType]) -> SemType {
    if matches!(
        method,
        "contains"
            | "contains_key"
            | "starts_with"
            | "ends_with"
            | "is_empty"
            | "is_some"
            | "is_none"
            | "is_ok"
            | "is_err"
    ) || method.starts_with("is_")
        || method.starts_with("has_")
    {
        return named_type("bool");
    }
    if method == "len" {
        return named_type("usize");
    }
    if matches!(method, "first" | "last" | "get") {
        return option_type(SemType::Unknown);
    }
    if matches!(method, "iter" | "into_iter" | "keys" | "values") {
        return SemType::Iter(Box::new(SemType::Unknown));
    }
    if method_name_suggests_mutating(method) {
        return SemType::Unit;
    }
    if args.is_empty() {
        return SemType::Unknown;
    }
    SemType::Unknown
}

fn expect_method_arity(
    type_name: &str,
    method: &str,
    actual: usize,
    expected: usize,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if actual != expected {
        diagnostics.push(Diagnostic::new(
            format!(
                "Method `{}::{}` expects {} args, got {}",
                type_name, method, expected, actual
            ),
            Span::new(0, 0),
        ));
    }
}

fn resolve_constructor_call(
    path: &[String],
    args: &[SemType],
    context: &Context,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<SemType> {
    if path.len() == 1 {
        if path[0] == "Some" {
            if args.len() != 1 {
                diagnostics.push(Diagnostic::new(
                    "Some(...) expects exactly one argument",
                    Span::new(0, 0),
                ));
                return Some(option_type(SemType::Unknown));
            }
            return Some(option_type(args[0].clone()));
        }
        if path[0] == "Ok" {
            if args.len() != 1 {
                diagnostics.push(Diagnostic::new(
                    "Ok(...) expects exactly one argument",
                    Span::new(0, 0),
                ));
                return Some(result_type(SemType::Unknown, SemType::Unknown));
            }
            return Some(result_type(args[0].clone(), SemType::Unknown));
        }
        if path[0] == "Err" {
            if args.len() != 1 {
                diagnostics.push(Diagnostic::new(
                    "Err(...) expects exactly one argument",
                    Span::new(0, 0),
                ));
                return Some(result_type(SemType::Unknown, SemType::Unknown));
            }
            return Some(result_type(SemType::Unknown, args[0].clone()));
        }
        return None;
    }

    if path.len() == 2 {
        let enum_name = &path[0];
        let variant_name = &path[1];
        if enum_name == "Option" {
            if variant_name == "Some" {
                if args.len() != 1 {
                    diagnostics.push(Diagnostic::new(
                        "Option::Some(...) expects one argument",
                        Span::new(0, 0),
                    ));
                    return Some(option_type(SemType::Unknown));
                }
                return Some(option_type(args[0].clone()));
            }
            if variant_name == "None" {
                if !args.is_empty() {
                    diagnostics.push(Diagnostic::new(
                        "Option::None expects no arguments",
                        Span::new(0, 0),
                    ));
                }
                return Some(option_type(SemType::Unknown));
            }
        }
        if enum_name == "Result" {
            if variant_name == "Ok" {
                if args.len() != 1 {
                    diagnostics.push(Diagnostic::new(
                        "Result::Ok(...) expects one argument",
                        Span::new(0, 0),
                    ));
                    return Some(result_type(SemType::Unknown, SemType::Unknown));
                }
                return Some(result_type(args[0].clone(), SemType::Unknown));
            }
            if variant_name == "Err" {
                if args.len() != 1 {
                    diagnostics.push(Diagnostic::new(
                        "Result::Err(...) expects one argument",
                        Span::new(0, 0),
                    ));
                    return Some(result_type(SemType::Unknown, SemType::Unknown));
                }
                return Some(result_type(SemType::Unknown, args[0].clone()));
            }
        }

        if let Some(variants) = context.enums.get(enum_name) {
            if let Some(expected_payload) = variants.get(variant_name) {
                if args.len() != expected_payload.len() {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Enum variant `{enum_name}::{variant_name}` expects {} argument(s), got {}",
                            expected_payload.len(),
                            args.len()
                        ),
                        Span::new(0, 0),
                    ));
                } else {
                    for (index, (actual, expected)) in args.iter().zip(expected_payload).enumerate()
                    {
                        if !is_compatible(actual, expected) {
                            diagnostics.push(Diagnostic::new(
                                format!(
                                    "Enum variant `{enum_name}::{variant_name}` arg {} expected `{}`, got `{}`",
                                    index + 1,
                                    type_to_string(expected),
                                    type_to_string(actual)
                                ),
                                Span::new(0, 0),
                            ));
                        }
                    }
                }
                return Some(named_type(enum_name));
            }
        }
    }

    None
}

fn resolve_try_type(
    inner: &SemType,
    return_ty: &SemType,
    diagnostics: &mut Vec<Diagnostic>,
) -> SemType {
    if let Some(inner_value) = option_inner(inner) {
        if *return_ty == SemType::Unknown {
            diagnostics.push(Diagnostic::new(
                "Functions using `?` must declare an Option/Result return type",
                Span::new(0, 0),
            ));
            return inner_value.clone();
        }
        if option_inner(return_ty).is_none() {
            diagnostics.push(Diagnostic::new(
                "The `?` operator on Option requires the function to return Option",
                Span::new(0, 0),
            ));
        }
        return inner_value.clone();
    }
    if let Some((ok_ty, err_ty)) = result_parts(inner) {
        if *return_ty == SemType::Unknown {
            diagnostics.push(Diagnostic::new(
                "Functions using `?` must declare an Option/Result return type",
                Span::new(0, 0),
            ));
            return ok_ty.clone();
        }
        if let Some((_, fn_err_ty)) = result_parts(return_ty) {
            if !is_compatible(err_ty, fn_err_ty) {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "`?` error type mismatch: expression has `{}`, function returns `{}`",
                        type_to_string(err_ty),
                        type_to_string(fn_err_ty)
                    ),
                    Span::new(0, 0),
                ));
            }
        } else {
            diagnostics.push(Diagnostic::new(
                "The `?` operator on Result requires the function to return Result",
                Span::new(0, 0),
            ));
        }
        return ok_ty.clone();
    }

    diagnostics.push(Diagnostic::new(
        "The `?` operator requires Option<T> or Result<T, E>",
        Span::new(0, 0),
    ));
    SemType::Unknown
}

fn resolve_add_type(left: &SemType, right: &SemType, diagnostics: &mut Vec<Diagnostic>) -> SemType {
    if is_compatible(left, &named_type("String")) && is_compatible(right, &named_type("String")) {
        return named_type("String");
    }
    if let Some(out_ty) = resolve_common_numeric_type(left, right) {
        return out_ty;
    }
    diagnostics.push(Diagnostic::new(
        format!(
            "`+` expects numeric operands or `String + String`, got `{}` and `{}`",
            type_to_string(left),
            type_to_string(right)
        ),
        Span::new(0, 0),
    ));
    SemType::Unknown
}

fn resolve_unary_neg_type(expr: &SemType, diagnostics: &mut Vec<Diagnostic>) -> SemType {
    let ty = normalize_numeric_unary_type(expr);
    if ty != SemType::Unknown {
        return ty;
    }
    diagnostics.push(Diagnostic::new(
        format!(
            "unary `-` expects a numeric operand, got `{}`",
            type_to_string(expr)
        ),
        Span::new(0, 0),
    ));
    SemType::Unknown
}

fn resolve_numeric_binary_type(
    op: &str,
    left: &SemType,
    right: &SemType,
    diagnostics: &mut Vec<Diagnostic>,
) -> SemType {
    if let Some(out_ty) = resolve_common_numeric_type(left, right) {
        return out_ty;
    }
    diagnostics.push(Diagnostic::new(
        format!(
            "`{op}` expects numeric operands, got `{}` and `{}`",
            type_to_string(left),
            type_to_string(right)
        ),
        Span::new(0, 0),
    ));
    SemType::Unknown
}

fn resolve_common_numeric_type(left: &SemType, right: &SemType) -> Option<SemType> {
    let left_name = canonical_numeric_name(left)?;
    let right_name = canonical_numeric_name(right)?;
    Some(named_type(promote_numeric_names(left_name, right_name)))
}

fn normalize_numeric_unary_type(expr: &SemType) -> SemType {
    let Some(name) = canonical_numeric_name(expr) else {
        return SemType::Unknown;
    };
    let promoted = if is_unsigned_numeric_name(name) {
        "i64"
    } else {
        name
    };
    named_type(promoted)
}

fn canonical_numeric_name(ty: &SemType) -> Option<&str> {
    if let SemType::Path { path, args } = ty
        && args.is_empty()
        && path.len() == 1
    {
        let name = path[0].as_str();
        if is_numeric_type_name(name) {
            return Some(name);
        }
    }
    None
}

fn is_numeric_type_name(name: &str) -> bool {
    matches!(
        name,
        "i8"
            | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "isize"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "usize"
            | "f32"
            | "f64"
    )
}

fn is_unsigned_numeric_name(name: &str) -> bool {
    matches!(name, "u8" | "u16" | "u32" | "u64" | "u128" | "usize")
}

fn promote_numeric_names<'a>(left: &'a str, right: &'a str) -> &'static str {
    if left == right {
        return match left {
            "i8" => "i8",
            "i16" => "i16",
            "i32" => "i32",
            "i64" => "i64",
            "i128" => "i128",
            "isize" => "isize",
            "u8" => "u8",
            "u16" => "u16",
            "u32" => "u32",
            "u64" => "u64",
            "u128" => "u128",
            "usize" => "usize",
            "f32" => "f32",
            "f64" => "f64",
            _ => "i64",
        };
    }
    if left == "f64" || right == "f64" {
        return "f64";
    }
    if left == "f32" || right == "f32" {
        return "f32";
    }
    if left == "i128" || right == "i128" || left == "u128" || right == "u128" {
        return "i128";
    }
    if left == "i64"
        || right == "i64"
        || left == "u64"
        || right == "u64"
        || left == "usize"
        || right == "usize"
        || left == "isize"
        || right == "isize"
    {
        return "i64";
    }
    "i32"
}

fn infer_for_item_type(iter_expr: &TypedExpr, iter_ty: &SemType) -> SemType {
    if matches!(iter_expr.kind, TypedExprKind::Range { .. }) {
        return named_type("i64");
    }
    if let SemType::Iter(item_ty) = iter_ty {
        return (**item_ty).clone();
    }
    if let SemType::Path { path, args } = iter_ty {
        if path.len() == 1 && path[0] == "Vec" && args.len() == 1 {
            return args[0].clone();
        }
    }
    SemType::Unknown
}

fn lower_stmt_with_context(
    stmt: &TypedStmt,
    context: &mut LoweringContext,
    state: &mut LoweringState,
) -> RustStmt {
    match stmt {
        TypedStmt::Const(def) => RustStmt::Const(RustConst {
            is_public: false,
            name: def.name.clone(),
            ty: def.ty.clone(),
            value: lower_expr_with_context(&def.value, context, ExprPosition::Value, state),
        }),
        TypedStmt::DestructureConst { pattern, value } => RustStmt::DestructureConst {
            pattern: lower_destructure_pattern(pattern),
            value: lower_expr_with_context(value, context, ExprPosition::Value, state),
        },
        TypedStmt::Assign { target, op, value } => RustStmt::Assign {
            target: lower_assign_target(target, context, state),
            op: match op {
                TypedAssignOp::Assign => RustAssignOp::Assign,
                TypedAssignOp::AddAssign => RustAssignOp::AddAssign,
            },
            value: lower_expr_with_context(value, context, ExprPosition::Value, state),
        },
        TypedStmt::Return(value) => RustStmt::Return(
            value
                .as_ref()
                .map(|expr| lower_expr_with_context(expr, context, ExprPosition::Value, state)),
        ),
        TypedStmt::If {
            condition,
            then_body,
            else_body,
        } => RustStmt::If {
            condition: lower_expr_with_context(condition, context, ExprPosition::Value, state),
            then_body: then_body
                .iter()
                .map(|stmt| lower_stmt_with_context(stmt, context, state))
                .collect(),
            else_body: else_body.as_ref().map(|block| {
                block
                    .iter()
                    .map(|stmt| lower_stmt_with_context(stmt, context, state))
                    .collect()
            }),
        },
        TypedStmt::While { condition, body } => RustStmt::While {
            condition: lower_expr_with_context(condition, context, ExprPosition::Value, state),
            body: body
                .iter()
                .map(|stmt| lower_stmt_with_context(stmt, context, state))
                .collect(),
        },
        TypedStmt::For {
            binding,
            iter,
            body,
        } => RustStmt::For {
            binding: lower_destructure_pattern(binding),
            iter: lower_expr_with_context(iter, context, ExprPosition::Value, state),
            body: body
                .iter()
                .map(|stmt| lower_stmt_with_context(stmt, context, state))
                .collect(),
        },
        TypedStmt::Loop { body } => RustStmt::Loop {
            body: body
                .iter()
                .map(|stmt| lower_stmt_with_context(stmt, context, state))
                .collect(),
        },
        TypedStmt::Break => RustStmt::Break,
        TypedStmt::Continue => RustStmt::Continue,
        TypedStmt::RustBlock(code) => RustStmt::Raw(code.clone()),
        TypedStmt::Expr(expr) => RustStmt::Expr(lower_expr_with_context(
            expr,
            context,
            ExprPosition::Value,
            state,
        )),
    }
}

fn lower_expr_with_context(
    expr: &TypedExpr,
    context: &mut LoweringContext,
    position: ExprPosition,
    state: &mut LoweringState,
) -> RustExpr {
    match &expr.kind {
        TypedExprKind::Int(value) => RustExpr::Int(*value),
        TypedExprKind::Bool(value) => RustExpr::Bool(*value),
        TypedExprKind::String(value) => RustExpr::String(value.clone()),
        TypedExprKind::Char(value) => RustExpr::Char(*value),
        TypedExprKind::Path(path) => {
            lower_path_expr(expr, path, &expr.ty, position, context, state)
        }
        TypedExprKind::Call { callee, args } => {
            if let TypedExprKind::Path(path) = &callee.kind
                && path.len() == 1
                && matches!(path[0].as_str(), "assert" | "assert_eq" | "assert_ne")
            {
                return RustExpr::MacroCall {
                    path: vec![path[0].clone()],
                    args: args
                        .iter()
                        .map(|arg| {
                            lower_expr_with_context(arg, context, ExprPosition::Value, state)
                        })
                        .collect(),
                };
            }
            let (lowered_callee, arg_modes) = lower_call_callee(callee, args, context, state);
            RustExpr::Call {
                callee: Box::new(lowered_callee),
                args: args
                    .iter()
                    .enumerate()
                    .map(|(index, arg)| {
                        if arg_modes.get(index) == Some(&CallArgMode::Borrowed) {
                            let lowered =
                                lower_expr_with_context(arg, context, ExprPosition::Value, state);
                            borrow_expr(lowered)
                        } else {
                            lower_expr_with_context(arg, context, ExprPosition::CallArgOwned, state)
                        }
                    })
                    .collect(),
            }
        }
        TypedExprKind::MacroCall { path, args } => RustExpr::MacroCall {
            path: path.clone(),
            args: args
                .iter()
                .map(|arg| lower_expr_with_context(arg, context, ExprPosition::Value, state))
                .collect(),
        },
        TypedExprKind::Field { base, field } => {
            let remaining_conflicting_before = if position == ExprPosition::ProjectionBase {
                0
            } else {
                let remaining = context.ownership_plan.remaining_conflicting_for_expr(expr);
                context.ownership_plan.consume_expr(expr);
                remaining
            };
            let place_name = place_for_expr(expr).map(|place| place.display_name());
            let lowered_field = RustExpr::Field {
                base: Box::new(lower_expr_with_context(
                    base,
                    context,
                    ExprPosition::ProjectionBase,
                    state,
                )),
                field: field.clone(),
            };
            if position == ExprPosition::CallArgOwned
                && remaining_conflicting_before > 1
                && should_clone_for_reuse(&expr.ty, state)
            {
                if let Some(name) = place_name {
                    let scope = if context.scope_name.is_empty() {
                        "<module>"
                    } else {
                        context.scope_name.as_str()
                    };
                    state.push_ownership_note(format!(
                        "auto-clone inserted in `{scope}` for `{name}` of type `{}`",
                        expr.ty.trim()
                    ));
                }
                clone_expr(lowered_field)
            } else {
                lowered_field
            }
        }
        TypedExprKind::Index { base, index } => {
            let remaining_conflicting_before = if position == ExprPosition::ProjectionBase {
                0
            } else {
                let remaining = context.ownership_plan.remaining_conflicting_for_expr(expr);
                context.ownership_plan.consume_expr(expr);
                remaining
            };
            let place_name = place_for_expr(expr).map(|place| place.display_name());
            let lowered_index_expr = RustExpr::Index {
                base: Box::new(lower_expr_with_context(
                    base,
                    context,
                    ExprPosition::ProjectionBase,
                    state,
                )),
                index: Box::new(lower_expr_with_context(
                    index,
                    context,
                    ExprPosition::Value,
                    state,
                )),
            };
            if position == ExprPosition::CallArgOwned
                && remaining_conflicting_before > 1
                && should_clone_for_reuse(&expr.ty, state)
            {
                if let Some(name) = place_name {
                    let scope = if context.scope_name.is_empty() {
                        "<module>"
                    } else {
                        context.scope_name.as_str()
                    };
                    state.push_ownership_note(format!(
                        "auto-clone inserted in `{scope}` for `{name}` of type `{}`",
                        expr.ty.trim()
                    ));
                }
                clone_expr(lowered_index_expr)
            } else {
                lowered_index_expr
            }
        }
        TypedExprKind::Match { scrutinee, arms } => RustExpr::Match {
            scrutinee: Box::new(lower_expr_with_context(
                scrutinee,
                context,
                ExprPosition::Value,
                state,
            )),
            arms: arms
                .iter()
                .map(|arm| RustMatchArm {
                    pattern: lower_pattern_to_rust(&arm.pattern),
                    guard: arm.guard.as_ref().and_then(|guard| {
                        if matches!(guard.kind, TypedExprKind::Bool(true)) {
                            None
                        } else {
                            Some(lower_expr_with_context(
                                guard,
                                context,
                                ExprPosition::Value,
                                state,
                            ))
                        }
                    }),
                    value: lower_expr_with_context(&arm.value, context, ExprPosition::Value, state),
                })
                .collect(),
        },
        TypedExprKind::Unary { op, expr: inner } => {
            let mut lowered_expr =
                lower_expr_with_context(inner, context, ExprPosition::Value, state);
            match op {
                TypedUnaryOp::Not => RustExpr::Unary {
                    op: RustUnaryOp::Not,
                    expr: Box::new(lowered_expr),
                },
                TypedUnaryOp::Neg => {
                    if is_numeric_type_name(&expr.ty) && inner.ty != expr.ty {
                        lowered_expr = cast_expr(lowered_expr, expr.ty.clone());
                    }
                    RustExpr::Unary {
                        op: RustUnaryOp::Neg,
                        expr: Box::new(lowered_expr),
                    }
                }
            }
        }
        TypedExprKind::Binary { op, left, right } => {
            let mut lowered_left =
                lower_expr_with_context(left, context, ExprPosition::Value, state);
            let mut lowered_right =
                lower_expr_with_context(right, context, ExprPosition::Value, state);

            if matches!(
                op,
                TypedBinaryOp::Add
                    | TypedBinaryOp::Sub
                    | TypedBinaryOp::Mul
                    | TypedBinaryOp::Div
                    | TypedBinaryOp::Rem
            ) && is_numeric_type_name(&expr.ty)
            {
                lowered_left = cast_expr_to_numeric_if_needed(lowered_left, &left.ty, &expr.ty);
                lowered_right =
                    cast_expr_to_numeric_if_needed(lowered_right, &right.ty, &expr.ty);
            } else if matches!(
                op,
                TypedBinaryOp::Eq
                    | TypedBinaryOp::Ne
                    | TypedBinaryOp::Lt
                    | TypedBinaryOp::Le
                    | TypedBinaryOp::Gt
                    | TypedBinaryOp::Ge
            ) {
                if let Some(cmp_ty) = resolve_common_numeric_output_name(&left.ty, &right.ty) {
                    lowered_left = cast_expr_to_numeric_if_needed(lowered_left, &left.ty, &cmp_ty);
                    lowered_right =
                        cast_expr_to_numeric_if_needed(lowered_right, &right.ty, &cmp_ty);
                }
            }

            RustExpr::Binary {
                op: match op {
                    TypedBinaryOp::Add => RustBinaryOp::Add,
                    TypedBinaryOp::Sub => RustBinaryOp::Sub,
                    TypedBinaryOp::Mul => RustBinaryOp::Mul,
                    TypedBinaryOp::Div => RustBinaryOp::Div,
                    TypedBinaryOp::Rem => RustBinaryOp::Rem,
                    TypedBinaryOp::And => RustBinaryOp::And,
                    TypedBinaryOp::Or => RustBinaryOp::Or,
                    TypedBinaryOp::Eq => RustBinaryOp::Eq,
                    TypedBinaryOp::Ne => RustBinaryOp::Ne,
                    TypedBinaryOp::Lt => RustBinaryOp::Lt,
                    TypedBinaryOp::Le => RustBinaryOp::Le,
                    TypedBinaryOp::Gt => RustBinaryOp::Gt,
                    TypedBinaryOp::Ge => RustBinaryOp::Ge,
                },
                left: Box::new(lowered_left),
                right: Box::new(lowered_right),
            }
        }
        TypedExprKind::Array(items) => RustExpr::Array(
            items
                .iter()
                .map(|item| lower_expr_with_context(item, context, ExprPosition::Value, state))
                .collect(),
        ),
        TypedExprKind::Tuple(items) => RustExpr::Tuple(
            items
                .iter()
                .map(|item| lower_expr_with_context(item, context, ExprPosition::Value, state))
                .collect(),
        ),
        TypedExprKind::StructLiteral { path, fields } => RustExpr::StructLiteral {
            path: path.clone(),
            fields: fields
                .iter()
                .map(|field| RustStructLiteralField {
                    name: field.name.clone(),
                    value: lower_expr_with_context(
                        &field.value,
                        context,
                        ExprPosition::Value,
                        state,
                    ),
                })
                .collect(),
        },
        TypedExprKind::Closure {
            params,
            return_type,
            body,
        } => {
            let mut closure_context = LoweringContext {
                ownership_plan: OwnershipPlan::from_stmts(body),
                scope_name: format!("{}::<closure>", context.scope_name),
            };
            RustExpr::Closure {
                params: params
                    .iter()
                    .map(|p| RustParam {
                        name: p.name.clone(),
                        ty: p.ty.clone(),
                    })
                    .collect(),
                return_type: return_type.clone(),
                body: body
                    .iter()
                    .map(|stmt| lower_stmt_with_context(stmt, &mut closure_context, state))
                    .collect(),
            }
        }
        TypedExprKind::Range {
            start,
            end,
            inclusive,
        } => RustExpr::Range {
            start: start.as_ref().map(|expr| {
                Box::new(lower_expr_with_context(
                    expr,
                    context,
                    ExprPosition::Value,
                    state,
                ))
            }),
            end: end.as_ref().map(|expr| {
                Box::new(lower_expr_with_context(
                    expr,
                    context,
                    ExprPosition::Value,
                    state,
                ))
            }),
            inclusive: *inclusive,
        },
        TypedExprKind::Try(inner) => RustExpr::Try(Box::new(lower_expr_with_context(
            inner,
            context,
            ExprPosition::Value,
            state,
        ))),
    }
}

fn lower_path_expr(
    expr: &TypedExpr,
    path: &[String],
    ty: &str,
    position: ExprPosition,
    context: &mut LoweringContext,
    state: &mut LoweringState,
) -> RustExpr {
    if position == ExprPosition::ProjectionBase {
        return RustExpr::Path(path.to_vec());
    }
    if path.len() != 1 {
        return RustExpr::Path(path.to_vec());
    }

    let name = path[0].clone();
    let remaining_conflicting = context.ownership_plan.remaining_conflicting_for_expr(expr);
    context.ownership_plan.consume_expr(expr);

    let path_expr = RustExpr::Path(path.to_vec());
    if position == ExprPosition::CallArgOwned
        && remaining_conflicting > 1
        && should_clone_for_reuse(ty, state)
    {
        let scope = if context.scope_name.is_empty() {
            "<module>"
        } else {
            context.scope_name.as_str()
        };
        state.push_ownership_note(format!(
            "auto-clone inserted in `{scope}` for `{name}` of type `{}`",
            ty.trim()
        ));
        return clone_expr(path_expr);
    }
    path_expr
}

fn cast_expr(expr: RustExpr, ty: String) -> RustExpr {
    RustExpr::Cast {
        expr: Box::new(expr),
        ty,
    }
}

fn cast_expr_to_numeric_if_needed(expr: RustExpr, from_ty: &str, to_ty: &str) -> RustExpr {
    if from_ty == to_ty {
        return expr;
    }
    if !is_numeric_type_name(from_ty) || !is_numeric_type_name(to_ty) {
        return expr;
    }
    cast_expr(expr, to_ty.to_string())
}

fn resolve_common_numeric_output_name(left: &str, right: &str) -> Option<String> {
    if !is_numeric_type_name(left) || !is_numeric_type_name(right) {
        return None;
    }
    Some(promote_numeric_names(left, right).to_string())
}

fn should_clone_for_reuse(ty: &str, state: &LoweringState) -> bool {
    state.registry.is_clone_candidate(ty)
}

fn clone_expr(expr: RustExpr) -> RustExpr {
    RustExpr::Call {
        callee: Box::new(RustExpr::Field {
            base: Box::new(expr),
            field: "clone".to_string(),
        }),
        args: Vec::new(),
    }
}

fn borrow_expr(expr: RustExpr) -> RustExpr {
    RustExpr::Borrow(Box::new(expr))
}

fn lower_call_callee(
    callee: &TypedExpr,
    args: &[TypedExpr],
    context: &mut LoweringContext,
    state: &mut LoweringState,
) -> (RustExpr, Vec<CallArgMode>) {
    let arg_count = args.len();
    if let Some(shim) = resolve_interop_shim(callee, state) {
        state.used_shims.insert(shim.name.clone());
        return (
            RustExpr::Path(vec![shim.name.clone()]),
            interop_arg_modes(&shim, arg_count),
        );
    }
    if let Some(modes) = resolve_method_call_modes(callee, args, context, state) {
        return (
            lower_method_callee(callee, modes.receiver, context, state),
            modes.args,
        );
    }
    if let Some(modes) = resolve_direct_borrow_arg_modes(callee, state, arg_count) {
        return (
            lower_expr_with_context(callee, context, ExprPosition::Value, state),
            modes,
        );
    }
    if let Some(modes) = resolve_heuristic_path_call_arg_modes(callee, args, context, state) {
        return (
            lower_expr_with_context(callee, context, ExprPosition::Value, state),
            modes,
        );
    }
    (
        lower_expr_with_context(callee, context, ExprPosition::Value, state),
        vec![CallArgMode::Owned; arg_count],
    )
}

fn resolve_interop_shim(callee: &TypedExpr, state: &LoweringState) -> Option<InteropShimDef> {
    let TypedExprKind::Path(path) = &callee.kind else {
        return None;
    };
    let resolved = state.resolve_callee_path(path);
    state.registry.resolve_shim(&resolved).cloned()
}

fn interop_arg_modes(shim: &InteropShimDef, arg_count: usize) -> Vec<CallArgMode> {
    let mut modes = vec![CallArgMode::Owned; arg_count];
    for index in &shim.borrowed_arg_indexes {
        set_borrowed(&mut modes, *index);
    }
    modes
}

fn resolve_direct_borrow_arg_modes(
    callee: &TypedExpr,
    state: &LoweringState,
    arg_count: usize,
) -> Option<Vec<CallArgMode>> {
    let TypedExprKind::Path(path) = &callee.kind else {
        return None;
    };
    let resolved = state.resolve_callee_path(path);
    state
        .registry
        .resolve_direct_borrow_modes(&resolved, arg_count)
}

fn resolve_method_call_modes(
    callee: &TypedExpr,
    args: &[TypedExpr],
    context: &LoweringContext,
    state: &LoweringState,
) -> Option<MethodCallModes> {
    let TypedExprKind::Field { base, field } = &callee.kind else {
        return None;
    };

    let mut arg_modes = vec![CallArgMode::Owned; args.len()];
    let receiver = if method_receiver_is_borrowed(&base.ty, field) {
        if method_borrows_first_arg(&base.ty, field) {
            set_borrowed(&mut arg_modes, 0);
        }
        CallArgMode::Borrowed
    } else if method_receiver_should_borrow_heuristic(&base.ty, field) {
        for (index, arg) in args.iter().enumerate() {
            if method_arg_should_borrow_heuristic(field, arg, context, state) {
                set_borrowed(&mut arg_modes, index);
            }
        }
        CallArgMode::Borrowed
    } else {
        CallArgMode::Owned
    };

    Some(MethodCallModes {
        receiver,
        args: arg_modes,
    })
}

fn resolve_heuristic_path_call_arg_modes(
    callee: &TypedExpr,
    args: &[TypedExpr],
    context: &LoweringContext,
    state: &LoweringState,
) -> Option<Vec<CallArgMode>> {
    let TypedExprKind::Path(path) = &callee.kind else {
        return None;
    };
    let Some(name) = path.last() else {
        return None;
    };
    if !call_name_suggests_read_only(name) {
        return None;
    }
    let mut modes = vec![CallArgMode::Owned; args.len()];
    let mut borrowed_any = false;
    for (index, arg) in args.iter().enumerate() {
        if method_arg_should_borrow_heuristic(name, arg, context, state) {
            set_borrowed(&mut modes, index);
            borrowed_any = true;
        }
    }
    if borrowed_any {
        Some(modes)
    } else {
        None
    }
}

fn lower_method_callee(
    callee: &TypedExpr,
    receiver_mode: CallArgMode,
    context: &mut LoweringContext,
    state: &mut LoweringState,
) -> RustExpr {
    let TypedExprKind::Field { base, field } = &callee.kind else {
        return lower_expr_with_context(callee, context, ExprPosition::Value, state);
    };
    let receiver_position = if receiver_mode == CallArgMode::Owned {
        ExprPosition::CallArgOwned
    } else {
        ExprPosition::Value
    };
    RustExpr::Field {
        base: Box::new(lower_expr_with_context(
            base,
            context,
            receiver_position,
            state,
        )),
        field: field.clone(),
    }
}

fn method_receiver_is_borrowed(base_ty: &str, field: &str) -> bool {
    match last_path_segment(base_ty) {
        "String" => matches!(
            field,
            "len"
                | "is_empty"
                | "contains"
                | "starts_with"
                | "ends_with"
                | "strip_prefix"
                | "split_once"
        ),
        "Vec" => matches!(
            field,
            "len" | "is_empty" | "contains" | "first" | "last" | "get" | "push"
        ),
        "Option" => matches!(field, "is_some" | "is_none"),
        "Result" => matches!(field, "is_ok" | "is_err"),
        "HashMap" | "BTreeMap" => matches!(field, "len" | "is_empty" | "contains_key"),
        "HashSet" | "BTreeSet" => matches!(field, "len" | "is_empty" | "contains"),
        _ => false,
    }
}

fn method_borrows_first_arg(base_ty: &str, field: &str) -> bool {
    match last_path_segment(base_ty) {
        "String" => matches!(
            field,
            "contains" | "starts_with" | "ends_with" | "strip_prefix" | "split_once"
        ),
        "Vec" => matches!(field, "contains"),
        "HashMap" | "BTreeMap" => matches!(field, "contains_key"),
        "HashSet" | "BTreeSet" => matches!(field, "contains"),
        _ => false,
    }
}

fn method_receiver_should_borrow_heuristic(base_ty: &str, method: &str) -> bool {
    if method_name_suggests_consuming(method) {
        return false;
    }
    if call_name_suggests_read_only(method) || method_name_suggests_mutating(method) {
        return true;
    }
    !is_copy_primitive_type(last_path_segment(base_ty))
}

fn method_arg_should_borrow_heuristic(
    method: &str,
    arg: &TypedExpr,
    context: &LoweringContext,
    state: &LoweringState,
) -> bool {
    if !call_name_suggests_read_only(method) {
        return false;
    }
    let remaining_conflicting = context.ownership_plan.remaining_conflicting_for_expr(arg);
    if remaining_conflicting <= 1 && !type_is_string_like(&arg.ty) {
        return false;
    }
    type_should_prefer_borrow(&arg.ty, state)
}

fn type_should_prefer_borrow(ty: &str, state: &LoweringState) -> bool {
    let ty = ty.trim();
    let (head, args) = split_type_head_and_args(ty);
    if args.is_empty() && is_copy_primitive_type(head) {
        return false;
    }
    if matches!(
        last_path_segment(head),
        "str"
            | "String"
            | "Vec"
            | "HashMap"
            | "BTreeMap"
            | "HashSet"
            | "BTreeSet"
            | "Option"
            | "Result"
    ) {
        return true;
    }
    state.registry.is_clone_candidate(ty)
}

fn type_is_string_like(ty: &str) -> bool {
    matches!(last_path_segment(ty.trim()), "str" | "String")
}

fn call_name_suggests_read_only(name: &str) -> bool {
    matches!(
        name,
        "len"
            | "is_empty"
            | "contains"
            | "contains_key"
            | "starts_with"
            | "ends_with"
            | "strip_prefix"
            | "split_once"
            | "first"
            | "last"
            | "get"
            | "find"
            | "position"
            | "peek"
            | "iter"
            | "keys"
            | "values"
            | "as_str"
            | "as_slice"
            | "is_some"
            | "is_none"
            | "is_ok"
            | "is_err"
    ) || name.starts_with("is_")
        || name.starts_with("has_")
        || name.starts_with("as_")
}

fn method_name_suggests_consuming(name: &str) -> bool {
    matches!(
        name,
        "into_inner"
            | "into_bytes"
            | "unwrap"
            | "expect"
            | "take"
            | "remove"
            | "pop"
            | "drain"
            | "split_off"
            | "replace"
            | "swap_remove"
            | "finish"
            | "build"
            | "collect"
    ) || name.starts_with("into_")
}

fn method_name_suggests_mutating(name: &str) -> bool {
    matches!(
        name,
        "push"
            | "push_str"
            | "insert"
            | "append"
            | "extend"
            | "clear"
            | "sort"
            | "sort_by"
            | "retain"
            | "truncate"
            | "reserve"
    ) || name.starts_with("set_")
        || name.starts_with("update_")
}

fn lower_interop_shim(shim: &InteropShimDef) -> RustFunction {
    let params = shim
        .param_types
        .iter()
        .enumerate()
        .map(|(index, ty)| RustParam {
            name: format!("__arg{index}"),
            ty: ty.clone(),
        })
        .collect::<Vec<_>>();
    let args = params
        .iter()
        .map(|param| RustExpr::Path(vec![param.name.clone()]))
        .collect::<Vec<_>>();
    let body = match shim.body {
        InteropShimBody::DirectCall => vec![RustStmt::Return(Some(RustExpr::Call {
            callee: Box::new(RustExpr::Path(shim.target_path.clone())),
            args,
        }))],
        InteropShimBody::StrStripPrefixKnown => {
            let strip_call = RustExpr::Call {
                callee: Box::new(RustExpr::Path(shim.target_path.clone())),
                args,
            };
            let unwrap_call = RustExpr::Call {
                callee: Box::new(RustExpr::Field {
                    base: Box::new(strip_call),
                    field: "unwrap".to_string(),
                }),
                args: Vec::new(),
            };
            let to_string_call = RustExpr::Call {
                callee: Box::new(RustExpr::Field {
                    base: Box::new(unwrap_call),
                    field: "to_string".to_string(),
                }),
                args: Vec::new(),
            };
            vec![RustStmt::Return(Some(to_string_call))]
        }
        InteropShimBody::StrSplitOnceKnown => {
            let split_call = RustExpr::Call {
                callee: Box::new(RustExpr::Path(shim.target_path.clone())),
                args,
            };
            let unwrap_call = RustExpr::Call {
                callee: Box::new(RustExpr::Field {
                    base: Box::new(split_call),
                    field: "unwrap".to_string(),
                }),
                args: Vec::new(),
            };
            vec![
                RustStmt::DestructureConst {
                    pattern: RustDestructurePattern::Tuple(vec![
                        RustDestructurePattern::Name("__left".to_string()),
                        RustDestructurePattern::Name("__right".to_string()),
                    ]),
                    value: unwrap_call,
                },
                RustStmt::Return(Some(RustExpr::Tuple(vec![
                    RustExpr::Call {
                        callee: Box::new(RustExpr::Field {
                            base: Box::new(RustExpr::Path(vec!["__left".to_string()])),
                            field: "to_string".to_string(),
                        }),
                        args: Vec::new(),
                    },
                    RustExpr::Call {
                        callee: Box::new(RustExpr::Field {
                            base: Box::new(RustExpr::Path(vec!["__right".to_string()])),
                            field: "to_string".to_string(),
                        }),
                        args: Vec::new(),
                    },
                ]))),
            ]
        }
    };
    RustFunction {
        is_public: false,
        name: shim.name.clone(),
        type_params: Vec::new(),
        params,
        return_type: shim.return_type.clone(),
        body,
    }
}

fn set_borrowed(modes: &mut [CallArgMode], index: usize) {
    if let Some(slot) = modes.get_mut(index) {
        *slot = CallArgMode::Borrowed;
    }
}

fn last_path_segment(path: &str) -> &str {
    let segment = path.rsplit("::").next().unwrap_or(path);
    segment.split('<').next().unwrap_or(segment)
}

fn is_known_clone_container(head: &str) -> bool {
    matches!(
        head,
        "Option"
            | "Result"
            | "Vec"
            | "HashMap"
            | "BTreeMap"
            | "std::option::Option"
            | "core::option::Option"
            | "std::result::Result"
            | "core::result::Result"
            | "std::vec::Vec"
            | "alloc::vec::Vec"
            | "std::collections::HashMap"
            | "std::collections::BTreeMap"
    )
}

fn is_copy_primitive_type(head: &str) -> bool {
    matches!(
        head,
        "bool"
            | "char"
            | "i8"
            | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "isize"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "usize"
            | "f32"
            | "f64"
    )
}

fn is_probably_nominal_type(head: &str) -> bool {
    head.chars()
        .next()
        .map(|ch| ch.is_ascii_uppercase())
        .unwrap_or(false)
}

fn split_type_head_and_args(ty: &str) -> (&str, Vec<&str>) {
    let ty = ty.trim();
    let Some(start) = ty.find('<') else {
        return (ty, Vec::new());
    };
    if !ty.ends_with('>') || start == 0 {
        return (ty, Vec::new());
    }
    let head = ty[..start].trim();
    let body = &ty[start + 1..ty.len() - 1];
    (head, split_top_level_commas(body))
}

fn parse_tuple_items(ty: &str) -> Option<Vec<&str>> {
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

fn place_for_expr(expr: &TypedExpr) -> Option<OwnershipPlace> {
    match &expr.kind {
        TypedExprKind::Path(path) if path.len() == 1 => Some(OwnershipPlace::from_root(&path[0])),
        TypedExprKind::Field { base, field } => place_for_expr(base)
            .map(|place| place.with_projection(OwnershipProjection::Field(field.clone()))),
        TypedExprKind::Index { base, .. } => {
            place_for_expr(base).map(|place| place.with_projection(OwnershipProjection::Index))
        }
        _ => None,
    }
}

fn collect_place_uses_in_stmts(stmts: &[TypedStmt]) -> HashMap<OwnershipPlace, usize> {
    let mut uses = HashMap::new();
    for stmt in stmts {
        collect_place_uses_in_stmt(stmt, &mut uses);
    }
    uses
}

fn collect_place_uses_in_stmt(stmt: &TypedStmt, uses: &mut HashMap<OwnershipPlace, usize>) {
    match stmt {
        TypedStmt::Const(def) => collect_place_uses_in_expr(&def.value, uses),
        TypedStmt::DestructureConst { value, .. } => collect_place_uses_in_expr(value, uses),
        TypedStmt::Assign { target, value, .. } => {
            collect_place_uses_in_expr(value, uses);
            collect_place_uses_in_assign_target(target, uses);
        }
        TypedStmt::Return(Some(expr)) => collect_place_uses_in_expr(expr, uses),
        TypedStmt::Return(None) => {}
        TypedStmt::If {
            condition,
            then_body,
            else_body,
        } => {
            collect_place_uses_in_expr(condition, uses);
            for stmt in then_body {
                collect_place_uses_in_stmt(stmt, uses);
            }
            if let Some(else_body) = else_body {
                for stmt in else_body {
                    collect_place_uses_in_stmt(stmt, uses);
                }
            }
        }
        TypedStmt::While { condition, body } => {
            collect_place_uses_in_expr(condition, uses);
            for stmt in body {
                collect_place_uses_in_stmt(stmt, uses);
            }
        }
        TypedStmt::For { iter, body, .. } => {
            collect_place_uses_in_expr(iter, uses);
            for stmt in body {
                collect_place_uses_in_stmt(stmt, uses);
            }
        }
        TypedStmt::Loop { body } => {
            for stmt in body {
                collect_place_uses_in_stmt(stmt, uses);
            }
        }
        TypedStmt::Break | TypedStmt::Continue => {}
        TypedStmt::RustBlock(_) => {}
        TypedStmt::Expr(expr) => collect_place_uses_in_expr(expr, uses),
    }
}

fn collect_place_uses_in_assign_target(
    target: &TypedAssignTarget,
    uses: &mut HashMap<OwnershipPlace, usize>,
) {
    match target {
        TypedAssignTarget::Path(_) => {}
        TypedAssignTarget::Field { base, .. } => collect_place_uses_in_expr(base, uses),
        TypedAssignTarget::Index { base, index } => {
            collect_place_uses_in_expr(base, uses);
            collect_place_uses_in_expr(index, uses);
        }
        TypedAssignTarget::Tuple(items) => {
            for item in items {
                collect_place_uses_in_assign_target(item, uses);
            }
        }
    }
}

fn collect_place_uses_in_expr(expr: &TypedExpr, uses: &mut HashMap<OwnershipPlace, usize>) {
    if let Some(place) = place_for_expr(expr) {
        *uses.entry(place).or_insert(0) += 1;
        return;
    }

    match &expr.kind {
        TypedExprKind::Call { callee, args } => {
            if let TypedExprKind::Field { base, .. } = &callee.kind {
                collect_place_uses_in_expr(base, uses);
            } else {
                collect_place_uses_in_expr(callee, uses);
            }
            for arg in args {
                collect_place_uses_in_expr(arg, uses);
            }
        }
        TypedExprKind::MacroCall { args, .. } => {
            for arg in args {
                collect_place_uses_in_expr(arg, uses);
            }
        }
        TypedExprKind::Field { .. } | TypedExprKind::Index { .. } => {}
        TypedExprKind::Match { scrutinee, arms } => {
            collect_place_uses_in_expr(scrutinee, uses);
            for arm in arms {
                collect_place_uses_in_expr(&arm.value, uses);
            }
        }
        TypedExprKind::Unary { expr, .. } => collect_place_uses_in_expr(expr, uses),
        TypedExprKind::Binary { left, right, .. } => {
            collect_place_uses_in_expr(left, uses);
            collect_place_uses_in_expr(right, uses);
        }
        TypedExprKind::Array(items) | TypedExprKind::Tuple(items) => {
            for item in items {
                collect_place_uses_in_expr(item, uses);
            }
        }
        TypedExprKind::StructLiteral { fields, .. } => {
            for field in fields {
                collect_place_uses_in_expr(&field.value, uses);
            }
        }
        TypedExprKind::Closure { .. } => {}
        TypedExprKind::Range { start, end, .. } => {
            if let Some(start) = start {
                collect_place_uses_in_expr(start, uses);
            }
            if let Some(end) = end {
                collect_place_uses_in_expr(end, uses);
            }
        }
        TypedExprKind::Try(inner) => collect_place_uses_in_expr(inner, uses),
        TypedExprKind::Int(_)
        | TypedExprKind::Bool(_)
        | TypedExprKind::String(_)
        | TypedExprKind::Char(_)
        | TypedExprKind::Path(_) => {}
    }
}

fn type_from_ast(ty: &Type) -> SemType {
    if ty.path.len() == 1 && ty.path[0] == "Tuple" {
        return SemType::Tuple(ty.args.iter().map(type_from_ast).collect());
    }
    SemType::Path {
        path: ty.path.clone(),
        args: ty.args.iter().map(type_from_ast).collect(),
    }
}

fn type_from_ast_with_impl_self(ty: &Type, impl_target: &str) -> SemType {
    if ty.path.len() == 1 && ty.path[0] == "Tuple" {
        return SemType::Tuple(
            ty.args
                .iter()
                .map(|arg| type_from_ast_with_impl_self(arg, impl_target))
                .collect(),
        );
    }
    let head = ty.path.first().map(|part| part.as_str());
    if ty.path.len() == 1 && head == Some("Self") {
        return SemType::Path {
            path: vec![impl_target.to_string()],
            args: Vec::new(),
        };
    }
    SemType::Path {
        path: ty.path.clone(),
        args: ty
            .args
            .iter()
            .map(|arg| type_from_ast_with_impl_self(arg, impl_target))
            .collect(),
    }
}

fn type_to_string(ty: &SemType) -> String {
    match ty {
        SemType::Unit => "()".to_string(),
        SemType::Unknown => "_".to_string(),
        SemType::Tuple(items) => {
            let text = items
                .iter()
                .map(type_to_string)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({text})")
        }
        SemType::Fn { params, ret } => {
            let params = params
                .iter()
                .map(type_to_string)
                .collect::<Vec<_>>()
                .join(", ");
            format!("fn({params}) -> {}", type_to_string(ret))
        }
        SemType::Iter(item) => format!("Iter<{}>", type_to_string(item)),
        SemType::Path { path, args } => {
            let head = path.join("::");
            if args.is_empty() {
                head
            } else {
                let args_text = args
                    .iter()
                    .map(type_to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{head}<{args_text}>")
            }
        }
    }
}

fn type_satisfies_bound(actual: &SemType, bound: &SemType) -> bool {
    if *actual == SemType::Unknown {
        return true;
    }
    let Some(bound_name) = bound_trait_name(bound) else {
        return true;
    };
    match bound_name {
        "Clone" => type_is_clone(actual),
        "Copy" => type_is_copy(actual),
        _ => true,
    }
}

fn bound_trait_name(bound: &SemType) -> Option<&str> {
    if let SemType::Path { path, .. } = bound {
        return path.last().map(|part| part.as_str());
    }
    None
}

fn type_is_clone(ty: &SemType) -> bool {
    match ty {
        SemType::Unknown | SemType::Unit => true,
        SemType::Tuple(items) => items.iter().all(type_is_clone),
        SemType::Iter(item) => type_is_clone(item),
        SemType::Fn { .. } => false,
        SemType::Path { path, args } => {
            let Some(head) = path.last().map(|part| part.as_str()) else {
                return false;
            };
            if is_copy_primitive_type(head) || head == "String" {
                return true;
            }
            if matches!(
                head,
                "Option"
                    | "Result"
                    | "Vec"
                    | "HashMap"
                    | "BTreeMap"
                    | "HashSet"
                    | "BTreeSet"
            ) {
                return args.iter().all(type_is_clone);
            }
            is_probably_nominal_type(head)
        }
    }
}

fn type_is_copy(ty: &SemType) -> bool {
    match ty {
        SemType::Unknown | SemType::Unit => true,
        SemType::Tuple(items) => items.iter().all(type_is_copy),
        SemType::Iter(_) | SemType::Fn { .. } => false,
        SemType::Path { path, args } => {
            let Some(head) = path.last().map(|part| part.as_str()) else {
                return false;
            };
            args.is_empty() && is_copy_primitive_type(head)
        }
    }
}

fn bind_generic_params(
    expected: &SemType,
    actual: &SemType,
    type_params: &HashSet<String>,
    bindings: &mut HashMap<String, SemType>,
) -> bool {
    match expected {
        SemType::Path { path, args } if path.len() == 1 && args.is_empty() => {
            let name = &path[0];
            if type_params.contains(name) {
                if let Some(existing) = bindings.get(name) {
                    return is_compatible(actual, existing) && is_compatible(existing, actual);
                }
                bindings.insert(name.clone(), actual.clone());
                return true;
            }
        }
        _ => {}
    }

    match (expected, actual) {
        (
            SemType::Path {
                path: ep,
                args: ea,
            },
            SemType::Path {
                path: ap,
                args: aa,
            },
        ) => {
            if ep != ap || ea.len() != aa.len() {
                return false;
            }
            ea.iter()
                .zip(aa.iter())
                .all(|(left, right)| bind_generic_params(left, right, type_params, bindings))
        }
        (SemType::Tuple(et), SemType::Tuple(at)) => {
            if et.len() != at.len() {
                return false;
            }
            et.iter()
                .zip(at.iter())
                .all(|(left, right)| bind_generic_params(left, right, type_params, bindings))
        }
        (
            SemType::Fn {
                params: ep,
                ret: er,
            },
            SemType::Fn {
                params: ap,
                ret: ar,
            },
        ) => {
            if ep.len() != ap.len() {
                return false;
            }
            ep.iter()
                .zip(ap.iter())
                .all(|(left, right)| bind_generic_params(left, right, type_params, bindings))
                && bind_generic_params(er, ar, type_params, bindings)
        }
        (SemType::Iter(expected_item), SemType::Iter(actual_item)) => {
            bind_generic_params(expected_item, actual_item, type_params, bindings)
        }
        _ => false,
    }
}

fn substitute_generic_type(
    ty: &SemType,
    type_params: &HashSet<String>,
    bindings: &HashMap<String, SemType>,
) -> SemType {
    match ty {
        SemType::Path { path, args } if path.len() == 1 && args.is_empty() => {
            if type_params.contains(&path[0]) {
                return bindings
                    .get(&path[0])
                    .cloned()
                    .unwrap_or(SemType::Unknown);
            }
            ty.clone()
        }
        SemType::Path { path, args } => SemType::Path {
            path: path.clone(),
            args: args
                .iter()
                .map(|arg| substitute_generic_type(arg, type_params, bindings))
                .collect(),
        },
        SemType::Tuple(items) => SemType::Tuple(
            items
                .iter()
                .map(|item| substitute_generic_type(item, type_params, bindings))
                .collect(),
        ),
        SemType::Fn { params, ret } => SemType::Fn {
            params: params
                .iter()
                .map(|param| substitute_generic_type(param, type_params, bindings))
                .collect(),
            ret: Box::new(substitute_generic_type(ret, type_params, bindings)),
        },
        SemType::Iter(item) => {
            SemType::Iter(Box::new(substitute_generic_type(item, type_params, bindings)))
        }
        SemType::Unit | SemType::Unknown => ty.clone(),
    }
}

fn is_compatible(actual: &SemType, expected: &SemType) -> bool {
    match (actual, expected) {
        (_, SemType::Unknown) | (SemType::Unknown, _) => true,
        (SemType::Unit, SemType::Unit) => true,
        (SemType::Unit, SemType::Tuple(items)) | (SemType::Tuple(items), SemType::Unit) => {
            items.is_empty()
        }
        (SemType::Tuple(left), SemType::Tuple(right)) => {
            if left.len() != right.len() {
                return false;
            }
            left.iter()
                .zip(right.iter())
                .all(|(a, b)| is_compatible(a, b))
        }
        (
            SemType::Fn {
                params: lp,
                ret: lr,
            },
            SemType::Fn {
                params: rp,
                ret: rr,
            },
        ) => {
            if lp.len() != rp.len() {
                return false;
            }
            lp.iter().zip(rp.iter()).all(|(a, b)| is_compatible(a, b)) && is_compatible(lr, rr)
        }
        (SemType::Iter(left), SemType::Iter(right)) => is_compatible(left, right),
        (
            SemType::Path {
                path: actual_path,
                args: actual_args,
            },
            SemType::Path {
                path: expected_path,
                args: expected_args,
            },
        ) => {
            if actual_path != expected_path || actual_args.len() != expected_args.len() {
                return false;
            }
            actual_args
                .iter()
                .zip(expected_args.iter())
                .all(|(left, right)| is_compatible(left, right))
        }
        _ => false,
    }
}

fn named_type(name: &str) -> SemType {
    SemType::Path {
        path: vec![name.to_string()],
        args: Vec::new(),
    }
}

fn option_type(inner: SemType) -> SemType {
    SemType::Path {
        path: vec!["Option".to_string()],
        args: vec![inner],
    }
}

fn result_type(ok: SemType, err: SemType) -> SemType {
    SemType::Path {
        path: vec!["Result".to_string()],
        args: vec![ok, err],
    }
}

fn option_inner(ty: &SemType) -> Option<&SemType> {
    if let SemType::Path { path, args } = ty {
        if path.len() == 1 && path[0] == "Option" && args.len() == 1 {
            return Some(&args[0]);
        }
    }
    None
}

fn result_parts(ty: &SemType) -> Option<(&SemType, &SemType)> {
    if let SemType::Path { path, args } = ty {
        if path.len() == 1 && path[0] == "Result" && args.len() == 2 {
            return Some((&args[0], &args[1]));
        }
    }
    None
}

fn slice_item_type(ty: &SemType) -> Option<SemType> {
    if let SemType::Path { path, args } = ty
        && path.len() == 1
        && path[0] == "Vec"
        && args.len() == 1
    {
        return Some(args[0].clone());
    }
    None
}

fn lower_pattern(
    pattern: &Pattern,
    scrutinee_ty: &SemType,
    context: &Context,
    locals: &mut HashMap<String, SemType>,
    diagnostics: &mut Vec<Diagnostic>,
) -> TypedPattern {
    match pattern {
        Pattern::Wildcard => TypedPattern::Wildcard,
        Pattern::Binding(name) => {
            locals.insert(name.clone(), scrutinee_ty.clone());
            TypedPattern::Binding(name.clone())
        }
        Pattern::Int(value) => {
            ensure_pattern_type(scrutinee_ty, &named_type("i64"), "int", diagnostics);
            TypedPattern::Int(*value)
        }
        Pattern::Bool(value) => {
            ensure_pattern_type(scrutinee_ty, &named_type("bool"), "bool", diagnostics);
            TypedPattern::Bool(*value)
        }
        Pattern::Char(value) => {
            ensure_pattern_type(scrutinee_ty, &named_type("char"), "char", diagnostics);
            TypedPattern::Char(*value)
        }
        Pattern::String(value) => {
            ensure_pattern_type(scrutinee_ty, &named_type("String"), "string", diagnostics);
            TypedPattern::String(value.clone())
        }
        Pattern::Tuple(items) => {
            if *scrutinee_ty == SemType::Unit && items.is_empty() {
                TypedPattern::Tuple(Vec::new())
            } else if *scrutinee_ty == SemType::Unknown {
                TypedPattern::Tuple(
                    items
                        .iter()
                        .map(|item| {
                            lower_pattern(item, &SemType::Unknown, context, locals, diagnostics)
                        })
                        .collect(),
                )
            } else if let SemType::Tuple(scrutinee_items) = scrutinee_ty {
                if items.len() != scrutinee_items.len() {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Tuple pattern arity mismatch: pattern has {}, scrutinee has {}",
                            items.len(),
                            scrutinee_items.len()
                        ),
                        Span::new(0, 0),
                    ));
                    return TypedPattern::Tuple(
                        items
                            .iter()
                            .map(|item| {
                                lower_pattern(item, &SemType::Unknown, context, locals, diagnostics)
                            })
                            .collect(),
                    );
                }
                TypedPattern::Tuple(
                    items
                        .iter()
                        .zip(scrutinee_items.iter())
                        .map(|(item, item_ty)| {
                            lower_pattern(item, item_ty, context, locals, diagnostics)
                        })
                        .collect(),
                )
            } else {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Tuple pattern requires tuple scrutinee, got `{}`",
                        type_to_string(scrutinee_ty)
                    ),
                    Span::new(0, 0),
                ));
                TypedPattern::Tuple(
                    items
                        .iter()
                        .map(|item| {
                            lower_pattern(item, &SemType::Unknown, context, locals, diagnostics)
                        })
                        .collect(),
                )
            }
        }
        Pattern::Slice {
            prefix,
            rest,
            suffix,
        } => {
            let item_ty = if let Some(item) = slice_item_type(scrutinee_ty) {
                item
            } else {
                if *scrutinee_ty != SemType::Unknown {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Slice pattern requires `Vec<T>` scrutinee, got `{}`",
                            type_to_string(scrutinee_ty)
                        ),
                        Span::new(0, 0),
                    ));
                }
                SemType::Unknown
            };

            if let Some(name) = rest {
                locals.insert(name.clone(), SemType::Unknown);
            }

            TypedPattern::Slice {
                prefix: prefix
                    .iter()
                    .map(|item| lower_pattern(item, &item_ty, context, locals, diagnostics))
                    .collect(),
                rest: rest.clone(),
                suffix: suffix
                    .iter()
                    .map(|item| lower_pattern(item, &item_ty, context, locals, diagnostics))
                    .collect(),
            }
        }
        Pattern::Or(items) => {
            let mut lowered = Vec::new();
            let mut binding_sets = Vec::new();
            for item in items {
                let mut alt_locals = locals.clone();
                let lowered_item =
                    lower_pattern(item, scrutinee_ty, context, &mut alt_locals, diagnostics);
                lowered.push(lowered_item);
                let new_bindings = alt_locals
                    .iter()
                    .filter(|(name, _)| !locals.contains_key(*name))
                    .map(|(name, ty)| (name.clone(), ty.clone()))
                    .collect::<HashMap<_, _>>();
                binding_sets.push(new_bindings);
            }

            if let Some(first) = binding_sets.first() {
                let all_same = binding_sets.iter().all(|set| set == first);
                if all_same {
                    for (name, ty) in first {
                        locals.insert(name.clone(), ty.clone());
                    }
                } else if binding_sets.iter().any(|set| !set.is_empty()) {
                    diagnostics.push(Diagnostic::new(
                        "Or-pattern alternatives must bind the same names with the same types",
                        Span::new(0, 0),
                    ));
                }
            }

            TypedPattern::Or(lowered)
        }
        Pattern::BindingAt { name, pattern } => {
            locals.insert(name.clone(), scrutinee_ty.clone());
            let lowered = lower_pattern(pattern, scrutinee_ty, context, locals, diagnostics);
            TypedPattern::BindingAt {
                name: name.clone(),
                pattern: Box::new(lowered),
            }
        }
        Pattern::Struct {
            path,
            fields,
            has_rest,
        } => {
            let struct_name = path.last().cloned().unwrap_or_default();
            let struct_fields = context.structs.get(&struct_name);
            if struct_fields.is_none() {
                diagnostics.push(Diagnostic::new(
                    format!("Unknown struct `{}` in pattern", path.join("::")),
                    Span::new(0, 0),
                ));
            } else if let SemType::Path {
                path: scrutinee_path,
                ..
            } = scrutinee_ty
            {
                if scrutinee_path.last() != Some(&struct_name) && *scrutinee_ty != SemType::Unknown
                {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Pattern struct `{}` does not match scrutinee type `{}`",
                            struct_name,
                            type_to_string(scrutinee_ty)
                        ),
                        Span::new(0, 0),
                    ));
                }
            } else if *scrutinee_ty != SemType::Unknown {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Struct pattern requires struct scrutinee, got `{}`",
                        type_to_string(scrutinee_ty)
                    ),
                    Span::new(0, 0),
                ));
            }

            if let Some(expected_fields) = struct_fields
                && !*has_rest
            {
                let bound = fields.iter().map(|field| field.name.as_str()).collect::<HashSet<_>>();
                let missing = expected_fields
                    .keys()
                    .filter(|name| !bound.contains(name.as_str()))
                    .cloned()
                    .collect::<Vec<_>>();
                if !missing.is_empty() {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Struct pattern `{}` is missing field(s): {} (add `..` to ignore remaining fields)",
                            struct_name,
                            missing.join(", ")
                        ),
                        Span::new(0, 0),
                    ));
                }
            }

            let lowered_fields = fields
                .iter()
                .map(|field| {
                    let field_ty = struct_fields
                        .and_then(|map| map.get(&field.name))
                        .cloned()
                        .unwrap_or_else(|| {
                            diagnostics.push(Diagnostic::new(
                                format!(
                                    "Unknown field `{}` in struct pattern `{}`",
                                    field.name, struct_name
                                ),
                                Span::new(0, 0),
                            ));
                            SemType::Unknown
                        });
                    TypedPatternField {
                        name: field.name.clone(),
                        pattern: lower_pattern(
                            &field.pattern,
                            &field_ty,
                            context,
                            locals,
                            diagnostics,
                        ),
                    }
                })
                .collect();
            TypedPattern::Struct {
                path: path.clone(),
                fields: lowered_fields,
                has_rest: *has_rest,
            }
        }
        Pattern::Range {
            start,
            end,
            inclusive,
        } => {
            ensure_pattern_type(scrutinee_ty, &named_type("i64"), "range", diagnostics);
            TypedPattern::Range {
                start: *start,
                end: *end,
                inclusive: *inclusive,
            }
        }
        Pattern::Variant { path, payload } => {
            let mut enum_name_opt: Option<String> = None;
            let mut variant_name_opt: Option<String> = None;

            if path.len() >= 2 {
                enum_name_opt = Some(path[0].clone());
                variant_name_opt = Some(path[1].clone());
            } else if path.len() == 1
                && let SemType::Path { path: ty_path, .. } = scrutinee_ty
                && let Some(enum_name) = ty_path.last()
            {
                enum_name_opt = Some(enum_name.clone());
                variant_name_opt = Some(path[0].clone());
            }

            let (enum_name, variant_name) = match (enum_name_opt, variant_name_opt) {
                (Some(enum_name), Some(variant_name)) => (enum_name, variant_name),
                _ => {
                    diagnostics.push(Diagnostic::new(
                        "Variant pattern must be `Enum::Variant` or inferable from enum scrutinee",
                        Span::new(0, 0),
                    ));
                    return TypedPattern::Variant {
                        path: path.clone(),
                        payload: payload.as_ref().map(|p| {
                            Box::new(lower_pattern(
                                p,
                                &SemType::Unknown,
                                context,
                                locals,
                                diagnostics,
                            ))
                        }),
                    };
                }
            };

            if let Some(pattern) = lower_builtin_variant_pattern(
                &enum_name,
                &variant_name,
                payload.as_deref(),
                scrutinee_ty,
                context,
                locals,
                diagnostics,
            ) {
                return pattern;
            }

            if let SemType::Path { path: ty_path, .. } = scrutinee_ty
                && ty_path.last() != Some(&enum_name)
                && *scrutinee_ty != SemType::Unknown
            {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Pattern enum `{}` does not match scrutinee type `{}`",
                        enum_name,
                        type_to_string(scrutinee_ty)
                    ),
                    Span::new(0, 0),
                ));
            }

            let lowered_payload = if let Some(variants) = context.enums.get(&enum_name) {
                if let Some(expected_payload) = variants.get(&variant_name) {
                    if expected_payload.is_empty() {
                        if payload.is_some() {
                            diagnostics.push(Diagnostic::new(
                                format!("Pattern `{enum_name}::{variant_name}` has no payload"),
                                Span::new(0, 0),
                            ));
                        }
                        None
                    } else if expected_payload.len() == 1 {
                        match payload {
                            Some(payload_pattern) => Some(Box::new(lower_pattern(
                                payload_pattern,
                                &expected_payload[0],
                                context,
                                locals,
                                diagnostics,
                            ))),
                            None => {
                                diagnostics.push(Diagnostic::new(
                                    format!(
                                        "Pattern `{enum_name}::{variant_name}` requires payload pattern"
                                    ),
                                    Span::new(0, 0),
                                ));
                                None
                            }
                        }
                    } else {
                        match payload {
                            Some(payload_pattern) => {
                                if let Pattern::Tuple(items) = payload_pattern.as_ref() {
                                    if items.len() != expected_payload.len() {
                                        diagnostics.push(Diagnostic::new(
                                            format!(
                                                "Pattern `{enum_name}::{variant_name}` expects {} payload fields, got {}",
                                                expected_payload.len(),
                                                items.len()
                                            ),
                                            Span::new(0, 0),
                                        ));
                                    }
                                    let lowered = items
                                        .iter()
                                        .zip(expected_payload.iter())
                                        .map(|(item, expected_ty)| {
                                            lower_pattern(item, expected_ty, context, locals, diagnostics)
                                        })
                                        .collect::<Vec<_>>();
                                    Some(Box::new(TypedPattern::Tuple(lowered)))
                                } else {
                                    diagnostics.push(Diagnostic::new(
                                        format!(
                                            "Pattern `{enum_name}::{variant_name}` with multiple payload fields requires tuple syntax"
                                        ),
                                        Span::new(0, 0),
                                    ));
                                    None
                                }
                            }
                            None => {
                                diagnostics.push(Diagnostic::new(
                                    format!(
                                        "Pattern `{enum_name}::{variant_name}` requires payload pattern"
                                    ),
                                    Span::new(0, 0),
                                ));
                                None
                            }
                        }
                    }
                } else {
                    diagnostics.push(Diagnostic::new(
                        format!("Unknown variant `{enum_name}::{variant_name}` in pattern"),
                        Span::new(0, 0),
                    ));
                    payload.as_ref().map(|p| {
                        Box::new(lower_pattern(
                            p,
                            &SemType::Unknown,
                            context,
                            locals,
                            diagnostics,
                        ))
                    })
                }
            } else {
                if let SemType::Path { path: ty_path, .. } = scrutinee_ty
                    && ty_path.last() == Some(&enum_name)
                {
                    // Allow matching on imported Rust enums without local variant metadata.
                    payload.as_ref().map(|p| {
                        Box::new(lower_pattern(
                            p,
                            &SemType::Unknown,
                            context,
                            locals,
                            diagnostics,
                        ))
                    })
                } else {
                    diagnostics.push(Diagnostic::new(
                        format!("Unknown enum `{enum_name}` in pattern"),
                        Span::new(0, 0),
                    ));
                    payload.as_ref().map(|p| {
                        Box::new(lower_pattern(
                            p,
                            &SemType::Unknown,
                            context,
                            locals,
                            diagnostics,
                        ))
                    })
                }
            };

            TypedPattern::Variant {
                path: if path.len() >= 2 {
                    path.clone()
                } else {
                    vec![enum_name, variant_name]
                },
                payload: lowered_payload,
            }
        }
    }
}

fn lower_builtin_variant_pattern(
    enum_name: &str,
    variant_name: &str,
    payload: Option<&Pattern>,
    scrutinee_ty: &SemType,
    context: &Context,
    locals: &mut HashMap<String, SemType>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<TypedPattern> {
    if enum_name == "Option" {
        let expected_payload = if let Some(inner) = option_inner(scrutinee_ty) {
            inner.clone()
        } else {
            if *scrutinee_ty != SemType::Unknown {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Pattern enum `Option` does not match scrutinee type `{}`",
                        type_to_string(scrutinee_ty)
                    ),
                    Span::new(0, 0),
                ));
            }
            SemType::Unknown
        };
        let lowered_payload = match variant_name {
            "Some" => match payload {
                Some(payload_pattern) => Some(Box::new(lower_pattern(
                    payload_pattern,
                    &expected_payload,
                    context,
                    locals,
                    diagnostics,
                ))),
                None => {
                    diagnostics.push(Diagnostic::new(
                        "Pattern `Option::Some` requires payload pattern",
                        Span::new(0, 0),
                    ));
                    None
                }
            },
            "None" => {
                if payload.is_some() {
                    diagnostics.push(Diagnostic::new(
                        "Pattern `Option::None` has no payload",
                        Span::new(0, 0),
                    ));
                }
                None
            }
            _ => {
                diagnostics.push(Diagnostic::new(
                    format!("Unknown variant `Option::{variant_name}` in pattern"),
                    Span::new(0, 0),
                ));
                payload.map(|payload_pattern| {
                    Box::new(lower_pattern(
                        payload_pattern,
                        &SemType::Unknown,
                        context,
                        locals,
                        diagnostics,
                    ))
                })
            }
        };
        return Some(TypedPattern::Variant {
            path: vec!["Option".to_string(), variant_name.to_string()],
            payload: lowered_payload,
        });
    }

    if enum_name == "Result" {
        let (expected_ok, expected_err) = if let Some((ok, err)) = result_parts(scrutinee_ty) {
            (ok.clone(), err.clone())
        } else {
            if *scrutinee_ty != SemType::Unknown {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Pattern enum `Result` does not match scrutinee type `{}`",
                        type_to_string(scrutinee_ty)
                    ),
                    Span::new(0, 0),
                ));
            }
            (SemType::Unknown, SemType::Unknown)
        };
        let lowered_payload = match variant_name {
            "Ok" => match payload {
                Some(payload_pattern) => Some(Box::new(lower_pattern(
                    payload_pattern,
                    &expected_ok,
                    context,
                    locals,
                    diagnostics,
                ))),
                None => {
                    diagnostics.push(Diagnostic::new(
                        "Pattern `Result::Ok` requires payload pattern",
                        Span::new(0, 0),
                    ));
                    None
                }
            },
            "Err" => match payload {
                Some(payload_pattern) => Some(Box::new(lower_pattern(
                    payload_pattern,
                    &expected_err,
                    context,
                    locals,
                    diagnostics,
                ))),
                None => {
                    diagnostics.push(Diagnostic::new(
                        "Pattern `Result::Err` requires payload pattern",
                        Span::new(0, 0),
                    ));
                    None
                }
            },
            _ => {
                diagnostics.push(Diagnostic::new(
                    format!("Unknown variant `Result::{variant_name}` in pattern"),
                    Span::new(0, 0),
                ));
                payload.map(|payload_pattern| {
                    Box::new(lower_pattern(
                        payload_pattern,
                        &SemType::Unknown,
                        context,
                        locals,
                        diagnostics,
                    ))
                })
            }
        };
        return Some(TypedPattern::Variant {
            path: vec!["Result".to_string(), variant_name.to_string()],
            payload: lowered_payload,
        });
    }

    None
}

fn lower_pattern_to_rust(pattern: &TypedPattern) -> RustPattern {
    match pattern {
        TypedPattern::Wildcard => RustPattern::Wildcard,
        TypedPattern::Binding(name) => RustPattern::Binding(name.clone()),
        TypedPattern::Int(value) => RustPattern::Int(*value),
        TypedPattern::Bool(value) => RustPattern::Bool(*value),
        TypedPattern::Char(value) => RustPattern::Char(*value),
        TypedPattern::String(value) => RustPattern::String(value.clone()),
        TypedPattern::Tuple(items) => {
            RustPattern::Tuple(items.iter().map(lower_pattern_to_rust).collect())
        }
        TypedPattern::Slice {
            prefix,
            rest,
            suffix,
        } => RustPattern::Slice {
            prefix: prefix.iter().map(lower_pattern_to_rust).collect(),
            rest: rest.clone(),
            suffix: suffix.iter().map(lower_pattern_to_rust).collect(),
        },
        TypedPattern::Or(items) => {
            RustPattern::Or(items.iter().map(lower_pattern_to_rust).collect())
        }
        TypedPattern::BindingAt { name, pattern } => RustPattern::BindingAt {
            name: name.clone(),
            pattern: Box::new(lower_pattern_to_rust(pattern)),
        },
        TypedPattern::Struct {
            path,
            fields,
            has_rest,
        } => RustPattern::Struct {
            path: path.clone(),
            fields: fields
                .iter()
                .map(|field| RustPatternField {
                    name: field.name.clone(),
                    pattern: lower_pattern_to_rust(&field.pattern),
                })
                .collect(),
            has_rest: *has_rest,
        },
        TypedPattern::Range {
            start,
            end,
            inclusive,
        } => RustPattern::Range {
            start: *start,
            end: *end,
            inclusive: *inclusive,
        },
        TypedPattern::Variant { path, payload } => RustPattern::Variant {
            path: path.clone(),
            payload: payload.as_ref().map(|p| Box::new(lower_pattern_to_rust(p))),
        },
    }
}

fn lower_destructure_pattern_typed(pattern: &DestructurePattern) -> TypedDestructurePattern {
    match pattern {
        DestructurePattern::Name(name) => TypedDestructurePattern::Name(name.clone()),
        DestructurePattern::Ignore => TypedDestructurePattern::Ignore,
        DestructurePattern::Tuple(items) => TypedDestructurePattern::Tuple(
            items.iter().map(lower_destructure_pattern_typed).collect(),
        ),
        DestructurePattern::Slice {
            prefix,
            rest,
            suffix,
        } => TypedDestructurePattern::Slice {
            prefix: prefix.iter().map(lower_destructure_pattern_typed).collect(),
            rest: rest.clone(),
            suffix: suffix.iter().map(lower_destructure_pattern_typed).collect(),
        },
    }
}

fn lower_destructure_pattern(pattern: &TypedDestructurePattern) -> RustDestructurePattern {
    match pattern {
        TypedDestructurePattern::Name(name) => RustDestructurePattern::Name(name.clone()),
        TypedDestructurePattern::Ignore => RustDestructurePattern::Ignore,
        TypedDestructurePattern::Tuple(items) => {
            RustDestructurePattern::Tuple(items.iter().map(lower_destructure_pattern).collect())
        }
        TypedDestructurePattern::Slice {
            prefix,
            rest,
            suffix,
        } => RustDestructurePattern::Slice {
            prefix: prefix.iter().map(lower_destructure_pattern).collect(),
            rest: rest.clone(),
            suffix: suffix.iter().map(lower_destructure_pattern).collect(),
        },
    }
}

fn lower_assign_target(
    target: &TypedAssignTarget,
    context: &mut LoweringContext,
    state: &mut LoweringState,
) -> RustAssignTarget {
    match target {
        TypedAssignTarget::Path(name) => RustAssignTarget::Path(name.clone()),
        TypedAssignTarget::Field { base, field } => RustAssignTarget::Field {
            base: lower_expr_with_context(base, context, ExprPosition::Value, state),
            field: field.clone(),
        },
        TypedAssignTarget::Index { base, index } => RustAssignTarget::Index {
            base: lower_expr_with_context(base, context, ExprPosition::Value, state),
            index: lower_expr_with_context(index, context, ExprPosition::Value, state),
        },
        TypedAssignTarget::Tuple(items) => RustAssignTarget::Tuple(
            items
                .iter()
                .map(|item| lower_assign_target(item, context, state))
                .collect(),
        ),
    }
}

fn bind_destructure_pattern(
    pattern: &DestructurePattern,
    value_ty: &SemType,
    locals: &mut HashMap<String, SemType>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match pattern {
        DestructurePattern::Name(name) => {
            locals.insert(name.clone(), value_ty.clone());
        }
        DestructurePattern::Ignore => {}
        DestructurePattern::Tuple(items) => {
            if *value_ty == SemType::Unknown {
                for item_pattern in items {
                    bind_destructure_pattern(item_pattern, &SemType::Unknown, locals, diagnostics);
                }
                return;
            }
            if let SemType::Tuple(value_items) = value_ty {
                if items.len() != value_items.len() {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Tuple destructure arity mismatch: pattern has {}, value has {}",
                            items.len(),
                            value_items.len()
                        ),
                        Span::new(0, 0),
                    ));
                    return;
                }
                for (item_pattern, item_ty) in items.iter().zip(value_items.iter()) {
                    bind_destructure_pattern(item_pattern, item_ty, locals, diagnostics);
                }
            } else {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Tuple destructure requires tuple value, got `{}`",
                        type_to_string(value_ty)
                    ),
                    Span::new(0, 0),
                ));
            }
        }
        DestructurePattern::Slice {
            prefix,
            rest,
            suffix,
        } => {
            let item_ty = if let Some(item) = slice_item_type(value_ty) {
                item
            } else {
                if *value_ty != SemType::Unknown {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Slice destructure requires Vec value, got `{}`",
                            type_to_string(value_ty)
                        ),
                        Span::new(0, 0),
                    ));
                }
                SemType::Unknown
            };
            for item in prefix {
                bind_destructure_pattern(item, &item_ty, locals, diagnostics);
            }
            for item in suffix {
                bind_destructure_pattern(item, &item_ty, locals, diagnostics);
            }
            if let Some(name) = rest {
                locals.insert(
                    name.clone(),
                    SemType::Path {
                        path: vec!["Vec".to_string()],
                        args: vec![item_ty],
                    },
                );
            }
        }
    }
}

fn collect_destructure_pattern_names(
    pattern: &DestructurePattern,
    names: &mut HashSet<String>,
) {
    match pattern {
        DestructurePattern::Name(name) => {
            names.insert(name.clone());
        }
        DestructurePattern::Ignore => {}
        DestructurePattern::Tuple(items) => {
            for item in items {
                collect_destructure_pattern_names(item, names);
            }
        }
        DestructurePattern::Slice {
            prefix,
            rest,
            suffix,
        } => {
            for item in prefix {
                collect_destructure_pattern_names(item, names);
            }
            for item in suffix {
                collect_destructure_pattern_names(item, names);
            }
            if let Some(name) = rest {
                names.insert(name.clone());
            }
        }
    }
}

fn ensure_pattern_type(
    actual: &SemType,
    expected: &SemType,
    label: &str,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if !is_compatible(actual, expected) {
        diagnostics.push(Diagnostic::new(
            format!(
                "{label} pattern requires `{}`, got `{}`",
                type_to_string(expected),
                type_to_string(actual)
            ),
            Span::new(0, 0),
        ));
    }
}

fn unify_types(existing: SemType, next: SemType, diagnostics: &mut Vec<Diagnostic>) -> SemType {
    if existing == SemType::Unknown {
        return next;
    }
    if next == SemType::Unknown {
        return existing;
    }
    if is_compatible(&existing, &next) && is_compatible(&next, &existing) {
        return existing;
    }
    diagnostics.push(Diagnostic::new(
        format!(
            "Match arm type mismatch: `{}` vs `{}`",
            type_to_string(&existing),
            type_to_string(&next)
        ),
        Span::new(0, 0),
    ));
    SemType::Unknown
}

fn resolve_path_value(path: &[String], context: &Context) -> Option<SemType> {
    if path.len() == 1 && path[0] == "None" {
        return Some(option_type(SemType::Unknown));
    }
    if path.len() == 2 && path[0] == "Option" && path[1] == "None" {
        return Some(option_type(SemType::Unknown));
    }
    if path.len() == 2 {
        let enum_name = &path[0];
        let variant_name = &path[1];
        if let Some(variants) = context.enums.get(enum_name)
            && let Some(payload) = variants.get(variant_name)
            && payload.is_empty()
        {
            return Some(named_type(enum_name));
        }
    }
    None
}

fn infer_return_type(
    returns: &[SemType],
    diagnostics: &mut Vec<Diagnostic>,
    function_name: &str,
) -> SemType {
    if returns.is_empty() {
        return SemType::Unit;
    }

    let mut current = returns[0].clone();
    for ty in &returns[1..] {
        if current == SemType::Unknown {
            current = ty.clone();
            continue;
        }
        if *ty == SemType::Unknown {
            continue;
        }
        if !is_compatible(&current, ty) || !is_compatible(ty, &current) {
            diagnostics.push(Diagnostic::new(
                format!(
                    "Cannot infer single return type for `{function_name}`: saw `{}` and `{}`",
                    type_to_string(&current),
                    type_to_string(ty)
                ),
                Span::new(0, 0),
            ));
            return SemType::Unknown;
        }
    }
    current
}

fn lower_block_with_types(
    block: &Block,
    context: &Context,
    locals: &HashMap<String, SemType>,
    immutable_locals: &HashSet<String>,
    return_ty: &SemType,
    inferred_returns: &mut Vec<SemType>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Vec<TypedStmt> {
    let mut block_locals = locals.clone();
    let mut block_immutable_locals = immutable_locals.clone();
    let mut out = Vec::new();
    for statement in &block.statements {
        if let Some(stmt) = lower_stmt_with_types(
            statement,
            context,
            &mut block_locals,
            &mut block_immutable_locals,
            return_ty,
            inferred_returns,
            diagnostics,
        ) {
            out.push(stmt);
        }
    }
    out
}

fn contains_unknown(ty: &SemType) -> bool {
    match ty {
        SemType::Unknown => true,
        SemType::Unit => false,
        SemType::Tuple(items) => items.iter().any(contains_unknown),
        SemType::Fn { params, ret } => params.iter().any(contains_unknown) || contains_unknown(ret),
        SemType::Iter(item) => contains_unknown(item),
        SemType::Path { args, .. } => args.iter().any(contains_unknown),
    }
}
