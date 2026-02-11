use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::hash::{DefaultHasher, Hash, Hasher};

use crate::DirectBorrowHint;
use crate::ast::{
    AssignOp, AssignTarget, BinaryOp, Block, DestructurePattern, EnumVariantFields, Expr,
    GenericParam, Item, Module, Pattern, Stmt, StructLiteralField, TraitMethodSig, Type, UnaryOp,
    UseTree, Visibility,
};
use crate::data::constraint_graph::ConstraintGraph;
use crate::data::union_find::UnionFind;
use crate::diag::{Diagnostic, Span};
use crate::ir::lowered::{
    RustAssignOp, RustAssignTarget, RustBinaryOp, RustConst, RustDestructurePattern, RustEnum,
    RustExpr, RustField, RustFunction, RustImpl, RustItem, RustMatchArm, RustModule, RustParam,
    RustPattern, RustPatternField, RustStatic, RustStmt, RustStruct, RustStructLiteralField,
    RustTrait, RustTraitMethod, RustTypeParam, RustUnaryOp, RustUse, RustUseTree, RustVariant,
    RustVariantFields,
};
use crate::ir::typed::{
    TypedAssignOp, TypedAssignTarget, TypedBinaryOp, TypedConst, TypedDestructurePattern,
    TypedEnum, TypedExpr, TypedExprKind, TypedField, TypedFunction, TypedImpl, TypedIndexKeyPassing,
    TypedIndexMetadata, TypedIndexMode, TypedIndexSource, TypedItem, TypedMatchArm, TypedModule,
    TypedParam, TypedPattern, TypedPatternField, TypedRustUse, TypedStatic, TypedStmt, TypedStruct,
    TypedStructLiteralField, TypedTrait, TypedTraitMethod, TypedTypeParam, TypedUnaryOp,
    TypedUseTree, TypedVariant, TypedVariantFields,
};
use crate::ownership_planner::{CloneDecision, ClonePlannerInput, decide_clone};
use crate::rustdex_backend;

mod index_capability;
use index_capability::{
    CapabilityIndexMode, CapabilityIndexSource, IndexCapability, resolve_index_capability,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum SemType {
    Path {
        path: Vec<String>,
        args: Vec<SemType>,
    },
    TraitObject(Vec<SemType>),
    Tuple(Vec<SemType>),
    Fn {
        params: Vec<SemType>,
        ret: Box<SemType>,
    },
    Iter(Box<SemType>),
    Unit,
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CapabilityReceiverMode {
    Owned,
    Borrowed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MethodCapability {
    receiver_mode: CapabilityReceiverMode,
    arg_modes: Vec<CallArgMode>,
    expected_args: Vec<SemType>,
    return_ty: SemType,
}

#[derive(Debug, Clone)]
struct FunctionSig {
    type_params: Vec<String>,
    type_param_bounds: HashMap<String, Vec<SemType>>,
    params: Vec<SemType>,
    return_type: SemType,
    structural_requirements: HashMap<String, StructuralRequirements>,
}

#[derive(Debug, Clone)]
struct ResolvedImplMethodSignature {
    param_sem_types: Vec<SemType>,
    return_sem_type: SemType,
    param_rust_types: Option<Vec<String>>,
    return_rust_type: Option<String>,
}

#[derive(Debug, Clone)]
struct TraitMethodSignatureOverride {
    param_sem_types: Vec<SemType>,
    return_sem_type: SemType,
    param_rust_types: Vec<String>,
    return_rust_type: String,
}

#[derive(Debug, Clone, Default)]
struct StructuralRequirements {
    fields: BTreeSet<String>,
    methods: HashMap<String, BTreeSet<usize>>,
}

impl StructuralRequirements {
    fn is_empty(&self) -> bool {
        self.fields.is_empty() && self.methods.values().all(BTreeSet::is_empty)
    }
}

fn structural_method_required_trait(method: &str, arity: usize) -> Option<SemType> {
    match (method, arity) {
        ("to_string", 0) => Some(SemType::Path {
            path: vec!["std".to_string(), "fmt".to_string(), "Display".to_string()],
            args: Vec::new(),
        }),
        ("clone", 0) => Some(named_type("Clone")),
        ("into_iter", 0) => Some(SemType::Path {
            path: vec![
                "std".to_string(),
                "iter".to_string(),
                "IntoIterator".to_string(),
            ],
            args: Vec::new(),
        }),
        _ => None,
    }
}

fn push_unique_bound(bounds: &mut Vec<SemType>, bound: SemType) {
    if bounds.iter().all(|existing| existing != &bound) {
        bounds.push(bound);
    }
}

fn merge_structural_method_bounds(
    type_param_bounds: &mut HashMap<String, Vec<SemType>>,
    structural_requirements: &HashMap<String, StructuralRequirements>,
) {
    for (type_param, requirements) in structural_requirements {
        let entry = type_param_bounds.entry(type_param.clone()).or_default();
        for (method, arities) in &requirements.methods {
            for arity in arities {
                if let Some(bound) = structural_method_required_trait(method, *arity) {
                    push_unique_bound(entry, bound);
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
struct StructuralSpecialization {
    specialized_name: String,
    bindings: HashMap<String, SemType>,
}

#[derive(Debug, Clone)]
struct ImplMethodTemplate {
    impl_def: crate::ast::ImplBlock,
    method: crate::ast::FunctionDef,
}

#[derive(Debug, Clone)]
struct EnumVariantInfo {
    payload_types: Vec<SemType>,
    named_fields: Option<Vec<String>>,
}

#[derive(Debug, Clone)]
struct Context {
    functions: HashMap<String, FunctionSig>,
    structs: HashMap<String, HashMap<String, SemType>>,
    struct_type_params: HashMap<String, Vec<String>>,
    enums: HashMap<String, HashMap<String, EnumVariantInfo>>,
    enum_type_params: HashMap<String, Vec<String>>,
    traits: HashMap<String, Vec<SemType>>,
    trait_impls: HashMap<String, HashSet<String>>,
    globals: HashMap<String, SemType>,
    rust_block_functions: HashSet<String>,
    structural_specializations: RefCell<HashMap<String, Vec<StructuralSpecialization>>>,
    structural_specialization_lookup: RefCell<HashMap<(String, Vec<String>), String>>,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            functions: HashMap::new(),
            structs: HashMap::new(),
            struct_type_params: HashMap::new(),
            enums: HashMap::new(),
            enum_type_params: HashMap::new(),
            traits: HashMap::new(),
            trait_impls: HashMap::new(),
            globals: HashMap::new(),
            rust_block_functions: HashSet::new(),
            structural_specializations: RefCell::new(HashMap::new()),
            structural_specialization_lookup: RefCell::new(HashMap::new()),
        }
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct TypecheckOptions {
    pub type_system: bool,
    pub rustdex_ready: bool,
}

thread_local! {
    static TYPE_SYSTEM_ENABLED: Cell<bool> = Cell::new(false);
    static STRICT_HOLE_REPORTED: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
    static STRICT_HOLE_EMITTED: Cell<bool> = Cell::new(false);
    static CURRENT_DIAG_SPAN: Cell<Option<Span>> = Cell::new(None);
}

fn with_typecheck_options_scope<T>(options: &TypecheckOptions, f: impl FnOnce() -> T) -> T {
    TYPE_SYSTEM_ENABLED.with(|cell| {
        let previous = cell.replace(options.type_system);
        STRICT_HOLE_REPORTED.with(|reported| {
            let previous_emitted = STRICT_HOLE_EMITTED.with(|emitted| emitted.replace(false));
            let previous_reported = std::mem::take(&mut *reported.borrow_mut());
            let out = f();
            *reported.borrow_mut() = previous_reported;
            STRICT_HOLE_EMITTED.with(|emitted| emitted.set(previous_emitted));
            cell.set(previous);
            out
        })
    })
}

fn numeric_coercion_enabled() -> bool {
    TYPE_SYSTEM_ENABLED.with(|cell| cell.get())
}

fn infer_local_bidi_enabled() -> bool {
    TYPE_SYSTEM_ENABLED.with(|cell| cell.get())
}

fn infer_literal_bidi_enabled() -> bool {
    TYPE_SYSTEM_ENABLED.with(|cell| cell.get())
}

fn infer_principal_fallback_enabled() -> bool {
    TYPE_SYSTEM_ENABLED.with(|cell| cell.get())
}

fn effect_rows_surface_enabled() -> bool {
    TYPE_SYSTEM_ENABLED.with(|cell| cell.get())
}

fn effect_rows_internal_enabled() -> bool {
    TYPE_SYSTEM_ENABLED.with(|cell| cell.get())
}

fn effect_rows_analysis_enabled() -> bool {
    effect_rows_surface_enabled() || effect_rows_internal_enabled()
}

fn strict_mode_enabled() -> bool {
    !infer_local_bidi_enabled()
}

fn with_diag_span_scope<T>(span: Option<Span>, f: impl FnOnce() -> T) -> T {
    CURRENT_DIAG_SPAN.with(|cell| {
        let previous = cell.replace(span);
        let out = f();
        cell.set(previous);
        out
    })
}

fn default_diag_span() -> Span {
    CURRENT_DIAG_SPAN
        .with(|cell| cell.get())
        .unwrap_or(Span::new(0, 0))
}

fn emit_strict_hole_once(key: String, message: String, diagnostics: &mut Vec<Diagnostic>) {
    if strict_mode_enabled() {
        let already_emitted = STRICT_HOLE_EMITTED.with(|emitted| {
            if emitted.get() {
                true
            } else {
                emitted.set(true);
                false
            }
        });
        if already_emitted {
            return;
        }
    }
    let should_emit = STRICT_HOLE_REPORTED.with(|reported| {
        let mut set = reported.borrow_mut();
        if set.contains(&key) {
            false
        } else {
            set.insert(key);
            true
        }
    });
    if should_emit {
        diagnostics.push(Diagnostic::new(message, default_diag_span()));
    }
}

fn emit_strict_unknown_operand_hint_once(expr: &Expr, diagnostics: &mut Vec<Diagnostic>) {
    match expr {
        Expr::Path(path) | Expr::PathWithTypeArgs { path, .. } if path.len() == 1 => {
            let name = &path[0];
            emit_strict_hole_once(
                format!("strict:arith:path:{name}"),
                format!(
                    "Cannot infer type for `{name}` in strict mode. Add an explicit type \
annotation or enable `--exp-type-system`."
                ),
                diagnostics,
            );
        }
        _ => {
            emit_strict_hole_once(
                format!("strict:arith:expr:{expr:?}"),
                "Cannot infer arithmetic operand type in strict mode. Add an explicit type \
annotation or enable `--exp-type-system`."
                    .to_string(),
                diagnostics,
            );
        }
    }
}

fn inference_hole_message(default_message: String, site: &str, annotation: &str) -> String {
    if infer_principal_fallback_enabled() {
        return format!("Principal fallback: add {annotation} on {site} to pin a principal type");
    }
    default_message
}

pub fn lower_to_typed(module: &Module) -> Result<TypedModule, Vec<Diagnostic>> {
    lower_to_typed_with_options(module, &TypecheckOptions::default())
}

pub fn lower_to_typed_with_options(
    module: &Module,
    options: &TypecheckOptions,
) -> Result<TypedModule, Vec<Diagnostic>> {
    with_typecheck_options_scope(options, || {
        if options.type_system && !options.rustdex_ready {
            return Err(vec![Diagnostic::new(
                "E_RUSTDEX_UNAVAILABLE: rustdex preflight did not complete; capability resolution requires rustdex index metadata",
                default_diag_span(),
            )]);
        }
        let mut diagnostics = Vec::new();
        let mut context = Context {
            functions: HashMap::new(),
            structs: HashMap::new(),
            struct_type_params: HashMap::new(),
            enums: HashMap::new(),
            enum_type_params: HashMap::new(),
            traits: HashMap::new(),
            trait_impls: HashMap::new(),
            globals: HashMap::new(),
            rust_block_functions: HashSet::new(),
            structural_specializations: RefCell::new(HashMap::new()),
            structural_specialization_lookup: RefCell::new(HashMap::new()),
        };

        collect_definitions(module, &mut context, &mut diagnostics);
        let items = module
            .items
            .iter()
            .filter_map(|item| lower_item(item, &mut context, &mut diagnostics))
            .collect::<Vec<_>>();
        let mut items = items;
        materialize_structural_specializations(module, &mut items, &mut context, &mut diagnostics);

        if diagnostics.is_empty() {
            if effect_rows_analysis_enabled() {
                let _ = analyze_effect_rows_internal(module, &items, &context, &mut diagnostics);
            }
            if !diagnostics.is_empty() {
                return Err(diagnostics);
            }
            Ok(TypedModule { items })
        } else {
            Err(diagnostics)
        }
    })
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct EffectRow {
    caps: BTreeSet<String>,
    open: bool,
}

fn analyze_effect_rows_internal(
    module: &Module,
    items: &[TypedItem],
    context: &Context,
    diagnostics: &mut Vec<Diagnostic>,
) -> HashMap<String, EffectRow> {
    let declared_rows = collect_declared_effect_rows(module);
    let trait_methods = collect_trait_methods(items);
    let trait_caps = expand_trait_capabilities(context, &trait_methods);
    let mut rows = HashMap::<String, EffectRow>::new();

    for item in items {
        match item {
            TypedItem::Function(def) => {
                let mut row = direct_effects_for_stmts(&def.body);
                merge_effect_rows(
                    &mut row,
                    &effect_caps_for_type_params(&def.type_params, &trait_caps),
                );
                validate_generic_method_capabilities(
                    &def.body,
                    &type_param_cap_rows(&def.type_params, &trait_caps),
                    &trait_caps,
                    diagnostics,
                );
                if effect_rows_surface_enabled()
                    && let Some(declared) = declared_rows.get(&def.name)
                {
                    validate_declared_effect_row(&def.name, &row, declared, diagnostics);
                }
                rows.insert(def.name.clone(), row);
            }
            TypedItem::Impl(def) => {
                let trait_row = def
                    .trait_target
                    .as_ref()
                    .and_then(|name| trait_caps.get(name))
                    .cloned()
                    .unwrap_or_default();
                for method in &def.methods {
                    let mut row = direct_effects_for_stmts(&method.body);
                    merge_effect_rows(&mut row, &trait_row);
                    let method_type_param_caps =
                        type_param_cap_rows(&method.type_params, &trait_caps);
                    merge_effect_rows(
                        &mut row,
                        &effect_caps_for_type_params(&method.type_params, &trait_caps),
                    );
                    validate_generic_method_capabilities(
                        &method.body,
                        &method_type_param_caps,
                        &trait_caps,
                        diagnostics,
                    );
                    let key = format!("{}::{}", def.target, method.name);
                    if effect_rows_surface_enabled()
                        && let Some(declared) = declared_rows.get(&key)
                    {
                        validate_declared_effect_row(&key, &row, declared, diagnostics);
                    }
                    rows.insert(key, row);
                }
            }
            _ => {}
        }
    }

    rows
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct DeclaredEffectRow {
    caps: BTreeSet<String>,
    open: bool,
}

fn collect_declared_effect_rows(module: &Module) -> HashMap<String, DeclaredEffectRow> {
    let mut out = HashMap::new();
    for item in &module.items {
        match item {
            Item::Function(def) => {
                if let Some(row) = def.effect_row.as_ref() {
                    out.insert(def.name.clone(), declared_effect_row_from_ast(row));
                }
            }
            Item::Impl(def) => {
                for method in &def.methods {
                    if let Some(row) = method.effect_row.as_ref() {
                        out.insert(
                            format!("{}::{}", def.target, method.name),
                            declared_effect_row_from_ast(row),
                        );
                    }
                }
            }
            _ => {}
        }
    }
    out
}

fn declared_effect_row_from_ast(row: &crate::ast::EffectRow) -> DeclaredEffectRow {
    DeclaredEffectRow {
        caps: row
            .caps
            .iter()
            .map(|cap| cap.join("::"))
            .collect::<BTreeSet<_>>(),
        open: row.rest.is_some(),
    }
}

fn validate_declared_effect_row(
    fn_name: &str,
    actual: &EffectRow,
    declared: &DeclaredEffectRow,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for cap in &actual.caps {
        if !declared.caps.contains(cap) && !declared.open {
            diagnostics.push(Diagnostic::new(
                format!(
                    "Effect row for `{fn_name}` is missing capability `{cap}`. Add `{cap}` or use an open row (`..r`)."
                ),
                default_diag_span(),
            ));
        }
    }
}

fn collect_trait_methods(items: &[TypedItem]) -> HashMap<String, BTreeSet<String>> {
    let mut methods = HashMap::<String, BTreeSet<String>>::new();
    for item in items {
        if let TypedItem::Trait(def) = item {
            let entry = methods.entry(def.name.clone()).or_default();
            for method in &def.methods {
                entry.insert(method.name.clone());
            }
        }
    }
    methods
}

fn expand_trait_capabilities(
    context: &Context,
    trait_methods: &HashMap<String, BTreeSet<String>>,
) -> HashMap<String, EffectRow> {
    fn walk(
        trait_name: &str,
        context: &Context,
        trait_methods: &HashMap<String, BTreeSet<String>>,
        cache: &mut HashMap<String, EffectRow>,
        visiting: &mut HashSet<String>,
    ) -> EffectRow {
        if let Some(found) = cache.get(trait_name) {
            return found.clone();
        }
        if !visiting.insert(trait_name.to_string()) {
            return EffectRow::default();
        }
        let mut row = EffectRow::default();
        row.caps.insert(format!("trait::{trait_name}"));
        if let Some(methods) = trait_methods.get(trait_name) {
            for method in methods {
                row.caps.insert(format!("method::{method}"));
            }
        }
        if let Some(bounds) = context.traits.get(trait_name) {
            for bound in bounds {
                if let Some(bound_name) = bound_trait_name(bound) {
                    let nested = walk(bound_name, context, trait_methods, cache, visiting);
                    merge_effect_rows(&mut row, &nested);
                }
            }
        }
        visiting.remove(trait_name);
        cache.insert(trait_name.to_string(), row.clone());
        row
    }

    let mut cache = HashMap::<String, EffectRow>::new();
    let mut visiting = HashSet::<String>::new();
    for trait_name in context.traits.keys() {
        let row = walk(
            trait_name,
            context,
            trait_methods,
            &mut cache,
            &mut visiting,
        );
        cache.insert(trait_name.clone(), row);
    }
    cache
}

fn type_param_cap_rows(
    params: &[TypedTypeParam],
    trait_caps: &HashMap<String, EffectRow>,
) -> HashMap<String, EffectRow> {
    params
        .iter()
        .map(|param| (param.name.clone(), type_param_cap_row(param, trait_caps)))
        .collect()
}

fn type_param_cap_row(
    param: &TypedTypeParam,
    trait_caps: &HashMap<String, EffectRow>,
) -> EffectRow {
    let mut row = EffectRow::default();
    for bound in &param.bounds {
        if let Some(caps) = trait_caps.get(bound) {
            merge_effect_rows(&mut row, caps);
        } else if !bound.trim().is_empty() {
            row.caps.insert(format!("trait::{bound}"));
            row.open = true;
        }
    }
    row
}

fn effect_caps_for_type_params(
    params: &[TypedTypeParam],
    trait_caps: &HashMap<String, EffectRow>,
) -> EffectRow {
    let mut row = EffectRow::default();
    for param in params {
        let param_row = type_param_cap_row(param, trait_caps);
        merge_effect_rows(&mut row, &param_row);
    }
    row
}

fn validate_generic_method_capabilities(
    stmts: &[TypedStmt],
    type_param_caps: &HashMap<String, EffectRow>,
    trait_caps: &HashMap<String, EffectRow>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for stmt in stmts {
        validate_stmt_method_capabilities(stmt, type_param_caps, trait_caps, diagnostics);
    }
}

fn validate_stmt_method_capabilities(
    stmt: &TypedStmt,
    type_param_caps: &HashMap<String, EffectRow>,
    trait_caps: &HashMap<String, EffectRow>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match stmt {
        TypedStmt::Const(def) => {
            validate_expr_method_capabilities(&def.value, type_param_caps, trait_caps, diagnostics)
        }
        TypedStmt::DestructureConst { value, .. } => {
            validate_expr_method_capabilities(value, type_param_caps, trait_caps, diagnostics)
        }
        TypedStmt::Assign { value, .. } => {
            validate_expr_method_capabilities(value, type_param_caps, trait_caps, diagnostics)
        }
        TypedStmt::Return(value) => {
            if let Some(value) = value {
                validate_expr_method_capabilities(value, type_param_caps, trait_caps, diagnostics);
            }
        }
        TypedStmt::If {
            condition,
            then_body,
            else_body,
        } => {
            validate_expr_method_capabilities(condition, type_param_caps, trait_caps, diagnostics);
            validate_generic_method_capabilities(
                then_body,
                type_param_caps,
                trait_caps,
                diagnostics,
            );
            if let Some(else_body) = else_body {
                validate_generic_method_capabilities(
                    else_body,
                    type_param_caps,
                    trait_caps,
                    diagnostics,
                );
            }
        }
        TypedStmt::While { condition, body } => {
            validate_expr_method_capabilities(condition, type_param_caps, trait_caps, diagnostics);
            validate_generic_method_capabilities(body, type_param_caps, trait_caps, diagnostics);
        }
        TypedStmt::For { iter, body, .. } => {
            validate_expr_method_capabilities(iter, type_param_caps, trait_caps, diagnostics);
            validate_generic_method_capabilities(body, type_param_caps, trait_caps, diagnostics);
        }
        TypedStmt::Loop { body } => {
            validate_generic_method_capabilities(body, type_param_caps, trait_caps, diagnostics)
        }
        TypedStmt::Expr(expr) => {
            validate_expr_method_capabilities(expr, type_param_caps, trait_caps, diagnostics)
        }
        TypedStmt::Break | TypedStmt::Continue | TypedStmt::RustBlock(_) => {}
    }
}

fn validate_expr_method_capabilities(
    expr: &TypedExpr,
    type_param_caps: &HashMap<String, EffectRow>,
    trait_caps: &HashMap<String, EffectRow>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match &expr.kind {
        TypedExprKind::Call { callee, args } => {
            if let TypedExprKind::Field { base, field } = &callee.kind
                && let Some(type_param_name) = type_param_name_for_expr(base, type_param_caps)
                && let Some(row) = type_param_caps.get(type_param_name)
            {
                let required_cap = format!("method::{field}");
                if !row.open && !row.caps.contains(&required_cap) {
                    let suggested = suggest_trait_bounds_for_method(field, trait_caps);
                    let hint = if suggested.is_empty() {
                        "add an explicit trait bound".to_string()
                    } else {
                        format!(
                            "add a bound like `{type_param_name}: {}`",
                            suggested.join(" + ")
                        )
                    };
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Type parameter `{type_param_name}` is missing capability `{required_cap}` for `{type_param_name}.{field}(...)`; {hint}"
                        ),
                        default_diag_span(),
                    ));
                }
            }
            validate_expr_method_capabilities(callee, type_param_caps, trait_caps, diagnostics);
            for arg in args {
                validate_expr_method_capabilities(arg, type_param_caps, trait_caps, diagnostics);
            }
        }
        TypedExprKind::MacroCall { args, .. } => {
            for arg in args {
                validate_expr_method_capabilities(arg, type_param_caps, trait_caps, diagnostics);
            }
        }
        TypedExprKind::Field { base, .. } => {
            validate_expr_method_capabilities(base, type_param_caps, trait_caps, diagnostics)
        }
        TypedExprKind::Index { base, index, .. } => {
            validate_expr_method_capabilities(base, type_param_caps, trait_caps, diagnostics);
            validate_expr_method_capabilities(index, type_param_caps, trait_caps, diagnostics);
        }
        TypedExprKind::Match { scrutinee, arms } => {
            validate_expr_method_capabilities(scrutinee, type_param_caps, trait_caps, diagnostics);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    validate_expr_method_capabilities(
                        guard,
                        type_param_caps,
                        trait_caps,
                        diagnostics,
                    );
                }
                validate_expr_method_capabilities(
                    &arm.value,
                    type_param_caps,
                    trait_caps,
                    diagnostics,
                );
            }
        }
        TypedExprKind::Unary { expr, .. } => {
            validate_expr_method_capabilities(expr, type_param_caps, trait_caps, diagnostics)
        }
        TypedExprKind::Binary { left, right, .. } => {
            validate_expr_method_capabilities(left, type_param_caps, trait_caps, diagnostics);
            validate_expr_method_capabilities(right, type_param_caps, trait_caps, diagnostics);
        }
        TypedExprKind::Array(items) | TypedExprKind::Tuple(items) => {
            for item in items {
                validate_expr_method_capabilities(item, type_param_caps, trait_caps, diagnostics);
            }
        }
        TypedExprKind::StructLiteral { fields, .. } => {
            for field in fields {
                validate_expr_method_capabilities(
                    &field.value,
                    type_param_caps,
                    trait_caps,
                    diagnostics,
                );
            }
        }
        TypedExprKind::Block { body, tail } => {
            validate_generic_method_capabilities(body, type_param_caps, trait_caps, diagnostics);
            if let Some(tail) = tail {
                validate_expr_method_capabilities(tail, type_param_caps, trait_caps, diagnostics);
            }
        }
        TypedExprKind::Closure { body, .. } => {
            validate_generic_method_capabilities(body, type_param_caps, trait_caps, diagnostics)
        }
        TypedExprKind::Range { start, end, .. } => {
            if let Some(start) = start {
                validate_expr_method_capabilities(start, type_param_caps, trait_caps, diagnostics);
            }
            if let Some(end) = end {
                validate_expr_method_capabilities(end, type_param_caps, trait_caps, diagnostics);
            }
        }
        TypedExprKind::Try(inner) => {
            validate_expr_method_capabilities(inner, type_param_caps, trait_caps, diagnostics)
        }
        TypedExprKind::Cast { expr, .. } => {
            validate_expr_method_capabilities(expr, type_param_caps, trait_caps, diagnostics)
        }
        TypedExprKind::Path(_)
        | TypedExprKind::Int(_)
        | TypedExprKind::Float(_)
        | TypedExprKind::Bool(_)
        | TypedExprKind::String(_)
        | TypedExprKind::Char(_) => {}
    }
}

fn type_param_name_for_expr<'a>(
    expr: &'a TypedExpr,
    type_param_caps: &'a HashMap<String, EffectRow>,
) -> Option<&'a str> {
    let ty = expr.ty.trim();
    let normalized = ty
        .strip_prefix("&mut ")
        .or_else(|| ty.strip_prefix('&'))
        .map(str::trim)
        .unwrap_or(ty);
    type_param_caps
        .contains_key(normalized)
        .then_some(normalized)
}

fn suggest_trait_bounds_for_method(
    method: &str,
    trait_caps: &HashMap<String, EffectRow>,
) -> Vec<String> {
    let required = format!("method::{method}");
    let mut matches = trait_caps
        .iter()
        .filter_map(|(trait_name, row)| row.caps.contains(&required).then_some(trait_name.clone()))
        .collect::<Vec<_>>();
    matches.sort();
    matches.truncate(3);
    matches
}

fn direct_effects_for_stmts(stmts: &[TypedStmt]) -> EffectRow {
    let mut row = EffectRow::default();
    for stmt in stmts {
        direct_effects_for_stmt(stmt, &mut row);
    }
    row
}

fn direct_effects_for_stmt(stmt: &TypedStmt, row: &mut EffectRow) {
    match stmt {
        TypedStmt::Const(def) => direct_effects_for_expr(&def.value, row),
        TypedStmt::DestructureConst { value, .. } => direct_effects_for_expr(value, row),
        TypedStmt::Assign { value, .. } => direct_effects_for_expr(value, row),
        TypedStmt::Return(value) => {
            if let Some(value) = value {
                direct_effects_for_expr(value, row);
            }
        }
        TypedStmt::If {
            condition,
            then_body,
            else_body,
        } => {
            direct_effects_for_expr(condition, row);
            for item in then_body {
                direct_effects_for_stmt(item, row);
            }
            if let Some(else_body) = else_body {
                for item in else_body {
                    direct_effects_for_stmt(item, row);
                }
            }
        }
        TypedStmt::While { condition, body } => {
            direct_effects_for_expr(condition, row);
            for item in body {
                direct_effects_for_stmt(item, row);
            }
        }
        TypedStmt::For { iter, body, .. } => {
            direct_effects_for_expr(iter, row);
            for item in body {
                direct_effects_for_stmt(item, row);
            }
        }
        TypedStmt::Loop { body } => {
            for item in body {
                direct_effects_for_stmt(item, row);
            }
        }
        TypedStmt::Expr(expr) => direct_effects_for_expr(expr, row),
        TypedStmt::Break | TypedStmt::Continue | TypedStmt::RustBlock(_) => {}
    }
}

fn direct_effects_for_expr(expr: &TypedExpr, row: &mut EffectRow) {
    match &expr.kind {
        TypedExprKind::Call { callee, args } => {
            if let TypedExprKind::Path(path) = &callee.kind {
                row.caps.insert(format!("call::{}", path.join("::")));
            }
            if let TypedExprKind::Field { field, .. } = &callee.kind {
                row.caps.insert(format!("method::{field}"));
            }
            direct_effects_for_expr(callee, row);
            for arg in args {
                direct_effects_for_expr(arg, row);
            }
        }
        TypedExprKind::MacroCall { path, args } => {
            row.caps.insert(format!("macro::{}", path.join("::")));
            for arg in args {
                direct_effects_for_expr(arg, row);
            }
        }
        TypedExprKind::Field { base, .. } => direct_effects_for_expr(base, row),
        TypedExprKind::Index { base, index, .. } => {
            direct_effects_for_expr(base, row);
            direct_effects_for_expr(index, row);
        }
        TypedExprKind::Match { scrutinee, arms } => {
            direct_effects_for_expr(scrutinee, row);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    direct_effects_for_expr(guard, row);
                }
                direct_effects_for_expr(&arm.value, row);
            }
        }
        TypedExprKind::Unary { expr, .. } => direct_effects_for_expr(expr, row),
        TypedExprKind::Binary { left, right, .. } => {
            direct_effects_for_expr(left, row);
            direct_effects_for_expr(right, row);
        }
        TypedExprKind::Array(items) | TypedExprKind::Tuple(items) => {
            for item in items {
                direct_effects_for_expr(item, row);
            }
        }
        TypedExprKind::StructLiteral { fields, .. } => {
            for field in fields {
                direct_effects_for_expr(&field.value, row);
            }
        }
        TypedExprKind::Block { body, tail } => {
            for stmt in body {
                direct_effects_for_stmt(stmt, row);
            }
            if let Some(tail) = tail {
                direct_effects_for_expr(tail, row);
            }
        }
        TypedExprKind::Closure { body, .. } => {
            for stmt in body {
                direct_effects_for_stmt(stmt, row);
            }
        }
        TypedExprKind::Range { start, end, .. } => {
            if let Some(start) = start {
                direct_effects_for_expr(start, row);
            }
            if let Some(end) = end {
                direct_effects_for_expr(end, row);
            }
        }
        TypedExprKind::Try(inner) => {
            row.caps.insert("control::try".to_string());
            direct_effects_for_expr(inner, row);
        }
        TypedExprKind::Cast { expr, .. } => direct_effects_for_expr(expr, row),
        TypedExprKind::Path(_)
        | TypedExprKind::Int(_)
        | TypedExprKind::Float(_)
        | TypedExprKind::Bool(_)
        | TypedExprKind::String(_)
        | TypedExprKind::Char(_) => {}
    }
}

fn merge_effect_rows(target: &mut EffectRow, source: &EffectRow) {
    target.caps.extend(source.caps.iter().cloned());
    target.open |= source.open;
}

pub fn lower_to_rust(module: &TypedModule) -> RustModule {
    lower_to_rust_with_hints(module, &[], &[])
}

pub fn lower_to_rust_with_hints(
    module: &TypedModule,
    hints: &[DirectBorrowHint],
    forced_clone_places: &[String],
) -> RustModule {
    let mut state = LoweringState::from_module(module);
    for hint in hints {
        state
            .registry
            .register_direct_borrow_policy(&hint.path, &hint.borrowed_arg_indexes);
    }
    for place in forced_clone_places {
        state.force_clone_place(place);
    }
    let mut items = Vec::new();
    for item in &module.items {
        let lowered = match item {
            TypedItem::RustUse(def) => RustItem::Use(RustUse {
                tree: typed_use_tree_to_rust(&def.tree),
            }),
            TypedItem::RustBlock(code) => RustItem::Raw(code.clone()),
            TypedItem::Struct(def) => RustItem::Struct(RustStruct {
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
                type_params: def
                    .type_params
                    .iter()
                    .map(|param| RustTypeParam {
                        name: param.name.clone(),
                        bounds: param.bounds.clone(),
                    })
                    .collect(),
                variants: def
                    .variants
                    .iter()
                    .map(|variant| RustVariant {
                        name: variant.name.clone(),
                        fields: match &variant.fields {
                            TypedVariantFields::Unit => RustVariantFields::Unit,
                            TypedVariantFields::Tuple(items) => {
                                RustVariantFields::Tuple(items.clone())
                            }
                            TypedVariantFields::Named(fields) => RustVariantFields::Named(
                                fields
                                    .iter()
                                    .map(|field| RustField {
                                        name: field.name.clone(),
                                        ty: field.ty.clone(),
                                    })
                                    .collect(),
                            ),
                        },
                    })
                    .collect(),
            }),
            TypedItem::Trait(def) => RustItem::Trait(RustTrait {
                is_public: def.is_public,
                name: def.name.clone(),
                supertraits: def.supertraits.clone(),
                methods: def
                    .methods
                    .iter()
                    .map(|method| RustTraitMethod {
                        name: method.name.clone(),
                        type_params: method
                            .type_params
                            .iter()
                            .map(|param| RustTypeParam {
                                name: param.name.clone(),
                                bounds: param.bounds.clone(),
                            })
                            .collect(),
                        params: method
                            .params
                            .iter()
                            .map(|param| RustParam {
                                name: param.name.clone(),
                                ty: param.ty.clone(),
                            })
                            .collect(),
                        return_type: method.return_type.clone(),
                    })
                    .collect(),
            }),
            TypedItem::Impl(def) => RustItem::Impl(RustImpl {
                type_params: def
                    .type_params
                    .iter()
                    .map(|param| RustTypeParam {
                        name: param.name.clone(),
                        bounds: param.bounds.clone(),
                    })
                    .collect(),
                target: def.target.clone(),
                target_args: def.target_args.clone(),
                trait_target: def.trait_target.clone(),
                methods: def
                    .methods
                    .iter()
                    .map(|method| lower_function(method, &mut state))
                    .collect(),
            }),
            TypedItem::Function(def) => RustItem::Function(lower_function(def, &mut state)),
            TypedItem::Const(def) => RustItem::Const(RustConst {
                is_public: def.is_public,
                is_const: def.is_const,
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
    loop_depth: usize,
    rebind_place: Option<OwnershipPlace>,
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
    known_functions: BTreeSet<String>,
    known_function_borrowed_args: HashMap<String, Vec<usize>>,
    ownership_notes: Vec<String>,
    ownership_note_keys: BTreeSet<String>,
    registry: InteropPolicyRegistry,
    forced_clone_places: BTreeSet<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprPosition {
    Value,
    CallArgOwned,
    CallArgBorrowed,
    OwnedOperand,
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
                    for path in flatten_typed_use_tree_paths(&def.tree) {
                        if let Some(name) = path.last()
                            && name != "*"
                        {
                            state
                                .imported_rust_paths
                                .entry(name.clone())
                                .or_default()
                                .push(path.clone());
                        }
                        state.registry.observe_import_path(&path);
                    }
                }
                TypedItem::Struct(def) => {
                    state.registry.register_user_cloneable_type(&def.name);
                }
                TypedItem::Enum(def) => {
                    state.registry.register_user_cloneable_type(&def.name);
                }
                TypedItem::Function(def) => {
                    state.known_functions.insert(def.name.clone());
                    state
                        .known_function_borrowed_args
                        .insert(def.name.clone(), borrowed_param_indexes(&def.params));
                }
                TypedItem::Impl(def) => {
                    for method in &def.methods {
                        let lookup = format!("{}::{}", def.target, method.name);
                        state.known_functions.insert(lookup.clone());
                        state
                            .known_function_borrowed_args
                            .insert(lookup, borrowed_param_indexes(&method.params));
                    }
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

    fn resolve_declared_borrow_modes(
        &self,
        path: &[String],
        arg_count: usize,
    ) -> Option<Vec<CallArgMode>> {
        let mut candidates = Vec::new();
        candidates.push(path.join("::"));
        if let Some(name) = path.last() {
            candidates.push(name.clone());
        }
        if path.len() >= 2 {
            candidates.push(format!(
                "{}::{}",
                path[path.len() - 2],
                path[path.len() - 1]
            ));
        }
        for key in candidates {
            if let Some(indexes) = self.known_function_borrowed_args.get(&key) {
                let mut modes = vec![CallArgMode::Owned; arg_count];
                for index in indexes {
                    set_borrowed(&mut modes, *index);
                }
                return Some(modes);
            }
        }
        None
    }

    fn push_ownership_note(&mut self, note: String) {
        if self.ownership_note_keys.insert(note.clone()) {
            self.ownership_notes.push(note);
        }
    }

    fn force_clone_place(&mut self, place: &str) {
        self.forced_clone_places.insert(place.to_string());
    }

    fn is_forced_clone_expr(&self, expr: &TypedExpr) -> bool {
        place_for_expr(expr)
            .map(|place| self.forced_clone_places.contains(&place.display_name()))
            .unwrap_or(false)
    }
}

fn flatten_typed_use_tree_paths(tree: &TypedUseTree) -> Vec<Vec<String>> {
    let mut out = Vec::new();
    let mut prefix = Vec::<String>::new();
    flatten_typed_use_tree_paths_into(tree, &mut prefix, &mut out);
    out
}

fn flatten_typed_use_tree_paths_into(
    tree: &TypedUseTree,
    prefix: &mut Vec<String>,
    out: &mut Vec<Vec<String>>,
) {
    match tree {
        TypedUseTree::Name(name) => {
            if name == "self" {
                if !prefix.is_empty() {
                    out.push(prefix.clone());
                }
                return;
            }
            let mut full = prefix.clone();
            full.push(name.clone());
            out.push(full);
        }
        TypedUseTree::Glob => {
            if !prefix.is_empty() {
                let mut full = prefix.clone();
                full.push("*".to_string());
                out.push(full);
            }
        }
        TypedUseTree::Path { segment, next } => {
            prefix.push(segment.clone());
            flatten_typed_use_tree_paths_into(next, prefix, out);
            prefix.pop();
        }
        TypedUseTree::Group(items) => {
            for item in items {
                flatten_typed_use_tree_paths_into(item, prefix, out);
            }
        }
    }
}

impl InteropPolicyRegistry {
    fn with_builtins() -> Self {
        let mut registry = Self::default();
        registry.register_builtin_shims();
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
        if is_probably_nominal_type(head_last) && self.import_looks_like_type(head_last) {
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
        ..LoweringContext::default()
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

fn collect_definitions(module: &Module, context: &mut Context, diagnostics: &mut Vec<Diagnostic>) {
    for item in &module.items {
        if let Item::Trait(def) = item {
            context.traits.entry(def.name.clone()).or_default();
        }
    }

    for item in &module.items {
        match item {
            Item::Struct(def) => {
                let fields = def
                    .fields
                    .iter()
                    .map(|field| {
                        (
                            field.name.clone(),
                            type_from_ast_in_context(&field.ty, context),
                        )
                    })
                    .collect::<HashMap<_, _>>();
                context.structs.insert(def.name.clone(), fields);
                context.struct_type_params.insert(
                    def.name.clone(),
                    def.type_params
                        .iter()
                        .map(|param| param.name.clone())
                        .collect(),
                );
            }
            Item::Enum(def) => {
                let variants = def
                    .variants
                    .iter()
                    .map(|variant| {
                        let (payload_types, named_fields) = match &variant.fields {
                            EnumVariantFields::Unit => (Vec::new(), None),
                            EnumVariantFields::Tuple(items) => {
                                (items.iter().map(type_from_ast).collect::<Vec<_>>(), None)
                            }
                            EnumVariantFields::Named(fields) => (
                                fields
                                    .iter()
                                    .map(|field| type_from_ast(&field.ty))
                                    .collect(),
                                Some(fields.iter().map(|field| field.name.clone()).collect()),
                            ),
                        };
                        (
                            variant.name.clone(),
                            EnumVariantInfo {
                                payload_types,
                                named_fields,
                            },
                        )
                    })
                    .collect::<HashMap<_, _>>();
                context.enums.insert(def.name.clone(), variants);
                context.enum_type_params.insert(
                    def.name.clone(),
                    def.type_params
                        .iter()
                        .map(|param| param.name.clone())
                        .collect(),
                );
            }
            Item::Trait(def) => {
                context.traits.insert(
                    def.name.clone(),
                    def.supertraits.iter().map(type_from_ast).collect(),
                );
            }
            Item::Function(def) => {
                let mut type_param_bounds = def
                    .type_params
                    .iter()
                    .map(|param| {
                        (
                            param.name.clone(),
                            param.bounds.iter().map(type_from_ast).collect::<Vec<_>>(),
                        )
                    })
                    .collect::<HashMap<_, _>>();
                let structural_requirements = collect_structural_requirements_for_function(
                    &def.type_params,
                    &def.params,
                    &def.body,
                );
                merge_structural_method_bounds(&mut type_param_bounds, &structural_requirements);
                let sig = FunctionSig {
                    type_params: def
                        .type_params
                        .iter()
                        .map(|param| param.name.clone())
                        .collect(),
                    type_param_bounds,
                    params: def
                        .params
                        .iter()
                        .map(|param| type_from_ast_in_context(&param.ty, context))
                        .collect(),
                    return_type: def
                        .return_type
                        .as_ref()
                        .map(|ty| type_from_ast_in_context(ty, context))
                        .unwrap_or(SemType::Unknown),
                    structural_requirements,
                };
                context.functions.insert(def.name.clone(), sig);
            }
            Item::Impl(def) => {
                if let Some(trait_target) = def.trait_target.as_ref() {
                    let trait_ty = type_from_ast_in_context(trait_target, context);
                    if let Some(trait_name) = trait_name_from_type(&trait_ty) {
                        context
                            .trait_impls
                            .entry(impl_lookup_name(&def.target))
                            .or_default()
                            .insert(trait_name.to_string());
                    }
                }
                for method in &def.methods {
                    let all_type_params =
                        merged_impl_and_method_type_params(&def.type_params, &method.type_params);
                    let resolved_sig =
                        resolve_impl_method_signature(method, def, context, diagnostics);
                    let method_params = resolved_sig
                        .param_sem_types
                        .iter()
                        .enumerate()
                        .filter_map(|(index, ty)| {
                            (!is_redundant_impl_self_param(&method.params, index))
                                .then_some(ty.clone())
                        })
                        .collect::<Vec<_>>();
                    let mut type_param_bounds = all_type_params
                        .iter()
                        .map(|param| {
                            (
                                param.name.clone(),
                                param
                                    .bounds
                                    .iter()
                                    .map(|bound| {
                                        type_from_ast_with_impl_self(
                                            bound,
                                            &def.target,
                                            &def.target_args,
                                        )
                                    })
                                    .collect::<Vec<_>>(),
                            )
                        })
                        .collect::<HashMap<_, _>>();
                    let structural_requirements = if def.trait_target.is_none() {
                        collect_structural_requirements_for_impl_method(def, method, context)
                    } else {
                        HashMap::new()
                    };
                    merge_structural_method_bounds(
                        &mut type_param_bounds,
                        &structural_requirements,
                    );
                    let sig = FunctionSig {
                        type_params: all_type_params
                            .iter()
                            .map(|param| param.name.clone())
                            .collect(),
                        type_param_bounds,
                        params: method_params,
                        return_type: resolved_sig.return_sem_type.clone(),
                        structural_requirements,
                    };
                    context.functions.insert(
                        format!("{}::{}", impl_lookup_name(&def.target), method.name),
                        sig,
                    );
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
                    .insert(def.name.clone(), type_from_ast_in_context(&def.ty, context));
            }
            Item::RustUse(_) => {}
            Item::RustBlock(code) => {
                context
                    .rust_block_functions
                    .extend(extract_rust_block_function_names(code));
            }
        }
    }
}

fn function_has_structural_requirements(sig: &FunctionSig) -> bool {
    sig.structural_requirements
        .values()
        .any(|requirements| !requirements.is_empty())
}

fn sem_type_contains_any_type_param(ty: &SemType, type_params: &HashSet<String>) -> bool {
    match ty {
        SemType::Path { path, args } => {
            (path.len() == 1 && args.is_empty() && type_params.contains(&path[0]))
                || args
                    .iter()
                    .any(|arg| sem_type_contains_any_type_param(arg, type_params))
        }
        SemType::TraitObject(bounds) => bounds
            .iter()
            .any(|bound| sem_type_contains_any_type_param(bound, type_params)),
        SemType::Tuple(items) => items
            .iter()
            .any(|item| sem_type_contains_any_type_param(item, type_params)),
        SemType::Fn { params, ret } => {
            params
                .iter()
                .any(|param| sem_type_contains_any_type_param(param, type_params))
                || sem_type_contains_any_type_param(ret, type_params)
        }
        SemType::Iter(item) => sem_type_contains_any_type_param(item, type_params),
        SemType::Unit | SemType::Unknown => false,
    }
}

fn specialization_bindings_are_concrete(bindings: &HashMap<String, SemType>) -> bool {
    if bindings.is_empty() {
        return false;
    }
    let type_params = bindings.keys().cloned().collect::<HashSet<_>>();
    bindings.values().all(|bound| {
        !contains_unknown(bound) && !sem_type_contains_any_type_param(bound, &type_params)
    })
}

fn materialize_structural_specializations(
    module: &Module,
    items: &mut Vec<TypedItem>,
    context: &mut Context,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let function_templates = module
        .items
        .iter()
        .filter_map(|item| {
            if let Item::Function(def) = item {
                Some((def.name.clone(), def))
            } else {
                None
            }
        })
        .collect::<HashMap<_, _>>();
    let impl_method_templates = module
        .items
        .iter()
        .filter_map(|item| {
            if let Item::Impl(def) = item {
                Some(def)
            } else {
                None
            }
        })
        .filter(|def| def.trait_target.is_none())
        .flat_map(|def| {
            let merged_params = |method: &crate::ast::FunctionDef| {
                merged_impl_and_method_type_params(&def.type_params, &method.type_params)
            };
            def.methods
                .iter()
                .map(|method| {
                    let mut template_method = method.clone();
                    template_method.type_params = merged_params(method);
                    (
                        format!("{}::{}", impl_lookup_name(&def.target), method.name),
                        ImplMethodTemplate {
                            impl_def: def.clone(),
                            method: template_method,
                        },
                    )
                })
                .collect::<Vec<_>>()
        })
        .collect::<HashMap<_, _>>();
    let structural_templates = context
        .functions
        .iter()
        .filter_map(|(name, sig)| function_has_structural_requirements(sig).then_some(name.clone()))
        .collect::<HashSet<_>>();
    if structural_templates.is_empty() {
        return;
    }

    let mut emitted = HashSet::<String>::new();
    loop {
        let pending = context
            .structural_specializations
            .borrow()
            .iter()
            .flat_map(|(template_name, specs)| {
                specs
                    .iter()
                    .filter(|spec| !emitted.contains(&spec.specialized_name))
                    .map(|spec| (template_name.clone(), spec.clone()))
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        if pending.is_empty() {
            break;
        }
        for (template_name, spec) in pending {
            if !specialization_bindings_are_concrete(&spec.bindings) {
                emitted.insert(spec.specialized_name.clone());
                context
                    .structural_specialization_lookup
                    .borrow_mut()
                    .retain(|_, value| value != &spec.specialized_name);
                continue;
            }
            emitted.insert(spec.specialized_name.clone());
            let Some(template_sig) = context.functions.get(&template_name).cloned() else {
                continue;
            };
            let specialized_item =
                if let Some(template_def) = function_templates.get(&template_name) {
                    let Some(specialized_def) = specialize_function_def_ast(
                        template_def,
                        &spec.specialized_name,
                        &spec.bindings,
                        diagnostics,
                    ) else {
                        continue;
                    };
                    let specialized_sig = substitute_function_sig(&template_sig, &spec.bindings);
                    context
                        .functions
                        .insert(spec.specialized_name.clone(), specialized_sig);
                    Item::Function(specialized_def)
                } else if let Some(template) = impl_method_templates.get(&template_name) {
                    let Some(specialized_def) =
                        specialize_impl_method_ast(template, &spec.bindings, diagnostics)
                    else {
                        continue;
                    };
                    Item::Impl(specialized_def)
                } else {
                    continue;
                };
            if let Some(lowered) = lower_item(&specialized_item, context, diagnostics) {
                items.push(lowered);
            }
        }
    }

    let lookup = context.structural_specialization_lookup.borrow().clone();
    rewrite_structural_calls(items, &lookup);

    let specialized_templates = lookup
        .keys()
        .map(|(template_name, _)| template_name.clone())
        .collect::<HashSet<_>>();
    items.retain(|item| {
        !matches!(
            item,
            TypedItem::Function(def)
                if structural_templates.contains(&def.name)
                    && specialized_templates.contains(&def.name)
        )
    });
    for item in items.iter_mut() {
        let TypedItem::Impl(def) = item else {
            continue;
        };
        if def.trait_target.is_some() {
            continue;
        }
        let method_owner = impl_lookup_name(&def.target);
        let impl_has_generics = !def.type_params.is_empty();
        def.methods.retain(|method| {
            let method_key = format!("{method_owner}::{}", method.name);
            let method_is_template = impl_has_generics || !method.type_params.is_empty();
            !method_is_template || !specialized_templates.contains(&method_key)
        });
    }
    items.retain(|item| !matches!(item, TypedItem::Impl(def) if def.trait_target.is_none() && def.methods.is_empty()));
}

fn substitute_function_sig(sig: &FunctionSig, bindings: &HashMap<String, SemType>) -> FunctionSig {
    let bound_names = bindings.keys().cloned().collect::<HashSet<_>>();
    FunctionSig {
        type_params: Vec::new(),
        type_param_bounds: HashMap::new(),
        params: sig
            .params
            .iter()
            .map(|param| substitute_generic_type(param, &bound_names, bindings))
            .collect(),
        return_type: substitute_generic_type(&sig.return_type, &bound_names, bindings),
        structural_requirements: HashMap::new(),
    }
}

fn specialize_function_def_ast(
    template: &crate::ast::FunctionDef,
    specialized_name: &str,
    bindings: &HashMap<String, SemType>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<crate::ast::FunctionDef> {
    for param in &template.type_params {
        if !bindings.contains_key(&param.name) {
            diagnostics.push(Diagnostic::new(
                format!(
                    "Cannot specialize structural function `{}` because type parameter `{}` is unresolved",
                    template.name, param.name
                ),
                default_diag_span(),
            ));
            return None;
        }
    }

    Some(crate::ast::FunctionDef {
        visibility: template.visibility,
        name: specialized_name.to_string(),
        type_params: Vec::new(),
        params: template
            .params
            .iter()
            .map(|param| crate::ast::Param {
                name: param.name.clone(),
                ty: substitute_type_in_ast_type(&param.ty, bindings),
            })
            .collect(),
        return_type: template
            .return_type
            .as_ref()
            .map(|ty| substitute_type_in_ast_type(ty, bindings)),
        effect_row: template.effect_row.clone(),
        body: substitute_type_in_block(&template.body, bindings),
        span: template.span,
    })
}

fn specialize_impl_method_ast(
    template: &ImplMethodTemplate,
    bindings: &HashMap<String, SemType>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<crate::ast::ImplBlock> {
    for param in &template.method.type_params {
        if !bindings.contains_key(&param.name) {
            diagnostics.push(Diagnostic::new(
                format!(
                    "Cannot specialize structural impl method `{}` because type parameter `{}` is unresolved",
                    template.method.name, param.name
                ),
                default_diag_span(),
            ));
            return None;
        }
    }
    let specialized_method = crate::ast::FunctionDef {
        visibility: template.method.visibility,
        name: template.method.name.clone(),
        type_params: Vec::new(),
        params: template
            .method
            .params
            .iter()
            .map(|param| crate::ast::Param {
                name: param.name.clone(),
                ty: substitute_type_in_ast_type(&param.ty, bindings),
            })
            .collect(),
        return_type: template
            .method
            .return_type
            .as_ref()
            .map(|ty| substitute_type_in_ast_type(ty, bindings)),
        effect_row: template.method.effect_row.clone(),
        body: substitute_type_in_block(&template.method.body, bindings),
        span: template.method.span,
    };
    Some(crate::ast::ImplBlock {
        type_params: Vec::new(),
        target: template.impl_def.target.clone(),
        target_args: template
            .impl_def
            .target_args
            .iter()
            .map(|arg| substitute_type_in_ast_type(arg, bindings))
            .collect(),
        trait_target: template
            .impl_def
            .trait_target
            .as_ref()
            .map(|trait_ty| substitute_type_in_ast_type(trait_ty, bindings)),
        methods: vec![specialized_method],
        span: template.impl_def.span,
    })
}

fn substitute_type_in_block(block: &Block, bindings: &HashMap<String, SemType>) -> Block {
    Block {
        statements: block
            .statements
            .iter()
            .map(|stmt| substitute_type_in_stmt(stmt, bindings))
            .collect(),
    }
}

fn substitute_type_in_stmt(stmt: &Stmt, bindings: &HashMap<String, SemType>) -> Stmt {
    match stmt {
        Stmt::Const(def) => Stmt::Const(crate::ast::ConstDef {
            visibility: def.visibility,
            name: def.name.clone(),
            ty: def
                .ty
                .as_ref()
                .map(|ty| substitute_type_in_ast_type(ty, bindings)),
            value: substitute_type_in_expr(&def.value, bindings),
            is_const: def.is_const,
            span: def.span,
        }),
        Stmt::DestructureConst {
            pattern,
            value,
            is_const,
        } => Stmt::DestructureConst {
            pattern: pattern.clone(),
            value: substitute_type_in_expr(value, bindings),
            is_const: *is_const,
        },
        Stmt::Assign { target, op, value } => Stmt::Assign {
            target: substitute_type_in_assign_target(target, bindings),
            op: *op,
            value: substitute_type_in_expr(value, bindings),
        },
        Stmt::Return(value) => Stmt::Return(
            value
                .as_ref()
                .map(|value| substitute_type_in_expr(value, bindings)),
        ),
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => Stmt::If {
            condition: substitute_type_in_expr(condition, bindings),
            then_block: substitute_type_in_block(then_block, bindings),
            else_block: else_block
                .as_ref()
                .map(|else_block| substitute_type_in_block(else_block, bindings)),
        },
        Stmt::While { condition, body } => Stmt::While {
            condition: substitute_type_in_expr(condition, bindings),
            body: substitute_type_in_block(body, bindings),
        },
        Stmt::For {
            binding,
            iter,
            body,
        } => Stmt::For {
            binding: binding.clone(),
            iter: substitute_type_in_expr(iter, bindings),
            body: substitute_type_in_block(body, bindings),
        },
        Stmt::Loop { body } => Stmt::Loop {
            body: substitute_type_in_block(body, bindings),
        },
        Stmt::Break => Stmt::Break,
        Stmt::Continue => Stmt::Continue,
        Stmt::RustBlock(code) => Stmt::RustBlock(code.clone()),
        Stmt::Expr(expr) => Stmt::Expr(substitute_type_in_expr(expr, bindings)),
        Stmt::TailExpr(expr) => Stmt::TailExpr(substitute_type_in_expr(expr, bindings)),
    }
}

fn substitute_type_in_assign_target(
    target: &AssignTarget,
    bindings: &HashMap<String, SemType>,
) -> AssignTarget {
    match target {
        AssignTarget::Path(name) => AssignTarget::Path(name.clone()),
        AssignTarget::Field { base, field } => AssignTarget::Field {
            base: Box::new(substitute_type_in_expr(base, bindings)),
            field: field.clone(),
        },
        AssignTarget::Index { base, index } => AssignTarget::Index {
            base: Box::new(substitute_type_in_expr(base, bindings)),
            index: Box::new(substitute_type_in_expr(index, bindings)),
        },
        AssignTarget::Tuple(items) => AssignTarget::Tuple(
            items
                .iter()
                .map(|item| substitute_type_in_assign_target(item, bindings))
                .collect(),
        ),
    }
}

fn substitute_type_in_expr(expr: &Expr, bindings: &HashMap<String, SemType>) -> Expr {
    match expr {
        Expr::Int(value) => Expr::Int(*value),
        Expr::Float(value) => Expr::Float(value.clone()),
        Expr::Bool(value) => Expr::Bool(*value),
        Expr::Char(value) => Expr::Char(*value),
        Expr::String(value) => Expr::String(value.clone()),
        Expr::Path(path) => Expr::Path(path.clone()),
        Expr::PathWithTypeArgs { path, type_args } => Expr::PathWithTypeArgs {
            path: path.clone(),
            type_args: type_args.clone(),
        },
        Expr::Call { callee, args } => Expr::Call {
            callee: Box::new(substitute_type_in_expr(callee, bindings)),
            args: args
                .iter()
                .map(|arg| substitute_type_in_expr(arg, bindings))
                .collect(),
        },
        Expr::MacroCall { path, args } => Expr::MacroCall {
            path: path.clone(),
            args: args
                .iter()
                .map(|arg| substitute_type_in_expr(arg, bindings))
                .collect(),
        },
        Expr::Field { base, field } => Expr::Field {
            base: Box::new(substitute_type_in_expr(base, bindings)),
            field: field.clone(),
        },
        Expr::Index { base, index } => Expr::Index {
            base: Box::new(substitute_type_in_expr(base, bindings)),
            index: Box::new(substitute_type_in_expr(index, bindings)),
        },
        Expr::Match { scrutinee, arms } => Expr::Match {
            scrutinee: Box::new(substitute_type_in_expr(scrutinee, bindings)),
            arms: arms
                .iter()
                .map(|arm| crate::ast::MatchArm {
                    pattern: arm.pattern.clone(),
                    guard: arm
                        .guard
                        .as_ref()
                        .map(|guard| substitute_type_in_expr(guard, bindings)),
                    value: substitute_type_in_expr(&arm.value, bindings),
                })
                .collect(),
        },
        Expr::Unary { op, expr } => Expr::Unary {
            op: op.clone(),
            expr: Box::new(substitute_type_in_expr(expr, bindings)),
        },
        Expr::Binary { op, left, right } => Expr::Binary {
            op: op.clone(),
            left: Box::new(substitute_type_in_expr(left, bindings)),
            right: Box::new(substitute_type_in_expr(right, bindings)),
        },
        Expr::Array(items) => Expr::Array(
            items
                .iter()
                .map(|item| substitute_type_in_expr(item, bindings))
                .collect(),
        ),
        Expr::Tuple(items) => Expr::Tuple(
            items
                .iter()
                .map(|item| substitute_type_in_expr(item, bindings))
                .collect(),
        ),
        Expr::StructLiteral { path, fields } => Expr::StructLiteral {
            path: path.clone(),
            fields: fields
                .iter()
                .map(|field| StructLiteralField {
                    name: field.name.clone(),
                    value: substitute_type_in_expr(&field.value, bindings),
                })
                .collect(),
        },
        Expr::Block(block) => Expr::Block(substitute_type_in_block(block, bindings)),
        Expr::Closure {
            params,
            return_type,
            body,
        } => Expr::Closure {
            params: params
                .iter()
                .map(|param| crate::ast::Param {
                    name: param.name.clone(),
                    ty: substitute_type_in_ast_type(&param.ty, bindings),
                })
                .collect(),
            return_type: return_type
                .as_ref()
                .map(|ret| substitute_type_in_ast_type(ret, bindings)),
            body: substitute_type_in_block(body, bindings),
        },
        Expr::Range {
            start,
            end,
            inclusive,
        } => Expr::Range {
            start: start
                .as_ref()
                .map(|start| Box::new(substitute_type_in_expr(start, bindings))),
            end: end
                .as_ref()
                .map(|end| Box::new(substitute_type_in_expr(end, bindings))),
            inclusive: *inclusive,
        },
        Expr::Cast { expr, target_type } => Expr::Cast {
            expr: Box::new(substitute_type_in_expr(expr, bindings)),
            target_type: substitute_type_in_ast_type(target_type, bindings),
        },
        Expr::Try(inner) => Expr::Try(Box::new(substitute_type_in_expr(inner, bindings))),
    }
}

fn substitute_type_in_ast_type(ty: &Type, bindings: &HashMap<String, SemType>) -> Type {
    if ty.path.len() == 1
        && ty.args.is_empty()
        && ty.trait_bounds.is_empty()
        && let Some(bound) = bindings.get(&ty.path[0])
    {
        return sem_type_to_ast_type(bound);
    }
    Type {
        path: ty.path.clone(),
        args: ty
            .args
            .iter()
            .map(|arg| substitute_type_in_ast_type(arg, bindings))
            .collect(),
        trait_bounds: ty
            .trait_bounds
            .iter()
            .map(|bound| substitute_type_in_ast_type(bound, bindings))
            .collect(),
    }
}

fn sem_type_to_ast_type(ty: &SemType) -> Type {
    match ty {
        SemType::Path { path, args } => Type {
            path: path.clone(),
            args: args.iter().map(sem_type_to_ast_type).collect(),
            trait_bounds: Vec::new(),
        },
        SemType::Tuple(items) => Type {
            path: vec!["Tuple".to_string()],
            args: items.iter().map(sem_type_to_ast_type).collect(),
            trait_bounds: Vec::new(),
        },
        SemType::TraitObject(bounds) => {
            if bounds.is_empty() {
                return Type {
                    path: vec!["_".to_string()],
                    args: Vec::new(),
                    trait_bounds: Vec::new(),
                };
            }
            let mut base = sem_type_to_ast_type(&bounds[0]);
            base.trait_bounds = bounds.iter().skip(1).map(sem_type_to_ast_type).collect();
            base
        }
        SemType::Unit => Type {
            path: vec!["Tuple".to_string()],
            args: Vec::new(),
            trait_bounds: Vec::new(),
        },
        SemType::Unknown | SemType::Fn { .. } | SemType::Iter(_) => Type {
            path: vec!["_".to_string()],
            args: Vec::new(),
            trait_bounds: Vec::new(),
        },
    }
}

fn rewrite_structural_calls(
    items: &mut [TypedItem],
    lookup: &HashMap<(String, Vec<String>), String>,
) {
    for item in items {
        match item {
            TypedItem::Function(def) => {
                for stmt in &mut def.body {
                    rewrite_structural_calls_in_stmt(stmt, lookup);
                }
            }
            TypedItem::Impl(def) => {
                for method in &mut def.methods {
                    for stmt in &mut method.body {
                        rewrite_structural_calls_in_stmt(stmt, lookup);
                    }
                }
            }
            TypedItem::Const(def) => rewrite_structural_calls_in_expr(&mut def.value, lookup),
            TypedItem::Static(def) => rewrite_structural_calls_in_expr(&mut def.value, lookup),
            TypedItem::RustUse(_)
            | TypedItem::RustBlock(_)
            | TypedItem::Struct(_)
            | TypedItem::Enum(_)
            | TypedItem::Trait(_) => {}
        }
    }
}

fn rewrite_structural_calls_in_stmt(
    stmt: &mut TypedStmt,
    lookup: &HashMap<(String, Vec<String>), String>,
) {
    match stmt {
        TypedStmt::Const(def) => rewrite_structural_calls_in_expr(&mut def.value, lookup),
        TypedStmt::DestructureConst { value, .. } => {
            rewrite_structural_calls_in_expr(value, lookup)
        }
        TypedStmt::Assign { value, .. } => rewrite_structural_calls_in_expr(value, lookup),
        TypedStmt::Return(value) => {
            if let Some(value) = value {
                rewrite_structural_calls_in_expr(value, lookup);
            }
        }
        TypedStmt::If {
            condition,
            then_body,
            else_body,
        } => {
            rewrite_structural_calls_in_expr(condition, lookup);
            for stmt in then_body {
                rewrite_structural_calls_in_stmt(stmt, lookup);
            }
            if let Some(else_body) = else_body {
                for stmt in else_body {
                    rewrite_structural_calls_in_stmt(stmt, lookup);
                }
            }
        }
        TypedStmt::While { condition, body } => {
            rewrite_structural_calls_in_expr(condition, lookup);
            for stmt in body {
                rewrite_structural_calls_in_stmt(stmt, lookup);
            }
        }
        TypedStmt::For { iter, body, .. } => {
            rewrite_structural_calls_in_expr(iter, lookup);
            for stmt in body {
                rewrite_structural_calls_in_stmt(stmt, lookup);
            }
        }
        TypedStmt::Loop { body } => {
            for stmt in body {
                rewrite_structural_calls_in_stmt(stmt, lookup);
            }
        }
        TypedStmt::Expr(expr) => rewrite_structural_calls_in_expr(expr, lookup),
        TypedStmt::Break | TypedStmt::Continue | TypedStmt::RustBlock(_) => {}
    }
}

fn rewrite_structural_calls_in_expr(
    expr: &mut TypedExpr,
    lookup: &HashMap<(String, Vec<String>), String>,
) {
    match &mut expr.kind {
        TypedExprKind::Call { callee, args } => {
            if let TypedExprKind::Path(path) = &mut callee.kind
                && path.len() == 1
            {
                let key = (
                    path[0].clone(),
                    args.iter().map(|arg| arg.ty.clone()).collect(),
                );
                if let Some(specialized_name) = lookup.get(&key) {
                    *path = vec![specialized_name.clone()];
                    callee.ty = "_".to_string();
                }
            }
            rewrite_structural_calls_in_expr(callee, lookup);
            for arg in args {
                rewrite_structural_calls_in_expr(arg, lookup);
            }
        }
        TypedExprKind::MacroCall { args, .. } => {
            for arg in args {
                rewrite_structural_calls_in_expr(arg, lookup);
            }
        }
        TypedExprKind::Field { base, .. } => rewrite_structural_calls_in_expr(base, lookup),
        TypedExprKind::Index { base, index, .. } => {
            rewrite_structural_calls_in_expr(base, lookup);
            rewrite_structural_calls_in_expr(index, lookup);
        }
        TypedExprKind::Match { scrutinee, arms } => {
            rewrite_structural_calls_in_expr(scrutinee, lookup);
            for arm in arms {
                if let Some(guard) = &mut arm.guard {
                    rewrite_structural_calls_in_expr(guard, lookup);
                }
                rewrite_structural_calls_in_expr(&mut arm.value, lookup);
            }
        }
        TypedExprKind::Unary { expr, .. } => rewrite_structural_calls_in_expr(expr, lookup),
        TypedExprKind::Cast { expr, .. } => rewrite_structural_calls_in_expr(expr, lookup),
        TypedExprKind::Binary { left, right, .. } => {
            rewrite_structural_calls_in_expr(left, lookup);
            rewrite_structural_calls_in_expr(right, lookup);
        }
        TypedExprKind::Array(items) | TypedExprKind::Tuple(items) => {
            for item in items {
                rewrite_structural_calls_in_expr(item, lookup);
            }
        }
        TypedExprKind::StructLiteral { fields, .. } => {
            for field in fields {
                rewrite_structural_calls_in_expr(&mut field.value, lookup);
            }
        }
        TypedExprKind::Block { body, tail } => {
            for stmt in body {
                rewrite_structural_calls_in_stmt(stmt, lookup);
            }
            if let Some(tail) = tail {
                rewrite_structural_calls_in_expr(tail, lookup);
            }
        }
        TypedExprKind::Closure { body, .. } => {
            for stmt in body {
                rewrite_structural_calls_in_stmt(stmt, lookup);
            }
        }
        TypedExprKind::Range { start, end, .. } => {
            if let Some(start) = start {
                rewrite_structural_calls_in_expr(start, lookup);
            }
            if let Some(end) = end {
                rewrite_structural_calls_in_expr(end, lookup);
            }
        }
        TypedExprKind::Try(inner) => rewrite_structural_calls_in_expr(inner, lookup),
        TypedExprKind::Path(_)
        | TypedExprKind::Int(_)
        | TypedExprKind::Float(_)
        | TypedExprKind::Bool(_)
        | TypedExprKind::String(_)
        | TypedExprKind::Char(_) => {}
    }
}

fn extract_rust_block_function_names(code: &str) -> Vec<String> {
    let mut out = Vec::new();
    let bytes = code.as_bytes();
    let mut i = 0usize;
    while i + 3 <= bytes.len() {
        if &bytes[i..i + 3] == b"fn " {
            let mut j = i + 3;
            while j < bytes.len() && bytes[j].is_ascii_whitespace() {
                j += 1;
            }
            let start = j;
            while j < bytes.len() && (bytes[j].is_ascii_alphanumeric() || bytes[j] == b'_') {
                j += 1;
            }
            if j > start {
                out.push(code[start..j].to_string());
            }
            i = j;
            continue;
        }
        i += 1;
    }
    out
}

fn lower_item(
    item: &Item,
    context: &mut Context,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<TypedItem> {
    match item {
        Item::RustUse(def) => Some(TypedItem::RustUse(TypedRustUse {
            tree: ast_use_tree_to_typed(&def.tree),
        })),
        Item::RustBlock(code) => Some(TypedItem::RustBlock(code.clone())),
        Item::Struct(def) => Some(TypedItem::Struct(TypedStruct {
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
                        .map(|bound| {
                            rust_trait_bound_string(&type_from_ast_in_context(bound, context))
                        })
                        .collect(),
                })
                .collect(),
            fields: def
                .fields
                .iter()
                .map(|field| TypedField {
                    name: field.name.clone(),
                    ty: rust_owned_type_string(&type_from_ast_in_context(&field.ty, context)),
                })
                .collect(),
        })),
        Item::Enum(def) => Some(TypedItem::Enum(TypedEnum {
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
                        .map(|bound| {
                            rust_trait_bound_string(&type_from_ast_in_context(bound, context))
                        })
                        .collect(),
                })
                .collect(),
            variants: def
                .variants
                .iter()
                .map(|variant| TypedVariant {
                    name: variant.name.clone(),
                    fields: match &variant.fields {
                        EnumVariantFields::Unit => TypedVariantFields::Unit,
                        EnumVariantFields::Tuple(items) => TypedVariantFields::Tuple(
                            items
                                .iter()
                                .map(|ty| type_from_ast_in_context(ty, context))
                                .map(|ty| rust_owned_type_string(&ty))
                                .collect(),
                        ),
                        EnumVariantFields::Named(fields) => TypedVariantFields::Named(
                            fields
                                .iter()
                                .map(|field| TypedField {
                                    name: field.name.clone(),
                                    ty: rust_owned_type_string(&type_from_ast_in_context(
                                        &field.ty, context,
                                    )),
                                })
                                .collect(),
                        ),
                    },
                })
                .collect(),
        })),
        Item::Trait(def) => Some(TypedItem::Trait(TypedTrait {
            is_public: def.visibility == Visibility::Public,
            name: def.name.clone(),
            supertraits: def
                .supertraits
                .iter()
                .map(|ty| type_from_ast_in_context(ty, context))
                .map(|ty| rust_trait_bound_string(&ty))
                .collect(),
            methods: def
                .methods
                .iter()
                .map(|method| lower_trait_method_sig(method, context))
                .collect(),
        })),
        Item::Impl(def) => {
            let mut methods = Vec::new();
            for method in &def.methods {
                let typed_method = with_diag_span_scope(method.span, || {
                    let resolved_sig =
                        resolve_impl_method_signature(method, def, context, diagnostics);
                    let mut locals = HashMap::new();
                    let mut immutable_locals = HashSet::new();
                    for (index, param) in method.params.iter().enumerate() {
                        if is_redundant_impl_self_param(&method.params, index) {
                            continue;
                        }
                        let resolved_param_ty = resolved_sig
                            .param_sem_types
                            .get(index)
                            .cloned()
                            .unwrap_or_else(|| {
                                type_from_ast_with_impl_self_in_context(
                                    &param.ty,
                                    &def.target,
                                    &def.target_args,
                                    context,
                                )
                            });
                        locals.insert(param.name.clone(), resolved_param_ty);
                    }
                    let declared_return_ty = (resolved_sig.return_sem_type != SemType::Unknown)
                        .then(|| resolved_sig.return_sem_type.clone());
                    let provisional_return_ty =
                        declared_return_ty.clone().unwrap_or(SemType::Unknown);
                    let mut body = Vec::new();
                    let mut inferred_returns = Vec::new();
                    for (index, statement) in method.body.statements.iter().enumerate() {
                        let is_last = index + 1 == method.body.statements.len();
                        if is_last && let Stmt::TailExpr(expr) = statement {
                            let (mut typed_expr, inferred) = infer_expr(
                                expr,
                                context,
                                &mut locals,
                                &provisional_return_ty,
                                diagnostics,
                            );
                            inferred_returns.push(inferred.clone());
                            let literal_adjusted = literal_bidi_adjusted_numeric_arg_type(
                                Some(expr),
                                &inferred,
                                &provisional_return_ty,
                            );
                            let compatible = is_compatible(&inferred, &provisional_return_ty)
                                || literal_adjusted.is_some();
                            if provisional_return_ty != SemType::Unknown && !compatible {
                                diagnostics.push(Diagnostic::new(
                                    mismatch_message(
                                        format!(
                                            "Return type mismatch: expected `{}`, got `{}`",
                                            type_to_string(&provisional_return_ty),
                                            type_to_string(&inferred)
                                        ),
                                        &inferred,
                                        &provisional_return_ty,
                                    ),
                                    default_diag_span(),
                                ));
                            } else if provisional_return_ty != SemType::Unknown {
                                typed_expr = maybe_insert_implicit_integral_cast(
                                    typed_expr,
                                    &inferred,
                                    &provisional_return_ty,
                                );
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
                            inference_hole_message(
                                format!(
                                    "Method `{}` return type could not be fully inferred; add an explicit return type",
                                    method.name
                                ),
                                &format!("method `{}`", method.name),
                                "an explicit return type",
                            ),
                            default_diag_span(),
                        ));
                    }
                    let has_rust_block = body.iter().any(|s| matches!(s, TypedStmt::RustBlock(_)));
                    if final_return_ty != SemType::Unit
                        && inferred_returns.is_empty()
                        && !has_rust_block
                    {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "Method `{}` must explicitly return `{}`",
                                method.name,
                                type_to_string(&final_return_ty)
                            ),
                            default_diag_span(),
                        ));
                    }
                    let method_lookup_name =
                        format!("{}::{}", impl_lookup_name(&def.target), method.name);
                    let inferred_method_bounds = context
                        .functions
                        .get(&method_lookup_name)
                        .map(|sig| sig.type_param_bounds.clone())
                        .unwrap_or_default();
                    TypedFunction {
                        is_public: method.visibility == Visibility::Public,
                        name: method.name.clone(),
                        type_params: method
                            .type_params
                            .iter()
                            .map(|param| TypedTypeParam {
                                name: param.name.clone(),
                                bounds: inferred_method_bounds
                                    .get(&param.name)
                                    .cloned()
                                    .unwrap_or_default()
                                    .into_iter()
                                    .map(|bound| rust_trait_bound_string(&bound))
                                    .collect(),
                            })
                            .collect(),
                        params: method
                            .params
                            .iter()
                            .enumerate()
                            .filter(|(index, _)| {
                                !is_redundant_impl_self_param(&method.params, *index)
                            })
                            .map(|(index, param)| TypedParam {
                                name: param.name.clone(),
                                ty: resolved_sig
                                    .param_rust_types
                                    .as_ref()
                                    .and_then(|types| types.get(index).cloned())
                                    .unwrap_or_else(|| {
                                        resolved_sig
                                            .param_sem_types
                                            .get(index)
                                            .map(rust_param_type_string)
                                            .unwrap_or_else(|| {
                                                rust_param_type_string(
                                                    &type_from_ast_with_impl_self_in_context(
                                                        &param.ty,
                                                        &def.target,
                                                        &def.target_args,
                                                        context,
                                                    ),
                                                )
                                            })
                                    }),
                            })
                            .collect(),
                        return_type: resolved_sig
                            .return_rust_type
                            .clone()
                            .unwrap_or_else(|| rust_owned_type_string(&final_return_ty)),
                        body,
                    }
                });
                methods.push(typed_method);
            }
            let impl_param_bounds = inferred_impl_type_param_bounds(def, context);
            Some(TypedItem::Impl(TypedImpl {
                type_params: def
                    .type_params
                    .iter()
                    .map(|param| TypedTypeParam {
                        name: param.name.clone(),
                        bounds: impl_param_bounds
                            .get(&param.name)
                            .cloned()
                            .unwrap_or_default()
                            .into_iter()
                            .map(|bound| rust_trait_bound_string(&bound))
                            .collect(),
                    })
                    .collect(),
                target: def.target.clone(),
                target_args: def
                    .target_args
                    .iter()
                    .map(|arg| rust_owned_type_string(&type_from_ast_in_context(arg, context)))
                    .collect(),
                trait_target: def
                    .trait_target
                    .as_ref()
                    .map(|ty| rust_trait_bound_string(&type_from_ast_in_context(ty, context))),
                methods,
            }))
        }
        Item::Const(def) => {
            let mut locals = HashMap::new();
            let (mut typed_value, inferred) = infer_expr(
                &def.value,
                context,
                &mut locals,
                &SemType::Unit,
                diagnostics,
            );
            let declared = def
                .ty
                .as_ref()
                .map(|ty| type_from_ast_in_context(ty, context));
            let final_ty = if let Some(declared_ty) = declared {
                let bidi_adjusted = literal_bidi_adjusted_numeric_arg_type(
                    Some(&def.value),
                    &inferred,
                    &declared_ty,
                );
                let compatible = is_compatible(&inferred, &declared_ty) || bidi_adjusted.is_some();
                if !compatible {
                    diagnostics.push(Diagnostic::new(
                        mismatch_message(
                            format!(
                                "Const `{}` type mismatch: expected `{}`, got `{}`",
                                def.name,
                                type_to_string(&declared_ty),
                                type_to_string(&inferred)
                            ),
                            &inferred,
                            &declared_ty,
                        ),
                        def.span.unwrap_or(default_diag_span()),
                    ));
                } else {
                    typed_value =
                        maybe_insert_implicit_integral_cast(typed_value, &inferred, &declared_ty);
                }
                declared_ty
            } else {
                inferred
            };
            if contains_unknown(&final_ty) {
                diagnostics.push(Diagnostic::new(
                    inference_hole_message(
                        format!(
                            "Const `{}` type could not be fully inferred; add an explicit type",
                            def.name
                        ),
                        &format!("const `{}`", def.name),
                        "an explicit type",
                    ),
                    def.span.unwrap_or(default_diag_span()),
                ));
            }
            Some(TypedItem::Const(TypedConst {
                is_public: def.visibility == Visibility::Public,
                is_const: def.is_const,
                name: def.name.clone(),
                ty: rust_owned_type_string(&final_ty),
                value: typed_value,
            }))
        }
        Item::Static(def) => {
            let mut locals = HashMap::new();
            let (mut typed_value, inferred) = infer_expr(
                &def.value,
                context,
                &mut locals,
                &SemType::Unit,
                diagnostics,
            );
            let declared = type_from_ast_in_context(&def.ty, context);
            if !is_compatible(&inferred, &declared) {
                diagnostics.push(Diagnostic::new(
                    mismatch_message(
                        format!(
                            "Static `{}` type mismatch: expected `{}`, got `{}`",
                            def.name,
                            type_to_string(&declared),
                            type_to_string(&inferred)
                        ),
                        &inferred,
                        &declared,
                    ),
                    def.span.unwrap_or(default_diag_span()),
                ));
            } else {
                typed_value =
                    maybe_insert_implicit_integral_cast(typed_value, &inferred, &declared);
            }
            if contains_unknown(&declared) {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Static `{}` type must be concrete and cannot include `_`",
                        def.name
                    ),
                    def.span.unwrap_or(default_diag_span()),
                ));
            }
            Some(TypedItem::Static(TypedStatic {
                is_public: def.visibility == Visibility::Public,
                name: def.name.clone(),
                ty: rust_owned_type_string(&declared),
                value: typed_value,
            }))
        }
        Item::Function(def) => with_diag_span_scope(def.span, || {
            let mut locals = HashMap::new();
            let mut immutable_locals = HashSet::new();
            for param in &def.params {
                locals.insert(
                    param.name.clone(),
                    type_from_ast_in_context(&param.ty, context),
                );
            }
            let declared_return_ty = def
                .return_type
                .as_ref()
                .map(|ty| type_from_ast_in_context(ty, context));
            let provisional_return_ty = declared_return_ty.clone().unwrap_or(SemType::Unknown);
            let mut body = Vec::new();
            let mut inferred_returns = Vec::new();
            for (index, statement) in def.body.statements.iter().enumerate() {
                let is_last = index + 1 == def.body.statements.len();
                if is_last && let Stmt::TailExpr(expr) = statement {
                    let (mut typed_expr, inferred) = infer_expr(
                        expr,
                        context,
                        &mut locals,
                        &provisional_return_ty,
                        diagnostics,
                    );
                    inferred_returns.push(inferred.clone());
                    let literal_adjusted = literal_bidi_adjusted_numeric_arg_type(
                        Some(expr),
                        &inferred,
                        &provisional_return_ty,
                    );
                    let compatible = is_compatible(&inferred, &provisional_return_ty)
                        || literal_adjusted.is_some();
                    if provisional_return_ty != SemType::Unknown && !compatible {
                        diagnostics.push(Diagnostic::new(
                            mismatch_message(
                                format!(
                                    "Return type mismatch: expected `{}`, got `{}`",
                                    type_to_string(&provisional_return_ty),
                                    type_to_string(&inferred)
                                ),
                                &inferred,
                                &provisional_return_ty,
                            ),
                            default_diag_span(),
                        ));
                    } else if provisional_return_ty != SemType::Unknown {
                        typed_expr = maybe_insert_implicit_integral_cast(
                            typed_expr,
                            &inferred,
                            &provisional_return_ty,
                        );
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
                    inference_hole_message(
                        format!(
                            "Function `{}` return type could not be fully inferred; add an explicit return type",
                            def.name
                        ),
                        &format!("function `{}`", def.name),
                        "an explicit return type",
                    ),
                    default_diag_span(),
                ));
            }
            let has_rust_block = body.iter().any(|s| matches!(s, TypedStmt::RustBlock(_)));
            if final_return_ty != SemType::Unit && inferred_returns.is_empty() && !has_rust_block {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Function `{}` must explicitly return `{}`",
                        def.name,
                        type_to_string(&final_return_ty)
                    ),
                    default_diag_span(),
                ));
            }

            let resolved_param_types = def
                .params
                .iter()
                .map(|param| resolve_function_param_type(param, context, &locals))
                .collect::<Vec<_>>();
            if let Some(sig) = context.functions.get_mut(&def.name) {
                sig.params = resolved_param_types.clone();
                sig.return_type = final_return_ty.clone();
            }
            let function_param_bounds = context
                .functions
                .get(&def.name)
                .map(|sig| sig.type_param_bounds.clone())
                .unwrap_or_default();

            Some(TypedItem::Function(TypedFunction {
                is_public: def.visibility == Visibility::Public,
                name: def.name.clone(),
                type_params: def
                    .type_params
                    .iter()
                    .map(|param| TypedTypeParam {
                        name: param.name.clone(),
                        bounds: function_param_bounds
                            .get(&param.name)
                            .cloned()
                            .unwrap_or_default()
                            .into_iter()
                            .map(|bound| rust_trait_bound_string(&bound))
                            .collect(),
                    })
                    .collect(),
                params: def
                    .params
                    .iter()
                    .zip(resolved_param_types.iter())
                    .map(|(param, ty)| TypedParam {
                        name: param.name.clone(),
                        ty: rust_param_type_string(ty),
                    })
                    .collect(),
                return_type: rust_owned_type_string(&final_return_ty),
                body,
            }))
        }),
    }
}

fn ast_use_tree_to_typed(tree: &UseTree) -> TypedUseTree {
    match tree {
        UseTree::Name(name) => TypedUseTree::Name(name.clone()),
        UseTree::Glob => TypedUseTree::Glob,
        UseTree::Path { segment, next } => TypedUseTree::Path {
            segment: segment.clone(),
            next: Box::new(ast_use_tree_to_typed(next)),
        },
        UseTree::Group(items) => {
            TypedUseTree::Group(items.iter().map(ast_use_tree_to_typed).collect())
        }
    }
}

fn typed_use_tree_to_rust(tree: &TypedUseTree) -> RustUseTree {
    match tree {
        TypedUseTree::Name(name) => RustUseTree::Name(name.clone()),
        TypedUseTree::Glob => RustUseTree::Glob,
        TypedUseTree::Path { segment, next } => RustUseTree::Path {
            segment: segment.clone(),
            next: Box::new(typed_use_tree_to_rust(next)),
        },
        TypedUseTree::Group(items) => {
            RustUseTree::Group(items.iter().map(typed_use_tree_to_rust).collect())
        }
    }
}

fn inferred_impl_type_param_bounds(
    imp: &crate::ast::ImplBlock,
    context: &Context,
) -> HashMap<String, Vec<SemType>> {
    let mut out = imp
        .type_params
        .iter()
        .map(|param| {
            (
                param.name.clone(),
                param
                    .bounds
                    .iter()
                    .map(|bound| type_from_ast_with_impl_self(bound, &imp.target, &imp.target_args))
                    .collect::<Vec<_>>(),
            )
        })
        .collect::<HashMap<_, _>>();

    let owner = impl_lookup_name(&imp.target);
    for method in &imp.methods {
        let method_key = format!("{owner}::{}", method.name);
        let Some(sig) = context.functions.get(&method_key) else {
            continue;
        };
        for param in &imp.type_params {
            let Some(inferred) = sig.type_param_bounds.get(&param.name) else {
                continue;
            };
            let entry = out.entry(param.name.clone()).or_default();
            for bound in inferred {
                push_unique_bound(entry, bound.clone());
            }
        }
    }

    out
}

fn resolve_function_param_type(
    param: &crate::ast::Param,
    context: &Context,
    locals: &HashMap<String, SemType>,
) -> SemType {
    let declared = type_from_ast_in_context(&param.ty, context);
    if !infer_local_bidi_enabled() || declared != SemType::Unknown {
        return declared;
    }
    locals.get(&param.name).cloned().unwrap_or(SemType::Unknown)
}

fn lower_trait_method_sig(method: &TraitMethodSig, context: &Context) -> TypedTraitMethod {
    TypedTraitMethod {
        name: method.name.clone(),
        type_params: method
            .type_params
            .iter()
            .map(|param| TypedTypeParam {
                name: param.name.clone(),
                bounds: param
                    .bounds
                    .iter()
                    .map(type_from_ast)
                    .map(|bound| rust_trait_bound_string(&bound))
                    .collect(),
            })
            .collect(),
        params: method
            .params
            .iter()
            .map(|param| TypedParam {
                name: param.name.clone(),
                ty: rust_param_type_string(&type_from_ast_in_context(&param.ty, context)),
            })
            .collect(),
        return_type: method
            .return_type
            .as_ref()
            .map(|ty| type_from_ast_in_context(ty, context))
            .map(|ty| rust_owned_type_string(&ty))
            .unwrap_or_else(|| rust_owned_type_string(&SemType::Unit)),
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
            let (mut typed_value, inferred) =
                infer_expr(&def.value, context, locals, return_ty, diagnostics);
            diagnose_const_mutations_in_expr(&typed_value, immutable_locals, diagnostics);
            let declared = def
                .ty
                .as_ref()
                .map(|ty| type_from_ast_in_context(ty, context));
            let final_ty = if let Some(declared_ty) = declared {
                let literal_adjusted = literal_bidi_adjusted_numeric_arg_type(
                    Some(&def.value),
                    &inferred,
                    &declared_ty,
                );
                let compatible =
                    is_compatible(&inferred, &declared_ty) || literal_adjusted.is_some();
                if !compatible {
                    diagnostics.push(Diagnostic::new(
                        mismatch_message(
                            format!(
                                "Const `{}` type mismatch: expected `{}`, got `{}`",
                                def.name,
                                type_to_string(&declared_ty),
                                type_to_string(&inferred)
                            ),
                            &inferred,
                            &declared_ty,
                        ),
                        default_diag_span(),
                    ));
                } else {
                    typed_value =
                        maybe_insert_implicit_integral_cast(typed_value, &inferred, &declared_ty);
                }
                declared_ty
            } else {
                inferred
            };
            if immutable_locals.contains(&def.name) {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Cannot redeclare `{}` because it is an immutable const in scope",
                        def.name
                    ),
                    default_diag_span(),
                ));
            } else {
                locals.insert(def.name.clone(), final_ty.clone());
                if def.is_const {
                    immutable_locals.insert(def.name.clone());
                }
            }
            Some(TypedStmt::Const(TypedConst {
                is_public: false,
                is_const: def.is_const,
                name: def.name.clone(),
                ty: rust_owned_type_string(&final_ty),
                value: typed_value,
            }))
        }
        Stmt::DestructureConst {
            pattern,
            value,
            is_const,
        } => {
            let (typed_value, value_ty) =
                infer_expr(value, context, locals, return_ty, diagnostics);
            diagnose_const_mutations_in_expr(&typed_value, immutable_locals, diagnostics);
            let mut binding_names = HashSet::new();
            collect_destructure_pattern_names(pattern, &mut binding_names);
            let mut has_const_shadow_conflict = false;
            for name in &binding_names {
                if immutable_locals.contains(name) {
                    has_const_shadow_conflict = true;
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Cannot redeclare `{}` because it is an immutable const in scope",
                            name
                        ),
                        default_diag_span(),
                    ));
                }
            }
            if !has_const_shadow_conflict {
                bind_destructure_pattern(pattern, &value_ty, locals, diagnostics);
            }
            if *is_const && !has_const_shadow_conflict {
                collect_destructure_pattern_names(pattern, immutable_locals);
            }
            Some(TypedStmt::DestructureConst {
                pattern: lower_destructure_pattern_typed(pattern),
                value: typed_value,
                is_const: *is_const,
            })
        }
        Stmt::Assign { target, op, value } => {
            if infer_local_bidi_enabled()
                && matches!(op, AssignOp::Assign)
                && let AssignTarget::Path(name) = target
                && !locals.contains_key(name)
                && !context.globals.contains_key(name)
            {
                if immutable_locals.contains(name) {
                    diagnostics.push(Diagnostic::new(
                        format!("Cannot assign to immutable const `{name}`"),
                        default_diag_span(),
                    ));
                    return None;
                }
                let (typed_value, value_ty) =
                    infer_expr(value, context, locals, return_ty, diagnostics);
                diagnose_const_mutations_in_expr(&typed_value, immutable_locals, diagnostics);
                locals.insert(name.clone(), value_ty.clone());
                return Some(TypedStmt::Const(TypedConst {
                    is_public: false,
                    is_const: false,
                    name: name.clone(),
                    ty: rust_owned_type_string(&value_ty),
                    value: typed_value,
                }));
            }
            let (typed_target, target_ty) = infer_assign_target(
                target,
                context,
                locals,
                immutable_locals,
                return_ty,
                diagnostics,
            )?;
            let (mut typed_value, value_ty) =
                infer_expr(value, context, locals, return_ty, diagnostics);
            diagnose_const_mutations_in_expr(&typed_value, immutable_locals, diagnostics);
            match op {
                AssignOp::Assign => {
                    let literal_adjusted =
                        literal_bidi_adjusted_numeric_arg_type(Some(value), &value_ty, &target_ty);
                    let compatible =
                        is_compatible(&value_ty, &target_ty) || literal_adjusted.is_some();
                    if !compatible {
                        diagnostics.push(Diagnostic::new(
                            mismatch_message(
                                format!(
                                    "Assignment type mismatch: expected `{}`, got `{}`",
                                    type_to_string(&target_ty),
                                    type_to_string(&value_ty)
                                ),
                                &value_ty,
                                &target_ty,
                            ),
                            default_diag_span(),
                        ));
                    } else {
                        typed_value =
                            maybe_insert_implicit_integral_cast(typed_value, &value_ty, &target_ty);
                    }
                }
                AssignOp::AddAssign => {
                    if matches!(typed_target, TypedAssignTarget::Tuple(_)) {
                        diagnostics.push(Diagnostic::new(
                            "`+=` is not supported on tuple assignment targets",
                            default_diag_span(),
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
                            default_diag_span(),
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
                let (mut typed_expr, inferred) =
                    infer_expr(expr, context, locals, return_ty, diagnostics);
                diagnose_const_mutations_in_expr(&typed_expr, immutable_locals, diagnostics);
                inferred_returns.push(inferred.clone());
                let literal_adjusted =
                    literal_bidi_adjusted_numeric_arg_type(Some(expr), &inferred, return_ty);
                let compatible = is_compatible(&inferred, return_ty) || literal_adjusted.is_some();
                if *return_ty != SemType::Unknown && !compatible {
                    diagnostics.push(Diagnostic::new(
                        mismatch_message(
                            format!(
                                "Return type mismatch: expected `{}`, got `{}`",
                                type_to_string(return_ty),
                                type_to_string(&inferred)
                            ),
                            &inferred,
                            return_ty,
                        ),
                        default_diag_span(),
                    ));
                } else if *return_ty != SemType::Unknown {
                    typed_expr =
                        maybe_insert_implicit_integral_cast(typed_expr, &inferred, return_ty);
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
                        default_diag_span(),
                    ));
                }
                Some(TypedStmt::Return(None))
            }
        }
        Stmt::Expr(expr) => {
            let (typed_expr, _) = infer_expr(expr, context, locals, return_ty, diagnostics);
            diagnose_const_mutations_in_expr(&typed_expr, immutable_locals, diagnostics);
            if matches!(typed_expr.kind, TypedExprKind::String(_)) {
                None
            } else {
                Some(TypedStmt::Expr(typed_expr))
            }
        }
        Stmt::TailExpr(expr) => {
            let (typed_expr, _) = infer_expr(expr, context, locals, return_ty, diagnostics);
            diagnose_const_mutations_in_expr(&typed_expr, immutable_locals, diagnostics);
            Some(TypedStmt::Expr(typed_expr))
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            let (typed_condition, condition_ty) =
                infer_expr(condition, context, locals, return_ty, diagnostics);
            diagnose_const_mutations_in_expr(&typed_condition, immutable_locals, diagnostics);
            if !is_compatible(&condition_ty, &named_type("bool")) {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "`if` condition must be `bool`, got `{}`",
                        type_to_string(&condition_ty)
                    ),
                    default_diag_span(),
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
            diagnose_const_mutations_in_expr(&typed_condition, immutable_locals, diagnostics);
            if !is_compatible(&condition_ty, &named_type("bool")) {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "`while` condition must be `bool`, got `{}`",
                        type_to_string(&condition_ty)
                    ),
                    default_diag_span(),
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
            diagnose_const_mutations_in_expr(&typed_iter, immutable_locals, diagnostics);
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
                    default_diag_span(),
                ));
            }
            if context.globals.contains_key(name) {
                diagnostics.push(Diagnostic::new(
                    format!("Cannot assign to global item `{name}`"),
                    default_diag_span(),
                ));
            }
            let ty = locals
                .get(name)
                .cloned()
                .or_else(|| context.globals.get(name).cloned());
            if let Some(ty) = ty {
                Some((TypedAssignTarget::Path(name.clone()), ty))
            } else {
                diagnostics.push(Diagnostic::new(
                    format!("Unknown assignment target `{name}`"),
                    default_diag_span(),
                ));
                None
            }
        }
        AssignTarget::Field { base, field } => {
            let (typed_base, base_ty) = infer_expr(base, context, locals, return_ty, diagnostics);
            if let Some(root) = typed_root_path_name(&typed_base)
                && immutable_locals.contains(root)
            {
                diagnostics.push(Diagnostic::new(
                    format!("Cannot assign through immutable const `{root}`"),
                    default_diag_span(),
                ));
            }
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
            if let Some(root) = typed_root_path_name(&typed_base)
                && immutable_locals.contains(root)
            {
                diagnostics.push(Diagnostic::new(
                    format!("Cannot assign through immutable const `{root}`"),
                    default_diag_span(),
                ));
            }
            let (typed_index, index_ty) =
                infer_expr(index, context, locals, return_ty, diagnostics);
            let resolved_ty = match resolve_index_capability(&base_ty, Some(context), diagnostics) {
                Some(IndexCapability {
                    mode: CapabilityIndexMode::DirectIndex,
                    key_ty,
                    value_ty,
                    ..
                }) => {
                    let vec_like = matches!(
                        &base_ty,
                        SemType::Path { path, .. } if path.last().is_some_and(|segment| segment == "Vec")
                    );
                    let direct_key_ok = if vec_like {
                        is_vector_index_type(&index_ty)
                            && index_key_type_compatible(&index_ty, &key_ty)
                    } else {
                        index_key_type_compatible(&index_ty, &key_ty)
                    };
                    if direct_key_ok {
                        value_ty
                    } else if vec_like && !matches!(typed_index.kind, TypedExprKind::Range { .. }) {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "Vector indexing expects integer index (or range), got `{}`",
                                type_to_string(&index_ty)
                            ),
                            default_diag_span(),
                        ));
                        SemType::Unknown
                    } else {
                        diagnostics.push(Diagnostic::new(
                            mismatch_message(
                                format!(
                                    "Index assignment key mismatch: expected `{}`, got `{}`",
                                    type_to_string(&key_ty),
                                    type_to_string(&index_ty)
                                ),
                                &index_ty,
                                &key_ty,
                            ),
                            default_diag_span(),
                        ));
                        emit_constraint_partition_diagnostic(
                            "index assignment key type",
                            &[index_ty.clone(), key_ty.clone()],
                            "an explicit index key type annotation",
                            diagnostics,
                        );
                        SemType::Unknown
                    }
                }
                Some(IndexCapability {
                    mode: CapabilityIndexMode::GetLikeOption,
                    ..
                }) => {
                    diagnostics.push(Diagnostic::new(
                        "Map-like subscript assignment is not supported; use insert/update APIs",
                        default_diag_span(),
                    ));
                    SemType::Unknown
                }
                None => {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Capability resolution failed for index assignment on `{}`",
                            type_to_string(&base_ty)
                        ),
                        default_diag_span(),
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
        Expr::Float(value) => (
            TypedExpr {
                kind: TypedExprKind::Float(value.clone()),
                ty: "f64".to_string(),
            },
            named_type("f64"),
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
        Expr::PathWithTypeArgs { path, type_args: _ } => {
            let ty = resolve_path_value(path, context).unwrap_or(SemType::Unknown);
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
            let mut resolved_type_path = path.clone();
            let mut resolved_ty = SemType::Path {
                path: resolved_type_path.clone(),
                args: Vec::new(),
            };
            let mut expected_fields: Option<HashMap<String, SemType>> = None;
            let mut type_param_names: Vec<String> = Vec::new();
            let mut generic_bindings = HashMap::new();
            let mut type_param_set = HashSet::new();
            let mut is_enum_variant_literal = false;

            if path.len() == 2 {
                let enum_name = &path[0];
                let variant_name = &path[1];
                if let Some(variants) = context.enums.get(enum_name)
                    && let Some(variant_info) = variants.get(variant_name)
                {
                    is_enum_variant_literal = true;
                    resolved_type_path = vec![enum_name.clone()];
                    resolved_ty = SemType::Path {
                        path: resolved_type_path.clone(),
                        args: Vec::new(),
                    };
                    type_param_names = context
                        .enum_type_params
                        .get(enum_name)
                        .cloned()
                        .unwrap_or_default();
                    type_param_set = type_param_names.iter().cloned().collect();
                    if let Some(field_names) = &variant_info.named_fields {
                        let field_map = field_names
                            .iter()
                            .zip(variant_info.payload_types.iter())
                            .map(|(name, ty)| (name.clone(), ty.clone()))
                            .collect::<HashMap<_, _>>();
                        expected_fields = Some(field_map);
                    } else if variant_info.payload_types.is_empty() {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "Enum variant `{enum_name}::{variant_name}` has no named fields; use `{enum_name}::{variant_name}`"
                            ),
                            default_diag_span(),
                        ));
                    } else {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "Enum variant `{enum_name}::{variant_name}` uses tuple payload; use `{enum_name}::{variant_name}(...)`"
                            ),
                            default_diag_span(),
                        ));
                    }
                }
            }

            if !is_enum_variant_literal {
                if let Some(struct_name) = path.last() {
                    if let Some(struct_fields) = context.structs.get(struct_name) {
                        expected_fields = Some(struct_fields.clone());
                    }
                    if let Some(params) = context.struct_type_params.get(struct_name) {
                        type_param_names = params.clone();
                        type_param_set = type_param_names.iter().cloned().collect();
                    }
                } else {
                    resolved_ty = SemType::Unknown;
                }
            }

            for StructLiteralField { name, value } in fields {
                let (mut typed_value, value_ty) =
                    infer_expr(value, context, locals, return_ty, diagnostics);
                if !seen.insert(name.clone()) {
                    diagnostics.push(Diagnostic::new(
                        format!("Duplicate struct literal field `{name}`"),
                        default_diag_span(),
                    ));
                }
                if let Some(expected) = expected_fields.as_ref().and_then(|map| map.get(name)) {
                    let bound = bind_generic_params(
                        expected,
                        &value_ty,
                        &type_param_set,
                        &mut generic_bindings,
                    );
                    let expected_with_known =
                        substitute_bound_generic_type(expected, &type_param_set, &generic_bindings);
                    let literal_adjusted = literal_bidi_adjusted_numeric_arg_type(
                        Some(value),
                        &value_ty,
                        &expected_with_known,
                    );
                    let compatible = is_compatible(&value_ty, &expected_with_known)
                        || literal_adjusted.is_some();
                    if !bound && !compatible {
                        diagnostics.push(Diagnostic::new(
                            mismatch_message(
                                format!(
                                    "Struct field `{name}` expected `{}`, got `{}`",
                                    type_to_string(&expected_with_known),
                                    type_to_string(&value_ty)
                                ),
                                &value_ty,
                                &expected_with_known,
                            ),
                            default_diag_span(),
                        ));
                    } else {
                        typed_value = maybe_insert_implicit_integral_cast(
                            typed_value,
                            &value_ty,
                            &expected_with_known,
                        );
                    }
                } else if expected_fields.is_some() {
                    diagnostics.push(Diagnostic::new(
                        if is_enum_variant_literal {
                            format!("Unknown enum variant field `{name}` in literal")
                        } else {
                            format!("Unknown struct field `{name}` in literal")
                        },
                        default_diag_span(),
                    ));
                }
                typed_fields.push(TypedStructLiteralField {
                    name: name.clone(),
                    value: typed_value,
                });
            }

            if let Some(struct_fields) = expected_fields.as_ref() {
                for field_name in struct_fields.keys() {
                    if !seen.contains(field_name) {
                        diagnostics.push(Diagnostic::new(
                            if is_enum_variant_literal {
                                format!("Missing enum variant field `{field_name}`")
                            } else {
                                format!("Missing struct literal field `{field_name}`")
                            },
                            default_diag_span(),
                        ));
                    }
                }
            }

            if !type_param_names.is_empty() {
                resolved_ty = SemType::Path {
                    path: resolved_type_path,
                    args: type_param_names
                        .iter()
                        .map(|name| {
                            generic_bindings
                                .get(name)
                                .cloned()
                                .unwrap_or(SemType::Unknown)
                        })
                        .collect(),
                };
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
                let (typed_base, base_ty) =
                    infer_expr(base, context, locals, return_ty, diagnostics);
                let resolved = resolve_method_call_type(
                    &base_ty,
                    field,
                    args,
                    &arg_types,
                    context,
                    diagnostics,
                );
                if let Some(expected_args) =
                    expected_method_arg_types_for_coercion(&base_ty, field, &arg_types, context)
                {
                    apply_expected_call_arg_coercions(&mut typed_args, &arg_types, &expected_args);
                }
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

            let explicit_type_args = match callee.as_ref() {
                Expr::PathWithTypeArgs { type_args, .. } => Some(
                    type_args
                        .iter()
                        .map(|ty| type_from_ast_in_context(ty, context))
                        .collect::<Vec<_>>(),
                ),
                _ => None,
            };
            let (typed_callee, callee_ty) =
                infer_expr(callee, context, locals, return_ty, diagnostics);
            let resolved = resolve_call_type(
                &typed_callee,
                &callee_ty,
                args,
                &arg_types,
                explicit_type_args.as_deref(),
                context,
                locals,
                diagnostics,
            );
            if let Some(expected_args) = expected_call_arg_types_for_coercion(
                callee,
                &callee_ty,
                &arg_types,
                explicit_type_args.as_deref(),
                context,
            ) {
                apply_expected_call_arg_coercions(&mut typed_args, &arg_types, &expected_args);
            }
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
            if path.len() == 1 && path[0] == "__quiche_as" && args.len() == 2 {
                let (typed_expr, _expr_ty) =
                    infer_expr(&args[0], context, locals, return_ty, diagnostics);
                let target_ty = quiche_macro_target_type(&args[1], context);
                return (
                    TypedExpr {
                        kind: TypedExprKind::Cast {
                            expr: Box::new(typed_expr),
                            target_type: rust_owned_type_string(&target_ty),
                        },
                        ty: type_to_string(&target_ty),
                    },
                    target_ty,
                );
            }
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
            let mut index_metadata = TypedIndexMetadata {
                mode: TypedIndexMode::Unknown,
                key_ty: "_".to_string(),
                value_ty: "_".to_string(),
                key_passing: TypedIndexKeyPassing::Owned,
                source: TypedIndexSource::Unknown,
            };

            let resolved = match resolve_index_capability(&base_ty, Some(context), diagnostics) {
                Some(IndexCapability {
                    mode: CapabilityIndexMode::DirectIndex,
                    key_ty,
                    value_ty,
                    source,
                }) => {
                    index_metadata = TypedIndexMetadata {
                        mode: TypedIndexMode::DirectIndex,
                        key_ty: type_to_string(&key_ty),
                        value_ty: type_to_string(&value_ty),
                        key_passing: TypedIndexKeyPassing::Owned,
                        source: match source {
                            CapabilityIndexSource::Builtin => TypedIndexSource::Builtin,
                            CapabilityIndexSource::CustomMethod => TypedIndexSource::CustomMethod,
                        },
                    };
                    let vec_like = matches!(
                        &base_ty,
                        SemType::Path { path, .. } if path.last().is_some_and(|segment| segment == "Vec")
                    );
                    if matches!(typed_index.kind, TypedExprKind::Range { .. }) {
                        if let SemType::Path { path, .. } = &base_ty
                            && path.last().is_some_and(|segment| segment == "Vec")
                        {
                            SemType::Path {
                                path: vec!["Vec".to_string()],
                                args: vec![value_ty],
                            }
                        } else {
                            diagnostics.push(Diagnostic::new(
                                "Range indexing is only supported for Vec-like index capabilities",
                                default_diag_span(),
                            ));
                            SemType::Unknown
                        }
                    } else {
                        let direct_key_ok = if vec_like {
                            is_vector_index_type(&index_ty)
                                && index_key_type_compatible(&index_ty, &key_ty)
                        } else {
                            index_key_type_compatible(&index_ty, &key_ty)
                        };
                        if direct_key_ok {
                            value_ty
                        } else if vec_like {
                            diagnostics.push(Diagnostic::new(
                                format!(
                                    "Vector indexing expects integer index (or range), got `{}`",
                                    type_to_string(&index_ty)
                                ),
                                default_diag_span(),
                            ));
                            SemType::Unknown
                        } else if index_ty == SemType::Unknown {
                            SemType::Unknown
                        } else {
                            diagnostics.push(Diagnostic::new(
                                mismatch_message(
                                    format!(
                                        "Type supports indexing, but key type mismatch: expected `{}`, got `{}`",
                                        type_to_string(&key_ty),
                                        type_to_string(&index_ty)
                                    ),
                                    &index_ty,
                                    &key_ty,
                                ),
                                default_diag_span(),
                            ));
                            emit_constraint_partition_diagnostic(
                                "index key type",
                                &[index_ty.clone(), key_ty.clone()],
                                "an explicit index key type annotation",
                                diagnostics,
                            );
                            SemType::Unknown
                        }
                    }
                }
                Some(IndexCapability {
                    mode: CapabilityIndexMode::GetLikeOption,
                    key_ty,
                    value_ty,
                    source,
                }) => {
                    index_metadata = TypedIndexMetadata {
                        mode: TypedIndexMode::GetLikeOption,
                        key_ty: type_to_string(&key_ty),
                        value_ty: type_to_string(&value_ty),
                        key_passing: match source {
                            CapabilityIndexSource::Builtin => TypedIndexKeyPassing::Borrowed,
                            CapabilityIndexSource::CustomMethod => TypedIndexKeyPassing::Owned,
                        },
                        source: match source {
                            CapabilityIndexSource::Builtin => TypedIndexSource::Builtin,
                            CapabilityIndexSource::CustomMethod => TypedIndexSource::CustomMethod,
                        },
                    };
                    if matches!(typed_index.kind, TypedExprKind::Range { .. }) {
                        diagnostics.push(Diagnostic::new(
                            "Map-like subscript does not support range keys",
                            default_diag_span(),
                        ));
                        SemType::Unknown
                    } else if index_ty == SemType::Unknown {
                        option_type(value_ty)
                    } else if index_key_type_compatible(&index_ty, &key_ty)
                        || literal_bidi_adjusted_numeric_arg_type(Some(index), &index_ty, &key_ty)
                            .is_some()
                    {
                        option_type(value_ty)
                    } else {
                        diagnostics.push(Diagnostic::new(
                            mismatch_message(
                                format!(
                                    "Type supports indexing, but key type mismatch: expected `{}`, got `{}`",
                                    type_to_string(&key_ty),
                                    type_to_string(&index_ty)
                                ),
                                &index_ty,
                                &key_ty,
                            ),
                            default_diag_span(),
                        ));
                        emit_constraint_partition_diagnostic(
                            "index key type",
                            &[index_ty.clone(), key_ty.clone()],
                            "an explicit index key type annotation",
                            diagnostics,
                        );
                        SemType::Unknown
                    }
                }
                None if base_ty == SemType::Unknown => SemType::Unknown,
                None => {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Capability resolution failed for indexing on `{}`",
                            type_to_string(&base_ty)
                        ),
                        default_diag_span(),
                    ));
                    SemType::Unknown
                }
            };

            let kind = TypedExprKind::Index {
                base: Box::new(typed_base),
                index: Box::new(typed_index),
                indexing: index_metadata,
            };

            (
                TypedExpr {
                    kind,
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
                            default_diag_span(),
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
                    default_diag_span(),
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
                            default_diag_span(),
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
            let (mut typed_left, mut left_ty) =
                infer_expr(left, context, locals, return_ty, diagnostics);
            let (mut typed_right, mut right_ty) =
                infer_expr(right, context, locals, return_ty, diagnostics);
            if infer_local_bidi_enabled() {
                apply_binary_operand_hints(
                    op,
                    left,
                    &mut typed_left,
                    &mut left_ty,
                    right,
                    &mut typed_right,
                    &mut right_ty,
                    return_ty,
                    locals,
                );
            }
            if strict_mode_enabled()
                && matches!(
                    op,
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem
                )
            {
                if left_ty == SemType::Unknown {
                    emit_strict_unknown_operand_hint_once(left, diagnostics);
                }
                if right_ty == SemType::Unknown {
                    emit_strict_unknown_operand_hint_once(right, diagnostics);
                }
            }
            let (typed_op, out_ty) = match op {
                BinaryOp::Add => {
                    let out_ty = resolve_add_type_with_literals(
                        left,
                        &left_ty,
                        right,
                        &right_ty,
                        diagnostics,
                    );
                    (TypedBinaryOp::Add, out_ty)
                }
                BinaryOp::Sub => {
                    let out_ty = resolve_numeric_binary_type_with_literals(
                        "-",
                        left,
                        &left_ty,
                        right,
                        &right_ty,
                        diagnostics,
                    );
                    (TypedBinaryOp::Sub, out_ty)
                }
                BinaryOp::Mul => {
                    let out_ty = resolve_numeric_binary_type_with_literals(
                        "*",
                        left,
                        &left_ty,
                        right,
                        &right_ty,
                        diagnostics,
                    );
                    (TypedBinaryOp::Mul, out_ty)
                }
                BinaryOp::Div => {
                    let out_ty = resolve_numeric_binary_type_with_literals(
                        "/",
                        left,
                        &left_ty,
                        right,
                        &right_ty,
                        diagnostics,
                    );
                    (TypedBinaryOp::Div, out_ty)
                }
                BinaryOp::Rem => {
                    let out_ty = resolve_numeric_binary_type_with_literals(
                        "%",
                        left,
                        &left_ty,
                        right,
                        &right_ty,
                        diagnostics,
                    );
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
                            default_diag_span(),
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
                    if !is_compatible(&left_ty, &right_ty)
                        && !literal_bidi_comparison_compatible(left, &left_ty, right, &right_ty)
                        && !(strict_mode_enabled()
                            && (left_ty == SemType::Unknown || right_ty == SemType::Unknown))
                    {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "comparison operands must be compatible, got `{}` and `{}`",
                                type_to_string(&left_ty),
                                type_to_string(&right_ty)
                            ),
                            default_diag_span(),
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
                if infer_local_bidi_enabled()
                    && let Some(merged) = merge_compatible_types(&elem_ty, ty)
                {
                    elem_ty = merged;
                    continue;
                }
                if !is_compatible(ty, &elem_ty) || !is_compatible(&elem_ty, ty) {
                    diagnostics.push(Diagnostic::new(
                        mismatch_message(
                            format!(
                                "Array literal element type mismatch: expected `{}`, got `{}`",
                                type_to_string(&elem_ty),
                                type_to_string(ty)
                            ),
                            ty,
                            &elem_ty,
                        ),
                        default_diag_span(),
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
                .map(|p| type_from_ast_in_context(&p.ty, context))
                .collect::<Vec<_>>();
            for (param, ty) in params.iter().zip(param_types.iter()) {
                closure_locals.insert(param.name.clone(), ty.clone());
            }

            let declared_return_ty = return_type
                .as_ref()
                .map(|ty| type_from_ast_in_context(ty, context));
            let provisional_return_ty = declared_return_ty.clone().unwrap_or(SemType::Unknown);
            let mut typed_body = Vec::new();
            let mut inferred_returns = Vec::new();
            for (index, statement) in body.statements.iter().enumerate() {
                let is_last = index + 1 == body.statements.len();
                if is_last {
                    if let Stmt::TailExpr(expr) | Stmt::Expr(expr) = statement {
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
                                mismatch_message(
                                    format!(
                                        "Closure return type mismatch: expected `{}`, got `{}`",
                                        type_to_string(&provisional_return_ty),
                                        type_to_string(&inferred)
                                    ),
                                    &inferred,
                                    &provisional_return_ty,
                                ),
                                default_diag_span(),
                            ));
                        }
                        typed_body.push(TypedStmt::Return(Some(typed_expr)));
                        continue;
                    }
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
                    default_diag_span(),
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
        Expr::Block(block) => {
            let mut block_locals = locals.clone();
            let mut block_immutable_locals = HashSet::new();
            let mut typed_body = Vec::new();
            let mut tail_expr = None;
            let mut tail_ty = SemType::Unit;
            let mut inferred_returns = Vec::new();

            for (index, statement) in block.statements.iter().enumerate() {
                let is_last = index + 1 == block.statements.len();
                if is_last && let Stmt::TailExpr(expr) = statement {
                    let (typed_expr, inferred) = infer_expr(
                        expr,
                        context,
                        &mut block_locals,
                        &SemType::Unknown,
                        diagnostics,
                    );
                    tail_ty = inferred;
                    tail_expr = Some(Box::new(typed_expr));
                    continue;
                }
                if let Some(stmt) = lower_stmt_with_types(
                    statement,
                    context,
                    &mut block_locals,
                    &mut block_immutable_locals,
                    &SemType::Unknown,
                    &mut inferred_returns,
                    diagnostics,
                ) {
                    typed_body.push(stmt);
                }
            }

            (
                TypedExpr {
                    kind: TypedExprKind::Block {
                        body: typed_body,
                        tail: tail_expr,
                    },
                    ty: type_to_string(&tail_ty),
                },
                tail_ty,
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
        Expr::Cast { expr, target_type } => {
            let (typed_expr, _expr_ty) = infer_expr(expr, context, locals, return_ty, diagnostics);
            let target_sem = type_from_ast_in_context(target_type, context);
            (
                TypedExpr {
                    kind: TypedExprKind::Cast {
                        expr: Box::new(typed_expr),
                        target_type: rust_owned_type_string(&target_sem),
                    },
                    ty: type_to_string(&target_sem),
                },
                target_sem,
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

fn diagnose_const_mutations_in_expr(
    expr: &TypedExpr,
    immutable_locals: &HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match &expr.kind {
        TypedExprKind::Call { callee, args } => {
            if let TypedExprKind::Field { base, field } = &callee.kind
                && typed_method_mutates_receiver(field)
                && let Some(root) = typed_root_path_name(base)
                && immutable_locals.contains(root)
            {
                diagnostics.push(Diagnostic::new(
                    format!("Cannot mutate immutable const `{root}` via method `{field}`"),
                    default_diag_span(),
                ));
            }
            diagnose_const_mutations_in_expr(callee, immutable_locals, diagnostics);
            for arg in args {
                diagnose_const_mutations_in_expr(arg, immutable_locals, diagnostics);
            }
        }
        TypedExprKind::MacroCall { args, .. } => {
            for arg in args {
                diagnose_const_mutations_in_expr(arg, immutable_locals, diagnostics);
            }
        }
        TypedExprKind::Field { base, .. } => {
            diagnose_const_mutations_in_expr(base, immutable_locals, diagnostics);
        }
        TypedExprKind::Index { base, index, .. } => {
            diagnose_const_mutations_in_expr(base, immutable_locals, diagnostics);
            diagnose_const_mutations_in_expr(index, immutable_locals, diagnostics);
        }
        TypedExprKind::Match { scrutinee, arms } => {
            diagnose_const_mutations_in_expr(scrutinee, immutable_locals, diagnostics);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    diagnose_const_mutations_in_expr(guard, immutable_locals, diagnostics);
                }
                diagnose_const_mutations_in_expr(&arm.value, immutable_locals, diagnostics);
            }
        }
        TypedExprKind::Unary { expr, .. } => {
            diagnose_const_mutations_in_expr(expr, immutable_locals, diagnostics);
        }
        TypedExprKind::Cast { expr, .. } => {
            diagnose_const_mutations_in_expr(expr, immutable_locals, diagnostics);
        }
        TypedExprKind::Binary { left, right, .. } => {
            diagnose_const_mutations_in_expr(left, immutable_locals, diagnostics);
            diagnose_const_mutations_in_expr(right, immutable_locals, diagnostics);
        }
        TypedExprKind::Array(items) | TypedExprKind::Tuple(items) => {
            for item in items {
                diagnose_const_mutations_in_expr(item, immutable_locals, diagnostics);
            }
        }
        TypedExprKind::StructLiteral { fields, .. } => {
            for field in fields {
                diagnose_const_mutations_in_expr(&field.value, immutable_locals, diagnostics);
            }
        }
        TypedExprKind::Block { body, tail } => {
            for stmt in body {
                diagnose_const_mutations_in_stmt(stmt, immutable_locals, diagnostics);
            }
            if let Some(tail) = tail {
                diagnose_const_mutations_in_expr(tail, immutable_locals, diagnostics);
            }
        }
        TypedExprKind::Closure { body, .. } => {
            for stmt in body {
                diagnose_const_mutations_in_stmt(stmt, immutable_locals, diagnostics);
            }
        }
        TypedExprKind::Range { start, end, .. } => {
            if let Some(start) = start {
                diagnose_const_mutations_in_expr(start, immutable_locals, diagnostics);
            }
            if let Some(end) = end {
                diagnose_const_mutations_in_expr(end, immutable_locals, diagnostics);
            }
        }
        TypedExprKind::Try(inner) => {
            diagnose_const_mutations_in_expr(inner, immutable_locals, diagnostics);
        }
        TypedExprKind::Int(_)
        | TypedExprKind::Float(_)
        | TypedExprKind::Bool(_)
        | TypedExprKind::Char(_)
        | TypedExprKind::String(_)
        | TypedExprKind::Path(_) => {}
    }
}

fn diagnose_const_mutations_in_stmt(
    stmt: &TypedStmt,
    immutable_locals: &HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match stmt {
        TypedStmt::Const(def) => {
            diagnose_const_mutations_in_expr(&def.value, immutable_locals, diagnostics)
        }
        TypedStmt::DestructureConst { value, .. } => {
            diagnose_const_mutations_in_expr(value, immutable_locals, diagnostics)
        }
        TypedStmt::Assign { target, value, .. } => {
            diagnose_const_mutations_in_assign_target(target, immutable_locals, diagnostics);
            diagnose_const_mutations_in_expr(value, immutable_locals, diagnostics);
        }
        TypedStmt::Return(Some(expr)) | TypedStmt::Expr(expr) => {
            diagnose_const_mutations_in_expr(expr, immutable_locals, diagnostics)
        }
        TypedStmt::If {
            condition,
            then_body,
            else_body,
        } => {
            diagnose_const_mutations_in_expr(condition, immutable_locals, diagnostics);
            for stmt in then_body {
                diagnose_const_mutations_in_stmt(stmt, immutable_locals, diagnostics);
            }
            if let Some(else_body) = else_body {
                for stmt in else_body {
                    diagnose_const_mutations_in_stmt(stmt, immutable_locals, diagnostics);
                }
            }
        }
        TypedStmt::While { condition, body } => {
            diagnose_const_mutations_in_expr(condition, immutable_locals, diagnostics);
            for stmt in body {
                diagnose_const_mutations_in_stmt(stmt, immutable_locals, diagnostics);
            }
        }
        TypedStmt::For { iter, body, .. } => {
            diagnose_const_mutations_in_expr(iter, immutable_locals, diagnostics);
            for stmt in body {
                diagnose_const_mutations_in_stmt(stmt, immutable_locals, diagnostics);
            }
        }
        TypedStmt::Loop { body } => {
            for stmt in body {
                diagnose_const_mutations_in_stmt(stmt, immutable_locals, diagnostics);
            }
        }
        TypedStmt::Return(None)
        | TypedStmt::Break
        | TypedStmt::Continue
        | TypedStmt::RustBlock(_) => {}
    }
}

fn diagnose_const_mutations_in_assign_target(
    target: &TypedAssignTarget,
    immutable_locals: &HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match target {
        TypedAssignTarget::Path(name) => {
            if immutable_locals.contains(name) {
                diagnostics.push(Diagnostic::new(
                    format!("Cannot assign to immutable const `{name}`"),
                    default_diag_span(),
                ));
            }
        }
        TypedAssignTarget::Field { base, .. } | TypedAssignTarget::Index { base, .. } => {
            if let Some(root) = typed_root_path_name(base)
                && immutable_locals.contains(root)
            {
                diagnostics.push(Diagnostic::new(
                    format!("Cannot assign through immutable const `{root}`"),
                    default_diag_span(),
                ));
            }
        }
        TypedAssignTarget::Tuple(items) => {
            for item in items {
                diagnose_const_mutations_in_assign_target(item, immutable_locals, diagnostics);
            }
        }
    }
}

fn typed_method_mutates_receiver(field: &str) -> bool {
    matches!(field, "push" | "push_str")
}

fn typed_root_path_name(expr: &TypedExpr) -> Option<&str> {
    match &expr.kind {
        TypedExprKind::Path(path) if path.len() == 1 => Some(path[0].as_str()),
        TypedExprKind::Field { base, .. } | TypedExprKind::Index { base, .. } => {
            typed_root_path_name(base)
        }
        _ => None,
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
                default_diag_span(),
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
                default_diag_span(),
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
                default_diag_span(),
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
        || expected_variants
            .iter()
            .any(|variant| !covered.contains(variant))
    {
        let missing = expected_variants
            .iter()
            .filter(|variant| !covered.contains(*variant))
            .cloned()
            .collect::<Vec<_>>()
            .join(", ");
        diagnostics.push(Diagnostic::new(
            format!("Non-exhaustive match on `{enum_name}`; missing variants: {missing}"),
            default_diag_span(),
        ));
    }
}

fn bool_tuple_arity(ty: &SemType) -> Option<usize> {
    let SemType::Tuple(items) = ty else {
        return None;
    };
    if items.is_empty()
        || items
            .iter()
            .any(|item| !is_concrete_named_type(item, "bool"))
    {
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

fn finite_enum_variants_for_tuple_domain(
    enum_name: &str,
    context: &Context,
) -> Option<Vec<String>> {
    if enum_name == "Option" {
        return Some(vec!["Some".to_string(), "None".to_string()]);
    }
    if enum_name == "Result" {
        return Some(vec!["Ok".to_string(), "Err".to_string()]);
    }
    context.enums.get(enum_name).and_then(|variants| {
        let payload_free = variants
            .iter()
            .filter_map(|(name, info)| info.payload_types.is_empty().then_some(name.clone()))
            .collect::<Vec<_>>();
        if payload_free.is_empty() {
            None
        } else {
            Some(payload_free)
        }
    })
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
    arms.iter().any(|arm| {
        guard_counts_for_exhaustiveness(arm.guard.as_ref()) && pattern_is_total(&arm.pattern)
    })
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
        TypedPattern::BindingAt { pattern, .. } => {
            collect_bool_coverage(pattern, has_true, has_false)
        }
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
        TypedPattern::BindingAt { pattern, .. } => {
            collect_enum_coverage(pattern, enum_name, covered)
        }
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
    if *base_ty == SemType::Unknown {
        // Keep structural-generic field accesses deferrable until call-site
        // specialization resolves concrete receiver types.
        return SemType::Unknown;
    }

    if let SemType::Path { path, .. } = base_ty {
        if let Some(struct_name) = path.last() {
            if let Some(fields) = context.structs.get(struct_name) {
                if let Some(field_ty) = fields.get(field) {
                    return field_ty.clone();
                }
                diagnostics.push(Diagnostic::new(
                    format!("Struct `{}` has no field `{field}`", struct_name),
                    default_diag_span(),
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
        default_diag_span(),
    ));
    SemType::Unknown
}

fn resolve_call_type(
    callee: &TypedExpr,
    callee_ty: &SemType,
    arg_exprs: &[Expr],
    args: &[SemType],
    explicit_type_args: Option<&[SemType]>,
    context: &Context,
    locals: &HashMap<String, SemType>,
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
                default_diag_span(),
            ));
            return *ret.clone();
        }
        for (index, (actual, expected)) in args.iter().zip(params.iter()).enumerate() {
            let adjusted_actual =
                literal_bidi_adjusted_numeric_arg_type(arg_exprs.get(index), actual, expected)
                    .unwrap_or_else(|| actual.clone());
            if !is_compatible(&adjusted_actual, expected) {
                diagnostics.push(Diagnostic::new(
                    mismatch_message(
                        format!(
                            "Arg {} mismatch: expected `{}`, got `{}`",
                            index + 1,
                            type_to_string(expected),
                            type_to_string(actual)
                        ),
                        actual,
                        expected,
                    ),
                    default_diag_span(),
                ));
            }
        }
        return *ret.clone();
    }

    if let TypedExprKind::Path(path) = &callee.kind {
        if let Some(ty) = resolve_builtin_assert_call(path, args, diagnostics) {
            return ty;
        }

        if let Some((resolved_name, sig)) = resolve_function_sig_for_path(path, context) {
            return resolve_named_function_call(
                sig,
                &resolved_name,
                Some(arg_exprs),
                args,
                explicit_type_args,
                context,
                diagnostics,
            );
        }

        if let Some(resolved) =
            resolve_constructor_call(path, args, explicit_type_args, context, diagnostics)
        {
            return resolved;
        }

        if let Some(resolved) = resolve_trait_associated_call(path, args, diagnostics) {
            return resolved;
        }

        if path.len() == 1 {
            let name = &path[0];
            if !locals.contains_key(name) && !context.rust_block_functions.contains(name) {
                diagnostics.push(Diagnostic::new(
                    format!("Unknown function `{name}`"),
                    default_diag_span(),
                ));
            }
        }
        return SemType::Unknown;
    }

    diagnostics.push(Diagnostic::new(
        "Unsupported call target",
        default_diag_span(),
    ));
    SemType::Unknown
}

fn expected_call_arg_types_for_coercion(
    callee_expr: &Expr,
    callee_ty: &SemType,
    args: &[SemType],
    explicit_type_args: Option<&[SemType]>,
    context: &Context,
) -> Option<Vec<SemType>> {
    if let SemType::Fn { params, .. } = callee_ty
        && params.len() == args.len()
    {
        return Some(params.clone());
    }

    let path = match callee_expr {
        Expr::Path(path) => Some(path.as_slice()),
        Expr::PathWithTypeArgs { path, .. } => Some(path.as_slice()),
        _ => None,
    }?;
    let (_, sig) = resolve_function_sig_for_path(path, context)?;
    if sig.params.len() != args.len() {
        return None;
    }
    Some(instantiate_expected_param_types_for_call(
        sig,
        args,
        explicit_type_args,
    ))
}

fn expected_method_arg_types_for_coercion(
    base_ty: &SemType,
    method: &str,
    args: &[SemType],
    context: &Context,
) -> Option<Vec<SemType>> {
    let mut capability_diags = Vec::new();
    if let Some(capability) =
        resolve_method_capability(base_ty, method, args.len(), context, &mut capability_diags)
    {
        return Some(capability.expected_args);
    }
    if let SemType::Path { path, .. } = base_ty {
        let type_name = path
            .last()
            .map(|segment| segment.as_str())
            .unwrap_or_default();
        let lookup = format!("{type_name}::{method}");
        if let Some(sig) = context.functions.get(&lookup) {
            if sig.params.len() == args.len() + 1 {
                let mut call_args = Vec::with_capacity(args.len() + 1);
                call_args.push(base_ty.clone());
                call_args.extend(args.iter().cloned());
                return Some(
                    instantiate_expected_param_types_for_call(sig, &call_args, None)
                        .into_iter()
                        .skip(1)
                        .collect(),
                );
            }
            if sig.params.len() == args.len() {
                return Some(instantiate_expected_param_types_for_call(sig, args, None));
            }
        }
    }
    None
}

fn resolve_function_sig_for_path<'a>(
    path: &[String],
    context: &'a Context,
) -> Option<(String, &'a FunctionSig)> {
    let joined = path.join("::");
    if let Some(sig) = context.functions.get(&joined) {
        return Some((joined, sig));
    }

    if path.len() == 1 {
        let name = &path[0];
        if let Some(sig) = context.functions.get(name) {
            return Some((name.clone(), sig));
        }
        return None;
    }

    // Crate transpilation currently stores local function signatures by bare
    // function name. Accept module-qualified call paths by falling back to
    // the final segment.
    let tail = path.last()?;
    let sig = context.functions.get(tail)?;
    Some((tail.clone(), sig))
}

fn resolve_trait_associated_call(
    path: &[String],
    args: &[SemType],
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<SemType> {
    if path.len() < 2 {
        return None;
    }
    let assoc_name = path.last()?.as_str();
    let type_name = path[path.len() - 2].as_str();
    let supports_assoc = if assoc_name == "try_from" && is_numeric_type_name(type_name) {
        true
    } else {
        match rustdex_has_method_strict(type_name, assoc_name, diagnostics) {
            Ok(value) => value,
            Err(()) => return Some(SemType::Unknown),
        }
    };
    if !supports_assoc {
        return None;
    }

    if assoc_name == "from_iter" {
        return Some(resolve_from_iter_associated_type(
            type_name,
            args,
            diagnostics,
        ));
    }
    if assoc_name == "default" {
        if !args.is_empty() {
            diagnostics.push(Diagnostic::new(
                format!(
                    "`{type_name}::default` expects 0 arg(s), got {}",
                    args.len()
                ),
                default_diag_span(),
            ));
        }
        return Some(SemType::Unknown);
    }
    if assoc_name == "from" {
        if args.len() != 1 {
            diagnostics.push(Diagnostic::new(
                format!("`{type_name}::from` expects 1 arg(s), got {}", args.len()),
                default_diag_span(),
            ));
        }
        return Some(resolve_from_associated_type(type_name, args));
    }
    if assoc_name == "try_from" {
        if args.len() != 1 {
            diagnostics.push(Diagnostic::new(
                format!(
                    "`{type_name}::try_from` expects 1 arg(s), got {}",
                    args.len()
                ),
                default_diag_span(),
            ));
        }
        return Some(SemType::Path {
            path: vec!["Result".to_string()],
            args: vec![named_type(type_name), SemType::Unknown],
        });
    }
    Some(SemType::Unknown)
}

fn resolve_from_associated_type(type_name: &str, args: &[SemType]) -> SemType {
    if args.len() != 1 {
        return SemType::Unknown;
    }
    let input = &args[0];
    match type_name {
        "Vec" => {
            if let SemType::Path { path, args } = input
                && path.last().is_some_and(|segment| segment == "Vec")
                && args.len() == 1
            {
                return SemType::Path {
                    path: vec!["Vec".to_string()],
                    args: vec![args[0].clone()],
                };
            }
            SemType::Path {
                path: vec!["Vec".to_string()],
                args: vec![SemType::Unknown],
            }
        }
        "HashSet" | "BTreeSet" => {
            if let SemType::Path { path, args } = input
                && path.last().is_some_and(|segment| segment == "Vec")
                && args.len() == 1
            {
                return SemType::Path {
                    path: vec![type_name.to_string()],
                    args: vec![args[0].clone()],
                };
            }
            SemType::Unknown
        }
        "HashMap" | "BTreeMap" => {
            if let SemType::Path { path, args } = input
                && path.last().is_some_and(|segment| segment == "Vec")
                && let Some(SemType::Tuple(items)) = args.first()
                && items.len() == 2
            {
                return SemType::Path {
                    path: vec![type_name.to_string()],
                    args: vec![items[0].clone(), items[1].clone()],
                };
            }
            SemType::Unknown
        }
        "String" => named_type("String"),
        _ => named_type(type_name),
    }
}

fn resolve_from_iter_associated_type(
    type_name: &str,
    args: &[SemType],
    diagnostics: &mut Vec<Diagnostic>,
) -> SemType {
    if args.len() != 1 {
        diagnostics.push(Diagnostic::new(
            format!(
                "`{type_name}::from_iter` expects 1 arg(s), got {}",
                args.len()
            ),
            default_diag_span(),
        ));
        return SemType::Unknown;
    }

    let Some(item_ty) = into_iterator_item_type(&args[0]) else {
        return SemType::Unknown;
    };

    match type_name {
        "Vec" => SemType::Path {
            path: vec!["Vec".to_string()],
            args: vec![item_ty],
        },
        "HashSet" => SemType::Path {
            path: vec!["HashSet".to_string()],
            args: vec![item_ty],
        },
        "BTreeSet" => SemType::Path {
            path: vec!["BTreeSet".to_string()],
            args: vec![item_ty],
        },
        "HashMap" => {
            if let SemType::Tuple(items) = item_ty
                && items.len() == 2
            {
                return SemType::Path {
                    path: vec!["HashMap".to_string()],
                    args: vec![items[0].clone(), items[1].clone()],
                };
            }
            SemType::Unknown
        }
        "BTreeMap" => {
            if let SemType::Tuple(items) = item_ty
                && items.len() == 2
            {
                return SemType::Path {
                    path: vec!["BTreeMap".to_string()],
                    args: vec![items[0].clone(), items[1].clone()],
                };
            }
            SemType::Unknown
        }
        "String" => named_type("String"),
        _ => SemType::Unknown,
    }
}

fn into_iterator_item_type(ty: &SemType) -> Option<SemType> {
    match ty {
        SemType::Iter(item) => Some(item.as_ref().clone()),
        SemType::Path { path, args } => match path.last().map(|s| s.as_str()) {
            Some("Vec") | Some("Option") if args.len() == 1 => Some(args[0].clone()),
            Some("Result") if args.len() >= 1 => Some(args[0].clone()),
            Some("HashSet") | Some("BTreeSet") if args.len() == 1 => Some(args[0].clone()),
            Some("HashMap") | Some("BTreeMap") if args.len() == 2 => {
                Some(SemType::Tuple(vec![args[0].clone(), args[1].clone()]))
            }
            _ => None,
        },
        _ => None,
    }
}

fn resolve_named_function_call(
    sig: &FunctionSig,
    name: &str,
    arg_exprs: Option<&[Expr]>,
    args: &[SemType],
    explicit_type_args: Option<&[SemType]>,
    context: &Context,
    diagnostics: &mut Vec<Diagnostic>,
) -> SemType {
    if sig.params.len() != args.len() {
        diagnostics.push(Diagnostic::new(
            format!(
                "Function `{name}` expects {} args, got {}",
                sig.params.len(),
                args.len()
            ),
            default_diag_span(),
        ));
        return sig.return_type.clone();
    }

    let type_param_set = sig.type_params.iter().cloned().collect::<HashSet<_>>();
    let mut generic_bindings = HashMap::new();
    if let Some(explicit) = explicit_type_args {
        if explicit.len() != sig.type_params.len() {
            diagnostics.push(Diagnostic::new(
                format!(
                    "Function `{name}` expects {} explicit type argument(s), got {}",
                    sig.type_params.len(),
                    explicit.len()
                ),
                default_diag_span(),
            ));
        }
        for (index, ty) in explicit.iter().enumerate() {
            if let Some(type_param) = sig.type_params.get(index) {
                generic_bindings.insert(type_param.clone(), ty.clone());
            }
        }
    }

    for (index, (actual, expected)) in args.iter().zip(&sig.params).enumerate() {
        let actual_expr = arg_exprs.and_then(|exprs| exprs.get(index));
        let expected_with_known =
            substitute_bound_generic_type(expected, &type_param_set, &generic_bindings);
        let adjusted_actual =
            literal_bidi_adjusted_numeric_arg_type(actual_expr, actual, &expected_with_known)
                .unwrap_or_else(|| actual.clone());
        if !bind_generic_params(
            &expected_with_known,
            &adjusted_actual,
            &type_param_set,
            &mut generic_bindings,
        ) && !is_compatible(&adjusted_actual, &expected_with_known)
        {
            diagnostics.push(Diagnostic::new(
                mismatch_message(
                    format!(
                        "Arg {} for `{name}`: expected `{}`, got `{}`",
                        index + 1,
                        type_to_string(&expected_with_known),
                        type_to_string(actual)
                    ),
                    actual,
                    &expected_with_known,
                ),
                default_diag_span(),
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
            if !type_satisfies_bound_with_context(actual_ty, bound, context) {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Type `{}` does not satisfy bound `{}` for `{}` in function `{}`",
                        type_to_string(actual_ty),
                        type_to_string(bound),
                        type_param,
                        name
                    ),
                    default_diag_span(),
                ));
            }
        }
    }

    validate_structural_call_requirements(sig, name, args, &generic_bindings, context, diagnostics);
    register_structural_specialization(sig, name, args, &generic_bindings, context);

    substitute_generic_type(&sig.return_type, &type_param_set, &generic_bindings)
}

fn validate_structural_call_requirements(
    sig: &FunctionSig,
    fn_name: &str,
    args: &[SemType],
    generic_bindings: &HashMap<String, SemType>,
    context: &Context,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for (type_param, requirements) in &sig.structural_requirements {
        if requirements.is_empty() {
            continue;
        }
        let Some(actual_ty) = generic_bindings.get(type_param) else {
            diagnostics.push(Diagnostic::new(
                format!(
                    "Unable to resolve structural type parameter `{type_param}` for `{fn_name}` call"
                ),
                default_diag_span(),
            ));
            continue;
        };
        if contains_unknown(actual_ty) {
            diagnostics.push(Diagnostic::new(
                format!(
                    "Structural type parameter `{type_param}` for `{fn_name}` remains unknown at call site"
                ),
                default_diag_span(),
            ));
            continue;
        }
        for field in &requirements.fields {
            if !structural_type_supports_field(actual_ty, field, context) {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "`{}` passed to `{fn_name}` does not provide required field `{field}` for `{type_param}`",
                        type_to_string(actual_ty)
                    ),
                    default_diag_span(),
                ));
            }
        }
        for (method, arities) in &requirements.methods {
            for arity in arities {
                if !structural_type_supports_method(
                    actual_ty,
                    method,
                    *arity,
                    context,
                    &sig.type_param_bounds,
                ) {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "`{}` passed to `{fn_name}` does not provide required method `{method}` with {} argument(s) for `{type_param}`",
                            type_to_string(actual_ty),
                            arity
                        ),
                        default_diag_span(),
                    ));
                }
            }
        }
    }

    if !sig.structural_requirements.is_empty() && args.iter().any(contains_unknown) {
        diagnostics.push(Diagnostic::new(
            format!(
                "Call to structural function `{fn_name}` must use fully resolved argument types"
            ),
            default_diag_span(),
        ));
    }
}

fn structural_type_supports_field(ty: &SemType, field: &str, context: &Context) -> bool {
    let SemType::Path { path, args: _ } = ty else {
        return false;
    };
    // Generic args don't affect field availability — Point<i32> still has Point's fields
    let Some(type_name) = path.last() else {
        return false;
    };
    context
        .structs
        .get(type_name)
        .map(|fields| fields.contains_key(field))
        .unwrap_or(false)
}

fn bounds_satisfy_trait_requirement(
    bounds: &[SemType],
    required_trait: &SemType,
    context: &Context,
) -> bool {
    let Some(required_name) = bound_trait_name(required_trait) else {
        return false;
    };
    bounds.iter().any(|bound| {
        bound_trait_name(bound)
            .map(|actual| {
                actual == required_name || trait_satisfies_trait(actual, required_name, context)
            })
            .unwrap_or(false)
    })
}

fn structural_type_supports_method(
    ty: &SemType,
    method: &str,
    arity: usize,
    context: &Context,
    type_param_bounds: &HashMap<String, Vec<SemType>>,
) -> bool {
    let SemType::Path { path, args: _ } = ty else {
        return false;
    };
    let Some(type_name) = path.last() else {
        return false;
    };
    if path.len() == 1
        && let Some(bounds) = type_param_bounds.get(type_name)
        && let Some(required_trait) = structural_method_required_trait(method, arity)
        && bounds_satisfy_trait_requirement(bounds, &required_trait, context)
    {
        return true;
    }
    let mut capability_diagnostics = Vec::new();
    if resolve_method_capability(ty, method, arity, context, &mut capability_diagnostics).is_some()
    {
        return true;
    }
    if method == "to_string" && arity == 0 && builtin_value_supports_to_string(type_name) {
        return true;
    }
    // Generic args don't affect method availability — Point<i32> still has Point::method
    context
        .functions
        .get(&format!("{type_name}::{method}"))
        .map(|sig| sig.params.len() == arity + 1)
        .unwrap_or(false)
}

fn builtin_value_supports_to_string(type_name: &str) -> bool {
    type_name == "String" || type_name == "str" || is_copy_primitive_type(type_name)
}

fn register_structural_specialization(
    sig: &FunctionSig,
    fn_name: &str,
    args: &[SemType],
    generic_bindings: &HashMap<String, SemType>,
    context: &Context,
) {
    let has_requirements = sig
        .structural_requirements
        .values()
        .any(|requirements| !requirements.is_empty());
    if !has_requirements {
        return;
    }

    let arg_key = args.iter().map(type_to_string).collect::<Vec<_>>();
    let key = (fn_name.to_string(), arg_key.clone());
    {
        let lookup = context.structural_specialization_lookup.borrow();
        if lookup.contains_key(&key) {
            return;
        }
    }

    let mut hasher = DefaultHasher::new();
    fn_name.hash(&mut hasher);
    for (name, ty) in generic_bindings {
        name.hash(&mut hasher);
        type_to_string(ty).hash(&mut hasher);
    }
    let suffix = hasher.finish();
    let specialized_name = format!("__elevate_row_{fn_name}_{suffix:x}");

    context
        .structural_specialization_lookup
        .borrow_mut()
        .insert(key, specialized_name.clone());

    let mut specializations = context.structural_specializations.borrow_mut();
    let entry = specializations.entry(fn_name.to_string()).or_default();
    if entry
        .iter()
        .any(|existing| existing.specialized_name == specialized_name)
    {
        return;
    }
    entry.push(StructuralSpecialization {
        specialized_name,
        bindings: generic_bindings.clone(),
    });
}

fn collect_structural_requirements_for_function(
    type_params: &[GenericParam],
    params: &[crate::ast::Param],
    body: &Block,
) -> HashMap<String, StructuralRequirements> {
    let structural_params = type_params
        .iter()
        .filter(|param| param.bounds.is_empty())
        .map(|param| param.name.clone())
        .collect::<HashSet<_>>();

    let mut out = structural_params
        .iter()
        .map(|name| (name.clone(), StructuralRequirements::default()))
        .collect::<HashMap<_, _>>();
    let param_bindings = params
        .iter()
        .filter_map(|param| {
            path_type_param_name(&param.ty, &structural_params)
                .map(|type_param| (param.name.clone(), type_param.to_string()))
        })
        .collect::<HashMap<_, _>>();
    let self_field_bindings = HashMap::<String, String>::new();
    for stmt in &body.statements {
        collect_structural_requirements_in_stmt(
            stmt,
            &param_bindings,
            &self_field_bindings,
            &mut out,
        );
    }
    out.retain(|_, req| !req.is_empty());
    out
}

fn collect_structural_requirements_for_impl_method(
    imp: &crate::ast::ImplBlock,
    method: &crate::ast::FunctionDef,
    context: &Context,
) -> HashMap<String, StructuralRequirements> {
    let all_type_params = merged_impl_and_method_type_params(&imp.type_params, &method.type_params);
    let structural_params = all_type_params
        .iter()
        .filter(|param| param.bounds.is_empty())
        .map(|param| param.name.clone())
        .collect::<HashSet<_>>();
    let mut out = structural_params
        .iter()
        .map(|name| (name.clone(), StructuralRequirements::default()))
        .collect::<HashMap<_, _>>();
    let param_bindings = method
        .params
        .iter()
        .filter_map(|param| {
            path_type_param_name(&param.ty, &structural_params)
                .map(|type_param| (param.name.clone(), type_param.to_string()))
        })
        .collect::<HashMap<_, _>>();
    let self_field_bindings =
        collect_self_field_bindings_for_impl(imp, context, &structural_params);
    for stmt in &method.body.statements {
        collect_structural_requirements_in_stmt(
            stmt,
            &param_bindings,
            &self_field_bindings,
            &mut out,
        );
    }
    out.retain(|_, req| !req.is_empty());
    out
}

fn collect_self_field_bindings_for_impl(
    imp: &crate::ast::ImplBlock,
    context: &Context,
    structural_params: &HashSet<String>,
) -> HashMap<String, String> {
    let target_name = impl_lookup_name(&imp.target);
    let Some(struct_fields) = context.structs.get(&target_name) else {
        return HashMap::new();
    };
    let struct_type_params = context
        .struct_type_params
        .get(&target_name)
        .cloned()
        .unwrap_or_default();
    let struct_param_set = struct_type_params.iter().cloned().collect::<HashSet<_>>();
    let struct_param_bindings = struct_type_params
        .iter()
        .zip(imp.target_args.iter())
        .map(|(name, ty)| {
            (
                name.clone(),
                type_from_ast_with_impl_self(ty, &imp.target, &imp.target_args),
            )
        })
        .collect::<HashMap<_, _>>();
    let mut out = HashMap::new();
    for (field_name, field_ty) in struct_fields {
        let instantiated =
            substitute_generic_type(field_ty, &struct_param_set, &struct_param_bindings);
        if let SemType::Path { path, args } = instantiated
            && args.is_empty()
            && path.len() == 1
            && structural_params.contains(&path[0])
        {
            out.insert(field_name.clone(), path[0].clone());
        }
    }
    out
}

fn path_type_param_name<'a>(ty: &'a Type, type_params: &HashSet<String>) -> Option<&'a str> {
    if ty.path.len() == 1
        && ty.args.is_empty()
        && ty.trait_bounds.is_empty()
        && type_params.contains(&ty.path[0])
    {
        return Some(ty.path[0].as_str());
    }
    None
}

fn collect_structural_requirements_in_stmt(
    stmt: &Stmt,
    param_bindings: &HashMap<String, String>,
    self_field_bindings: &HashMap<String, String>,
    out: &mut HashMap<String, StructuralRequirements>,
) {
    match stmt {
        Stmt::Const(def) => collect_structural_requirements_in_expr(
            &def.value,
            param_bindings,
            self_field_bindings,
            out,
        ),
        Stmt::Assign { target, value, .. } => {
            collect_structural_requirements_in_assign_target(
                target,
                param_bindings,
                self_field_bindings,
                out,
            );
            collect_structural_requirements_in_expr(
                value,
                param_bindings,
                self_field_bindings,
                out,
            );
        }
        Stmt::Return(value) => {
            if let Some(value) = value {
                collect_structural_requirements_in_expr(
                    value,
                    param_bindings,
                    self_field_bindings,
                    out,
                );
            }
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            collect_structural_requirements_in_expr(
                condition,
                param_bindings,
                self_field_bindings,
                out,
            );
            for stmt in &then_block.statements {
                collect_structural_requirements_in_stmt(
                    stmt,
                    param_bindings,
                    self_field_bindings,
                    out,
                );
            }
            if let Some(else_block) = else_block {
                for stmt in &else_block.statements {
                    collect_structural_requirements_in_stmt(
                        stmt,
                        param_bindings,
                        self_field_bindings,
                        out,
                    );
                }
            }
        }
        Stmt::While { condition, body } => {
            collect_structural_requirements_in_expr(
                condition,
                param_bindings,
                self_field_bindings,
                out,
            );
            for stmt in &body.statements {
                collect_structural_requirements_in_stmt(
                    stmt,
                    param_bindings,
                    self_field_bindings,
                    out,
                );
            }
        }
        Stmt::For { iter, body, .. } => {
            collect_structural_requirements_in_expr(iter, param_bindings, self_field_bindings, out);
            for stmt in &body.statements {
                collect_structural_requirements_in_stmt(
                    stmt,
                    param_bindings,
                    self_field_bindings,
                    out,
                );
            }
        }
        Stmt::Loop { body } => {
            for stmt in &body.statements {
                collect_structural_requirements_in_stmt(
                    stmt,
                    param_bindings,
                    self_field_bindings,
                    out,
                );
            }
        }
        Stmt::Expr(expr) | Stmt::TailExpr(expr) => {
            collect_structural_requirements_in_expr(expr, param_bindings, self_field_bindings, out);
        }
        Stmt::DestructureConst { value, .. } => {
            collect_structural_requirements_in_expr(
                value,
                param_bindings,
                self_field_bindings,
                out,
            );
        }
        Stmt::Break | Stmt::Continue | Stmt::RustBlock(_) => {}
    }
}

fn collect_structural_requirements_in_assign_target(
    target: &AssignTarget,
    param_bindings: &HashMap<String, String>,
    self_field_bindings: &HashMap<String, String>,
    out: &mut HashMap<String, StructuralRequirements>,
) {
    match target {
        AssignTarget::Path(_) => {}
        AssignTarget::Field { base, field } => {
            if let Some(type_param) =
                resolve_structural_type_param_for_expr(base, param_bindings, self_field_bindings)
            {
                out.entry(type_param)
                    .or_default()
                    .fields
                    .insert(field.clone());
            }
            collect_structural_requirements_in_expr(base, param_bindings, self_field_bindings, out);
        }
        AssignTarget::Index { base, index } => {
            collect_structural_requirements_in_expr(base, param_bindings, self_field_bindings, out);
            collect_structural_requirements_in_expr(
                index,
                param_bindings,
                self_field_bindings,
                out,
            );
        }
        AssignTarget::Tuple(items) => {
            for item in items {
                collect_structural_requirements_in_assign_target(
                    item,
                    param_bindings,
                    self_field_bindings,
                    out,
                );
            }
        }
    }
}

fn collect_structural_requirements_in_expr(
    expr: &Expr,
    param_bindings: &HashMap<String, String>,
    self_field_bindings: &HashMap<String, String>,
    out: &mut HashMap<String, StructuralRequirements>,
) {
    match expr {
        Expr::Call { callee, args } => {
            if let Expr::Field { base, field } = callee.as_ref() {
                if let Some(type_param) = resolve_structural_type_param_for_expr(
                    base,
                    param_bindings,
                    self_field_bindings,
                ) {
                    out.entry(type_param)
                        .or_default()
                        .methods
                        .entry(field.clone())
                        .or_default()
                        .insert(args.len());
                }
                collect_structural_requirements_in_expr(
                    base,
                    param_bindings,
                    self_field_bindings,
                    out,
                );
            } else {
                collect_structural_requirements_in_expr(
                    callee,
                    param_bindings,
                    self_field_bindings,
                    out,
                );
            }
            for arg in args {
                collect_structural_requirements_in_expr(
                    arg,
                    param_bindings,
                    self_field_bindings,
                    out,
                );
            }
        }
        Expr::MacroCall { args, .. } => {
            for arg in args {
                collect_structural_requirements_in_expr(
                    arg,
                    param_bindings,
                    self_field_bindings,
                    out,
                );
            }
        }
        Expr::Field { base, field } => {
            if let Some(type_param) =
                resolve_structural_type_param_for_expr(base, param_bindings, self_field_bindings)
            {
                out.entry(type_param)
                    .or_default()
                    .fields
                    .insert(field.clone());
            }
            collect_structural_requirements_in_expr(base, param_bindings, self_field_bindings, out);
        }
        Expr::Index { base, index } => {
            collect_structural_requirements_in_expr(base, param_bindings, self_field_bindings, out);
            collect_structural_requirements_in_expr(
                index,
                param_bindings,
                self_field_bindings,
                out,
            );
        }
        Expr::Match { scrutinee, arms } => {
            collect_structural_requirements_in_expr(
                scrutinee,
                param_bindings,
                self_field_bindings,
                out,
            );
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_structural_requirements_in_expr(
                        guard,
                        param_bindings,
                        self_field_bindings,
                        out,
                    );
                }
                collect_structural_requirements_in_expr(
                    &arm.value,
                    param_bindings,
                    self_field_bindings,
                    out,
                );
            }
        }
        Expr::Unary { expr, .. } => {
            collect_structural_requirements_in_expr(expr, param_bindings, self_field_bindings, out)
        }
        Expr::Binary { left, right, .. } => {
            collect_structural_requirements_in_expr(left, param_bindings, self_field_bindings, out);
            collect_structural_requirements_in_expr(
                right,
                param_bindings,
                self_field_bindings,
                out,
            );
        }
        Expr::Array(items) | Expr::Tuple(items) => {
            for item in items {
                collect_structural_requirements_in_expr(
                    item,
                    param_bindings,
                    self_field_bindings,
                    out,
                );
            }
        }
        Expr::StructLiteral { fields, .. } => {
            for field in fields {
                collect_structural_requirements_in_expr(
                    &field.value,
                    param_bindings,
                    self_field_bindings,
                    out,
                );
            }
        }
        Expr::Block(block) => {
            for stmt in &block.statements {
                collect_structural_requirements_in_stmt(
                    stmt,
                    param_bindings,
                    self_field_bindings,
                    out,
                );
            }
        }
        Expr::Closure { body, .. } => {
            for stmt in &body.statements {
                collect_structural_requirements_in_stmt(
                    stmt,
                    param_bindings,
                    self_field_bindings,
                    out,
                );
            }
        }
        Expr::Range { start, end, .. } => {
            if let Some(start) = start {
                collect_structural_requirements_in_expr(
                    start,
                    param_bindings,
                    self_field_bindings,
                    out,
                );
            }
            if let Some(end) = end {
                collect_structural_requirements_in_expr(
                    end,
                    param_bindings,
                    self_field_bindings,
                    out,
                );
            }
        }
        Expr::Cast { expr, .. } => {
            collect_structural_requirements_in_expr(expr, param_bindings, self_field_bindings, out)
        }
        Expr::Try(inner) => {
            collect_structural_requirements_in_expr(inner, param_bindings, self_field_bindings, out)
        }
        Expr::Int(_)
        | Expr::Float(_)
        | Expr::Bool(_)
        | Expr::Char(_)
        | Expr::String(_)
        | Expr::Path(_)
        | Expr::PathWithTypeArgs { .. } => {}
    }
}

fn resolve_structural_type_param_for_expr(
    expr: &Expr,
    param_bindings: &HashMap<String, String>,
    self_field_bindings: &HashMap<String, String>,
) -> Option<String> {
    match expr {
        Expr::Path(path) if path.len() == 1 => param_bindings.get(&path[0]).cloned(),
        Expr::PathWithTypeArgs { path, .. } if path.len() == 1 => {
            param_bindings.get(&path[0]).cloned()
        }
        Expr::Field { base, field } if matches!(base.as_ref(), Expr::Path(path) if path.len() == 1 && path[0] == "self") => {
            self_field_bindings.get(field).cloned()
        }
        _ => None,
    }
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
        "view" => {
            if args.len() != 1 {
                diagnostics.push(Diagnostic::new(
                    "view(...) expects exactly one argument",
                    default_diag_span(),
                ));
                return Some(SemType::Unknown);
            }
            Some(args[0].clone())
        }
        "assert" => {
            if args.len() != 1 {
                diagnostics.push(Diagnostic::new(
                    "assert(...) expects exactly one boolean argument",
                    default_diag_span(),
                ));
                return Some(SemType::Unit);
            }
            if !is_compatible(&args[0], &named_type("bool")) {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "assert(...) expects `bool`, got `{}`",
                        type_to_string(&args[0])
                    ),
                    default_diag_span(),
                ));
            }
            Some(SemType::Unit)
        }
        "assert_eq" | "assert_ne" => {
            if args.len() != 2 {
                diagnostics.push(Diagnostic::new(
                    format!("{name}(...) expects exactly two arguments"),
                    default_diag_span(),
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
                    default_diag_span(),
                ));
            }
            Some(SemType::Unit)
        }
        "strcat" => Some(named_type("String")),
        _ => None,
    }
}

fn resolve_method_call_type(
    base_ty: &SemType,
    method: &str,
    arg_exprs: &[Expr],
    args: &[SemType],
    context: &Context,
    diagnostics: &mut Vec<Diagnostic>,
) -> SemType {
    if let SemType::Path { path, .. } = base_ty
        && let Some(type_name) = path.last()
    {
        let lookup = format!("{type_name}::{method}");
        if let Some(sig) = context.functions.get(&lookup) {
            if sig.params.len() == args.len() + 1 {
                let mut call_args = Vec::with_capacity(args.len() + 1);
                call_args.push(base_ty.clone());
                call_args.extend(args.iter().cloned());
                return resolve_named_function_call(
                    sig,
                    &lookup,
                    None,
                    &call_args,
                    None,
                    context,
                    diagnostics,
                );
            }
            if sig.params.len() == args.len() {
                return resolve_named_function_call(
                    sig,
                    &lookup,
                    Some(arg_exprs),
                    args,
                    None,
                    context,
                    diagnostics,
                );
            }
            diagnostics.push(Diagnostic::new(
                format!(
                    "Method `{lookup}` expects {} arg(s), got {}",
                    sig.params.len().saturating_sub(1),
                    args.len()
                ),
                default_diag_span(),
            ));
            return sig.return_type.clone();
        }
    }

    if let Some(capability) =
        resolve_method_capability(base_ty, method, args.len(), context, diagnostics)
    {
        for (index, expected) in capability.expected_args.iter().enumerate() {
            let Some(actual) = args.get(index) else {
                break;
            };
            if !method_arg_type_compatible(arg_exprs.get(index), actual, expected) {
                let vec_get_index_error = index == 0
                    && method == "get"
                    && matches!(
                        base_ty,
                        SemType::Path { path, .. } if path.last().is_some_and(|segment| segment == "Vec")
                    )
                    && !is_vector_index_type(actual);
                diagnostics.push(Diagnostic::new(
                    if vec_get_index_error {
                        format!(
                            "Method `Vec::get` expects integer index, got `{}`",
                            type_to_string(actual)
                        )
                    } else {
                        mismatch_message(
                            format!(
                                "Arg {} for method `{}`: expected `{}`, got `{}`",
                                index + 1,
                                method,
                                type_to_string(expected),
                                type_to_string(actual)
                            ),
                            actual,
                            expected,
                        )
                    },
                    default_diag_span(),
                ));
            }
        }
        return capability.return_ty;
    }

    if is_abstract_structural_symbol(base_ty, context) {
        if method == "to_string" && args.is_empty() {
            return named_type("String");
        }
        if method.starts_with("is_") || method.starts_with("has_") {
            return named_type("bool");
        }
        return SemType::Unknown;
    }

    diagnostics.push(Diagnostic::new(
        format!(
            "Capability resolution failed for method `{method}` on `{}`",
            type_to_string(base_ty)
        ),
        default_diag_span(),
    ));
    SemType::Unknown
}

fn is_abstract_structural_symbol(ty: &SemType, context: &Context) -> bool {
    let SemType::Path { path, args } = ty else {
        return false;
    };
    if path.len() != 1 || !args.is_empty() {
        return false;
    }
    let name = path[0].as_str();
    if is_numeric_type_name(name)
        || is_copy_primitive_type(name)
        || matches!(
            name,
            "String"
                | "str"
                | "Vec"
                | "Option"
                | "Result"
                | "HashMap"
                | "BTreeMap"
                | "HashSet"
                | "BTreeSet"
        )
    {
        return false;
    }
    !context.structs.contains_key(name) && !context.enums.contains_key(name)
}

fn rustdex_has_method_strict(
    type_name: &str,
    method: &str,
    diagnostics: &mut Vec<Diagnostic>,
) -> Result<bool, ()> {
    if !TYPE_SYSTEM_ENABLED.with(|cell| cell.get()) {
        return Ok(true);
    }
    let mut observed_metadata = false;
    let mut last_error = None::<rustdex_backend::RustdexError>;
    for candidate in rustdex_type_name_candidates(type_name) {
        match rustdex_backend::type_has_associated_method(&candidate, method) {
            Ok(true) => return Ok(true),
            Ok(false) => {
                observed_metadata = true;
            }
            Err(err) => {
                last_error = Some(err);
            }
        }
    }
    if observed_metadata {
        Ok(false)
    } else {
        diagnostics.push(Diagnostic::new(
            format!(
                "E_CAPABILITY_METADATA_MISSING: rustdex metadata unavailable for `{type_name}::{method}`; capability resolution requires rustdex ({})",
                last_error
                    .map(|err| format!("{err:?}"))
                    .unwrap_or_else(|| "no backend response".to_string())
            ),
            default_diag_span(),
        ));
        Err(())
    }
}

fn rustdex_type_name_candidates(type_name: &str) -> Vec<String> {
    let mut candidates = vec![type_name.to_string()];
    match type_name {
        "String" => {
            candidates.push("std::string::String".to_string());
            candidates.push("alloc::string::String".to_string());
        }
        "Vec" => {
            candidates.push("std::vec::Vec".to_string());
            candidates.push("alloc::vec::Vec".to_string());
        }
        "Option" => {
            candidates.push("std::option::Option".to_string());
            candidates.push("core::option::Option".to_string());
        }
        "Result" => {
            candidates.push("std::result::Result".to_string());
            candidates.push("core::result::Result".to_string());
        }
        "HashMap" => {
            candidates.push("std::collections::HashMap".to_string());
        }
        "BTreeMap" => {
            candidates.push("std::collections::BTreeMap".to_string());
        }
        "HashSet" => {
            candidates.push("std::collections::HashSet".to_string());
        }
        "BTreeSet" => {
            candidates.push("std::collections::BTreeSet".to_string());
        }
        _ => {}
    }
    candidates
}

fn resolve_method_capability(
    base_ty: &SemType,
    method: &str,
    actual_arity: usize,
    _context: &Context,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<MethodCapability> {
    if let SemType::Iter(item_ty) = base_ty {
        let capability = match method {
            "map" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Owned,
                arg_modes: vec![CallArgMode::Owned],
                expected_args: vec![SemType::Unknown],
                return_ty: SemType::Iter(Box::new(SemType::Unknown)),
            },
            "flat_map" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Owned,
                arg_modes: vec![CallArgMode::Owned],
                expected_args: vec![SemType::Unknown],
                return_ty: SemType::Iter(Box::new(SemType::Unknown)),
            },
            "filter" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Owned,
                arg_modes: vec![CallArgMode::Owned],
                expected_args: vec![SemType::Unknown],
                return_ty: SemType::Iter(item_ty.clone()),
            },
            "collect" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Owned,
                arg_modes: vec![],
                expected_args: vec![],
                return_ty: SemType::Path {
                    path: vec!["Vec".to_string()],
                    args: vec![item_ty.as_ref().clone()],
                },
            },
            "fold" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Owned,
                arg_modes: vec![CallArgMode::Owned, CallArgMode::Owned],
                expected_args: vec![SemType::Unknown, SemType::Unknown],
                return_ty: SemType::Unknown,
            },
            "enumerate" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Owned,
                arg_modes: vec![],
                expected_args: vec![],
                return_ty: SemType::Iter(Box::new(SemType::Tuple(vec![
                    named_type("usize"),
                    item_ty.as_ref().clone(),
                ]))),
            },
            "count" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Owned,
                arg_modes: vec![],
                expected_args: vec![],
                return_ty: named_type("usize"),
            },
            "sum" | "product" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Owned,
                arg_modes: vec![],
                expected_args: vec![],
                return_ty: item_ty.as_ref().clone(),
            },
            "any" | "all" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Owned,
                arg_modes: vec![CallArgMode::Owned],
                expected_args: vec![SemType::Unknown],
                return_ty: named_type("bool"),
            },
            "for_each" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Owned,
                arg_modes: vec![CallArgMode::Owned],
                expected_args: vec![SemType::Unknown],
                return_ty: SemType::Unit,
            },
            _ => return None,
        };
        expect_method_arity(
            "Iter",
            method,
            actual_arity,
            capability.expected_args.len(),
            diagnostics,
        );
        return Some(capability);
    }

    let (type_name, generic_args): (&str, &[SemType]) = match base_ty {
        SemType::Path { path, args } => (
            path.last()
                .map(|segment| segment.as_str())
                .unwrap_or_default(),
            args.as_slice(),
        ),
        _ => ("", &[]),
    };
    if type_name.is_empty() {
        return None;
    }

    if method == "to_string" && actual_arity == 0 && builtin_value_supports_to_string(type_name) {
        return Some(MethodCapability {
            receiver_mode: CapabilityReceiverMode::Borrowed,
            arg_modes: vec![],
            expected_args: vec![],
            return_ty: named_type("String"),
        });
    }

    if rustdex_has_method_strict(type_name, method, diagnostics).is_err() {
        return None;
    }

    let capability = match type_name {
        "String" => match method {
            "len" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Borrowed,
                arg_modes: vec![],
                expected_args: vec![],
                return_ty: named_type("usize"),
            },
            "is_empty" | "contains" | "starts_with" | "ends_with" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Borrowed,
                arg_modes: if method == "is_empty" {
                    vec![]
                } else {
                    vec![CallArgMode::Borrowed]
                },
                expected_args: if method == "is_empty" {
                    vec![]
                } else {
                    vec![named_type("String")]
                },
                return_ty: named_type("bool"),
            },
            "strip_prefix" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Borrowed,
                arg_modes: vec![CallArgMode::Borrowed],
                expected_args: vec![named_type("String")],
                return_ty: option_type(named_type("String")),
            },
            "split_once" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Borrowed,
                arg_modes: vec![CallArgMode::Borrowed],
                expected_args: vec![named_type("String")],
                return_ty: option_type(SemType::Tuple(vec![
                    named_type("String"),
                    named_type("String"),
                ])),
            },
            "push_str" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Borrowed,
                arg_modes: vec![CallArgMode::Borrowed],
                expected_args: vec![named_type("String")],
                return_ty: SemType::Unit,
            },
            "into_bytes" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Owned,
                arg_modes: vec![],
                expected_args: vec![],
                return_ty: SemType::Path {
                    path: vec!["Vec".to_string()],
                    args: vec![named_type("u8")],
                },
            },
            "chars" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Borrowed,
                arg_modes: vec![],
                expected_args: vec![],
                return_ty: SemType::Iter(Box::new(named_type("char"))),
            },
            "to_string" => MethodCapability {
                receiver_mode: CapabilityReceiverMode::Borrowed,
                arg_modes: vec![],
                expected_args: vec![],
                return_ty: named_type("String"),
            },
            _ => return None,
        },
        "Vec" => {
            let item_ty = generic_args.first().cloned().unwrap_or(SemType::Unknown);
            match method {
                "len" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: named_type("usize"),
                },
                "is_empty" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: named_type("bool"),
                },
                "contains" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![CallArgMode::Borrowed],
                    expected_args: vec![item_ty.clone()],
                    return_ty: named_type("bool"),
                },
                "push" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![CallArgMode::Owned],
                    expected_args: vec![item_ty.clone()],
                    return_ty: SemType::Unit,
                },
                "first" | "last" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: option_type(item_ty.clone()),
                },
                "get" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![CallArgMode::Owned],
                    expected_args: vec![named_type("usize")],
                    return_ty: option_type(item_ty),
                },
                "iter" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: SemType::Iter(Box::new(
                        generic_args.first().cloned().unwrap_or(SemType::Unknown),
                    )),
                },
                "into_iter" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Owned,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: SemType::Iter(Box::new(
                        generic_args.first().cloned().unwrap_or(SemType::Unknown),
                    )),
                },
                _ => return None,
            }
        }
        "Option" => {
            let inner = generic_args.first().cloned().unwrap_or(SemType::Unknown);
            match method {
                "is_some" | "is_none" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: named_type("bool"),
                },
                "into_iter" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Owned,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: SemType::Iter(Box::new(inner)),
                },
                "unwrap_or" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Owned,
                    arg_modes: vec![CallArgMode::Owned],
                    expected_args: vec![inner.clone()],
                    return_ty: inner,
                },
                _ => return None,
            }
        }
        "Result" => {
            let ok_ty = generic_args.first().cloned().unwrap_or(SemType::Unknown);
            match method {
                "is_ok" | "is_err" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: named_type("bool"),
                },
                "into_iter" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Owned,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: SemType::Iter(Box::new(ok_ty.clone())),
                },
                "unwrap_or" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Owned,
                    arg_modes: vec![CallArgMode::Owned],
                    expected_args: vec![ok_ty.clone()],
                    return_ty: ok_ty,
                },
                _ => return None,
            }
        }
        "HashMap" | "BTreeMap" => {
            let key_ty = generic_args.first().cloned().unwrap_or(SemType::Unknown);
            let value_ty = generic_args.get(1).cloned().unwrap_or(SemType::Unknown);
            match method {
                "len" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: named_type("usize"),
                },
                "is_empty" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: named_type("bool"),
                },
                "contains_key" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![CallArgMode::Borrowed],
                    expected_args: vec![key_ty.clone()],
                    return_ty: named_type("bool"),
                },
                "get" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![CallArgMode::Borrowed],
                    expected_args: vec![key_ty.clone()],
                    return_ty: option_type(value_ty.clone()),
                },
                "keys" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: SemType::Iter(Box::new(key_ty.clone())),
                },
                "values" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: SemType::Iter(Box::new(value_ty.clone())),
                },
                "iter" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: SemType::Iter(Box::new(SemType::Tuple(vec![key_ty, value_ty]))),
                },
                "into_iter" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Owned,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: SemType::Iter(Box::new(SemType::Tuple(vec![key_ty, value_ty]))),
                },
                _ => return None,
            }
        }
        "HashSet" | "BTreeSet" => {
            let item_ty = generic_args.first().cloned().unwrap_or(SemType::Unknown);
            match method {
                "len" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: named_type("usize"),
                },
                "is_empty" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: named_type("bool"),
                },
                "contains" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![CallArgMode::Borrowed],
                    expected_args: vec![item_ty.clone()],
                    return_ty: named_type("bool"),
                },
                "iter" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: SemType::Iter(Box::new(item_ty.clone())),
                },
                "into_iter" => MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Owned,
                    arg_modes: vec![],
                    expected_args: vec![],
                    return_ty: SemType::Iter(Box::new(item_ty)),
                },
                _ => return None,
            }
        }
        _ => {
            if method.starts_with("is_") || method.starts_with("has_") {
                MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![CallArgMode::Borrowed; actual_arity],
                    expected_args: vec![SemType::Unknown; actual_arity],
                    return_ty: named_type("bool"),
                }
            } else if method_name_suggests_mutating(method) {
                MethodCapability {
                    receiver_mode: CapabilityReceiverMode::Borrowed,
                    arg_modes: vec![CallArgMode::Owned; actual_arity],
                    expected_args: vec![SemType::Unknown; actual_arity],
                    return_ty: SemType::Unit,
                }
            } else {
                return None;
            }
        }
    };

    expect_method_arity(
        type_name,
        method,
        actual_arity,
        capability.expected_args.len(),
        diagnostics,
    );
    Some(capability)
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
            default_diag_span(),
        ));
    }
}

fn index_key_type_compatible(actual: &SemType, expected_key: &SemType) -> bool {
    if is_compatible(actual, expected_key) || is_compatible(expected_key, actual) {
        return true;
    }
    if type_to_string(actual) == type_to_string(expected_key) {
        return true;
    }
    if is_vector_index_type(actual) && is_vector_index_type(expected_key) {
        return true;
    }
    false
}

fn resolve_constructor_call(
    path: &[String],
    args: &[SemType],
    explicit_type_args: Option<&[SemType]>,
    context: &Context,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<SemType> {
    if path.len() == 1 {
        if path[0] == "Some" {
            if args.len() != 1 {
                diagnostics.push(Diagnostic::new(
                    "Some(...) expects exactly one argument",
                    default_diag_span(),
                ));
                return Some(option_type(SemType::Unknown));
            }
            return Some(option_type(args[0].clone()));
        }
        if path[0] == "Ok" {
            if args.len() != 1 {
                diagnostics.push(Diagnostic::new(
                    "Ok(...) expects exactly one argument",
                    default_diag_span(),
                ));
                return Some(result_type(SemType::Unknown, SemType::Unknown));
            }
            return Some(result_type(args[0].clone(), SemType::Unknown));
        }
        if path[0] == "Err" {
            if args.len() != 1 {
                diagnostics.push(Diagnostic::new(
                    "Err(...) expects exactly one argument",
                    default_diag_span(),
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
                        default_diag_span(),
                    ));
                    return Some(option_type(SemType::Unknown));
                }
                return Some(option_type(args[0].clone()));
            }
            if variant_name == "None" {
                if !args.is_empty() {
                    diagnostics.push(Diagnostic::new(
                        "Option::None expects no arguments",
                        default_diag_span(),
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
                        default_diag_span(),
                    ));
                    return Some(result_type(SemType::Unknown, SemType::Unknown));
                }
                return Some(result_type(args[0].clone(), SemType::Unknown));
            }
            if variant_name == "Err" {
                if args.len() != 1 {
                    diagnostics.push(Diagnostic::new(
                        "Result::Err(...) expects one argument",
                        default_diag_span(),
                    ));
                    return Some(result_type(SemType::Unknown, SemType::Unknown));
                }
                return Some(result_type(SemType::Unknown, args[0].clone()));
            }
        }

        if let Some(variants) = context.enums.get(enum_name) {
            if let Some(variant_info) = variants.get(variant_name) {
                let expected_payload = &variant_info.payload_types;
                let type_param_names = context
                    .enum_type_params
                    .get(enum_name)
                    .cloned()
                    .unwrap_or_default();
                let type_param_set = type_param_names.iter().cloned().collect::<HashSet<_>>();
                let mut generic_bindings = HashMap::new();
                if let Some(explicit) = explicit_type_args {
                    if explicit.len() != type_param_names.len() {
                        diagnostics.push(Diagnostic::new(
                            format!(
                                "Enum variant `{enum_name}::{variant_name}` expects {} explicit type argument(s), got {}",
                                type_param_names.len(),
                                explicit.len()
                            ),
                            default_diag_span(),
                        ));
                    }
                    for (index, ty) in explicit.iter().enumerate() {
                        if let Some(name) = type_param_names.get(index) {
                            generic_bindings.insert(name.clone(), ty.clone());
                        }
                    }
                }
                if variant_info.named_fields.is_some() {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Enum variant `{enum_name}::{variant_name}` uses named fields; construct it with `{enum_name}::{variant_name} {{ ... }}`"
                        ),
                        default_diag_span(),
                    ));
                    return Some(SemType::Path {
                        path: vec![enum_name.clone()],
                        args: type_param_names
                            .iter()
                            .map(|name| {
                                generic_bindings
                                    .get(name)
                                    .cloned()
                                    .unwrap_or(SemType::Unknown)
                            })
                            .collect(),
                    });
                }
                if args.len() != expected_payload.len() {
                    diagnostics.push(Diagnostic::new(
                        format!(
                            "Enum variant `{enum_name}::{variant_name}` expects {} argument(s), got {}",
                            expected_payload.len(),
                            args.len()
                        ),
                        default_diag_span(),
                    ));
                } else {
                    for (index, (actual, expected)) in args.iter().zip(expected_payload).enumerate()
                    {
                        let expected_with_known = substitute_bound_generic_type(
                            expected,
                            &type_param_set,
                            &generic_bindings,
                        );
                        let bound = bind_generic_params(
                            &expected_with_known,
                            actual,
                            &type_param_set,
                            &mut generic_bindings,
                        );
                        if !bound && !is_compatible(actual, &expected_with_known) {
                            diagnostics.push(Diagnostic::new(
                                mismatch_message(
                                    format!(
                                        "Enum variant `{enum_name}::{variant_name}` arg {} expected `{}`, got `{}`",
                                        index + 1,
                                        type_to_string(&expected_with_known),
                                        type_to_string(actual)
                                    ),
                                    actual,
                                    &expected_with_known,
                                ),
                                default_diag_span(),
                            ));
                        }
                    }
                }
                return Some(SemType::Path {
                    path: vec![enum_name.clone()],
                    args: type_param_names
                        .iter()
                        .map(|name| {
                            generic_bindings
                                .get(name)
                                .cloned()
                                .unwrap_or(SemType::Unknown)
                        })
                        .collect(),
                });
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
                default_diag_span(),
            ));
            return inner_value.clone();
        }
        if option_inner(return_ty).is_none() {
            diagnostics.push(Diagnostic::new(
                "The `?` operator on Option requires the function to return Option",
                default_diag_span(),
            ));
        }
        return inner_value.clone();
    }
    if let Some((ok_ty, err_ty)) = result_parts(inner) {
        if *return_ty == SemType::Unknown {
            diagnostics.push(Diagnostic::new(
                "Functions using `?` must declare an Option/Result return type",
                default_diag_span(),
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
                    default_diag_span(),
                ));
            }
        } else {
            diagnostics.push(Diagnostic::new(
                "The `?` operator on Result requires the function to return Result",
                default_diag_span(),
            ));
        }
        return ok_ty.clone();
    }

    diagnostics.push(Diagnostic::new(
        "The `?` operator requires Option<T> or Result<T, E>",
        default_diag_span(),
    ));
    SemType::Unknown
}

#[allow(clippy::too_many_arguments)]
fn apply_binary_operand_hints(
    op: &BinaryOp,
    left_expr: &Expr,
    typed_left: &mut TypedExpr,
    left_ty: &mut SemType,
    right_expr: &Expr,
    typed_right: &mut TypedExpr,
    right_ty: &mut SemType,
    return_ty: &SemType,
    locals: &mut HashMap<String, SemType>,
) {
    let Some(hint) = binary_operand_hint(op, left_ty, right_ty, return_ty) else {
        return;
    };
    refine_expr_type_hint(left_expr, typed_left, left_ty, &hint, locals);
    refine_expr_type_hint(right_expr, typed_right, right_ty, &hint, locals);
}

fn binary_operand_hint(
    op: &BinaryOp,
    left_ty: &SemType,
    right_ty: &SemType,
    return_ty: &SemType,
) -> Option<SemType> {
    match op {
        BinaryOp::Add => {
            if is_exact_string_type(return_ty)
                || is_exact_string_type(left_ty)
                || is_exact_string_type(right_ty)
            {
                return Some(named_type("String"));
            }
            numeric_hint_from_context(left_ty, right_ty, return_ty).or_else(|| {
                if *left_ty == SemType::Unknown && *right_ty == SemType::Unknown {
                    Some(named_type("i64"))
                } else {
                    None
                }
            })
        }
        BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
            numeric_hint_from_context(left_ty, right_ty, return_ty).or_else(|| {
                if *left_ty == SemType::Unknown && *right_ty == SemType::Unknown {
                    Some(named_type("i64"))
                } else {
                    None
                }
            })
        }
        _ => None,
    }
}

fn numeric_hint_from_context(
    left_ty: &SemType,
    right_ty: &SemType,
    return_ty: &SemType,
) -> Option<SemType> {
    if let Some(name) = canonical_numeric_name(return_ty) {
        return Some(named_type(name));
    }
    if let Some(name) = canonical_numeric_name(left_ty) {
        return Some(named_type(name));
    }
    if let Some(name) = canonical_numeric_name(right_ty) {
        return Some(named_type(name));
    }
    None
}

fn refine_expr_type_hint(
    expr: &Expr,
    typed_expr: &mut TypedExpr,
    expr_ty: &mut SemType,
    hint_ty: &SemType,
    locals: &mut HashMap<String, SemType>,
) {
    if *expr_ty != SemType::Unknown || *hint_ty == SemType::Unknown {
        return;
    }
    match expr {
        Expr::Path(path) | Expr::PathWithTypeArgs { path, .. }
            if path.len() == 1
                && locals
                    .get(&path[0])
                    .cloned()
                    .is_some_and(|existing| existing == SemType::Unknown) =>
        {
            locals.insert(path[0].clone(), hint_ty.clone());
        }
        _ => {}
    }
    *expr_ty = hint_ty.clone();
    typed_expr.ty = type_to_string(expr_ty);
}

fn is_exact_string_type(ty: &SemType) -> bool {
    matches!(
        ty,
        SemType::Path { path, args } if path.len() == 1 && path[0] == "String" && args.is_empty()
    )
}

fn resolve_add_type(left: &SemType, right: &SemType, diagnostics: &mut Vec<Diagnostic>) -> SemType {
    if is_exact_string_type(left) && is_exact_string_type(right) {
        return named_type("String");
    }
    if infer_local_bidi_enabled()
        && ((is_exact_string_type(left) && *right == SemType::Unknown)
            || (is_exact_string_type(right) && *left == SemType::Unknown))
    {
        return named_type("String");
    }
    if let Some(out_ty) = resolve_common_numeric_type(left, right) {
        return out_ty;
    }
    if strict_mode_enabled() && (*left == SemType::Unknown || *right == SemType::Unknown) {
        return SemType::Unknown;
    }
    diagnostics.push(Diagnostic::new(
        format!(
            "`+` expects numeric operands or `String + String`, got `{}` and `{}`",
            type_to_string(left),
            type_to_string(right)
        ),
        default_diag_span(),
    ));
    SemType::Unknown
}

fn resolve_add_type_with_literals(
    left_expr: &Expr,
    left_ty: &SemType,
    right_expr: &Expr,
    right_ty: &SemType,
    diagnostics: &mut Vec<Diagnostic>,
) -> SemType {
    if let Some(preserved) =
        preserve_integer_type_for_default_int_literal(left_expr, left_ty, right_expr, right_ty)
    {
        return preserved;
    }
    resolve_add_type(left_ty, right_ty, diagnostics)
}

fn resolve_unary_neg_type(expr: &SemType, diagnostics: &mut Vec<Diagnostic>) -> SemType {
    let ty = normalize_numeric_unary_type(expr);
    if ty != SemType::Unknown {
        return ty;
    }
    if strict_mode_enabled() && *expr == SemType::Unknown {
        return SemType::Unknown;
    }
    diagnostics.push(Diagnostic::new(
        format!(
            "unary `-` expects a numeric operand, got `{}`",
            type_to_string(expr)
        ),
        default_diag_span(),
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
    if strict_mode_enabled() && (*left == SemType::Unknown || *right == SemType::Unknown) {
        return SemType::Unknown;
    }
    diagnostics.push(Diagnostic::new(
        format!(
            "`{op}` expects numeric operands, got `{}` and `{}`",
            type_to_string(left),
            type_to_string(right)
        ),
        default_diag_span(),
    ));
    SemType::Unknown
}

fn resolve_numeric_binary_type_with_literals(
    op: &str,
    left_expr: &Expr,
    left_ty: &SemType,
    right_expr: &Expr,
    right_ty: &SemType,
    diagnostics: &mut Vec<Diagnostic>,
) -> SemType {
    if let Some(preserved) =
        preserve_integer_type_for_default_int_literal(left_expr, left_ty, right_expr, right_ty)
    {
        return preserved;
    }
    resolve_numeric_binary_type(op, left_ty, right_ty, diagnostics)
}

fn preserve_integer_type_for_default_int_literal(
    left_expr: &Expr,
    left_ty: &SemType,
    right_expr: &Expr,
    right_ty: &SemType,
) -> Option<SemType> {
    if matches!(left_expr, Expr::Int(_)) && is_integral_numeric_type(right_ty) {
        return Some(right_ty.clone());
    }
    if matches!(right_expr, Expr::Int(_)) && is_integral_numeric_type(left_ty) {
        return Some(left_ty.clone());
    }
    None
}

fn literal_bidi_adjusted_numeric_arg_type(
    expr: Option<&Expr>,
    actual: &SemType,
    expected: &SemType,
) -> Option<SemType> {
    if infer_literal_bidi_enabled()
        && expr.is_some_and(|expr| matches!(expr, Expr::Int(_)))
        && canonical_numeric_name(expected).is_some_and(is_integral_numeric_type_name)
        && canonical_numeric_name(actual) == Some("i64")
    {
        return Some(expected.clone());
    }
    None
}

fn method_arg_type_compatible(expr: Option<&Expr>, actual: &SemType, expected: &SemType) -> bool {
    if is_compatible(actual, expected) {
        return true;
    }
    if literal_bidi_adjusted_numeric_arg_type(expr, actual, expected).is_some() {
        return true;
    }
    if expr.is_some_and(|value| matches!(value, Expr::Int(_)))
        && canonical_numeric_name(expected).is_some_and(is_integral_numeric_type_name)
        && canonical_numeric_name(actual) == Some("i64")
    {
        return true;
    }
    if !numeric_coercion_enabled() {
        return false;
    }
    let (Some(actual_name), Some(expected_name)) = (
        canonical_numeric_name(actual),
        canonical_numeric_name(expected),
    ) else {
        return false;
    };
    is_integral_numeric_type_name(actual_name)
        && is_integral_numeric_type_name(expected_name)
        && can_implicitly_coerce_integral_name(actual_name, expected_name)
}

fn literal_bidi_comparison_compatible(
    left_expr: &Expr,
    left_ty: &SemType,
    right_expr: &Expr,
    right_ty: &SemType,
) -> bool {
    if !infer_literal_bidi_enabled() {
        return false;
    }
    (matches!(left_expr, Expr::Int(_))
        && canonical_numeric_name(left_ty) == Some("i64")
        && canonical_numeric_name(right_ty).is_some_and(is_integral_numeric_type_name))
        || (matches!(right_expr, Expr::Int(_))
            && canonical_numeric_name(right_ty) == Some("i64")
            && canonical_numeric_name(left_ty).is_some_and(is_integral_numeric_type_name))
}

fn is_integral_numeric_type(ty: &SemType) -> bool {
    let Some(name) = canonical_numeric_name(ty) else {
        return false;
    };
    !matches!(name, "f32" | "f64")
}

fn resolve_common_numeric_type(left: &SemType, right: &SemType) -> Option<SemType> {
    let left_name = canonical_numeric_name(left)?;
    let right_name = canonical_numeric_name(right)?;
    let left_integral = is_integral_numeric_type_name(left_name);
    let right_integral = is_integral_numeric_type_name(right_name);
    if left_integral != right_integral {
        // Keep float/int mixing explicit. Whole-number coercions are handled
        // separately via numeric_coercion + target-aware compatibility checks.
        return None;
    }
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
        "i8" | "i16"
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

fn is_integral_numeric_type_name(name: &str) -> bool {
    is_numeric_type_name(name) && !matches!(name, "f32" | "f64")
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct IntegralTypeSpec {
    bits: u16,
    signed: bool,
}

fn integral_type_spec(name: &str) -> Option<IntegralTypeSpec> {
    let spec = match name {
        "i8" => IntegralTypeSpec {
            bits: 8,
            signed: true,
        },
        "i16" => IntegralTypeSpec {
            bits: 16,
            signed: true,
        },
        "i32" => IntegralTypeSpec {
            bits: 32,
            signed: true,
        },
        "i64" => IntegralTypeSpec {
            bits: 64,
            signed: true,
        },
        "i128" => IntegralTypeSpec {
            bits: 128,
            signed: true,
        },
        "isize" => IntegralTypeSpec {
            bits: 64,
            signed: true,
        },
        "u8" => IntegralTypeSpec {
            bits: 8,
            signed: false,
        },
        "u16" => IntegralTypeSpec {
            bits: 16,
            signed: false,
        },
        "u32" => IntegralTypeSpec {
            bits: 32,
            signed: false,
        },
        "u64" => IntegralTypeSpec {
            bits: 64,
            signed: false,
        },
        "u128" => IntegralTypeSpec {
            bits: 128,
            signed: false,
        },
        "usize" => IntegralTypeSpec {
            bits: 64,
            signed: false,
        },
        _ => return None,
    };
    Some(spec)
}

fn can_implicitly_coerce_integral_name(actual: &str, expected: &str) -> bool {
    if actual == expected {
        return true;
    }
    let Some(actual_spec) = integral_type_spec(actual) else {
        return false;
    };
    let Some(expected_spec) = integral_type_spec(expected) else {
        return false;
    };

    match (actual_spec.signed, expected_spec.signed) {
        (false, false) => expected_spec.bits >= actual_spec.bits,
        (true, true) => expected_spec.bits >= actual_spec.bits,
        (false, true) => expected_spec.bits > actual_spec.bits,
        // Signed -> unsigned is supported implicitly with abs/saturating_abs
        // wrapping before cast, so width parity is acceptable.
        (true, false) => expected_spec.bits >= actual_spec.bits,
    }
}

fn integral_coercion_hint(actual: &SemType, expected: &SemType) -> Option<String> {
    if !numeric_coercion_enabled() {
        return None;
    }
    let (Some(actual_name), Some(expected_name)) = (
        canonical_numeric_name(actual),
        canonical_numeric_name(expected),
    ) else {
        return None;
    };
    if !is_integral_numeric_type_name(actual_name) || !is_integral_numeric_type_name(expected_name)
    {
        return None;
    }
    if can_implicitly_coerce_integral_name(actual_name, expected_name) {
        return None;
    }
    let (Some(actual_spec), Some(expected_spec)) = (
        integral_type_spec(actual_name),
        integral_type_spec(expected_name),
    ) else {
        return None;
    };
    if actual_spec.signed && !expected_spec.signed {
        return Some(format!(
            "hint: for signed->unsigned conversions use `abs(...)`/`saturating_abs()` before casting to `{expected_name}`"
        ));
    }
    if expected_spec.bits < actual_spec.bits {
        return Some(format!(
            "hint: target `{expected_name}` is narrower than `{actual_name}`; use a wider target type or checked/saturating conversion"
        ));
    }
    if !actual_spec.signed && expected_spec.signed && expected_spec.bits == actual_spec.bits {
        return Some(format!(
            "hint: `{expected_name}` cannot represent all `{actual_name}` values; choose a wider signed target"
        ));
    }
    None
}

fn mismatch_message(message: String, actual: &SemType, expected: &SemType) -> String {
    if let Some(hint) = integral_coercion_hint(actual, expected) {
        format!("{message}. {hint}")
    } else {
        message
    }
}

fn emit_constraint_partition_diagnostic(
    site: &str,
    observed: &[SemType],
    annotation_hint: &str,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if observed.len() < 2 {
        return;
    }

    let mut graph = ConstraintGraph::<String, &'static str>::new();
    let mut nodes = Vec::with_capacity(observed.len());
    for ty in observed {
        nodes.push(graph.add_node(type_to_string(ty)));
    }

    let mut uf = UnionFind::with_size(observed.len());
    for i in 0..observed.len() {
        for j in (i + 1)..observed.len() {
            if is_compatible(&observed[i], &observed[j]) && is_compatible(&observed[j], &observed[i])
            {
                uf.union(i, j);
                let _ = graph.add_edge(nodes[i], nodes[j], "compatible");
                let _ = graph.add_edge(nodes[j], nodes[i], "compatible");
            }
        }
    }

    let mut groups = BTreeMap::<usize, BTreeSet<String>>::new();
    for (index, ty) in observed.iter().enumerate() {
        let root = uf.find(index);
        groups.entry(root).or_default().insert(type_to_string(ty));
    }
    if groups.len() <= 1 {
        return;
    }

    let groups_text = groups
        .values()
        .map(|group| group.iter().cloned().collect::<Vec<_>>().join(" | "))
        .collect::<Vec<_>>()
        .join("] vs [");
    let compatible_edge_count = nodes
        .iter()
        .map(|node| graph.neighbors(*node).len())
        .sum::<usize>()
        / 2;
    let mut message = format!(
        "Constraint groups for {site} are incompatible: [{groups_text}] (compat links: {compatible_edge_count})"
    );
    if infer_principal_fallback_enabled() {
        message.push_str(&format!(
            ". Principal fallback: add {annotation_hint} on {site} to pin a principal type"
        ));
    }
    diagnostics.push(Diagnostic::new(message, default_diag_span()));
}

fn maybe_insert_implicit_integral_cast(
    typed_expr: TypedExpr,
    actual: &SemType,
    expected: &SemType,
) -> TypedExpr {
    if !numeric_coercion_enabled() {
        return typed_expr;
    }
    let (Some(actual_name), Some(expected_name)) = (
        canonical_numeric_name(actual),
        canonical_numeric_name(expected),
    ) else {
        return typed_expr;
    };
    if !is_integral_numeric_type_name(actual_name) || !is_integral_numeric_type_name(expected_name)
    {
        return typed_expr;
    }
    if actual_name == expected_name
        || !can_implicitly_coerce_integral_name(actual_name, expected_name)
    {
        return typed_expr;
    }

    let actual_spec = integral_type_spec(actual_name);
    let expected_spec = integral_type_spec(expected_name);
    let cast_input = if matches!(
        (actual_spec, expected_spec),
        (
            Some(IntegralTypeSpec { signed: true, .. }),
            Some(IntegralTypeSpec { signed: false, .. })
        )
    ) {
        let abs_base = TypedExpr {
            kind: TypedExprKind::Cast {
                expr: Box::new(typed_expr),
                target_type: rust_owned_type_string(actual),
            },
            ty: type_to_string(actual),
        };
        // Signed -> unsigned implicit conversions normalize via saturating_abs
        // so negative values do not wrap to huge unsigned integers.
        TypedExpr {
            kind: TypedExprKind::Call {
                callee: Box::new(TypedExpr {
                    kind: TypedExprKind::Field {
                        base: Box::new(abs_base),
                        field: "saturating_abs".to_string(),
                    },
                    ty: "_".to_string(),
                }),
                args: Vec::new(),
            },
            ty: type_to_string(actual),
        }
    } else {
        typed_expr
    };

    TypedExpr {
        kind: TypedExprKind::Cast {
            expr: Box::new(cast_input),
            target_type: rust_owned_type_string(expected),
        },
        ty: type_to_string(expected),
    }
}

fn apply_expected_call_arg_coercions(
    typed_args: &mut [TypedExpr],
    arg_types: &[SemType],
    expected_args: &[SemType],
) {
    for (index, expected) in expected_args.iter().enumerate() {
        let (Some(current), Some(actual)) = (typed_args.get(index).cloned(), arg_types.get(index))
        else {
            break;
        };
        if !is_compatible(actual, expected) {
            continue;
        }
        if let Some(slot) = typed_args.get_mut(index) {
            *slot = maybe_insert_implicit_integral_cast(current, actual, expected);
        }
    }
}

fn instantiate_expected_param_types_for_call(
    sig: &FunctionSig,
    args: &[SemType],
    explicit_type_args: Option<&[SemType]>,
) -> Vec<SemType> {
    let type_param_set = sig.type_params.iter().cloned().collect::<HashSet<_>>();
    let mut generic_bindings = HashMap::new();
    if let Some(explicit) = explicit_type_args {
        for (index, ty) in explicit.iter().enumerate() {
            if let Some(type_param) = sig.type_params.get(index) {
                generic_bindings.insert(type_param.clone(), ty.clone());
            }
        }
    }
    for (actual, expected) in args.iter().zip(&sig.params) {
        let expected_with_known =
            substitute_bound_generic_type(expected, &type_param_set, &generic_bindings);
        let _ = bind_generic_params(
            &expected_with_known,
            actual,
            &type_param_set,
            &mut generic_bindings,
        );
    }
    sig.params
        .iter()
        .map(|expected| substitute_bound_generic_type(expected, &type_param_set, &generic_bindings))
        .collect()
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
            is_const: def.is_const,
            name: def.name.clone(),
            ty: inferred_local_const_rust_ty(&def.ty, &def.value),
            value: lower_expr_with_context(&def.value, context, ExprPosition::Value, state),
        }),
        TypedStmt::DestructureConst {
            pattern,
            value,
            is_const,
        } => RustStmt::DestructureConst {
            pattern: lower_destructure_pattern(pattern),
            value: lower_expr_with_context(value, context, ExprPosition::Value, state),
            is_const: *is_const,
        },
        TypedStmt::Assign { target, op, value } => RustStmt::Assign {
            target: lower_assign_target(target, context, state),
            op: match op {
                TypedAssignOp::Assign => RustAssignOp::Assign,
                TypedAssignOp::AddAssign => RustAssignOp::AddAssign,
            },
            value: {
                let saved_rebind = context.rebind_place.clone();
                if matches!(op, TypedAssignOp::Assign) {
                    context.rebind_place = assignment_rebind_place(target, value);
                }
                let lowered = lower_expr_with_context(value, context, ExprPosition::Value, state);
                context.rebind_place = saved_rebind;
                lowered
            },
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
            body: {
                context.loop_depth += 1;
                let lowered = body
                    .iter()
                    .map(|stmt| lower_stmt_with_context(stmt, context, state))
                    .collect();
                context.loop_depth = context.loop_depth.saturating_sub(1);
                lowered
            },
        },
        TypedStmt::For {
            binding,
            iter,
            body,
        } => RustStmt::For {
            binding: lower_destructure_pattern(binding),
            iter: lower_expr_with_context(iter, context, ExprPosition::OwnedOperand, state),
            body: {
                context.loop_depth += 1;
                let lowered = body
                    .iter()
                    .map(|stmt| lower_stmt_with_context(stmt, context, state))
                    .collect();
                context.loop_depth = context.loop_depth.saturating_sub(1);
                lowered
            },
        },
        TypedStmt::Loop { body } => RustStmt::Loop {
            body: {
                context.loop_depth += 1;
                let lowered = body
                    .iter()
                    .map(|stmt| lower_stmt_with_context(stmt, context, state))
                    .collect();
                context.loop_depth = context.loop_depth.saturating_sub(1);
                lowered
            },
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

fn assignment_rebind_place(
    target: &TypedAssignTarget,
    value: &TypedExpr,
) -> Option<OwnershipPlace> {
    let target_place = place_for_assign_target(target)?;
    let uses = count_exact_place_uses_in_expr(value, &target_place);
    if uses == 1 { Some(target_place) } else { None }
}

fn place_for_assign_target(target: &TypedAssignTarget) -> Option<OwnershipPlace> {
    match target {
        TypedAssignTarget::Path(name) => Some(OwnershipPlace::from_root(name)),
        TypedAssignTarget::Field { base, field } => place_for_expr(base)
            .map(|place| place.with_projection(OwnershipProjection::Field(field.clone()))),
        TypedAssignTarget::Index { base, .. } => {
            place_for_expr(base).map(|place| place.with_projection(OwnershipProjection::Index))
        }
        TypedAssignTarget::Tuple(_) => None,
    }
}

fn count_exact_place_uses_in_expr(expr: &TypedExpr, target: &OwnershipPlace) -> usize {
    let mut count = 0usize;
    count_exact_place_uses_in_expr_rec(expr, target, &mut count);
    count
}

fn count_exact_place_uses_in_expr_rec(
    expr: &TypedExpr,
    target: &OwnershipPlace,
    count: &mut usize,
) {
    if let Some(place) = place_for_expr(expr) {
        if &place == target {
            *count += 1;
        }
        return;
    }

    match &expr.kind {
        TypedExprKind::Call { callee, args } => {
            count_exact_place_uses_in_expr_rec(callee, target, count);
            for arg in args {
                count_exact_place_uses_in_expr_rec(arg, target, count);
            }
        }
        TypedExprKind::MacroCall { args, .. } => {
            for arg in args {
                count_exact_place_uses_in_expr_rec(arg, target, count);
            }
        }
        TypedExprKind::Binary { left, right, .. } => {
            count_exact_place_uses_in_expr_rec(left, target, count);
            count_exact_place_uses_in_expr_rec(right, target, count);
        }
        TypedExprKind::Unary { expr, .. } => {
            count_exact_place_uses_in_expr_rec(expr, target, count)
        }
        TypedExprKind::Cast { expr, .. } => count_exact_place_uses_in_expr_rec(expr, target, count),
        TypedExprKind::Tuple(items) => {
            for item in items {
                count_exact_place_uses_in_expr_rec(item, target, count);
            }
        }
        TypedExprKind::Array(items) => {
            for item in items {
                count_exact_place_uses_in_expr_rec(item, target, count);
            }
        }
        TypedExprKind::StructLiteral { fields, .. } => {
            for field in fields {
                count_exact_place_uses_in_expr_rec(&field.value, target, count);
            }
        }
        TypedExprKind::Block { body, tail } => {
            for stmt in body {
                count_exact_place_uses_in_stmt_rec(stmt, target, count);
            }
            if let Some(tail) = tail {
                count_exact_place_uses_in_expr_rec(tail, target, count);
            }
        }
        TypedExprKind::Match { scrutinee, arms } => {
            count_exact_place_uses_in_expr_rec(scrutinee, target, count);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    count_exact_place_uses_in_expr_rec(guard, target, count);
                }
                count_exact_place_uses_in_expr_rec(&arm.value, target, count);
            }
        }
        TypedExprKind::Closure { body, .. } => {
            for stmt in body {
                count_exact_place_uses_in_stmt_rec(stmt, target, count);
            }
        }
        TypedExprKind::Range { start, end, .. } => {
            if let Some(start) = start {
                count_exact_place_uses_in_expr_rec(start, target, count);
            }
            if let Some(end) = end {
                count_exact_place_uses_in_expr_rec(end, target, count);
            }
        }
        TypedExprKind::Try(inner) => count_exact_place_uses_in_expr_rec(inner, target, count),
        TypedExprKind::Field { .. }
        | TypedExprKind::Index { .. }
        | TypedExprKind::Path(_)
        | TypedExprKind::Int(_)
        | TypedExprKind::Float(_)
        | TypedExprKind::Bool(_)
        | TypedExprKind::String(_)
        | TypedExprKind::Char(_) => {}
    }
}

fn count_exact_place_uses_in_stmt_rec(
    stmt: &TypedStmt,
    target: &OwnershipPlace,
    count: &mut usize,
) {
    match stmt {
        TypedStmt::Const(def) => count_exact_place_uses_in_expr_rec(&def.value, target, count),
        TypedStmt::DestructureConst { value, .. } => {
            count_exact_place_uses_in_expr_rec(value, target, count)
        }
        TypedStmt::Assign {
            target: _, value, ..
        } => {
            count_exact_place_uses_in_expr_rec(value, target, count);
        }
        TypedStmt::Return(value) => {
            if let Some(value) = value {
                count_exact_place_uses_in_expr_rec(value, target, count);
            }
        }
        TypedStmt::If {
            condition,
            then_body,
            else_body,
        } => {
            count_exact_place_uses_in_expr_rec(condition, target, count);
            for stmt in then_body {
                count_exact_place_uses_in_stmt_rec(stmt, target, count);
            }
            if let Some(block) = else_body {
                for stmt in block {
                    count_exact_place_uses_in_stmt_rec(stmt, target, count);
                }
            }
        }
        TypedStmt::While { condition, body } => {
            count_exact_place_uses_in_expr_rec(condition, target, count);
            for stmt in body {
                count_exact_place_uses_in_stmt_rec(stmt, target, count);
            }
        }
        TypedStmt::For { iter, body, .. } => {
            count_exact_place_uses_in_expr_rec(iter, target, count);
            for stmt in body {
                count_exact_place_uses_in_stmt_rec(stmt, target, count);
            }
        }
        TypedStmt::Loop { body } => {
            for stmt in body {
                count_exact_place_uses_in_stmt_rec(stmt, target, count);
            }
        }
        TypedStmt::Expr(expr) => count_exact_place_uses_in_expr_rec(expr, target, count),
        TypedStmt::Break | TypedStmt::Continue | TypedStmt::RustBlock(_) => {}
    }
}

fn inferred_local_const_rust_ty(original_ty: &str, value: &TypedExpr) -> String {
    if is_view_call_expr(value) {
        let trimmed = original_ty.trim();
        if trimmed != "_" && !trimmed.starts_with('&') {
            return format!("&{trimmed}");
        }
    }
    original_ty.to_string()
}

fn is_view_call_expr(expr: &TypedExpr) -> bool {
    let TypedExprKind::Call { callee, args } = &expr.kind else {
        return false;
    };
    let TypedExprKind::Path(path) = &callee.kind else {
        return false;
    };
    path.len() == 1 && path[0] == "view" && args.len() == 1
}

fn lower_expr_with_context(
    expr: &TypedExpr,
    context: &mut LoweringContext,
    position: ExprPosition,
    state: &mut LoweringState,
) -> RustExpr {
    match &expr.kind {
        TypedExprKind::Int(value) => RustExpr::Int(*value),
        TypedExprKind::Float(value) => RustExpr::Float(value.clone()),
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
            if let TypedExprKind::Path(path) = &callee.kind
                && path.len() == 1
                && path[0] == "strcat"
            {
                return RustExpr::MacroCall {
                    path: vec!["crate".to_string(), "strcat".to_string()],
                    args: args
                        .iter()
                        .map(|arg| {
                            lower_expr_with_context(arg, context, ExprPosition::Value, state)
                        })
                        .collect(),
                };
            }
            if let TypedExprKind::Path(path) = &callee.kind
                && path.len() == 1
                && path[0] == "view"
                && args.len() == 1
            {
                return borrow_expr(lower_expr_with_context(
                    &args[0],
                    context,
                    ExprPosition::Value,
                    state,
                ));
            }
            if let Some(rewritten) =
                lower_map_from_vec_associated_from_call(callee, args, context, state)
            {
                return rewritten;
            }
            let (lowered_callee, arg_modes) = lower_call_callee(callee, args, context, state);
            let lowered_call = RustExpr::Call {
                callee: Box::new(lowered_callee),
                args: args
                    .iter()
                    .enumerate()
                    .map(|(index, arg)| {
                        if arg_modes.get(index) == Some(&CallArgMode::Borrowed) {
                            let lowered = lower_expr_with_context(
                                arg,
                                context,
                                ExprPosition::CallArgBorrowed,
                                state,
                            );
                            if is_boxed_trait_object_type(&arg.ty) {
                                RustExpr::Call {
                                    callee: Box::new(RustExpr::Field {
                                        base: Box::new(lowered),
                                        field: "as_ref".to_string(),
                                    }),
                                    args: Vec::new(),
                                }
                            } else {
                                borrow_expr(lowered)
                            }
                        } else {
                            lower_expr_with_context(arg, context, ExprPosition::CallArgOwned, state)
                        }
                    })
                    .collect(),
            };
            if method_returns_option_item_via_get(callee) {
                RustExpr::Call {
                    callee: Box::new(RustExpr::Field {
                        base: Box::new(lowered_call),
                        field: "cloned".to_string(),
                    }),
                    args: Vec::new(),
                }
            } else {
                lowered_call
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
            let remaining_conflicting_before = if matches!(
                position,
                ExprPosition::ProjectionBase | ExprPosition::CallArgBorrowed
            ) {
                if position == ExprPosition::CallArgBorrowed {
                    context.ownership_plan.consume_expr(expr);
                }
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
            let (decision, forced_clone) = clone_decision_for_expr(
                expr,
                &expr.ty,
                position,
                remaining_conflicting_before,
                context,
                state,
            );
            match decision {
                CloneDecision::Clone { is_hot } => {
                    if !forced_clone && let Some(name) = place_name {
                        push_auto_clone_notes(state, context, &name, expr.ty.trim(), is_hot);
                    }
                    clone_expr(lowered_field)
                }
                CloneDecision::Move => lowered_field,
            }
        }
        TypedExprKind::Index {
            base,
            index,
            indexing,
        } => {
            let remaining_conflicting_before = if matches!(
                position,
                ExprPosition::ProjectionBase | ExprPosition::CallArgBorrowed
            ) {
                if position == ExprPosition::CallArgBorrowed {
                    context.ownership_plan.consume_expr(expr);
                }
                0
            } else {
                let remaining = context.ownership_plan.remaining_conflicting_for_expr(expr);
                context.ownership_plan.consume_expr(expr);
                remaining
            };
            let place_name = place_for_expr(expr).map(|place| place.display_name());
            let base_ty = base.ty.clone();
            let base_sem = sem_type_from_typed_type_string(&base_ty);
            let base_type_path = match &base_sem {
                SemType::Path { path, .. } => Some(path.clone()),
                _ => None,
            };
            let lowered_base =
                lower_expr_with_context(base, context, ExprPosition::ProjectionBase, state);
            let key_position = match indexing.key_passing {
                TypedIndexKeyPassing::Borrowed => ExprPosition::CallArgBorrowed,
                TypedIndexKeyPassing::Owned => ExprPosition::CallArgOwned,
                TypedIndexKeyPassing::CloneIfNeeded => ExprPosition::Value,
            };
            let clone_reused_custom_key = indexing.source == TypedIndexSource::CustomMethod
                && indexing.key_passing == TypedIndexKeyPassing::Owned
                && should_clone_for_reuse(index.ty.trim(), state)
                && context.ownership_plan.remaining_conflicting_for_expr(index) > 0;
            let lowered_key = lower_index_expr(index, &base_ty, key_position, context, state);
            let lowered_key_arg = match indexing.key_passing {
                TypedIndexKeyPassing::Borrowed => borrow_expr(lowered_key),
                TypedIndexKeyPassing::Owned => {
                    if clone_reused_custom_key {
                        clone_expr(lowered_key)
                    } else {
                        lowered_key
                    }
                }
                TypedIndexKeyPassing::CloneIfNeeded => clone_expr(lowered_key),
            };
            let lowered_index_expr = match indexing.mode {
                TypedIndexMode::DirectIndex => {
                    if indexing.source == TypedIndexSource::CustomMethod {
                        let mut method_path =
                            base_type_path.clone().unwrap_or_else(|| vec!["index".to_string()]);
                        method_path.push("index".to_string());
                        RustExpr::Call {
                            callee: Box::new(RustExpr::Path(method_path)),
                            args: vec![lowered_base, lowered_key_arg],
                        }
                    } else {
                        let mut lowered = RustExpr::Index {
                            base: Box::new(lowered_base),
                            index: Box::new(lowered_key_arg),
                        };
                        if matches!(index.kind, TypedExprKind::Range { .. })
                            && last_path_segment(&expr.ty) == "Vec"
                        {
                            lowered = RustExpr::Call {
                                callee: Box::new(RustExpr::Field {
                                    base: Box::new(lowered),
                                    field: "to_vec".to_string(),
                                }),
                                args: Vec::new(),
                            };
                        }
                        lowered
                    }
                }
                TypedIndexMode::GetLikeOption => {
                    if indexing.source == TypedIndexSource::CustomMethod {
                        let mut method_path =
                            base_type_path.clone().unwrap_or_else(|| vec!["get".to_string()]);
                        method_path.push("get".to_string());
                        RustExpr::Call {
                            callee: Box::new(RustExpr::Path(method_path)),
                            args: vec![lowered_base, lowered_key_arg],
                        }
                    } else {
                        RustExpr::Call {
                            callee: Box::new(RustExpr::Field {
                                base: Box::new(RustExpr::Call {
                                    callee: Box::new(RustExpr::Field {
                                        base: Box::new(lowered_base),
                                        field: "get".to_string(),
                                    }),
                                    args: vec![lowered_key_arg],
                                }),
                                field: "cloned".to_string(),
                            }),
                            args: Vec::new(),
                        }
                    }
                }
                TypedIndexMode::Unknown => RustExpr::Index {
                    base: Box::new(lowered_base),
                    index: Box::new(lowered_key_arg),
                },
            };
            let (decision, forced_clone) = clone_decision_for_expr(
                expr,
                &expr.ty,
                position,
                remaining_conflicting_before,
                context,
                state,
            );
            match decision {
                CloneDecision::Clone { is_hot } => {
                    if !forced_clone && let Some(name) = place_name {
                        push_auto_clone_notes(state, context, &name, expr.ty.trim(), is_hot);
                    }
                    clone_expr(lowered_index_expr)
                }
                CloneDecision::Move => lowered_index_expr,
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
                lower_expr_with_context(inner, context, ExprPosition::OwnedOperand, state);
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
        TypedExprKind::Cast {
            expr: inner,
            target_type,
        } => RustExpr::Cast {
            expr: Box::new(lower_expr_with_context(
                inner,
                context,
                ExprPosition::Value,
                state,
            )),
            ty: target_type.clone(),
        },
        TypedExprKind::Binary { op, left, right } => {
            let operand_position = if matches!(
                op,
                TypedBinaryOp::Add
                    | TypedBinaryOp::Sub
                    | TypedBinaryOp::Mul
                    | TypedBinaryOp::Div
                    | TypedBinaryOp::Rem
            ) {
                ExprPosition::OwnedOperand
            } else {
                ExprPosition::Value
            };
            let mut lowered_left = lower_expr_with_context(left, context, operand_position, state);
            let mut lowered_right =
                lower_expr_with_context(right, context, operand_position, state);

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
                lowered_right = cast_expr_to_numeric_if_needed(lowered_right, &right.ty, &expr.ty);
            } else if matches!(op, TypedBinaryOp::Add)
                && type_is_string_like(&expr.ty)
                && type_is_string_like(&left.ty)
                && type_is_string_like(&right.ty)
                && !matches!(lowered_right, RustExpr::Borrow(_))
            {
                // Rust `String +` expects a borrowed RHS (`&str`-compatible).
                lowered_right = borrow_expr(lowered_right);
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
        TypedExprKind::Block { body, tail } => {
            let mut block_context = LoweringContext {
                ownership_plan: OwnershipPlan::from_stmts(body),
                scope_name: format!("{}::<block>", context.scope_name),
                loop_depth: context.loop_depth,
                ..LoweringContext::default()
            };
            RustExpr::Block {
                body: body
                    .iter()
                    .map(|stmt| lower_stmt_with_context(stmt, &mut block_context, state))
                    .collect(),
                tail: tail.as_ref().map(|expr| {
                    Box::new(lower_expr_with_context(
                        expr,
                        &mut block_context,
                        ExprPosition::Value,
                        state,
                    ))
                }),
            }
        }
        TypedExprKind::Closure {
            params,
            return_type,
            body,
        } => {
            let mut closure_context = LoweringContext {
                ownership_plan: OwnershipPlan::from_stmts(body),
                scope_name: format!("{}::<closure>", context.scope_name),
                loop_depth: context.loop_depth,
                ..LoweringContext::default()
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

fn lower_map_from_vec_associated_from_call(
    callee: &TypedExpr,
    args: &[TypedExpr],
    context: &mut LoweringContext,
    state: &mut LoweringState,
) -> Option<RustExpr> {
    let TypedExprKind::Path(path) = &callee.kind else {
        return None;
    };
    if args.len() != 1 || path.len() < 2 {
        return None;
    }
    if path.last().is_none_or(|segment| segment != "from") {
        return None;
    }
    let receiver = path[path.len() - 2].as_str();
    if receiver != "HashMap" && receiver != "BTreeMap" {
        return None;
    }
    let arg_sem = sem_type_from_typed_type_string(&args[0].ty);
    let takes_vec_tuple = matches!(
        arg_sem,
        SemType::Path { path, args }
            if path.last().is_some_and(|segment| segment == "Vec")
                && matches!(args.first(), Some(SemType::Tuple(items)) if items.len() == 2)
    );
    if !takes_vec_tuple {
        return None;
    }
    let lowered_arg = lower_expr_with_context(&args[0], context, ExprPosition::CallArgOwned, state);
    let iter_arg = RustExpr::Call {
        callee: Box::new(RustExpr::Field {
            base: Box::new(lowered_arg),
            field: "into_iter".to_string(),
        }),
        args: Vec::new(),
    };
    let mut from_iter_path = path.clone();
    if let Some(last) = from_iter_path.last_mut() {
        *last = "from_iter".to_string();
    }
    Some(RustExpr::Call {
        callee: Box::new(RustExpr::Path(from_iter_path)),
        args: vec![iter_arg],
    })
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
    if position == ExprPosition::CallArgBorrowed {
        if path.len() == 1 {
            context.ownership_plan.consume_expr(expr);
        }
        return RustExpr::Path(path.to_vec());
    }
    if path.len() != 1 {
        return RustExpr::Path(path.to_vec());
    }

    let name = path[0].clone();
    let remaining_conflicting = context.ownership_plan.remaining_conflicting_for_expr(expr);
    context.ownership_plan.consume_expr(expr);

    let path_expr = RustExpr::Path(path.to_vec());
    let (decision, forced_clone) =
        clone_decision_for_expr(expr, ty, position, remaining_conflicting, context, state);
    if let CloneDecision::Clone { is_hot } = decision {
        if !forced_clone {
            push_auto_clone_notes(state, context, &name, ty.trim(), is_hot);
        }
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

fn cast_integral_expr_for_coercion(expr: RustExpr, from_ty: &str, to_ty: &str) -> RustExpr {
    if from_ty == to_ty {
        return expr;
    }
    let Some(from_spec) = integral_type_spec(from_ty) else {
        return cast_expr(expr, to_ty.to_string());
    };
    let Some(to_spec) = integral_type_spec(to_ty) else {
        return cast_expr(expr, to_ty.to_string());
    };
    if from_spec.signed && !to_spec.signed {
        let abs_base = cast_expr(expr, from_ty.to_string());
        let abs_expr = RustExpr::Call {
            callee: Box::new(RustExpr::Field {
                base: Box::new(abs_base),
                field: "saturating_abs".to_string(),
            }),
            args: Vec::new(),
        };
        return cast_expr(abs_expr, to_ty.to_string());
    }
    cast_expr(expr, to_ty.to_string())
}

fn cast_expr_to_numeric_if_needed(expr: RustExpr, from_ty: &str, to_ty: &str) -> RustExpr {
    if from_ty == to_ty {
        return expr;
    }
    if !is_numeric_type_name(from_ty) || !is_numeric_type_name(to_ty) {
        return expr;
    }
    if is_integral_numeric_type_name(from_ty) && is_integral_numeric_type_name(to_ty) {
        return cast_integral_expr_for_coercion(expr, from_ty, to_ty);
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

fn clone_decision_for_expr(
    expr: &TypedExpr,
    ty: &str,
    position: ExprPosition,
    remaining_conflicting_uses: usize,
    context: &LoweringContext,
    state: &LoweringState,
) -> (CloneDecision, bool) {
    let in_owned_position = matches!(
        position,
        ExprPosition::CallArgOwned | ExprPosition::OwnedOperand
    );
    let forced_clone = in_owned_position && state.is_forced_clone_expr(expr);
    let decision = decide_clone(ClonePlannerInput {
        ty,
        in_owned_position,
        remaining_conflicting_uses,
        forced_clone,
        preserve_rebind_move: should_preserve_rebind_move(expr, context),
        clone_candidate: should_clone_for_reuse(ty, state),
        loop_depth: context.loop_depth,
    });
    (decision, forced_clone)
}

fn should_preserve_rebind_move(expr: &TypedExpr, context: &LoweringContext) -> bool {
    let Some(rebind) = &context.rebind_place else {
        return false;
    };
    let Some(place) = place_for_expr(expr) else {
        return false;
    };
    place == *rebind
}

fn push_auto_clone_notes(
    state: &mut LoweringState,
    context: &LoweringContext,
    name: &str,
    ty: &str,
    is_hot: bool,
) {
    let scope = if context.scope_name.is_empty() {
        "<module>"
    } else {
        context.scope_name.as_str()
    };
    state.push_ownership_note(format!(
        "auto-clone inserted in `{scope}` for `{name}` of type `{}`",
        ty.trim()
    ));
    if is_hot {
        state.push_ownership_note(format!(
            "hot-clone:auto place=`{name}` type=`{}` scope=`{scope}` loop-depth={}",
            ty.trim(),
            context.loop_depth
        ));
    }
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
    if let Some(modes) = resolve_associated_call_modes(callee, args) {
        return (
            lower_expr_with_context(callee, context, ExprPosition::Value, state),
            modes,
        );
    }
    if let Some(modes) = resolve_direct_borrow_arg_modes(callee, state, arg_count) {
        return (
            lower_expr_with_context(callee, context, ExprPosition::Value, state),
            modes,
        );
    }
    if let Some(modes) = resolve_declared_borrow_arg_modes(callee, state, arg_count) {
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

fn method_returns_option_item_via_get(callee: &TypedExpr) -> bool {
    let TypedExprKind::Field { base, field } = &callee.kind else {
        return false;
    };
    if field != "get" {
        return false;
    }
    let base_sem = sem_type_from_typed_type_string(&base.ty);
    let mut diagnostics = Vec::new();
    matches!(
        resolve_index_capability(&base_sem, None, &mut diagnostics),
        Some(IndexCapability {
            mode: CapabilityIndexMode::GetLikeOption,
            ..
        })
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

    if let Some(type_name) = method_base_type_name(&base.ty) {
        let lookup = format!("{type_name}::{field}");
        if let Some(indexes) = state.known_function_borrowed_args.get(&lookup) {
            let mut arg_modes = vec![CallArgMode::Owned; args.len()];
            let mut receiver = CallArgMode::Owned;
            for index in indexes {
                if *index == 0 {
                    receiver = CallArgMode::Borrowed;
                } else {
                    set_borrowed(&mut arg_modes, *index - 1);
                }
            }
            return Some(MethodCallModes {
                receiver,
                args: arg_modes,
            });
        }
    }

    let base_sem = sem_type_from_typed_type_string(&base.ty);
    let arg_sem_types = args
        .iter()
        .map(|arg| sem_type_from_typed_type_string(&arg.ty))
        .collect::<Vec<_>>();
    let mut cap_diagnostics = Vec::new();
    if let Some(capability) = resolve_method_capability(
        &base_sem,
        field,
        arg_sem_types.len(),
        &Context::default(),
        &mut cap_diagnostics,
    ) {
        let receiver = match capability.receiver_mode {
            CapabilityReceiverMode::Owned => CallArgMode::Owned,
            CapabilityReceiverMode::Borrowed => CallArgMode::Borrowed,
        };
        let mut arg_modes = capability.arg_modes;
        if arg_modes.len() < args.len() {
            arg_modes.resize(args.len(), CallArgMode::Owned);
        }
        return Some(MethodCallModes {
            receiver,
            args: arg_modes,
        });
    }

    let mut arg_modes = vec![CallArgMode::Owned; args.len()];
    let receiver = if method_receiver_should_borrow_heuristic(&base.ty, field) {
        for (index, arg) in args.iter().enumerate() {
            if method_arg_should_borrow_heuristic(field, arg, context, state, true) {
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

fn resolve_associated_call_modes(
    callee: &TypedExpr,
    args: &[TypedExpr],
) -> Option<Vec<CallArgMode>> {
    let TypedExprKind::Path(path) = &callee.kind else {
        return None;
    };
    if path.len() < 2 || args.is_empty() {
        return None;
    }
    let method = path.last()?;
    let receiver_sem = sem_type_from_typed_type_string(&args[0].ty);
    if !uses_builtin_capability_catalog(&receiver_sem) {
        return None;
    }
    let mut diagnostics = Vec::new();
    let capability = resolve_method_capability(
        &receiver_sem,
        method,
        args.len() - 1,
        &Context::default(),
        &mut diagnostics,
    )?;
    let mut modes = vec![CallArgMode::Owned; args.len()];
    modes[0] = match capability.receiver_mode {
        CapabilityReceiverMode::Owned => CallArgMode::Owned,
        CapabilityReceiverMode::Borrowed => CallArgMode::Borrowed,
    };
    for (index, mode) in capability.arg_modes.iter().enumerate() {
        if let Some(slot) = modes.get_mut(index + 1) {
            *slot = *mode;
        }
    }
    Some(modes)
}

fn uses_builtin_capability_catalog(ty: &SemType) -> bool {
    match ty {
        SemType::Iter(_) => true,
        SemType::Path { path, .. } => matches!(
            path.last().map(|segment| segment.as_str()),
            Some("String")
                | Some("Vec")
                | Some("Option")
                | Some("Result")
                | Some("HashMap")
                | Some("BTreeMap")
                | Some("HashSet")
                | Some("BTreeSet")
        ),
        _ => false,
    }
}

fn method_base_type_name(base_ty: &str) -> Option<&str> {
    let (head, _) = split_type_head_and_args(base_ty.trim());
    let name = last_path_segment(head);
    if name.is_empty() { None } else { Some(name) }
}

fn resolve_declared_borrow_arg_modes(
    callee: &TypedExpr,
    state: &LoweringState,
    arg_count: usize,
) -> Option<Vec<CallArgMode>> {
    let TypedExprKind::Path(path) = &callee.kind else {
        return None;
    };
    let resolved = state.resolve_callee_path(path);
    state.resolve_declared_borrow_modes(&resolved, arg_count)
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
    let resolved = state.resolve_callee_path(path);
    if is_known_function_path(state, &resolved) {
        return None;
    }
    let Some(name) = path.last() else {
        return None;
    };
    if !call_name_suggests_read_only(name) && !call_name_prefers_borrow_heuristic(name) {
        return None;
    }
    let mut modes = vec![CallArgMode::Owned; args.len()];
    let mut borrowed_any = false;
    for (index, arg) in args.iter().enumerate() {
        if path_arg_should_borrow_heuristic(name, arg, context, state, true) {
            set_borrowed(&mut modes, index);
            borrowed_any = true;
        }
    }
    if borrowed_any { Some(modes) } else { None }
}

fn is_known_function_path(state: &LoweringState, path: &[String]) -> bool {
    if state.known_functions.contains(&path.join("::")) {
        return true;
    }
    if let Some(name) = path.last()
        && state.known_functions.contains(name)
    {
        return true;
    }
    if path.len() >= 2 {
        let short = format!("{}::{}", path[path.len() - 2], path[path.len() - 1]);
        if state.known_functions.contains(&short) {
            return true;
        }
    }
    false
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
    allow_nominal: bool,
) -> bool {
    if !call_name_suggests_read_only(method) {
        return false;
    }
    if type_is_string_like(&arg.ty) {
        return true;
    }
    let remaining_conflicting = context.ownership_plan.remaining_conflicting_for_expr(arg);
    if remaining_conflicting <= 1 {
        return false;
    }
    if is_known_borrow_container_type(&arg.ty) {
        return type_should_prefer_borrow(&arg.ty, state);
    }
    let ty = arg.ty.trim();
    let head = last_path_segment(ty);
    if allow_nominal && is_probably_nominal_type(head) && !is_copy_primitive_type(head) {
        return true;
    }
    if !allow_nominal {
        return false;
    }
    type_should_prefer_borrow(&arg.ty, state)
}

fn path_arg_should_borrow_heuristic(
    callee_name: &str,
    arg: &TypedExpr,
    context: &LoweringContext,
    state: &LoweringState,
    allow_nominal: bool,
) -> bool {
    let remaining_conflicting = context.ownership_plan.remaining_conflicting_for_expr(arg);
    let optimistic_borrow_call = call_name_prefers_borrow_heuristic(callee_name);
    let read_only_call = call_name_suggests_read_only(callee_name);
    if !read_only_call && !optimistic_borrow_call {
        return false;
    }

    if read_only_call && type_is_string_like(&arg.ty) {
        return true;
    }

    if optimistic_borrow_call && !read_only_call {
        if !expr_is_borrowable_place(arg) {
            return false;
        }
        if !is_known_borrow_container_type(&arg.ty) {
            return false;
        }
        return true;
    }

    if remaining_conflicting <= 1 {
        return false;
    }

    if optimistic_borrow_call && !expr_is_borrowable_place(arg) {
        return false;
    }

    if is_known_borrow_container_type(&arg.ty) {
        return type_should_prefer_borrow(&arg.ty, state);
    }

    let ty = arg.ty.trim();
    let head = last_path_segment(ty);
    if allow_nominal && is_probably_nominal_type(head) && !is_copy_primitive_type(head) {
        return false;
    }

    if !allow_nominal {
        return false;
    }

    false
}

fn expr_is_borrowable_place(expr: &TypedExpr) -> bool {
    matches!(
        expr.kind,
        TypedExprKind::Path(_) | TypedExprKind::Field { .. } | TypedExprKind::Index { .. }
    )
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

fn is_known_borrow_container_type(ty: &str) -> bool {
    matches!(
        last_path_segment(ty.trim()),
        "Vec" | "HashMap" | "BTreeMap" | "HashSet" | "BTreeSet" | "Option" | "Result"
    )
}

fn lower_index_expr(
    index: &TypedExpr,
    base_ty: &str,
    position: ExprPosition,
    context: &mut LoweringContext,
    state: &mut LoweringState,
) -> RustExpr {
    let vec_like = last_path_segment(base_ty.trim()) == "Vec";
    match &index.kind {
        TypedExprKind::Range {
            start,
            end,
            inclusive,
        } => RustExpr::Range {
            start: start
                .as_ref()
                .map(|value| Box::new(lower_index_range_bound(value, vec_like, context, state))),
            end: end
                .as_ref()
                .map(|value| Box::new(lower_index_range_bound(value, vec_like, context, state))),
            inclusive: *inclusive,
        },
        _ => {
            let lowered = lower_expr_with_context(index, context, position, state);
            let index_name = last_path_segment(index.ty.trim());
            if vec_like && is_integral_numeric_type_name(index_name) && index_name != "usize" {
                cast_integral_expr_for_coercion(lowered, index_name, "usize")
            } else {
                lowered
            }
        }
    }
}

fn lower_index_range_bound(
    expr: &TypedExpr,
    cast_to_usize: bool,
    context: &mut LoweringContext,
    state: &mut LoweringState,
) -> RustExpr {
    let lowered = lower_expr_with_context(expr, context, ExprPosition::Value, state);
    let expr_name = last_path_segment(expr.ty.trim());
    if cast_to_usize && is_integral_numeric_type_name(expr_name) && expr_name != "usize" {
        cast_integral_expr_for_coercion(lowered, expr_name, "usize")
    } else {
        lowered
    }
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

fn call_name_prefers_borrow_heuristic(name: &str) -> bool {
    if name.starts_with("draw_")
        || name.starts_with("render_")
        || name.starts_with("display_")
        || name.starts_with("show_")
        || name.starts_with("print_")
        || name.starts_with("log_")
        || name.starts_with("trace_")
        || name.starts_with("debug_")
        || name.starts_with("inspect_")
        || name.ends_with("_scene")
    {
        return true;
    }
    false
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
                    is_const: true,
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

fn borrowed_param_indexes(params: &[TypedParam]) -> Vec<usize> {
    params
        .iter()
        .enumerate()
        .filter_map(|(index, param)| {
            if param.ty.trim_start().starts_with('&') {
                Some(index)
            } else {
                None
            }
        })
        .collect()
}

fn last_path_segment(path: &str) -> &str {
    let segment = path.rsplit("::").next().unwrap_or(path);
    segment.split('<').next().unwrap_or(segment)
}

fn sem_type_from_typed_type_string(ty: &str) -> SemType {
    let trimmed = ty.trim();
    if trimmed == "_" {
        return SemType::Unknown;
    }
    if trimmed == "()" {
        return SemType::Unit;
    }
    if let Some(items) = parse_tuple_items(trimmed) {
        return SemType::Tuple(
            items
                .into_iter()
                .map(sem_type_from_typed_type_string)
                .collect(),
        );
    }
    let (head, args) = split_type_head_and_args(trimmed);
    let path = head
        .split("::")
        .filter(|segment| !segment.is_empty())
        .map(|segment| segment.to_string())
        .collect::<Vec<_>>();
    if path.is_empty() {
        return SemType::Unknown;
    }
    SemType::Path {
        path,
        args: args
            .into_iter()
            .map(sem_type_from_typed_type_string)
            .collect(),
    }
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

fn is_boxed_trait_object_type(ty: &str) -> bool {
    let trimmed = ty.trim();
    trimmed.starts_with("Box<dyn ") || trimmed.starts_with("Box<(dyn ")
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
    const LOOP_BODY_WEIGHT: usize = 3;
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
            let mut loop_uses = HashMap::new();
            collect_place_uses_in_expr(condition, &mut loop_uses);
            for stmt in body {
                collect_place_uses_in_stmt(stmt, &mut loop_uses);
            }
            merge_weighted_place_uses(uses, &loop_uses, LOOP_BODY_WEIGHT);
        }
        TypedStmt::For { iter, body, .. } => {
            collect_place_uses_in_expr(iter, uses);
            let mut loop_uses = HashMap::new();
            for stmt in body {
                collect_place_uses_in_stmt(stmt, &mut loop_uses);
            }
            merge_weighted_place_uses(uses, &loop_uses, LOOP_BODY_WEIGHT);
        }
        TypedStmt::Loop { body } => {
            let mut loop_uses = HashMap::new();
            for stmt in body {
                collect_place_uses_in_stmt(stmt, &mut loop_uses);
            }
            merge_weighted_place_uses(uses, &loop_uses, LOOP_BODY_WEIGHT);
        }
        TypedStmt::Break | TypedStmt::Continue => {}
        TypedStmt::RustBlock(_) => {}
        TypedStmt::Expr(expr) => collect_place_uses_in_expr(expr, uses),
    }
}

fn merge_weighted_place_uses(
    target: &mut HashMap<OwnershipPlace, usize>,
    source: &HashMap<OwnershipPlace, usize>,
    weight: usize,
) {
    for (place, count) in source {
        let weighted = count.saturating_mul(weight);
        let slot = target.entry(place.clone()).or_insert(0);
        *slot = slot.saturating_add(weighted);
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
        TypedExprKind::Cast { expr, .. } => collect_place_uses_in_expr(expr, uses),
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
        TypedExprKind::Block { body, tail } => {
            for stmt in body {
                collect_place_uses_in_stmt(stmt, uses);
            }
            if let Some(tail) = tail {
                collect_place_uses_in_expr(tail, uses);
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
        | TypedExprKind::Float(_)
        | TypedExprKind::Bool(_)
        | TypedExprKind::String(_)
        | TypedExprKind::Char(_)
        | TypedExprKind::Path(_) => {}
    }
}

fn type_from_ast_in_context(ty: &Type, context: &Context) -> SemType {
    promote_trait_shorthand(type_from_ast(ty), context)
}

fn type_from_ast_with_impl_self_in_context(
    ty: &Type,
    impl_target: &str,
    impl_target_args: &[Type],
    context: &Context,
) -> SemType {
    promote_trait_shorthand(
        type_from_ast_with_impl_self(ty, impl_target, impl_target_args),
        context,
    )
}

fn promote_trait_shorthand(ty: SemType, context: &Context) -> SemType {
    match ty {
        SemType::Path { path, args } => {
            let args = args
                .into_iter()
                .map(|arg| promote_trait_shorthand(arg, context))
                .collect::<Vec<_>>();
            if path.len() == 1 && args.is_empty() && context.traits.contains_key(&path[0]) {
                SemType::TraitObject(vec![SemType::Path {
                    path,
                    args: Vec::new(),
                }])
            } else {
                SemType::Path { path, args }
            }
        }
        SemType::TraitObject(bounds) => SemType::TraitObject(
            bounds
                .into_iter()
                .map(|bound| promote_trait_shorthand(bound, context))
                .collect(),
        ),
        SemType::Tuple(items) => SemType::Tuple(
            items
                .into_iter()
                .map(|item| promote_trait_shorthand(item, context))
                .collect(),
        ),
        SemType::Fn { params, ret } => SemType::Fn {
            params: params
                .into_iter()
                .map(|param| promote_trait_shorthand(param, context))
                .collect(),
            ret: Box::new(promote_trait_shorthand(*ret, context)),
        },
        SemType::Iter(item) => SemType::Iter(Box::new(promote_trait_shorthand(*item, context))),
        SemType::Unit | SemType::Unknown => ty,
    }
}

fn quiche_macro_target_type(expr: &Expr, context: &Context) -> SemType {
    match expr {
        Expr::Path(path) => promote_trait_shorthand(
            SemType::Path {
                path: path.clone(),
                args: Vec::new(),
            },
            context,
        ),
        Expr::PathWithTypeArgs { path, type_args } => promote_trait_shorthand(
            SemType::Path {
                path: path.clone(),
                args: type_args.iter().map(type_from_ast).collect(),
            },
            context,
        ),
        _ => SemType::Unknown,
    }
}

fn merged_impl_and_method_type_params(
    impl_params: &[GenericParam],
    method_params: &[GenericParam],
) -> Vec<GenericParam> {
    let mut merged = impl_params.to_vec();
    for param in method_params {
        if merged.iter().all(|existing| existing.name != param.name) {
            merged.push(param.clone());
        }
    }
    merged
}

fn resolve_impl_method_signature(
    method: &crate::ast::FunctionDef,
    imp: &crate::ast::ImplBlock,
    context: &Context,
    diagnostics: &mut Vec<Diagnostic>,
) -> ResolvedImplMethodSignature {
    let mut resolved = ResolvedImplMethodSignature {
        param_sem_types: method
            .params
            .iter()
            .map(|param| {
                type_from_ast_with_impl_self_in_context(
                    &param.ty,
                    &imp.target,
                    &imp.target_args,
                    context,
                )
            })
            .collect(),
        return_sem_type: method
            .return_type
            .as_ref()
            .map(|ty| {
                type_from_ast_with_impl_self_in_context(ty, &imp.target, &imp.target_args, context)
            })
            .unwrap_or(SemType::Unknown),
        param_rust_types: None,
        return_rust_type: None,
    };
    let Some(trait_target) = imp.trait_target.as_ref() else {
        return resolved;
    };
    let trait_ty = type_from_ast_in_context(trait_target, context);
    let Some(trait_name) = trait_name_from_type(&trait_ty) else {
        return resolved;
    };

    let signature_override = resolve_rustdex_trait_method_signature(
        trait_name,
        &method.name,
        &imp.target,
        &imp.target_args,
        context,
        diagnostics,
    )
    .or_else(|| {
        resolve_std_trait_method_signature(
            trait_name,
            &method.name,
            &imp.target,
            &imp.target_args,
            context,
        )
    });
    let Some(signature_override) = signature_override else {
        return resolved;
    };

    if signature_override.param_sem_types.len() > resolved.param_sem_types.len() {
        diagnostics.push(Diagnostic::new(
            format!(
                "Method `{}` in trait `{}` requires at least {} parameter(s), but impl declares {}",
                method.name,
                trait_name,
                signature_override.param_sem_types.len(),
                resolved.param_sem_types.len()
            ),
            default_diag_span(),
        ));
        return resolved;
    }

    for (index, inferred_ty) in signature_override.param_sem_types.iter().enumerate() {
        resolved.param_sem_types[index] = inferred_ty.clone();
    }
    resolved.return_sem_type = signature_override.return_sem_type.clone();

    let mut inferred_rust_params = resolved
        .param_sem_types
        .iter()
        .map(rust_param_type_string)
        .collect::<Vec<_>>();
    for (index, rust_ty) in signature_override.param_rust_types.iter().enumerate() {
        if index < inferred_rust_params.len() {
            inferred_rust_params[index] = rust_ty.clone();
        }
    }
    resolved.param_rust_types = Some(inferred_rust_params);
    resolved.return_rust_type = Some(signature_override.return_rust_type);
    resolved
}

fn resolve_rustdex_trait_method_signature(
    trait_name: &str,
    method_name: &str,
    impl_target: &str,
    impl_target_args: &[Type],
    context: &Context,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<TraitMethodSignatureOverride> {
    let signature = match rustdex_backend::trait_method_signature(trait_name, method_name) {
        Ok(signature) => signature,
        Err(rustdex_backend::RustdexError::SignatureUnavailable { .. }) => return None,
        Err(err) => {
            diagnostics.push(Diagnostic::new(
                format!(
                    "E_CAPABILITY_METADATA_MISSING: rustdex trait signature metadata unavailable for `{trait_name}::{method_name}` ({err:?})"
                ),
                default_diag_span(),
            ));
            return None;
        }
    };
    let impl_target_sem = sem_type_for_impl_target(impl_target, impl_target_args, context);
    let mut param_rust_types = signature.param_rust_types;
    if param_rust_types.is_empty() {
        return None;
    }
    let param_sem_types = param_rust_types
        .iter()
        .map(|ty| sem_type_from_rust_signature_type(ty, &impl_target_sem))
        .collect::<Vec<_>>();
    let return_rust_type = signature.return_rust_type;
    if return_rust_type.is_empty() {
        return None;
    }
    let return_sem_type = sem_type_from_rust_signature_type(&return_rust_type, &impl_target_sem);
    Some(TraitMethodSignatureOverride {
        param_sem_types,
        return_sem_type,
        param_rust_types: std::mem::take(&mut param_rust_types),
        return_rust_type,
    })
}

fn resolve_std_trait_method_signature(
    trait_name: &str,
    method_name: &str,
    impl_target: &str,
    impl_target_args: &[Type],
    context: &Context,
) -> Option<TraitMethodSignatureOverride> {
    if method_name != "fmt" {
        return None;
    }
    if !matches!(
        trait_name,
        "Display"
            | "Debug"
            | "Binary"
            | "Octal"
            | "LowerHex"
            | "UpperHex"
            | "LowerExp"
            | "UpperExp"
            | "Pointer"
    ) {
        return None;
    }
    let target_sem = sem_type_for_impl_target(impl_target, impl_target_args, context);
    let target_rust = rust_owned_type_string(&target_sem);
    Some(TraitMethodSignatureOverride {
        param_sem_types: vec![
            target_sem,
            SemType::Path {
                path: vec![
                    "std".to_string(),
                    "fmt".to_string(),
                    "Formatter".to_string(),
                ],
                args: Vec::new(),
            },
        ],
        return_sem_type: SemType::Path {
            path: vec!["std".to_string(), "fmt".to_string(), "Result".to_string()],
            args: Vec::new(),
        },
        param_rust_types: vec![
            format!("&{target_rust}"),
            "&mut std::fmt::Formatter<'_>".to_string(),
        ],
        return_rust_type: "std::fmt::Result".to_string(),
    })
}

fn sem_type_for_impl_target(
    impl_target: &str,
    impl_target_args: &[Type],
    context: &Context,
) -> SemType {
    SemType::Path {
        path: impl_target
            .split("::")
            .map(|segment| segment.to_string())
            .collect(),
        args: impl_target_args
            .iter()
            .map(|arg| type_from_ast_in_context(arg, context))
            .collect(),
    }
}

fn sem_type_from_rust_signature_type(ty: &str, impl_target: &SemType) -> SemType {
    let mut inner = ty.trim();
    if inner == "self" {
        return impl_target.clone();
    }
    while let Some(stripped) = inner.strip_prefix('&') {
        inner = stripped.trim_start();
        if let Some(stripped_mut) = inner.strip_prefix("mut ") {
            inner = stripped_mut.trim_start();
        }
    }
    if let Some(stripped_mut) = inner.strip_prefix("mut ") {
        inner = stripped_mut.trim_start();
    }
    inner = inner.trim_matches(|ch| ch == '(' || ch == ')');
    if inner == "Self" {
        return impl_target.clone();
    }
    if inner == "_" {
        return SemType::Unknown;
    }
    if inner == "()" {
        return SemType::Unit;
    }
    let without_generics = inner.split('<').next().unwrap_or(inner).trim();
    if without_generics.is_empty() {
        return SemType::Unknown;
    }
    SemType::Path {
        path: without_generics
            .split("::")
            .map(|segment| segment.to_string())
            .collect(),
        args: Vec::new(),
    }
}

fn impl_lookup_name(target: &str) -> String {
    target.rsplit("::").next().unwrap_or(target).to_string()
}

fn is_redundant_impl_self_param(params: &[crate::ast::Param], index: usize) -> bool {
    if index == 0 {
        return false;
    }
    let Some(first) = params.first() else {
        return false;
    };
    first.name == "self" && params[index].name == "self"
}

fn type_from_ast(ty: &Type) -> SemType {
    if ty.path.len() == 1 && ty.path[0] == "_" && ty.args.is_empty() && ty.trait_bounds.is_empty() {
        return SemType::Unknown;
    }
    if !ty.trait_bounds.is_empty() {
        let mut bounds = Vec::with_capacity(1 + ty.trait_bounds.len());
        bounds.push(SemType::Path {
            path: ty.path.clone(),
            args: ty.args.iter().map(type_from_ast).collect(),
        });
        bounds.extend(ty.trait_bounds.iter().map(type_from_ast));
        return SemType::TraitObject(bounds);
    }
    if ty.path.len() == 1 && ty.path[0] == "Tuple" {
        return SemType::Tuple(ty.args.iter().map(type_from_ast).collect());
    }
    SemType::Path {
        path: ty.path.clone(),
        args: ty.args.iter().map(type_from_ast).collect(),
    }
}

fn type_from_ast_with_impl_self(
    ty: &Type,
    impl_target: &str,
    impl_target_args: &[Type],
) -> SemType {
    if ty.path.len() == 1 && ty.path[0] == "_" && ty.args.is_empty() && ty.trait_bounds.is_empty() {
        return SemType::Unknown;
    }
    if !ty.trait_bounds.is_empty() {
        let mut bounds = Vec::with_capacity(1 + ty.trait_bounds.len());
        let base = Type {
            path: ty.path.clone(),
            args: ty.args.clone(),
            trait_bounds: Vec::new(),
        };
        bounds.push(type_from_ast_with_impl_self(
            &base,
            impl_target,
            impl_target_args,
        ));
        bounds.extend(
            ty.trait_bounds
                .iter()
                .map(|arg| type_from_ast_with_impl_self(arg, impl_target, impl_target_args)),
        );
        return SemType::TraitObject(bounds);
    }
    if ty.path.len() == 1 && ty.path[0] == "Tuple" {
        return SemType::Tuple(
            ty.args
                .iter()
                .map(|arg| type_from_ast_with_impl_self(arg, impl_target, impl_target_args))
                .collect(),
        );
    }
    let head = ty.path.first().map(|part| part.as_str());
    if ty.path.len() == 1 && head == Some("Self") {
        return SemType::Path {
            path: vec![impl_target.to_string()],
            args: impl_target_args.iter().map(type_from_ast).collect(),
        };
    }
    SemType::Path {
        path: ty.path.clone(),
        args: ty
            .args
            .iter()
            .map(|arg| type_from_ast_with_impl_self(arg, impl_target, impl_target_args))
            .collect(),
    }
}

fn type_to_string(ty: &SemType) -> String {
    match ty {
        SemType::Unit => "()".to_string(),
        SemType::Unknown => "_".to_string(),
        SemType::TraitObject(bounds) => bounds
            .iter()
            .map(type_to_string)
            .collect::<Vec<_>>()
            .join(" + "),
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

fn rust_owned_type_string(ty: &SemType) -> String {
    rust_type_string(ty, RustTypeRenderMode::Owned)
}

fn rust_param_type_string(ty: &SemType) -> String {
    rust_type_string(ty, RustTypeRenderMode::ParamTopLevel)
}

fn rust_trait_bound_string(ty: &SemType) -> String {
    rust_type_string(ty, RustTypeRenderMode::TraitBound)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RustTypeRenderMode {
    Owned,
    ParamTopLevel,
    TraitBound,
}

fn rust_type_string(ty: &SemType, mode: RustTypeRenderMode) -> String {
    match ty {
        SemType::Unit => "()".to_string(),
        SemType::Unknown => "_".to_string(),
        SemType::TraitObject(bounds) => render_rust_trait_object(bounds, mode),
        SemType::Tuple(items) => {
            let body = items
                .iter()
                .map(|item| rust_type_string(item, RustTypeRenderMode::Owned))
                .collect::<Vec<_>>()
                .join(", ");
            format!("({body})")
        }
        SemType::Fn { params, ret } => {
            let params = params
                .iter()
                .map(|param| rust_type_string(param, RustTypeRenderMode::Owned))
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "fn({params}) -> {}",
                rust_type_string(ret, RustTypeRenderMode::Owned)
            )
        }
        SemType::Iter(item) => {
            format!(
                "Iter<{}>",
                rust_type_string(item, RustTypeRenderMode::Owned)
            )
        }
        SemType::Path { path, args } => {
            let head = path.join("::");
            if args.is_empty() {
                head
            } else {
                let args = args
                    .iter()
                    .map(|arg| rust_type_string(arg, RustTypeRenderMode::Owned))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{head}<{args}>")
            }
        }
    }
}

fn render_rust_trait_object(bounds: &[SemType], mode: RustTypeRenderMode) -> String {
    let bounds = bounds
        .iter()
        .map(|bound| rust_type_string(bound, RustTypeRenderMode::TraitBound))
        .collect::<Vec<_>>()
        .join(" + ");
    if bounds.is_empty() {
        return "_".to_string();
    }
    match mode {
        RustTypeRenderMode::TraitBound => bounds,
        RustTypeRenderMode::Owned => format!("Box<dyn {bounds}>"),
        RustTypeRenderMode::ParamTopLevel => {
            if bounds.contains('+') {
                format!("&(dyn {bounds})")
            } else {
                format!("&dyn {bounds}")
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
    if let SemType::TraitObject(bounds) = actual {
        return trait_object_has_trait(bounds, bound_name);
    }
    match bound_name {
        "Clone" => type_is_clone(actual),
        "Copy" => type_is_copy(actual),
        "Debug" => type_is_debug(actual),
        "Default" => type_is_default(actual),
        "PartialEq" => type_is_partial_eq(actual),
        "Eq" => type_is_eq(actual),
        "PartialOrd" => type_is_partial_ord(actual),
        "Ord" => type_is_ord(actual),
        "Hash" => type_is_hash(actual),
        _ => true,
    }
}

fn type_satisfies_bound_with_context(actual: &SemType, bound: &SemType, context: &Context) -> bool {
    if *actual == SemType::Unknown {
        return true;
    }
    let Some(bound_name) = bound_trait_name(bound) else {
        return true;
    };
    if let SemType::TraitObject(bounds) = actual {
        return trait_object_has_trait(bounds, bound_name);
    }
    let builtin_ok = match bound_name {
        "Clone" => Some(type_is_clone(actual)),
        "Copy" => Some(type_is_copy(actual)),
        "Debug" => Some(type_is_debug(actual)),
        "Default" => Some(type_is_default(actual)),
        "PartialEq" => Some(type_is_partial_eq(actual)),
        "Eq" => Some(type_is_eq(actual)),
        "PartialOrd" => Some(type_is_partial_ord(actual)),
        "Ord" => Some(type_is_ord(actual)),
        "Hash" => Some(type_is_hash(actual)),
        _ => None,
    };
    if let Some(ok) = builtin_ok {
        return ok;
    }
    if context.traits.contains_key(bound_name) {
        if let Some(type_name) = concrete_type_name(actual) {
            return type_implements_trait(type_name, bound_name, context);
        }
        return true;
    }
    true
}

fn concrete_type_name(ty: &SemType) -> Option<&str> {
    if let SemType::Path { path, args } = ty
        && args.is_empty()
    {
        return path.last().map(|part| part.as_str());
    }
    None
}

fn type_implements_trait(type_name: &str, trait_name: &str, context: &Context) -> bool {
    let Some(implemented_traits) = context.trait_impls.get(type_name) else {
        return false;
    };
    implemented_traits
        .iter()
        .any(|implemented| trait_satisfies_trait(implemented, trait_name, context))
}

fn trait_satisfies_trait(actual_trait: &str, required_trait: &str, context: &Context) -> bool {
    if actual_trait == required_trait {
        return true;
    }
    let mut stack = vec![actual_trait];
    let mut visited = HashSet::<String>::new();
    while let Some(next) = stack.pop() {
        if !visited.insert(next.to_string()) {
            continue;
        }
        let Some(bounds) = context.traits.get(next) else {
            continue;
        };
        for bound in bounds {
            if let Some(bound_name) = bound_trait_name(bound) {
                if bound_name == required_trait {
                    return true;
                }
                stack.push(bound_name);
            }
        }
    }
    false
}

fn bound_trait_name(bound: &SemType) -> Option<&str> {
    if let SemType::Path { path, .. } = bound {
        return path.last().map(|part| part.as_str());
    }
    None
}

fn trait_name_from_type(ty: &SemType) -> Option<&str> {
    bound_trait_name(ty).or_else(|| {
        if let SemType::TraitObject(bounds) = ty {
            return bounds.first().and_then(bound_trait_name);
        }
        None
    })
}

fn trait_object_has_trait(bounds: &[SemType], trait_name: &str) -> bool {
    bounds
        .iter()
        .any(|bound| bound_trait_name(bound) == Some(trait_name))
}

fn type_is_clone(ty: &SemType) -> bool {
    match ty {
        SemType::Unknown | SemType::Unit => true,
        SemType::TraitObject(bounds) => trait_object_has_trait(bounds, "Clone"),
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
                "Option" | "Result" | "Vec" | "HashMap" | "BTreeMap" | "HashSet" | "BTreeSet"
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
        SemType::TraitObject(_) => false,
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

fn type_is_debug(ty: &SemType) -> bool {
    match ty {
        SemType::Unknown | SemType::Unit => true,
        SemType::TraitObject(bounds) => trait_object_has_trait(bounds, "Debug"),
        SemType::Tuple(items) => items.iter().all(type_is_debug),
        SemType::Iter(item) => type_is_debug(item),
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
                "Option" | "Result" | "Vec" | "HashMap" | "BTreeMap" | "HashSet" | "BTreeSet"
            ) {
                return args.iter().all(type_is_debug);
            }
            is_probably_nominal_type(head)
        }
    }
}

fn type_is_default(ty: &SemType) -> bool {
    match ty {
        SemType::Unknown | SemType::Unit => true,
        SemType::TraitObject(bounds) => trait_object_has_trait(bounds, "Default"),
        SemType::Tuple(items) => items.iter().all(type_is_default),
        SemType::Iter(_) | SemType::Fn { .. } => false,
        SemType::Path { path, args } => {
            let Some(head) = path.last().map(|part| part.as_str()) else {
                return false;
            };
            if is_copy_primitive_type(head) || head == "String" {
                return true;
            }
            match head {
                "Option" | "Vec" | "HashMap" | "BTreeMap" | "HashSet" | "BTreeSet" => true,
                "Result" => args.iter().all(type_is_default),
                _ => false,
            }
        }
    }
}

fn type_is_partial_eq(ty: &SemType) -> bool {
    match ty {
        SemType::Unknown | SemType::Unit => true,
        SemType::TraitObject(bounds) => trait_object_has_trait(bounds, "PartialEq"),
        SemType::Tuple(items) => items.iter().all(type_is_partial_eq),
        SemType::Iter(_) | SemType::Fn { .. } => false,
        SemType::Path { path, args } => {
            let Some(head) = path.last().map(|part| part.as_str()) else {
                return false;
            };
            if is_copy_primitive_type(head) || head == "String" {
                return true;
            }
            if matches!(
                head,
                "Option" | "Result" | "Vec" | "HashMap" | "BTreeMap" | "HashSet" | "BTreeSet"
            ) {
                return args.iter().all(type_is_partial_eq);
            }
            false
        }
    }
}

fn type_is_eq(ty: &SemType) -> bool {
    match ty {
        SemType::TraitObject(bounds) => trait_object_has_trait(bounds, "Eq"),
        SemType::Path { path, args } => {
            let Some(head) = path.last().map(|part| part.as_str()) else {
                return false;
            };
            if is_float_primitive_type(head) {
                return false;
            }
            if args.is_empty() && is_copy_primitive_type(head) {
                return true;
            }
            if head == "String" {
                return true;
            }
            if matches!(
                head,
                "Option" | "Result" | "Vec" | "HashMap" | "BTreeMap" | "HashSet" | "BTreeSet"
            ) {
                return args.iter().all(type_is_eq);
            }
            false
        }
        SemType::Tuple(items) => items.iter().all(type_is_eq),
        SemType::Unknown | SemType::Unit => true,
        SemType::Iter(_) | SemType::Fn { .. } => false,
    }
}

fn type_is_partial_ord(ty: &SemType) -> bool {
    match ty {
        SemType::Unknown | SemType::Unit => true,
        SemType::TraitObject(bounds) => trait_object_has_trait(bounds, "PartialOrd"),
        SemType::Tuple(items) => items.iter().all(type_is_partial_ord),
        SemType::Iter(_) | SemType::Fn { .. } => false,
        SemType::Path { path, args } => {
            let Some(head) = path.last().map(|part| part.as_str()) else {
                return false;
            };
            if is_copy_primitive_type(head) || head == "String" {
                return true;
            }
            if matches!(head, "Option" | "Result" | "Vec") {
                return args.iter().all(type_is_partial_ord);
            }
            false
        }
    }
}

fn type_is_ord(ty: &SemType) -> bool {
    match ty {
        SemType::Unknown | SemType::Unit => true,
        SemType::TraitObject(bounds) => trait_object_has_trait(bounds, "Ord"),
        SemType::Tuple(items) => items.iter().all(type_is_ord),
        SemType::Iter(_) | SemType::Fn { .. } => false,
        SemType::Path { path, args } => {
            let Some(head) = path.last().map(|part| part.as_str()) else {
                return false;
            };
            if is_float_primitive_type(head) {
                return false;
            }
            if args.is_empty() && is_copy_primitive_type(head) {
                return true;
            }
            if head == "String" {
                return true;
            }
            if matches!(head, "Option" | "Result" | "Vec") {
                return args.iter().all(type_is_ord);
            }
            false
        }
    }
}

fn type_is_hash(ty: &SemType) -> bool {
    match ty {
        SemType::Unknown | SemType::Unit => true,
        SemType::TraitObject(bounds) => trait_object_has_trait(bounds, "Hash"),
        SemType::Tuple(items) => items.iter().all(type_is_hash),
        SemType::Iter(_) | SemType::Fn { .. } => false,
        SemType::Path { path, args } => {
            let Some(head) = path.last().map(|part| part.as_str()) else {
                return false;
            };
            if is_float_primitive_type(head) {
                return false;
            }
            if args.is_empty() && is_copy_primitive_type(head) {
                return true;
            }
            if head == "String" {
                return true;
            }
            if matches!(head, "Option" | "Result" | "Vec") {
                return args.iter().all(type_is_hash);
            }
            false
        }
    }
}

fn is_float_primitive_type(head: &str) -> bool {
    matches!(head, "f32" | "f64")
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
        (SemType::Path { path: ep, args: ea }, SemType::Path { path: ap, args: aa }) => {
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
        (SemType::TraitObject(_), _) => is_compatible(actual, expected),
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
                return bindings.get(&path[0]).cloned().unwrap_or(SemType::Unknown);
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
        SemType::TraitObject(bounds) => SemType::TraitObject(
            bounds
                .iter()
                .map(|bound| substitute_generic_type(bound, type_params, bindings))
                .collect(),
        ),
        SemType::Iter(item) => SemType::Iter(Box::new(substitute_generic_type(
            item,
            type_params,
            bindings,
        ))),
        SemType::Unit | SemType::Unknown => ty.clone(),
    }
}

fn substitute_bound_generic_type(
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
                    .unwrap_or_else(|| ty.clone());
            }
            ty.clone()
        }
        SemType::Path { path, args } => SemType::Path {
            path: path.clone(),
            args: args
                .iter()
                .map(|arg| substitute_bound_generic_type(arg, type_params, bindings))
                .collect(),
        },
        SemType::Tuple(items) => SemType::Tuple(
            items
                .iter()
                .map(|item| substitute_bound_generic_type(item, type_params, bindings))
                .collect(),
        ),
        SemType::Fn { params, ret } => SemType::Fn {
            params: params
                .iter()
                .map(|param| substitute_bound_generic_type(param, type_params, bindings))
                .collect(),
            ret: Box::new(substitute_bound_generic_type(ret, type_params, bindings)),
        },
        SemType::TraitObject(bounds) => SemType::TraitObject(
            bounds
                .iter()
                .map(|bound| substitute_bound_generic_type(bound, type_params, bindings))
                .collect(),
        ),
        SemType::Iter(item) => SemType::Iter(Box::new(substitute_bound_generic_type(
            item,
            type_params,
            bindings,
        ))),
        SemType::Unit | SemType::Unknown => ty.clone(),
    }
}

fn is_vector_index_type(ty: &SemType) -> bool {
    is_integral_numeric_type(ty)
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
        (_, SemType::TraitObject(bounds)) => bounds
            .iter()
            .all(|bound| type_satisfies_bound(actual, bound)),
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
            if numeric_coercion_enabled()
                && let (Some(actual_name), Some(expected_name)) = (
                    canonical_numeric_name(actual),
                    canonical_numeric_name(expected),
                )
                && is_integral_numeric_type_name(actual_name)
                && is_integral_numeric_type_name(expected_name)
                && can_implicitly_coerce_integral_name(actual_name, expected_name)
            {
                return true;
            }
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

fn merge_compatible_types(left: &SemType, right: &SemType) -> Option<SemType> {
    match (left, right) {
        (SemType::Unknown, _) => return Some(right.clone()),
        (_, SemType::Unknown) => return Some(left.clone()),
        (SemType::Unit, SemType::Unit) => return Some(SemType::Unit),
        (SemType::Unit, SemType::Tuple(items)) | (SemType::Tuple(items), SemType::Unit) => {
            if items.is_empty() {
                return Some(SemType::Unit);
            }
            return None;
        }
        (SemType::Tuple(left_items), SemType::Tuple(right_items)) => {
            if left_items.len() != right_items.len() {
                return None;
            }
            let mut merged = Vec::with_capacity(left_items.len());
            for (left_item, right_item) in left_items.iter().zip(right_items.iter()) {
                merged.push(merge_compatible_types(left_item, right_item)?);
            }
            return Some(SemType::Tuple(merged));
        }
        (
            SemType::Fn {
                params: left_params,
                ret: left_ret,
            },
            SemType::Fn {
                params: right_params,
                ret: right_ret,
            },
        ) => {
            if left_params.len() != right_params.len() {
                return None;
            }
            let mut params = Vec::with_capacity(left_params.len());
            for (left_param, right_param) in left_params.iter().zip(right_params.iter()) {
                params.push(merge_compatible_types(left_param, right_param)?);
            }
            let ret = merge_compatible_types(left_ret, right_ret)?;
            return Some(SemType::Fn {
                params,
                ret: Box::new(ret),
            });
        }
        (SemType::Iter(left_item), SemType::Iter(right_item)) => {
            let merged = merge_compatible_types(left_item, right_item)?;
            return Some(SemType::Iter(Box::new(merged)));
        }
        (
            SemType::Path {
                path: left_path,
                args: left_args,
            },
            SemType::Path {
                path: right_path,
                args: right_args,
            },
        ) => {
            if left_path != right_path || left_args.len() != right_args.len() {
                if is_compatible(left, right) && is_compatible(right, left) {
                    return Some(prefer_more_concrete_type(left, right));
                }
                return None;
            }
            let mut args = Vec::with_capacity(left_args.len());
            for (left_arg, right_arg) in left_args.iter().zip(right_args.iter()) {
                args.push(merge_compatible_types(left_arg, right_arg)?);
            }
            return Some(SemType::Path {
                path: left_path.clone(),
                args,
            });
        }
        (SemType::TraitObject(left_bounds), SemType::TraitObject(right_bounds)) => {
            if left_bounds.len() != right_bounds.len() {
                if is_compatible(left, right) && is_compatible(right, left) {
                    return Some(prefer_more_concrete_type(left, right));
                }
                return None;
            }
            let mut bounds = Vec::with_capacity(left_bounds.len());
            for (left_bound, right_bound) in left_bounds.iter().zip(right_bounds.iter()) {
                bounds.push(merge_compatible_types(left_bound, right_bound)?);
            }
            return Some(SemType::TraitObject(bounds));
        }
        _ => {}
    }

    if is_compatible(left, right) && is_compatible(right, left) {
        Some(prefer_more_concrete_type(left, right))
    } else {
        None
    }
}

fn prefer_more_concrete_type(left: &SemType, right: &SemType) -> SemType {
    if type_specificity_score(right) > type_specificity_score(left) {
        return right.clone();
    }
    left.clone()
}

fn type_specificity_score(ty: &SemType) -> usize {
    match ty {
        SemType::Unknown => 0,
        SemType::Unit => 1,
        SemType::TraitObject(bounds) => {
            1 + bounds.iter().map(type_specificity_score).sum::<usize>()
        }
        SemType::Tuple(items) => 1 + items.iter().map(type_specificity_score).sum::<usize>(),
        SemType::Fn { params, ret } => {
            1 + params.iter().map(type_specificity_score).sum::<usize>()
                + type_specificity_score(ret)
        }
        SemType::Iter(item) => 1 + type_specificity_score(item),
        SemType::Path { args, .. } => 1 + args.iter().map(type_specificity_score).sum::<usize>(),
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
                        default_diag_span(),
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
                    default_diag_span(),
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
                        default_diag_span(),
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
                        default_diag_span(),
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
                    default_diag_span(),
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
                        default_diag_span(),
                    ));
                }
            } else if *scrutinee_ty != SemType::Unknown {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Struct pattern requires struct scrutinee, got `{}`",
                        type_to_string(scrutinee_ty)
                    ),
                    default_diag_span(),
                ));
            }

            if let Some(expected_fields) = struct_fields
                && !*has_rest
            {
                let bound = fields
                    .iter()
                    .map(|field| field.name.as_str())
                    .collect::<HashSet<_>>();
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
                        default_diag_span(),
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
                                default_diag_span(),
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
                        default_diag_span(),
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
                    default_diag_span(),
                ));
            }

            let lowered_payload = if let Some(variants) = context.enums.get(&enum_name) {
                if let Some(variant_info) = variants.get(&variant_name) {
                    let expected_payload = &variant_info.payload_types;
                    if expected_payload.is_empty() {
                        if payload.is_some() {
                            diagnostics.push(Diagnostic::new(
                                format!("Pattern `{enum_name}::{variant_name}` has no payload"),
                                default_diag_span(),
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
                                    default_diag_span(),
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
                                            default_diag_span(),
                                        ));
                                    }
                                    let lowered = items
                                        .iter()
                                        .zip(expected_payload.iter())
                                        .map(|(item, expected_ty)| {
                                            lower_pattern(
                                                item,
                                                expected_ty,
                                                context,
                                                locals,
                                                diagnostics,
                                            )
                                        })
                                        .collect::<Vec<_>>();
                                    Some(Box::new(TypedPattern::Tuple(lowered)))
                                } else {
                                    diagnostics.push(Diagnostic::new(
                                        format!(
                                            "Pattern `{enum_name}::{variant_name}` with multiple payload fields requires tuple syntax"
                                        ),
                                        default_diag_span(),
                                    ));
                                    None
                                }
                            }
                            None => {
                                diagnostics.push(Diagnostic::new(
                                    format!(
                                        "Pattern `{enum_name}::{variant_name}` requires payload pattern"
                                    ),
                                    default_diag_span(),
                                ));
                                None
                            }
                        }
                    }
                } else {
                    diagnostics.push(Diagnostic::new(
                        format!("Unknown variant `{enum_name}::{variant_name}` in pattern"),
                        default_diag_span(),
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
                        default_diag_span(),
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
                    default_diag_span(),
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
                        default_diag_span(),
                    ));
                    None
                }
            },
            "None" => {
                if payload.is_some() {
                    diagnostics.push(Diagnostic::new(
                        "Pattern `Option::None` has no payload",
                        default_diag_span(),
                    ));
                }
                None
            }
            _ => {
                diagnostics.push(Diagnostic::new(
                    format!("Unknown variant `Option::{variant_name}` in pattern"),
                    default_diag_span(),
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
                    default_diag_span(),
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
                        default_diag_span(),
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
                        default_diag_span(),
                    ));
                    None
                }
            },
            _ => {
                diagnostics.push(Diagnostic::new(
                    format!("Unknown variant `Result::{variant_name}` in pattern"),
                    default_diag_span(),
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
            index: lower_index_expr(index, &base.ty, ExprPosition::Value, context, state),
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
                        default_diag_span(),
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
                    default_diag_span(),
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
                        default_diag_span(),
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

fn collect_destructure_pattern_names(pattern: &DestructurePattern, names: &mut HashSet<String>) {
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
            default_diag_span(),
        ));
    }
}

fn unify_types(existing: SemType, next: SemType, diagnostics: &mut Vec<Diagnostic>) -> SemType {
    if infer_local_bidi_enabled() {
        if let Some(merged) = merge_compatible_types(&existing, &next) {
            return merged;
        }
    }
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
        default_diag_span(),
    ));
    emit_constraint_partition_diagnostic(
        "match arm result type",
        &[existing.clone(), next.clone()],
        "an explicit arm value type annotation",
        diagnostics,
    );
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
            && let Some(variant_info) = variants.get(variant_name)
            && variant_info.payload_types.is_empty()
        {
            let args = context
                .enum_type_params
                .get(enum_name)
                .map(|params| params.iter().map(|_| SemType::Unknown).collect())
                .unwrap_or_default();
            return Some(SemType::Path {
                path: vec![enum_name.clone()],
                args,
            });
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
        if infer_local_bidi_enabled()
            && let Some(merged) = merge_compatible_types(&current, ty)
        {
            current = merged;
            continue;
        }
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
                default_diag_span(),
            ));
            emit_constraint_partition_diagnostic(
                &format!("return type for `{function_name}`"),
                &[current.clone(), ty.clone()],
                "an explicit return type annotation",
                diagnostics,
            );
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
        SemType::TraitObject(bounds) => bounds.iter().any(contains_unknown),
        SemType::Tuple(items) => items.iter().any(contains_unknown),
        SemType::Fn { params, ret } => params.iter().any(contains_unknown) || contains_unknown(ret),
        SemType::Iter(item) => contains_unknown(item),
        SemType::Path { args, .. } => args.iter().any(contains_unknown),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolve_function_sig_for_path_falls_back_to_tail_segment() {
        let mut context = Context {
            functions: HashMap::new(),
            structs: HashMap::new(),
            struct_type_params: HashMap::new(),
            enums: HashMap::new(),
            enum_type_params: HashMap::new(),
            traits: HashMap::new(),
            trait_impls: HashMap::new(),
            globals: HashMap::new(),
            rust_block_functions: HashSet::new(),
            structural_specializations: RefCell::new(HashMap::new()),
            structural_specialization_lookup: RefCell::new(HashMap::new()),
        };
        context.functions.insert(
            "board_left".to_string(),
            FunctionSig {
                type_params: Vec::new(),
                type_param_bounds: HashMap::new(),
                params: Vec::new(),
                return_type: named_type("i64"),
                structural_requirements: HashMap::new(),
            },
        );

        let path = vec!["gamekit".to_string(), "board_left".to_string()];
        let resolved = resolve_function_sig_for_path(&path, &context);
        assert!(resolved.is_some());
        let (name, sig) = resolved.expect("path should resolve");
        assert_eq!(name, "board_left");
        assert_eq!(sig.return_type, named_type("i64"));
    }

    #[test]
    fn resolve_from_iter_associated_type_infers_vec_item() {
        let mut diagnostics = Vec::new();
        let out = resolve_from_iter_associated_type(
            "Vec",
            &[SemType::Iter(Box::new(named_type("i64")))],
            &mut diagnostics,
        );
        assert!(diagnostics.is_empty());
        assert_eq!(
            out,
            SemType::Path {
                path: vec!["Vec".to_string()],
                args: vec![named_type("i64")],
            }
        );
    }

    #[test]
    fn resolve_from_iter_associated_type_infers_hashmap_kv() {
        let mut diagnostics = Vec::new();
        let out = resolve_from_iter_associated_type(
            "HashMap",
            &[SemType::Iter(Box::new(SemType::Tuple(vec![
                named_type("String"),
                named_type("i64"),
            ])))],
            &mut diagnostics,
        );
        assert!(diagnostics.is_empty());
        assert_eq!(
            out,
            SemType::Path {
                path: vec!["HashMap".to_string()],
                args: vec![named_type("String"), named_type("i64")],
            }
        );
    }
}
