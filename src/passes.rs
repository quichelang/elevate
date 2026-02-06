use std::collections::HashMap;

use crate::ast::{Block, Expr, Item, Module, Pattern, Stmt, Type, Visibility};
use crate::diag::{Diagnostic, Span};
use crate::ir::lowered::{
    RustConst, RustEnum, RustExpr, RustField, RustFunction, RustImpl, RustItem, RustMatchArm,
    RustModule, RustParam, RustPattern, RustStatic, RustStmt, RustStruct, RustUse, RustVariant,
};
use crate::ir::typed::{
    TypedConst, TypedEnum, TypedExpr, TypedExprKind, TypedField, TypedFunction, TypedImpl, TypedItem,
    TypedMatchArm, TypedModule, TypedParam, TypedPattern, TypedRustUse, TypedStatic, TypedStmt,
    TypedStruct, TypedVariant,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum SemType {
    Path { path: Vec<String>, args: Vec<SemType> },
    Unit,
    Unknown,
}

#[derive(Debug, Clone)]
struct FunctionSig {
    params: Vec<SemType>,
    return_type: SemType,
}

#[derive(Debug, Clone)]
struct Context {
    functions: HashMap<String, FunctionSig>,
    structs: HashMap<String, HashMap<String, SemType>>,
    enums: HashMap<String, HashMap<String, Option<SemType>>>,
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
    let items = module
        .items
        .iter()
        .map(|item| match item {
            TypedItem::RustUse(def) => RustItem::Use(RustUse {
                path: def.path.clone(),
            }),
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
                    .map(|method| RustFunction {
                        is_public: method.is_public,
                        name: method.name.clone(),
                        params: method
                            .params
                            .iter()
                            .map(|param| RustParam {
                                name: param.name.clone(),
                                ty: param.ty.clone(),
                            })
                            .collect(),
                        return_type: method.return_type.clone(),
                        body: method.body.iter().map(lower_stmt).collect(),
                    })
                    .collect(),
            }),
            TypedItem::Function(def) => RustItem::Function(RustFunction {
                is_public: def.is_public,
                name: def.name.clone(),
                params: def
                    .params
                    .iter()
                    .map(|param| RustParam {
                        name: param.name.clone(),
                        ty: param.ty.clone(),
                    })
                    .collect(),
                return_type: def.return_type.clone(),
                body: def.body.iter().map(lower_stmt).collect(),
            }),
            TypedItem::Const(def) => RustItem::Const(RustConst {
                is_public: def.is_public,
                name: def.name.clone(),
                ty: def.ty.clone(),
                value: lower_expr(&def.value),
            }),
            TypedItem::Static(def) => RustItem::Static(RustStatic {
                is_public: def.is_public,
                name: def.name.clone(),
                ty: def.ty.clone(),
                value: lower_expr(&def.value),
            }),
        })
        .collect();
    RustModule { items }
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
                            variant.payload.as_ref().map(type_from_ast),
                        )
                    })
                    .collect::<HashMap<_, _>>();
                context.enums.insert(def.name.clone(), variants);
            }
            Item::Function(def) => {
                let sig = FunctionSig {
                    params: def.params.iter().map(|param| type_from_ast(&param.ty)).collect(),
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
                        params: method
                            .params
                            .iter()
                            .map(|param| type_from_ast(&param.ty))
                            .collect(),
                        return_type: method
                            .return_type
                            .as_ref()
                            .map(type_from_ast)
                            .unwrap_or(SemType::Unit),
                    };
                    context
                        .functions
                        .insert(format!("{}::{}", def.target, method.name), sig);
                }
            }
            Item::Const(def) => {
                let ty = def.ty.as_ref().map(type_from_ast).unwrap_or(SemType::Unknown);
                context.globals.insert(def.name.clone(), ty);
            }
            Item::Static(def) => {
                context.globals.insert(def.name.clone(), type_from_ast(&def.ty));
            }
            Item::RustUse(_) => {}
        }
    }

}

fn lower_item(item: &Item, context: &mut Context, diagnostics: &mut Vec<Diagnostic>) -> Option<TypedItem> {
    match item {
        Item::RustUse(def) => Some(TypedItem::RustUse(TypedRustUse {
            path: def.path.clone(),
        })),
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
                        .as_ref()
                        .map(type_from_ast)
                        .map(|ty| type_to_string(&ty)),
                })
                .collect(),
        })),
        Item::Impl(def) => {
            let mut methods = Vec::new();
            for method in &def.methods {
                let mut locals = HashMap::new();
                for param in &method.params {
                    locals.insert(param.name.clone(), type_from_ast(&param.ty));
                }
                let declared_return_ty = method.return_type.as_ref().map(type_from_ast);
                let provisional_return_ty = declared_return_ty.clone().unwrap_or(SemType::Unknown);
                let mut body = Vec::new();
                let mut inferred_returns = Vec::new();
                for statement in &method.body.statements {
                    if let Some(stmt) = lower_stmt_with_types(
                        statement,
                        context,
                        &mut locals,
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
                    params: method
                        .params
                        .iter()
                        .map(|param| TypedParam {
                            name: param.name.clone(),
                            ty: type_to_string(&type_from_ast(&param.ty)),
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
            let (typed_value, inferred) =
                infer_expr(&def.value, context, &mut locals, &SemType::Unit, diagnostics);
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
            let (typed_value, inferred) =
                infer_expr(&def.value, context, &mut locals, &SemType::Unit, diagnostics);
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
            for param in &def.params {
                locals.insert(param.name.clone(), type_from_ast(&param.ty));
            }
            let declared_return_ty = def.return_type.as_ref().map(type_from_ast);
            let provisional_return_ty = declared_return_ty.clone().unwrap_or(SemType::Unknown);
            let mut body = Vec::new();
            let mut inferred_returns = Vec::new();
            for statement in &def.body.statements {
                if let Some(stmt) = lower_stmt_with_types(
                    statement,
                    context,
                    &mut locals,
                    &provisional_return_ty,
                    &mut inferred_returns,
                    diagnostics,
                ) {
                    body.push(stmt);
                }
            }
            let final_return_ty = declared_return_ty.unwrap_or_else(|| {
                infer_return_type(&inferred_returns, diagnostics, &def.name)
            });
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
    return_ty: &SemType,
    inferred_returns: &mut Vec<SemType>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<TypedStmt> {
    match stmt {
        Stmt::Const(def) => {
            let (typed_value, inferred) = infer_expr(&def.value, context, locals, return_ty, diagnostics);
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
            Some(TypedStmt::Const(TypedConst {
                is_public: false,
                name: def.name.clone(),
                ty: type_to_string(&final_ty),
                value: typed_value,
            }))
        }
        Stmt::Return(value) => {
            if let Some(expr) = value {
                let (typed_expr, inferred) = infer_expr(expr, context, locals, return_ty, diagnostics);
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

            let then_body =
                lower_block_with_types(then_block, context, locals, return_ty, inferred_returns, diagnostics);
            let else_body = else_block.as_ref().map(|block| {
                lower_block_with_types(block, context, locals, return_ty, inferred_returns, diagnostics)
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
            let typed_body =
                lower_block_with_types(body, context, locals, return_ty, inferred_returns, diagnostics);
            Some(TypedStmt::While {
                condition: typed_condition,
                body: typed_body,
            })
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
            let (typed_callee, callee_ty) = infer_expr(callee, context, locals, return_ty, diagnostics);
            let mut typed_args = Vec::new();
            let mut arg_types = Vec::new();
            for arg in args {
                let (typed_arg, ty) = infer_expr(arg, context, locals, return_ty, diagnostics);
                typed_args.push(typed_arg);
                arg_types.push(ty);
            }
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
                let (typed_value, arm_ty) =
                    infer_expr(&arm.value, context, &mut arm_locals, return_ty, diagnostics);
                resolved_arm_ty = match resolved_arm_ty {
                    Some(existing) => Some(unify_types(existing, arm_ty, diagnostics)),
                    None => Some(arm_ty),
                };
                typed_arms.push(TypedMatchArm {
                    pattern: typed_pattern,
                    value: typed_value,
                });
            }

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
        Expr::Try(inner) => {
            let (typed_inner, inner_ty) = infer_expr(inner, context, locals, return_ty, diagnostics);
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
    _callee_ty: &SemType,
    args: &[SemType],
    context: &Context,
    diagnostics: &mut Vec<Diagnostic>,
) -> SemType {
    if let TypedExprKind::Path(path) = &callee.kind {
        let lookup_name = path.join("::");
        if let Some(sig) = context.functions.get(&lookup_name) {
            let name = lookup_name;
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

            for (index, (actual, expected)) in args.iter().zip(&sig.params).enumerate() {
                if !is_compatible(actual, expected) {
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
            return sig.return_type.clone();
        }

        if path.len() == 1 {
            let name = &path[0];
            if let Some(sig) = context.functions.get(name) {
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

                for (index, (actual, expected)) in args.iter().zip(&sig.params).enumerate() {
                    if !is_compatible(actual, expected) {
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
                return sig.return_type.clone();
            }
        }

        if let Some(resolved) = resolve_constructor_call(path, args, context, diagnostics) {
            return resolved;
        }

        return SemType::Unknown;
    }

    diagnostics.push(Diagnostic::new(
        "Unsupported call target",
        Span::new(0, 0),
    ));
    SemType::Unknown
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
            if let Some(payload) = variants.get(variant_name) {
                match payload {
                    Some(expected) => {
                        if args.len() != 1 {
                            diagnostics.push(Diagnostic::new(
                                format!(
                                    "Enum variant `{enum_name}::{variant_name}` expects one argument"
                                ),
                                Span::new(0, 0),
                            ));
                        } else if !is_compatible(&args[0], expected) {
                            diagnostics.push(Diagnostic::new(
                                format!(
                                    "Enum variant `{enum_name}::{variant_name}` expected `{}`, got `{}`",
                                    type_to_string(expected),
                                    type_to_string(&args[0])
                                ),
                                Span::new(0, 0),
                            ));
                        }
                    }
                    None => {
                        if !args.is_empty() {
                            diagnostics.push(Diagnostic::new(
                                format!(
                                    "Enum variant `{enum_name}::{variant_name}` expects no arguments"
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

fn resolve_try_type(inner: &SemType, return_ty: &SemType, diagnostics: &mut Vec<Diagnostic>) -> SemType {
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

fn lower_stmt(stmt: &TypedStmt) -> RustStmt {
    match stmt {
        TypedStmt::Const(def) => RustStmt::Const(RustConst {
            is_public: false,
            name: def.name.clone(),
            ty: def.ty.clone(),
            value: lower_expr(&def.value),
        }),
        TypedStmt::Return(value) => RustStmt::Return(value.as_ref().map(lower_expr)),
        TypedStmt::If {
            condition,
            then_body,
            else_body,
        } => RustStmt::If {
            condition: lower_expr(condition),
            then_body: then_body.iter().map(lower_stmt).collect(),
            else_body: else_body
                .as_ref()
                .map(|block| block.iter().map(lower_stmt).collect()),
        },
        TypedStmt::While { condition, body } => RustStmt::While {
            condition: lower_expr(condition),
            body: body.iter().map(lower_stmt).collect(),
        },
        TypedStmt::Expr(expr) => RustStmt::Expr(lower_expr(expr)),
    }
}

fn lower_expr(expr: &TypedExpr) -> RustExpr {
    match &expr.kind {
        TypedExprKind::Int(value) => RustExpr::Int(*value),
        TypedExprKind::Bool(value) => RustExpr::Bool(*value),
        TypedExprKind::String(value) => RustExpr::String(value.clone()),
        TypedExprKind::Path(path) => RustExpr::Path(path.clone()),
        TypedExprKind::Call { callee, args } => RustExpr::Call {
            callee: Box::new(lower_expr(callee)),
            args: args.iter().map(lower_expr).collect(),
        },
        TypedExprKind::Field { base, field } => RustExpr::Field {
            base: Box::new(lower_expr(base)),
            field: field.clone(),
        },
        TypedExprKind::Match { scrutinee, arms } => RustExpr::Match {
            scrutinee: Box::new(lower_expr(scrutinee)),
            arms: arms
                .iter()
                .map(|arm| RustMatchArm {
                    pattern: lower_pattern_to_rust(&arm.pattern),
                    value: lower_expr(&arm.value),
                })
                .collect(),
        },
        TypedExprKind::Try(inner) => RustExpr::Try(Box::new(lower_expr(inner))),
    }
}

fn type_from_ast(ty: &Type) -> SemType {
    SemType::Path {
        path: ty.path.clone(),
        args: ty.args.iter().map(type_from_ast).collect(),
    }
}

fn type_to_string(ty: &SemType) -> String {
    match ty {
        SemType::Unit => "()".to_string(),
        SemType::Unknown => "_".to_string(),
        SemType::Path { path, args } => {
            let head = path.join("::");
            if args.is_empty() {
                head
            } else {
                let args_text = args.iter().map(type_to_string).collect::<Vec<_>>().join(", ");
                format!("{head}<{args_text}>")
            }
        }
    }
}

fn is_compatible(actual: &SemType, expected: &SemType) -> bool {
    match (actual, expected) {
        (_, SemType::Unknown) | (SemType::Unknown, _) => true,
        (SemType::Unit, SemType::Unit) => true,
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

fn lower_pattern(
    pattern: &Pattern,
    scrutinee_ty: &SemType,
    context: &Context,
    locals: &mut HashMap<String, SemType>,
    diagnostics: &mut Vec<Diagnostic>,
) -> TypedPattern {
    match pattern {
        Pattern::Wildcard => TypedPattern::Wildcard,
        Pattern::Variant { path, binding } => {
            if path.len() != 2 {
                diagnostics.push(Diagnostic::new(
                    "Variant patterns must use `Enum::Variant` form",
                    Span::new(0, 0),
                ));
                return TypedPattern::Variant {
                    path: path.clone(),
                    binding: binding.clone(),
                };
            }

            let enum_name = &path[0];
            let variant_name = &path[1];

            if let SemType::Path { path: ty_path, .. } = scrutinee_ty
                && ty_path.last() != Some(enum_name)
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

            if let Some(variants) = context.enums.get(enum_name) {
                if let Some(payload) = variants.get(variant_name) {
                    match payload {
                        Some(payload_ty) => {
                            if let Some(name) = binding {
                                locals.insert(name.clone(), payload_ty.clone());
                            }
                        }
                        None => {
                            if binding.is_some() {
                                diagnostics.push(Diagnostic::new(
                                    format!(
                                        "Pattern `{enum_name}::{variant_name}` has no payload binding"
                                    ),
                                    Span::new(0, 0),
                                ));
                            }
                        }
                    }
                } else {
                    diagnostics.push(Diagnostic::new(
                        format!("Unknown variant `{enum_name}::{variant_name}` in pattern"),
                        Span::new(0, 0),
                    ));
                }
            } else {
                diagnostics.push(Diagnostic::new(
                    format!("Unknown enum `{enum_name}` in pattern"),
                    Span::new(0, 0),
                ));
            }

            TypedPattern::Variant {
                path: path.clone(),
                binding: binding.clone(),
            }
        }
    }
}

fn lower_pattern_to_rust(pattern: &TypedPattern) -> RustPattern {
    match pattern {
        TypedPattern::Wildcard => RustPattern::Wildcard,
        TypedPattern::Variant { path, binding } => RustPattern::Variant {
            path: path.clone(),
            binding: binding.clone(),
        },
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
            && payload.is_none()
        {
            return Some(named_type(enum_name));
        }
    }
    None
}

fn infer_return_type(returns: &[SemType], diagnostics: &mut Vec<Diagnostic>, function_name: &str) -> SemType {
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
    return_ty: &SemType,
    inferred_returns: &mut Vec<SemType>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Vec<TypedStmt> {
    let mut block_locals = locals.clone();
    let mut out = Vec::new();
    for statement in &block.statements {
        if let Some(stmt) = lower_stmt_with_types(
            statement,
            context,
            &mut block_locals,
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
        SemType::Path { args, .. } => args.iter().any(contains_unknown),
    }
}
