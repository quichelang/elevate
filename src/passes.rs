use std::collections::HashMap;

use crate::ast::{Expr, Item, Module, Stmt, Type};
use crate::diag::{Diagnostic, Span};
use crate::ir::lowered::{
    RustConst, RustEnum, RustExpr, RustField, RustFunction, RustItem, RustModule, RustParam,
    RustStatic, RustStmt, RustStruct, RustUse, RustVariant,
};
use crate::ir::typed::{
    TypedConst, TypedEnum, TypedExpr, TypedExprKind, TypedField, TypedFunction, TypedItem,
    TypedModule, TypedParam, TypedRustUse, TypedStatic, TypedStmt, TypedStruct, TypedVariant,
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
            TypedItem::Function(def) => RustItem::Function(RustFunction {
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
                name: def.name.clone(),
                ty: def.ty.clone(),
                value: lower_expr(&def.value),
            }),
            TypedItem::Static(def) => RustItem::Static(RustStatic {
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
            Some(TypedItem::Const(TypedConst {
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
            Some(TypedItem::Static(TypedStatic {
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
            let return_ty = def
                .return_type
                .as_ref()
                .map(type_from_ast)
                .unwrap_or(SemType::Unit);
            let mut body = Vec::new();
            let mut has_explicit_return = false;
            for statement in &def.body.statements {
                if matches!(statement, Stmt::Return(_)) {
                    has_explicit_return = true;
                }
                if let Some(stmt) = lower_stmt_with_types(statement, context, &mut locals, &return_ty, diagnostics) {
                    body.push(stmt);
                }
            }
            if return_ty != SemType::Unit && !has_explicit_return {
                diagnostics.push(Diagnostic::new(
                    format!(
                        "Function `{}` must explicitly return `{}`",
                        def.name,
                        type_to_string(&return_ty)
                    ),
                    Span::new(0, 0),
                ));
            }

            Some(TypedItem::Function(TypedFunction {
                name: def.name.clone(),
                params: def
                    .params
                    .iter()
                    .map(|param| TypedParam {
                        name: param.name.clone(),
                        ty: type_to_string(&type_from_ast(&param.ty)),
                    })
                    .collect(),
                return_type: type_to_string(&return_ty),
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
                name: def.name.clone(),
                ty: type_to_string(&final_ty),
                value: typed_value,
            }))
        }
        Stmt::Return(value) => {
            if let Some(expr) = value {
                let (typed_expr, inferred) = infer_expr(expr, context, locals, return_ty, diagnostics);
                if !is_compatible(&inferred, return_ty) {
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
                if *return_ty != SemType::Unit {
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
                    .unwrap_or(SemType::Unknown)
            } else {
                SemType::Unknown
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
        if option_inner(return_ty).is_none() {
            diagnostics.push(Diagnostic::new(
                "The `?` operator on Option requires the function to return Option",
                Span::new(0, 0),
            ));
        }
        return inner_value.clone();
    }
    if let Some((ok_ty, err_ty)) = result_parts(inner) {
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
            name: def.name.clone(),
            ty: def.ty.clone(),
            value: lower_expr(&def.value),
        }),
        TypedStmt::Return(value) => RustStmt::Return(value.as_ref().map(lower_expr)),
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
