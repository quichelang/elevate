use crate::ast::{Expr, Item, Module, Type};
use crate::ir::lowered::{
    RustConst, RustEnum, RustExpr, RustField, RustFunction, RustItem, RustModule, RustParam,
    RustStatic, RustStruct, RustVariant,
};
use crate::ir::typed::{
    TypedConst, TypedEnum, TypedExpr, TypedField, TypedFunction, TypedItem, TypedModule, TypedParam,
    TypedStatic, TypedStruct, TypedVariant,
};

pub fn lower_to_typed(module: &Module) -> TypedModule {
    let items = module
        .items
        .iter()
        .map(|item| match item {
            Item::Struct(def) => TypedItem::Struct(TypedStruct {
                name: def.name.clone(),
                fields: def
                    .fields
                    .iter()
                    .map(|field| TypedField {
                        name: field.name.clone(),
                        ty: lower_type(&field.ty),
                    })
                    .collect(),
            }),
            Item::Enum(def) => TypedItem::Enum(TypedEnum {
                name: def.name.clone(),
                variants: def
                    .variants
                    .iter()
                    .map(|variant| TypedVariant {
                        name: variant.name.clone(),
                        payload: variant.payload.as_ref().map(lower_type),
                    })
                    .collect(),
            }),
            Item::Function(def) => TypedItem::Function(TypedFunction {
                name: def.name.clone(),
                params: def
                    .params
                    .iter()
                    .map(|param| TypedParam {
                        name: param.name.clone(),
                        ty: lower_type(&param.ty),
                    })
                    .collect(),
                return_type: def.return_type.as_ref().map(lower_type),
            }),
            Item::Const(def) => TypedItem::Const(TypedConst {
                name: def.name.clone(),
                ty: def.ty.as_ref().map(lower_type),
                value: lower_expr(&def.value),
            }),
            Item::Static(def) => TypedItem::Static(TypedStatic {
                name: def.name.clone(),
                ty: lower_type(&def.ty),
                value: lower_expr(&def.value),
            }),
        })
        .collect();
    TypedModule { items }
}

pub fn lower_to_rust(module: &TypedModule) -> RustModule {
    let items = module
        .items
        .iter()
        .map(|item| match item {
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
            }),
            TypedItem::Const(def) => RustItem::Const(RustConst {
                name: def.name.clone(),
                ty: def.ty.clone(),
                value: lower_typed_expr(&def.value),
            }),
            TypedItem::Static(def) => RustItem::Static(RustStatic {
                name: def.name.clone(),
                ty: def.ty.clone(),
                value: lower_typed_expr(&def.value),
            }),
        })
        .collect();
    RustModule { items }
}

fn lower_type(ty: &Type) -> String {
    match ty {
        Type::Named(name) => name.clone(),
    }
}

fn lower_expr(expr: &Expr) -> TypedExpr {
    match expr {
        Expr::Int(value) => TypedExpr::Int(*value),
        Expr::Bool(value) => TypedExpr::Bool(*value),
        Expr::String(value) => TypedExpr::String(value.clone()),
        Expr::Var(name) => TypedExpr::Var(name.clone()),
        Expr::Call { callee, args } => TypedExpr::Call {
            callee: callee.clone(),
            args: args.iter().map(lower_expr).collect(),
        },
        Expr::Field { base, field } => TypedExpr::Field {
            base: Box::new(lower_expr(base)),
            field: field.clone(),
        },
    }
}

fn lower_typed_expr(expr: &TypedExpr) -> RustExpr {
    match expr {
        TypedExpr::Int(value) => RustExpr::Int(*value),
        TypedExpr::Bool(value) => RustExpr::Bool(*value),
        TypedExpr::String(value) => RustExpr::String(value.clone()),
        TypedExpr::Var(name) => RustExpr::Var(name.clone()),
        TypedExpr::Call { callee, args } => RustExpr::Call {
            callee: callee.clone(),
            args: args.iter().map(lower_typed_expr).collect(),
        },
        TypedExpr::Field { base, field } => RustExpr::Field {
            base: Box::new(lower_typed_expr(base)),
            field: field.clone(),
        },
    }
}
