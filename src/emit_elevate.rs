//! Emit Elevate source code from a TypedModule.
//!
//! This is the inverse of the parse→typecheck pipeline: given a `TypedModule`,
//! produce valid `.ers` source text that can be re-parsed and re-compiled.

use crate::ir::typed::{
    TypedAssignOp, TypedAssignTarget, TypedBinaryOp, TypedConst, TypedDestructurePattern,
    TypedEnum, TypedExpr, TypedExprKind, TypedFunction, TypedImpl, TypedItem, TypedMatchArm,
    TypedModule, TypedParam, TypedPattern, TypedPatternField, TypedStatic, TypedStmt,
    TypedStructLiteralField, TypedTrait, TypedTraitMethod, TypedTypeParam, TypedUnaryOp,
    TypedVariantFields,
};

/// Convert a typed module back into Elevate source code.
pub fn emit_typed_module(module: &TypedModule) -> String {
    let mut out = String::new();
    for (i, item) in module.items.iter().enumerate() {
        if i > 0 {
            out.push('\n');
        }
        emit_item(item, &mut out);
        out.push('\n');
    }
    out
}

fn emit_item(item: &TypedItem, out: &mut String) {
    match item {
        TypedItem::RustUse(u) => emit_use(u, out),
        TypedItem::RustBlock(code) => {
            out.push_str("rust {\n");
            out.push_str(code);
            out.push_str("\n}\n");
        }
        TypedItem::Struct(s) => emit_struct(s, out),
        TypedItem::Enum(e) => emit_enum(e, out),
        TypedItem::Trait(t) => emit_trait(t, out),
        TypedItem::Impl(imp) => emit_impl(imp, out),
        TypedItem::Function(f) => emit_function(f, out, 0),
        TypedItem::Const(c) => emit_top_const(c, out),
        TypedItem::Static(s) => emit_static(s, out),
    }
}

// ---------------------------------------------------------------------------
// Use statements
// ---------------------------------------------------------------------------

fn emit_use(u: &crate::ir::typed::TypedRustUse, out: &mut String) {
    out.push_str("use ");
    emit_use_tree(&u.tree, out);
    out.push_str(";\n");
}

fn emit_use_tree(tree: &crate::ir::typed::TypedUseTree, out: &mut String) {
    use crate::ir::typed::TypedUseTree;
    match tree {
        TypedUseTree::Name(name) => out.push_str(name),
        TypedUseTree::Glob => out.push('*'),
        TypedUseTree::Path { segment, next } => {
            out.push_str(segment);
            out.push_str("::");
            emit_use_tree(next, out);
        }
        TypedUseTree::Group(items) => {
            out.push('{');
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                emit_use_tree(item, out);
            }
            out.push('}');
        }
    }
}

// ---------------------------------------------------------------------------
// Struct
// ---------------------------------------------------------------------------

fn emit_struct(s: &crate::ir::typed::TypedStruct, out: &mut String) {
    if s.is_public {
        out.push_str("pub ");
    }
    out.push_str("struct ");
    out.push_str(&s.name);
    emit_type_params(&s.type_params, out);
    out.push_str(" {\n");
    for field in &s.fields {
        out.push_str("    ");
        out.push_str(&field.name);
        out.push_str(": ");
        out.push_str(&field.ty);
        out.push_str(",\n");
    }
    out.push_str("}\n");
}

// ---------------------------------------------------------------------------
// Enum
// ---------------------------------------------------------------------------

fn emit_enum(e: &TypedEnum, out: &mut String) {
    if e.is_public {
        out.push_str("pub ");
    }
    out.push_str("enum ");
    out.push_str(&e.name);
    emit_type_params(&e.type_params, out);
    out.push_str(" {\n");
    for variant in &e.variants {
        out.push_str("    ");
        out.push_str(&variant.name);
        match &variant.fields {
            TypedVariantFields::Unit => {}
            TypedVariantFields::Tuple(types) => {
                out.push('(');
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    out.push_str(ty);
                }
                out.push(')');
            }
            TypedVariantFields::Named(fields) => {
                out.push_str(" {\n");
                for field in fields {
                    out.push_str("        ");
                    out.push_str(&field.name);
                    out.push_str(": ");
                    out.push_str(&field.ty);
                    out.push_str(",\n");
                }
                out.push_str("    }");
            }
        }
        out.push_str(",\n");
    }
    out.push_str("}\n");
}

// ---------------------------------------------------------------------------
// Trait
// ---------------------------------------------------------------------------

fn emit_trait(t: &TypedTrait, out: &mut String) {
    if t.is_public {
        out.push_str("pub ");
    }
    out.push_str("trait ");
    out.push_str(&t.name);
    if !t.supertraits.is_empty() {
        out.push_str(": ");
        for (i, s) in t.supertraits.iter().enumerate() {
            if i > 0 {
                out.push_str(" + ");
            }
            out.push_str(s);
        }
    }
    out.push_str(" {\n");
    for method in &t.methods {
        emit_trait_method(method, out);
    }
    out.push_str("}\n");
}

fn emit_trait_method(m: &TypedTraitMethod, out: &mut String) {
    out.push_str("    fn ");
    out.push_str(&m.name);
    emit_type_params(&m.type_params, out);
    out.push('(');
    emit_params(&m.params, out);
    out.push(')');
    emit_return_type(&m.return_type, out);
    out.push_str(";\n");
}

// ---------------------------------------------------------------------------
// Impl
// ---------------------------------------------------------------------------

fn emit_impl(imp: &TypedImpl, out: &mut String) {
    out.push_str("impl");
    emit_type_params(&imp.type_params, out);
    if let Some(trait_target) = &imp.trait_target {
        out.push(' ');
        out.push_str(trait_target);
        out.push_str(" for ");
    } else {
        out.push(' ');
    }
    out.push_str(&imp.target);
    if !imp.target_args.is_empty() {
        out.push('<');
        for (i, arg) in imp.target_args.iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            out.push_str(arg);
        }
        out.push('>');
    }
    out.push_str(" {\n");
    for method in &imp.methods {
        emit_function(method, out, 1);
        out.push('\n');
    }
    out.push_str("}\n");
}

// ---------------------------------------------------------------------------
// Function
// ---------------------------------------------------------------------------

fn emit_function(f: &TypedFunction, out: &mut String, indent_level: usize) {
    let indent = make_indent(indent_level);
    out.push_str(&indent);
    if f.is_public {
        out.push_str("pub ");
    }
    out.push_str("fn ");
    out.push_str(&f.name);
    emit_type_params(&f.type_params, out);
    out.push('(');
    emit_params(&f.params, out);
    out.push(')');
    emit_return_type(&f.return_type, out);
    out.push_str(" {\n");
    emit_stmts(&f.body, out, indent_level + 1);
    out.push_str(&indent);
    out.push_str("}\n");
}

fn emit_params(params: &[TypedParam], out: &mut String) {
    for (i, param) in params.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        out.push_str(&param.name);
        out.push_str(": ");
        out.push_str(&param.ty);
    }
}

fn emit_return_type(return_type: &str, out: &mut String) {
    if !return_type.is_empty() && return_type != "()" {
        out.push_str(" -> ");
        out.push_str(return_type);
    }
}

// ---------------------------------------------------------------------------
// Constants and statics
// ---------------------------------------------------------------------------

fn emit_top_const(c: &TypedConst, out: &mut String) {
    if c.is_public {
        out.push_str("pub ");
    }
    if c.is_const {
        out.push_str("const ");
    } else {
        out.push_str("let ");
    }
    out.push_str(&c.name);
    if !c.ty.is_empty() && c.ty != "_" {
        out.push_str(": ");
        out.push_str(&c.ty);
    }
    out.push_str(" = ");
    emit_expr(&c.value, out);
    out.push_str(";\n");
}

fn emit_static(s: &TypedStatic, out: &mut String) {
    if s.is_public {
        out.push_str("pub ");
    }
    out.push_str("static ");
    out.push_str(&s.name);
    out.push_str(": ");
    out.push_str(&s.ty);
    out.push_str(" = ");
    emit_expr(&s.value, out);
    out.push_str(";\n");
}

// ---------------------------------------------------------------------------
// Type parameters
// ---------------------------------------------------------------------------

fn emit_type_params(params: &[TypedTypeParam], out: &mut String) {
    if params.is_empty() {
        return;
    }
    out.push('<');
    for (i, param) in params.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        out.push_str(&param.name);
        if !param.bounds.is_empty() {
            out.push_str(": ");
            for (j, bound) in param.bounds.iter().enumerate() {
                if j > 0 {
                    out.push_str(" + ");
                }
                out.push_str(bound);
            }
        }
    }
    out.push('>');
}

// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

fn emit_stmts(stmts: &[TypedStmt], out: &mut String, indent_level: usize) {
    for stmt in stmts {
        emit_stmt(stmt, out, indent_level);
    }
}

fn emit_stmt(stmt: &TypedStmt, out: &mut String, indent_level: usize) {
    let indent = make_indent(indent_level);
    match stmt {
        TypedStmt::Const(c) => {
            out.push_str(&indent);
            if c.is_const {
                out.push_str("const ");
            } else {
                out.push_str("let ");
            }
            out.push_str(&c.name);
            if !c.ty.is_empty() && c.ty != "_" {
                out.push_str(": ");
                out.push_str(&c.ty);
            }
            out.push_str(" = ");
            emit_expr(&c.value, out);
            out.push_str(";\n");
        }
        TypedStmt::DestructureConst {
            pattern,
            value,
            is_const,
        } => {
            out.push_str(&indent);
            if *is_const {
                out.push_str("const ");
            } else {
                out.push_str("let ");
            }
            emit_destructure_pattern(pattern, out);
            out.push_str(" = ");
            emit_expr(value, out);
            out.push_str(";\n");
        }
        TypedStmt::Assign { target, op, value } => {
            out.push_str(&indent);
            emit_assign_target(target, out);
            match op {
                TypedAssignOp::Assign => out.push_str(" = "),
                TypedAssignOp::AddAssign => out.push_str(" += "),
            }
            emit_expr(value, out);
            out.push_str(";\n");
        }
        TypedStmt::Return(expr) => {
            out.push_str(&indent);
            out.push_str("return");
            if let Some(e) = expr {
                out.push(' ');
                emit_expr(e, out);
            }
            out.push_str(";\n");
        }
        TypedStmt::If {
            condition,
            then_body,
            else_body,
        } => {
            out.push_str(&indent);
            out.push_str("if ");
            emit_expr(condition, out);
            out.push_str(" {\n");
            emit_stmts(then_body, out, indent_level + 1);
            out.push_str(&indent);
            if let Some(else_stmts) = else_body {
                out.push_str("} else {\n");
                emit_stmts(else_stmts, out, indent_level + 1);
                out.push_str(&indent);
            }
            out.push_str("}\n");
        }
        TypedStmt::While { condition, body } => {
            out.push_str(&indent);
            out.push_str("while ");
            emit_expr(condition, out);
            out.push_str(" {\n");
            emit_stmts(body, out, indent_level + 1);
            out.push_str(&indent);
            out.push_str("}\n");
        }
        TypedStmt::For {
            binding,
            iter,
            body,
        } => {
            out.push_str(&indent);
            out.push_str("for ");
            emit_destructure_pattern(binding, out);
            out.push_str(" in ");
            emit_expr(iter, out);
            out.push_str(" {\n");
            emit_stmts(body, out, indent_level + 1);
            out.push_str(&indent);
            out.push_str("}\n");
        }
        TypedStmt::Loop { body } => {
            out.push_str(&indent);
            out.push_str("loop {\n");
            emit_stmts(body, out, indent_level + 1);
            out.push_str(&indent);
            out.push_str("}\n");
        }
        TypedStmt::Break => {
            out.push_str(&indent);
            out.push_str("break;\n");
        }
        TypedStmt::Continue => {
            out.push_str(&indent);
            out.push_str("continue;\n");
        }
        TypedStmt::RustBlock(code) => {
            out.push_str(&indent);
            out.push_str("rust {\n");
            out.push_str(code);
            out.push('\n');
            out.push_str(&indent);
            out.push_str("}\n");
        }
        TypedStmt::Expr(e) => {
            out.push_str(&indent);
            emit_expr(e, out);
            out.push_str(";\n");
        }
    }
}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

fn emit_expr(expr: &TypedExpr, out: &mut String) {
    emit_expr_kind(&expr.kind, out);
}

fn emit_expr_kind(kind: &TypedExprKind, out: &mut String) {
    match kind {
        TypedExprKind::Int(v) => {
            out.push_str(&v.to_string());
        }
        TypedExprKind::Float(v) => {
            out.push_str(v);
        }
        TypedExprKind::Bool(v) => {
            out.push_str(if *v { "true" } else { "false" });
        }
        TypedExprKind::Char(c) => {
            emit_char_literal(*c, out);
        }
        TypedExprKind::String(s) => {
            out.push('"');
            for ch in s.chars() {
                match ch {
                    '\\' => out.push_str("\\\\"),
                    '"' => out.push_str("\\\""),
                    '\n' => out.push_str("\\n"),
                    '\r' => out.push_str("\\r"),
                    '\t' => out.push_str("\\t"),
                    _ => out.push(ch),
                }
            }
            out.push('"');
        }
        TypedExprKind::Path(segments) => {
            for (i, seg) in segments.iter().enumerate() {
                if i > 0 {
                    out.push_str("::");
                }
                out.push_str(seg);
            }
        }
        TypedExprKind::Call { callee, args } => {
            let needs_parens = needs_parens_for_call(&callee.kind);
            if needs_parens {
                out.push('(');
            }
            emit_expr(callee, out);
            if needs_parens {
                out.push(')');
            }
            out.push('(');
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                emit_expr(arg, out);
            }
            out.push(')');
        }
        TypedExprKind::MacroCall { path, args } => {
            for (i, seg) in path.iter().enumerate() {
                if i > 0 {
                    out.push_str("::");
                }
                out.push_str(seg);
            }
            out.push_str("!(");
            let is_format_style = is_format_style_macro(path);
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                // Format-style macros: first argument is a raw format string
                if is_format_style && i == 0 {
                    if let TypedExprKind::String(s) = &arg.kind {
                        out.push('"');
                        out.push_str(s);
                        out.push('"');
                        continue;
                    }
                }
                emit_expr(arg, out);
            }
            out.push(')');
        }
        TypedExprKind::Field { base, field } => {
            let needs_parens = needs_parens_for_field(&base.kind);
            if needs_parens {
                out.push('(');
            }
            emit_expr(base, out);
            if needs_parens {
                out.push(')');
            }
            out.push('.');
            out.push_str(field);
        }
        TypedExprKind::Index { base, index, .. } => {
            emit_expr(base, out);
            out.push('[');
            emit_expr(index, out);
            out.push(']');
        }
        TypedExprKind::Match { scrutinee, arms } => {
            out.push_str("match ");
            emit_expr(scrutinee, out);
            out.push_str(" {\n");
            for arm in arms {
                emit_match_arm(arm, out);
            }
            out.push('}');
        }
        TypedExprKind::Unary { op, expr } => {
            match op {
                TypedUnaryOp::Not => out.push('!'),
                TypedUnaryOp::Neg => out.push('-'),
            }
            let needs_parens = needs_parens_for_unary(&expr.kind);
            if needs_parens {
                out.push('(');
            }
            emit_expr(expr, out);
            if needs_parens {
                out.push(')');
            }
        }
        TypedExprKind::Binary { op, left, right } => {
            let needs_left_parens = needs_parens_for_binary_child(&left.kind, op, true);
            let needs_right_parens = needs_parens_for_binary_child(&right.kind, op, false);
            if needs_left_parens {
                out.push('(');
            }
            emit_expr(left, out);
            if needs_left_parens {
                out.push(')');
            }
            out.push(' ');
            out.push_str(binary_op_str(op));
            out.push(' ');
            if needs_right_parens {
                out.push('(');
            }
            emit_expr(right, out);
            if needs_right_parens {
                out.push(')');
            }
        }
        TypedExprKind::Array(elems) => {
            out.push('[');
            for (i, elem) in elems.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                emit_expr(elem, out);
            }
            out.push(']');
        }
        TypedExprKind::Tuple(elems) => {
            out.push('(');
            for (i, elem) in elems.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                emit_expr(elem, out);
            }
            if elems.len() == 1 {
                out.push(',');
            }
            out.push(')');
        }
        TypedExprKind::StructLiteral { path, fields } => {
            for (i, seg) in path.iter().enumerate() {
                if i > 0 {
                    out.push_str("::");
                }
                out.push_str(seg);
            }
            out.push_str(" { ");
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                emit_struct_literal_field(field, out);
            }
            if !fields.is_empty() {
                out.push_str(", ");
            }
            out.push('}');
        }
        TypedExprKind::Block { body, tail } => {
            out.push_str("{\n");
            emit_stmts(body, out, 1);
            if let Some(tail_expr) = tail {
                out.push_str("    ");
                emit_expr(tail_expr, out);
                out.push('\n');
            }
            out.push('}');
        }
        TypedExprKind::Closure {
            params,
            return_type,
            body,
        } => {
            out.push('|');
            emit_params(params, out);
            out.push('|');
            if !return_type.is_empty() && return_type != "()" {
                out.push_str(" -> ");
                out.push_str(return_type);
            }
            out.push_str(" {\n");
            emit_stmts(body, out, 1);
            out.push('}');
        }
        TypedExprKind::Range {
            start,
            end,
            inclusive,
        } => {
            if let Some(s) = start {
                emit_expr(s, out);
            }
            if *inclusive {
                out.push_str("..=");
            } else {
                out.push_str("..");
            }
            if let Some(e) = end {
                emit_expr(e, out);
            }
        }
        TypedExprKind::Cast { expr, target_type } => {
            emit_expr(expr, out);
            out.push_str(" as ");
            out.push_str(target_type);
        }
        TypedExprKind::Try(expr) => {
            emit_expr(expr, out);
            out.push('?');
        }
    }
}

// ---------------------------------------------------------------------------
// Match arms
// ---------------------------------------------------------------------------

fn emit_match_arm(arm: &TypedMatchArm, out: &mut String) {
    out.push_str("    ");
    emit_pattern(&arm.pattern, out);
    if let Some(guard) = &arm.guard {
        out.push_str(" if ");
        emit_expr(guard, out);
    }
    out.push_str(" => ");
    emit_expr(&arm.value, out);
    out.push_str(";\n");
}

// ---------------------------------------------------------------------------
// Patterns
// ---------------------------------------------------------------------------

fn emit_pattern(pattern: &TypedPattern, out: &mut String) {
    match pattern {
        TypedPattern::Wildcard => out.push('_'),
        TypedPattern::Binding(name) => out.push_str(name),
        TypedPattern::Int(v) => out.push_str(&v.to_string()),
        TypedPattern::Bool(v) => out.push_str(if *v { "true" } else { "false" }),
        TypedPattern::Char(c) => emit_char_literal(*c, out),
        TypedPattern::String(s) => {
            out.push('"');
            out.push_str(s);
            out.push('"');
        }
        TypedPattern::Tuple(pats) => {
            out.push('(');
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                emit_pattern(p, out);
            }
            if pats.len() == 1 {
                out.push(',');
            }
            out.push(')');
        }
        TypedPattern::Slice {
            prefix,
            rest,
            suffix,
        } => {
            out.push('[');
            let mut first = true;
            for p in prefix {
                if !first {
                    out.push_str(", ");
                }
                first = false;
                emit_pattern(p, out);
            }
            if let Some(rest_name) = rest {
                if !first {
                    out.push_str(", ");
                }
                first = false;
                out.push_str(rest_name);
                out.push_str("..");
            }
            for p in suffix {
                if !first {
                    out.push_str(", ");
                }
                first = false;
                emit_pattern(p, out);
            }
            out.push(']');
        }
        TypedPattern::Or(pats) => {
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    out.push_str(" | ");
                }
                emit_pattern(p, out);
            }
        }
        TypedPattern::BindingAt { name, pattern } => {
            out.push_str(name);
            out.push_str(" @ ");
            emit_pattern(pattern, out);
        }
        TypedPattern::Struct {
            path,
            fields,
            has_rest,
        } => {
            for (i, seg) in path.iter().enumerate() {
                if i > 0 {
                    out.push_str("::");
                }
                out.push_str(seg);
            }
            out.push_str(" { ");
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                emit_pattern_field(field, out);
            }
            if *has_rest {
                if !fields.is_empty() {
                    out.push_str(", ");
                }
                out.push_str("..");
            }
            out.push_str(" }");
        }
        TypedPattern::Range {
            start,
            end,
            inclusive,
        } => {
            if let Some(s) = start {
                out.push_str(&s.to_string());
            }
            if *inclusive {
                out.push_str("..=");
            } else {
                out.push_str("..");
            }
            if let Some(e) = end {
                out.push_str(&e.to_string());
            }
        }
        TypedPattern::Variant { path, payload } => {
            for (i, seg) in path.iter().enumerate() {
                if i > 0 {
                    out.push_str("::");
                }
                out.push_str(seg);
            }
            if let Some(inner) = payload {
                out.push('(');
                emit_pattern(inner, out);
                out.push(')');
            }
        }
    }
}

fn emit_pattern_field(field: &TypedPatternField, out: &mut String) {
    out.push_str(&field.name);
    // Only emit `: pattern` if the pattern is not just a binding with the same name
    if let TypedPattern::Binding(name) = &field.pattern {
        if name == &field.name {
            return;
        }
    }
    out.push_str(": ");
    emit_pattern(&field.pattern, out);
}

// ---------------------------------------------------------------------------
// Assign targets
// ---------------------------------------------------------------------------

fn emit_assign_target(target: &TypedAssignTarget, out: &mut String) {
    match target {
        TypedAssignTarget::Path(name) => out.push_str(name),
        TypedAssignTarget::Field { base, field } => {
            emit_expr(base, out);
            out.push('.');
            out.push_str(field);
        }
        TypedAssignTarget::Index { base, index } => {
            emit_expr(base, out);
            out.push('[');
            emit_expr(index, out);
            out.push(']');
        }
        TypedAssignTarget::Tuple(targets) => {
            out.push('(');
            for (i, t) in targets.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                emit_assign_target(t, out);
            }
            out.push(')');
        }
    }
}

// ---------------------------------------------------------------------------
// Destructure patterns
// ---------------------------------------------------------------------------

fn emit_destructure_pattern(pattern: &TypedDestructurePattern, out: &mut String) {
    match pattern {
        TypedDestructurePattern::Name(name) => out.push_str(name),
        TypedDestructurePattern::Ignore => out.push('_'),
        TypedDestructurePattern::Tuple(pats) => {
            out.push('(');
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                emit_destructure_pattern(p, out);
            }
            if pats.len() == 1 {
                out.push(',');
            }
            out.push(')');
        }
        TypedDestructurePattern::Slice {
            prefix,
            rest,
            suffix,
        } => {
            out.push('[');
            let mut first = true;
            for p in prefix {
                if !first {
                    out.push_str(", ");
                }
                first = false;
                emit_destructure_pattern(p, out);
            }
            if let Some(rest_name) = rest {
                if !first {
                    out.push_str(", ");
                }
                first = false;
                out.push_str(rest_name);
                out.push_str("..");
            }
            for p in suffix {
                if !first {
                    out.push_str(", ");
                }
                first = false;
                emit_destructure_pattern(p, out);
            }
            out.push(']');
        }
    }
}

// ---------------------------------------------------------------------------
// Struct literal fields
// ---------------------------------------------------------------------------

fn emit_struct_literal_field(field: &TypedStructLiteralField, out: &mut String) {
    out.push_str(&field.name);
    // Shorthand when value is a path with the same name
    if let TypedExprKind::Path(segments) = &field.value.kind {
        if segments.len() == 1 && segments[0] == field.name {
            return;
        }
    }
    out.push_str(": ");
    emit_expr(&field.value, out);
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn make_indent(level: usize) -> String {
    "    ".repeat(level)
}

fn emit_char_literal(c: char, out: &mut String) {
    out.push('\'');
    match c {
        '\\' => out.push_str("\\\\"),
        '\'' => out.push_str("\\'"),
        '\n' => out.push_str("\\n"),
        '\r' => out.push_str("\\r"),
        '\t' => out.push_str("\\t"),
        _ => out.push(c),
    }
    out.push('\'');
}

fn binary_op_str(op: &TypedBinaryOp) -> &'static str {
    match op {
        TypedBinaryOp::Add => "+",
        TypedBinaryOp::Sub => "-",
        TypedBinaryOp::Mul => "*",
        TypedBinaryOp::Div => "/",
        TypedBinaryOp::Rem => "%",
        TypedBinaryOp::And => "&&",
        TypedBinaryOp::Or => "||",
        TypedBinaryOp::Eq => "==",
        TypedBinaryOp::Ne => "!=",
        TypedBinaryOp::Lt => "<",
        TypedBinaryOp::Le => "<=",
        TypedBinaryOp::Gt => ">",
        TypedBinaryOp::Ge => ">=",
    }
}

fn binary_op_precedence(op: &TypedBinaryOp) -> u8 {
    match op {
        TypedBinaryOp::Or => 1,
        TypedBinaryOp::And => 2,
        TypedBinaryOp::Eq | TypedBinaryOp::Ne => 3,
        TypedBinaryOp::Lt | TypedBinaryOp::Le | TypedBinaryOp::Gt | TypedBinaryOp::Ge => 4,
        TypedBinaryOp::Add | TypedBinaryOp::Sub => 5,
        TypedBinaryOp::Mul | TypedBinaryOp::Div | TypedBinaryOp::Rem => 6,
    }
}

fn needs_parens_for_binary_child(
    child: &TypedExprKind,
    parent_op: &TypedBinaryOp,
    _is_left: bool,
) -> bool {
    if let TypedExprKind::Binary { op: child_op, .. } = child {
        binary_op_precedence(child_op) < binary_op_precedence(parent_op)
    } else {
        false
    }
}

fn needs_parens_for_call(kind: &TypedExprKind) -> bool {
    matches!(
        kind,
        TypedExprKind::Binary { .. } | TypedExprKind::Unary { .. } | TypedExprKind::Closure { .. }
    )
}

fn needs_parens_for_field(kind: &TypedExprKind) -> bool {
    matches!(
        kind,
        TypedExprKind::Binary { .. } | TypedExprKind::Unary { .. } | TypedExprKind::Cast { .. }
    )
}

fn needs_parens_for_unary(kind: &TypedExprKind) -> bool {
    matches!(kind, TypedExprKind::Binary { .. })
}

fn is_format_style_macro(path: &[String]) -> bool {
    let name = path.last().map(|s| s.as_str()).unwrap_or("");
    matches!(
        name,
        "format" | "println" | "print" | "eprintln" | "eprint" | "write" | "writeln" | "panic"
    )
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: parse + typecheck source, then emit and return the string.
    fn round_trip(source: &str) -> String {
        let output = crate::compile_source(source).expect("compile failed");
        emit_typed_module(&output.typed)
    }

    #[test]
    fn emit_simple_struct() {
        let source = r#"
            pub struct Point { x: i64, y: i64, }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("pub struct Point"));
        assert!(emitted.contains("x: "));
        assert!(emitted.contains("y: "));
    }

    #[test]
    fn emit_simple_function() {
        let source = r#"
            pub fn id(v: i64) -> i64 {
                return v;
            }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("pub fn id(v: i64) -> i64"));
        assert!(emitted.contains("return v;"));
    }

    #[test]
    fn emit_use_statement() {
        let source = r#"
            use std::fmt::Display;

            pub fn dummy() -> i64 {
                return 1;
            }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("use std::fmt::Display;"));
    }

    #[test]
    fn emit_enum_with_variants() {
        let source = r#"
            pub enum Shape {
                Circle(f64),
                Rect { w: f64, h: f64, },
                None,
            }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("pub enum Shape"));
        assert!(emitted.contains("Circle("));
        assert!(emitted.contains("None"));
    }

    #[test]
    fn emit_match_expression() {
        let source = r#"
            enum Kind { A, B, }

            fn classify(k: Kind) -> i64 {
                return match k {
                    Kind::A => 1;
                    Kind::B => 2;
                    _ => 0;
                };
            }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("match k"));
        assert!(emitted.contains("Kind::A => 1;"));
    }

    #[test]
    fn emit_if_else() {
        let source = r#"
            fn check(flag: bool) -> i64 {
                if flag {
                    return 1;
                } else {
                    return 2;
                }
            }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("if flag"));
        assert!(emitted.contains("} else {"));
    }

    #[test]
    fn emit_const_and_let() {
        let source = r#"
            fn demo() -> i64 {
                const x = 5;
                let y = x;
                y = y + 1;
                return y;
            }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("const x"));
        assert!(emitted.contains("let y"));
    }

    #[test]
    fn emit_for_loop() {
        let source = r#"
            fn sum(values: Vec<i64>) -> i64 {
                let total = 0;
                for v in values {
                    total += v;
                }
                return total;
            }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("for v in"));
        assert!(emitted.contains("total += v;"));
    }

    #[test]
    fn emit_impl_block() {
        let source = r#"
            struct Pair { a: i64, b: i64, }

            impl Pair {
                fn sum(self) -> i64 {
                    return self.a + self.b;
                }
            }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("impl Pair"));
        assert!(emitted.contains("fn sum(self: Pair) -> i64") || emitted.contains("fn sum(self"));
    }

    #[test]
    fn emit_generic_struct() {
        let source = r#"
            pub struct Wrapper<T> { inner: T, }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("struct Wrapper<T>"));
        assert!(emitted.contains("inner: T"));
    }

    #[test]
    fn emit_try_expression() {
        let source = r#"
            use std::num::ParseIntError;

            fn parse(input: String) -> Result<i64, ParseIntError> {
                const v = Result::Ok(1)?;
                return Result::Ok(v);
            }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("?"));
    }

    #[test]
    fn emit_while_loop() {
        let source = r#"
            fn countdown(n: i64) -> i64 {
                let i = n;
                while i > 0 {
                    i = i - 1;
                }
                return i;
            }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("while "));
    }

    #[test]
    fn emit_break_continue() {
        let source = r#"
            fn demo() -> i64 {
                let i = 0;
                loop {
                    if i > 5 {
                        break;
                    }
                    i += 1;
                    continue;
                }
                return i;
            }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("loop {"));
        assert!(emitted.contains("break;"));
        assert!(emitted.contains("continue;"));
    }

    #[test]
    fn emit_binary_expr_precedence() {
        let source = r#"
            fn calc(a: i64, b: i64) -> i64 {
                return a + b * 2;
            }
        "#;
        let emitted = round_trip(source);
        // The emitted code must preserve semantics; either no parens needed
        // or correct parens are applied
        assert!(emitted.contains("a + b * 2") || emitted.contains("a + (b * 2)"));
    }

    #[test]
    fn emit_array_literal() {
        let source = r#"
            fn items() -> Vec<i64> {
                return vec!(1, 2, 3);
            }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("vec!(1, 2, 3)"));
    }

    #[test]
    fn emit_struct_literal() {
        let source = r#"
            struct Pt { x: i64, y: i64, }

            fn origin() -> Pt {
                return Pt { x: 0, y: 0, };
            }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("Pt {"));
        assert!(emitted.contains("x:") || emitted.contains("x :"));
    }

    #[test]
    fn emit_closure() {
        let source = r#"
            fn apply(f: i64) -> i64 {
                const adder = |x: i64| -> i64 {
                    return x + 1;
                };
                return adder(f);
            }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("|x: i64|"));
    }

    #[test]
    fn emit_cast() {
        // `as` casts aren't valid Elevate surface syntax, so test via constructed AST
        use crate::ir::typed::{TypedExpr, TypedExprKind};
        let expr = TypedExpr {
            kind: TypedExprKind::Cast {
                expr: Box::new(TypedExpr {
                    kind: TypedExprKind::Path(vec!["x".to_string()]),
                    ty: "i32".to_string(),
                }),
                target_type: "i64".to_string(),
            },
            ty: "i64".to_string(),
        };
        let mut out = String::new();
        emit_expr(&expr, &mut out);
        assert_eq!(out, "x as i64");
    }

    #[test]
    fn emit_trait_def() {
        let source = r#"
            pub trait Greetable {
                fn greet(self) -> String;
            }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("pub trait Greetable"));
        assert!(emitted.contains("fn greet("));
    }

    #[test]
    fn emit_macro_println() {
        let source = r#"
            fn hello() {
                println!("hello world");
                return;
            }
        "#;
        let emitted = round_trip(source);
        assert!(emitted.contains("println!(\"hello world\")"));
    }
}
