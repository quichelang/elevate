use crate::ast::{
    AssignOp, AssignTarget, BinaryOp, Block, ConstDef, DestructurePattern, EffectRow, EnumDef,
    EnumVariant, EnumVariantFields, Expr, Field, FunctionDef, GenericParam, ImplBlock, Item,
    MatchArm, Module, Param, Pattern, PatternField, RustUse, StaticDef, Stmt, StructDef,
    StructLiteralField, TraitDef, TraitMethodSig, Type, UnaryOp, Visibility,
};
use crate::diag::Diagnostic;
use crate::lexer::{Token, TokenKind};

pub fn parse_module(tokens: Vec<Token>) -> Result<Module, Vec<Diagnostic>> {
    let mut parser = Parser::new(tokens);
    let module = parser.parse_module();
    if parser.diagnostics.is_empty() {
        Ok(module)
    } else {
        Err(parser.diagnostics)
    }
}

struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
    diagnostics: Vec<Diagnostic>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: 0,
            diagnostics: Vec::new(),
        }
    }

    fn parse_module(&mut self) -> Module {
        let mut items = Vec::new();
        while !self.at(TokenKind::Eof) {
            if let Some(item) = self.parse_item() {
                items.push(item);
            } else {
                self.synchronize_top_level();
            }
        }
        Module { items }
    }

    fn parse_item(&mut self) -> Option<Item> {
        if let TokenKind::RustBlock(code) = self.peek().kind.clone() {
            self.advance();
            return Some(Item::RustBlock(code));
        }

        let visibility = if self.match_kind(TokenKind::Pub) {
            Visibility::Public
        } else {
            Visibility::Private
        };

        if self.match_kind(TokenKind::Rust) {
            if visibility == Visibility::Public {
                self.error_current("`pub rust ...` is not supported; use `rust ...`");
            }
            if self.at(TokenKind::Use) {
                self.error_current(
                    "`rust` prefix on imports is no longer supported; write `use ...;`",
                );
                return None;
            }
            self.error_current("Expected inline `rust { ... }` block");
            return None;
        }
        if self.match_kind(TokenKind::Use) {
            return self.parse_use_item().map(Item::RustUse);
        }
        if self.match_kind(TokenKind::Struct) {
            return self.parse_struct(visibility).map(Item::Struct);
        }
        if self.match_kind(TokenKind::Enum) {
            return self.parse_enum(visibility).map(Item::Enum);
        }
        if self.match_kind(TokenKind::Trait) {
            return self.parse_trait(visibility).map(Item::Trait);
        }
        if self.match_kind(TokenKind::Impl) {
            if visibility == Visibility::Public {
                self.error_current("`pub impl` is not supported");
            }
            return self.parse_impl().map(Item::Impl);
        }
        if self.match_kind(TokenKind::Fn) {
            return self.parse_function(visibility, None).map(Item::Function);
        }
        if self.match_kind(TokenKind::Const) {
            return self.parse_const_item(visibility).map(Item::Const);
        }
        if self.match_kind(TokenKind::Static) {
            return self.parse_static_item(visibility).map(Item::Static);
        }
        if visibility == Visibility::Public {
            self.error_current("Expected item after `pub`");
            return None;
        }

        self.error_current(
            "Expected top-level item (`use`, `rust { ... }`, `struct`, `enum`, `trait`, `impl`, `fn`, `const`, `static`)",
        );
        None
    }

    fn parse_use_item(&mut self) -> Option<RustUse> {
        let start = self.previous_span_end();
        let tree = self.parse_use_tree()?;
        self.expect(TokenKind::Semicolon, "Expected ';' after use import")?;
        Some(RustUse {
            tree,
            span: Some(self.span_from_start(start)),
        })
    }

    fn parse_use_tree(&mut self) -> Option<crate::ast::UseTree> {
        let segment = self.expect_ident("Expected import path after `use`")?;
        self.parse_use_tree_tail(segment)
    }

    fn parse_use_tree_tail(&mut self, segment: String) -> Option<crate::ast::UseTree> {
        if !self.match_kind(TokenKind::ColonColon) {
            return Some(crate::ast::UseTree::Name(segment));
        }
        if self.match_kind(TokenKind::Star) {
            return Some(crate::ast::UseTree::Path {
                segment,
                next: Box::new(crate::ast::UseTree::Glob),
            });
        }
        if self.match_kind(TokenKind::LBrace) {
            return Some(crate::ast::UseTree::Path {
                segment,
                next: Box::new(self.parse_use_group()?),
            });
        }
        let next = self.expect_ident("Expected identifier after '::'")?;
        Some(crate::ast::UseTree::Path {
            segment,
            next: Box::new(self.parse_use_tree_tail(next)?),
        })
    }

    fn parse_use_group(&mut self) -> Option<crate::ast::UseTree> {
        let mut imports = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            imports.push(self.parse_use_tree()?);
            if !self.match_kind(TokenKind::Comma) {
                break;
            }
            if self.at(TokenKind::RBrace) {
                break;
            }
        }
        self.expect(TokenKind::RBrace, "Expected '}' to close grouped import")?;
        if imports.is_empty() {
            self.error_current("Expected at least one import inside grouped import");
            return None;
        }
        Some(crate::ast::UseTree::Group(imports))
    }

    fn parse_struct(&mut self, visibility: Visibility) -> Option<StructDef> {
        let start = self.previous_span_end();
        let name = self.expect_ident("Expected struct name")?;
        let type_params = self.parse_optional_generic_params()?;
        self.expect(TokenKind::LBrace, "Expected '{' after struct name")?;
        let mut fields = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let field_name = self.expect_ident("Expected field name")?;
            self.expect(TokenKind::Colon, "Expected ':' after field name")?;
            let field_ty = self.parse_type()?;
            self.expect(TokenKind::Semicolon, "Expected ';' after field declaration")?;
            fields.push(Field {
                name: field_name,
                ty: field_ty,
            });
        }
        self.expect(TokenKind::RBrace, "Expected '}' after struct body")?;
        Some(StructDef {
            visibility,
            name,
            type_params,
            fields,
            span: Some(self.span_from_start(start)),
        })
    }

    fn parse_enum(&mut self, visibility: Visibility) -> Option<EnumDef> {
        let start = self.previous_span_end();
        let name = self.expect_ident("Expected enum name")?;
        let type_params = self.parse_optional_generic_params()?;
        self.expect(TokenKind::LBrace, "Expected '{' after enum name")?;
        let mut variants = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let variant_name = self.expect_ident("Expected enum variant name")?;
            let fields = if self.match_kind(TokenKind::LParen) {
                let mut payload = Vec::new();
                if !self.at(TokenKind::RParen) {
                    payload.push(self.parse_type()?);
                    while self.match_kind(TokenKind::Comma) {
                        if self.at(TokenKind::RParen) {
                            break;
                        }
                        payload.push(self.parse_type()?);
                    }
                }
                self.expect(
                    TokenKind::RParen,
                    "Expected ')' after variant payload type list",
                )?;
                EnumVariantFields::Tuple(payload)
            } else if self.match_kind(TokenKind::LBrace) {
                let mut named = Vec::new();
                while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
                    let field_name =
                        self.expect_ident("Expected field name in enum variant payload")?;
                    self.expect(
                        TokenKind::Colon,
                        "Expected ':' after enum variant field name",
                    )?;
                    let field_ty = self.parse_type()?;
                    self.expect(
                        TokenKind::Semicolon,
                        "Expected ';' after enum variant field declaration",
                    )?;
                    named.push(Field {
                        name: field_name,
                        ty: field_ty,
                    });
                }
                self.expect(
                    TokenKind::RBrace,
                    "Expected '}' after enum variant named payload",
                )?;
                EnumVariantFields::Named(named)
            } else {
                EnumVariantFields::Unit
            };
            self.expect(TokenKind::Semicolon, "Expected ';' after enum variant")?;
            variants.push(EnumVariant {
                name: variant_name,
                fields,
            });
        }
        self.expect(TokenKind::RBrace, "Expected '}' after enum body")?;
        Some(EnumDef {
            visibility,
            name,
            type_params,
            variants,
            span: Some(self.span_from_start(start)),
        })
    }

    fn parse_trait(&mut self, visibility: Visibility) -> Option<TraitDef> {
        let start = self.previous_span_end();
        let name = self.expect_ident("Expected trait name")?;
        let mut supertraits = Vec::new();
        if self.match_kind(TokenKind::Colon) {
            supertraits.push(self.parse_type_bound()?);
            while self.match_kind(TokenKind::Plus) {
                supertraits.push(self.parse_type_bound()?);
            }
        }
        self.expect(TokenKind::LBrace, "Expected '{' after trait name")?;
        let mut methods = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            self.expect(TokenKind::Fn, "Expected `fn` in trait body")?;
            methods.push(self.parse_trait_method_signature()?);
        }
        self.expect(TokenKind::RBrace, "Expected '}' after trait body")?;
        Some(TraitDef {
            visibility,
            name,
            supertraits,
            methods,
            span: Some(self.span_from_start(start)),
        })
    }

    fn parse_function(
        &mut self,
        visibility: Visibility,
        impl_target: Option<(&str, &[Type])>,
    ) -> Option<FunctionDef> {
        let start = self.previous_span_end();
        let name = self.expect_ident("Expected function name")?;
        let type_params = self.parse_optional_generic_params()?;
        self.expect(TokenKind::LParen, "Expected '(' after function name")?;
        let mut params = Vec::new();
        while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
            let param_name = self.expect_ident("Expected parameter name")?;
            let param_ty =
                if param_name == "self" && impl_target.is_some() && !self.at(TokenKind::Colon) {
                    let (impl_target_name, impl_target_args) =
                        impl_target.expect("impl target checked above");
                    Type {
                        path: vec![impl_target_name.to_string()],
                        args: impl_target_args.to_vec(),
                        trait_bounds: Vec::new(),
                    }
                } else {
                    if self.match_kind(TokenKind::Colon) {
                        self.parse_type()?
                    } else {
                        inferred_placeholder_type()
                    }
                };
            params.push(Param {
                name: param_name,
                ty: param_ty,
            });
            if !self.match_kind(TokenKind::Comma) {
                break;
            }
        }
        self.expect(TokenKind::RParen, "Expected ')' after parameter list")?;
        let return_type = if self.match_kind(TokenKind::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };
        let effect_row = self.parse_optional_effect_row()?;
        let body = self.parse_block()?;
        Some(FunctionDef {
            visibility,
            name,
            type_params,
            params,
            return_type,
            effect_row,
            body,
            span: Some(self.span_from_start(start)),
        })
    }

    fn parse_trait_method_signature(&mut self) -> Option<TraitMethodSig> {
        let start = self.previous_span_end();
        let name = self.expect_ident("Expected trait method name")?;
        let type_params = self.parse_optional_generic_params()?;
        self.expect(TokenKind::LParen, "Expected '(' after trait method name")?;
        let mut params = Vec::new();
        while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
            let param_name = self.expect_ident("Expected parameter name")?;
            let param_ty = if param_name == "self" && !self.at(TokenKind::Colon) {
                Type {
                    path: vec!["Self".to_string()],
                    args: Vec::new(),
                    trait_bounds: Vec::new(),
                }
            } else {
                self.expect(TokenKind::Colon, "Expected ':' after parameter name")?;
                self.parse_type()?
            };
            params.push(Param {
                name: param_name,
                ty: param_ty,
            });
            if !self.match_kind(TokenKind::Comma) {
                break;
            }
        }
        self.expect(TokenKind::RParen, "Expected ')' after parameter list")?;
        let return_type = if self.match_kind(TokenKind::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };
        let effect_row = self.parse_optional_effect_row()?;
        self.expect(
            TokenKind::Semicolon,
            "Expected ';' after trait method signature",
        )?;
        Some(TraitMethodSig {
            name,
            type_params,
            params,
            return_type,
            effect_row,
            span: Some(self.span_from_start(start)),
        })
    }

    fn parse_impl(&mut self) -> Option<ImplBlock> {
        let start = self.previous_span_end();
        let type_params = self.parse_optional_generic_params()?;
        let first = self.parse_type()?;
        let mut trait_target = None;
        let target_ty = if self.match_kind(TokenKind::For) {
            trait_target = Some(first);
            self.parse_type()?
        } else {
            first
        };
        if target_ty.path.is_empty() {
            self.error_current("Expected concrete nominal type after `impl`");
            return None;
        }
        let target = target_ty.path.join("::");
        let target_args = target_ty.args.clone();
        self.expect(TokenKind::LBrace, "Expected '{' to start impl block")?;
        let mut methods = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let method_visibility = if self.match_kind(TokenKind::Pub) {
                Visibility::Public
            } else {
                Visibility::Private
            };
            self.expect(TokenKind::Fn, "Expected `fn` in impl block")?;
            methods.push(self.parse_function(method_visibility, Some((&target, &target_args)))?);
        }
        self.expect(TokenKind::RBrace, "Expected '}' after impl block")?;
        Some(ImplBlock {
            type_params,
            target,
            target_args,
            trait_target,
            methods,
            span: Some(self.span_from_start(start)),
        })
    }

    fn parse_optional_generic_params(&mut self) -> Option<Vec<GenericParam>> {
        let mut type_params = Vec::new();
        if self.match_kind(TokenKind::Lt) {
            while !self.at(TokenKind::Gt) && !self.at(TokenKind::Eof) {
                let param_name = self.expect_ident("Expected generic type parameter name")?;
                let mut bounds = Vec::new();
                if self.match_kind(TokenKind::Colon) {
                    bounds.push(self.parse_type_bound()?);
                    while self.match_kind(TokenKind::Plus) {
                        bounds.push(self.parse_type_bound()?);
                    }
                }
                type_params.push(GenericParam {
                    name: param_name,
                    bounds,
                });
                if !self.match_kind(TokenKind::Comma) {
                    break;
                }
            }
            self.expect(TokenKind::Gt, "Expected '>' after generic type parameters")?;
        }
        Some(type_params)
    }

    fn parse_optional_effect_row(&mut self) -> Option<Option<EffectRow>> {
        if !self.match_kind(TokenKind::Bang) {
            return Some(None);
        }
        self.expect(TokenKind::LBracket, "Expected '[' after '!' in effect row")?;
        let mut caps = Vec::new();
        let mut rest = None;
        while !self.at(TokenKind::RBracket) && !self.at(TokenKind::Eof) {
            if self.match_kind(TokenKind::DotDot) {
                rest = Some(self.expect_ident("Expected row tail variable after `..`")?);
                if self.match_kind(TokenKind::Plus) {
                    self.error_current("`..row` must be the last element in an effect row");
                    return None;
                }
                break;
            }
            caps.push(self.parse_path("Expected effect capability path")?);
            if !self.match_kind(TokenKind::Plus) {
                break;
            }
        }
        self.expect(TokenKind::RBracket, "Expected ']' after effect row")?;
        Some(Some(EffectRow { caps, rest }))
    }

    fn parse_const_item(&mut self, visibility: Visibility) -> Option<ConstDef> {
        let start = self.previous_span_end();
        let name = self.expect_ident("Expected const name")?;
        let ty = if self.match_kind(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect(TokenKind::Equal, "Expected '=' after const declaration")?;
        let value = self.parse_expr()?;
        self.expect(TokenKind::Semicolon, "Expected ';' after const item")?;
        Some(ConstDef {
            visibility,
            name,
            ty,
            value,
            is_const: true,
            span: Some(self.span_from_start(start)),
        })
    }

    fn parse_static_item(&mut self, visibility: Visibility) -> Option<StaticDef> {
        let start = self.previous_span_end();
        let name = self.expect_ident("Expected static name")?;
        self.expect(TokenKind::Colon, "Expected ':' after static name")?;
        let ty = self.parse_type()?;
        self.expect(TokenKind::Equal, "Expected '=' after static type")?;
        let value = self.parse_expr()?;
        self.expect(TokenKind::Semicolon, "Expected ';' after static item")?;
        Some(StaticDef {
            visibility,
            name,
            ty,
            value,
            span: Some(self.span_from_start(start)),
        })
    }

    fn parse_block(&mut self) -> Option<Block> {
        self.expect(TokenKind::LBrace, "Expected '{' to start block")?;
        let mut statements = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let stmt = self.parse_stmt()?;
            statements.push(stmt);
        }
        self.expect(TokenKind::RBrace, "Expected '}' after block")?;
        Some(Block { statements })
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        if self.match_kind(TokenKind::Const) {
            return self.parse_local_binding_stmt(true);
        }
        if self.match_kind(TokenKind::Let) {
            return self.parse_local_binding_stmt(false);
        }
        if self.match_kind(TokenKind::Return) {
            if self.match_kind(TokenKind::Semicolon) {
                return Some(Stmt::Return(None));
            }
            let expr = self.parse_expr()?;
            self.expect(TokenKind::Semicolon, "Expected ';' after return value")?;
            return Some(Stmt::Return(Some(expr)));
        }
        if self.match_kind(TokenKind::If) {
            let condition = self.parse_expr()?;
            let then_block = self.parse_block()?;
            let else_block = if self.match_kind(TokenKind::Else) {
                Some(self.parse_block()?)
            } else {
                None
            };
            let should_rewrite_as_tail_if = self.at(TokenKind::RBrace)
                && block_ends_with_tail_expr(&then_block)
                && else_block
                    .as_ref()
                    .is_some_and(block_ends_with_tail_expr);
            if should_rewrite_as_tail_if {
                let else_block = else_block.expect("tail-if rewrite requires else branch");
                return Some(Stmt::TailExpr(Expr::Match {
                    scrutinee: Box::new(condition),
                    arms: vec![
                        MatchArm {
                            pattern: Pattern::Bool(true),
                            guard: None,
                            value: Expr::Block(then_block),
                        },
                        MatchArm {
                            pattern: Pattern::Bool(false),
                            guard: None,
                            value: Expr::Block(else_block),
                        },
                    ],
                }));
            }
            return Some(Stmt::If {
                condition,
                then_block,
                else_block,
            });
        }
        if self.match_kind(TokenKind::While) {
            let condition = self.parse_expr()?;
            let body = self.parse_block()?;
            return Some(Stmt::While { condition, body });
        }
        if self.match_kind(TokenKind::For) {
            let binding = self.parse_destructure_pattern_atom()?;
            self.expect(TokenKind::In, "Expected `in` after `for` loop binding")?;
            let iter = self.parse_expr()?;
            let body = self.parse_block()?;
            return Some(Stmt::For {
                binding,
                iter,
                body,
            });
        }
        if self.match_kind(TokenKind::Loop) {
            let body = self.parse_block()?;
            return Some(Stmt::Loop { body });
        }
        if self.match_kind(TokenKind::Break) {
            self.expect(TokenKind::Semicolon, "Expected ';' after `break`")?;
            return Some(Stmt::Break);
        }
        if self.match_kind(TokenKind::Continue) {
            self.expect(TokenKind::Semicolon, "Expected ';' after `continue`")?;
            return Some(Stmt::Continue);
        }
        if let TokenKind::RustBlock(code) = self.peek().kind.clone() {
            self.advance();
            return Some(Stmt::RustBlock(code));
        }

        let expr = self.parse_expr()?;
        if self.match_kind(TokenKind::Equal) || self.match_kind(TokenKind::PlusEqual) {
            let op = if matches!(self.tokens[self.cursor - 1].kind, TokenKind::PlusEqual) {
                AssignOp::AddAssign
            } else {
                AssignOp::Assign
            };
            let value = self.parse_expr()?;
            self.expect(
                TokenKind::Semicolon,
                "Expected ';' after assignment statement",
            )?;
            let target = self.expr_to_assign_target(expr)?;
            return Some(Stmt::Assign { target, op, value });
        }
        if self.match_kind(TokenKind::Semicolon) {
            return Some(Stmt::Expr(expr));
        }
        if self.at(TokenKind::RBrace) {
            return Some(Stmt::TailExpr(expr));
        }
        self.error_current("Expected ';' after expression statement");
        None
    }

    fn parse_local_binding_stmt(&mut self, is_const: bool) -> Option<Stmt> {
        let start = self.previous_span_end();
        if self.match_kind(TokenKind::LParen) || self.match_kind(TokenKind::LBracket) {
            let pattern = if matches!(self.tokens[self.cursor - 1].kind, TokenKind::LParen) {
                self.parse_destructure_pattern_tuple()?
            } else {
                self.parse_destructure_pattern_slice()?
            };
            self.expect(
                TokenKind::Equal,
                "Expected '=' after destructuring binding pattern",
            )?;
            let value = self.parse_expr()?;
            self.expect(
                TokenKind::Semicolon,
                "Expected ';' after destructuring binding",
            )?;
            return Some(Stmt::DestructureConst {
                pattern,
                value,
                is_const,
            });
        }
        let keyword = if is_const { "const" } else { "let" };
        let name = self.expect_ident(&format!("Expected {keyword} name"))?;
        let ty = if self.match_kind(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect(
            TokenKind::Equal,
            &format!("Expected '=' after {keyword} declaration"),
        )?;
        let value = self.parse_expr()?;
        self.expect(
            TokenKind::Semicolon,
            &format!("Expected ';' after {keyword} statement"),
        )?;
        Some(Stmt::Const(ConstDef {
            visibility: Visibility::Private,
            name,
            ty,
            value,
            is_const,
            span: Some(self.span_from_start(start)),
        }))
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        if self.match_kind(TokenKind::Match) {
            return self.parse_match_expr();
        }
        self.parse_range_expr()
    }

    fn parse_range_expr(&mut self) -> Option<Expr> {
        if self.match_kind(TokenKind::DotDot) || self.match_kind(TokenKind::DotDotEq) {
            let inclusive = matches!(self.tokens[self.cursor - 1].kind, TokenKind::DotDotEq);
            let end = if self.range_rhs_starts_here() {
                Some(Box::new(self.parse_or_expr()?))
            } else {
                None
            };
            return Some(Expr::Range {
                start: None,
                end,
                inclusive,
            });
        }

        let left = self.parse_or_expr()?;
        if self.match_kind(TokenKind::DotDot) || self.match_kind(TokenKind::DotDotEq) {
            let inclusive = matches!(self.tokens[self.cursor - 1].kind, TokenKind::DotDotEq);
            let end = if self.range_rhs_starts_here() {
                Some(Box::new(self.parse_or_expr()?))
            } else {
                None
            };
            return Some(Expr::Range {
                start: Some(Box::new(left)),
                end,
                inclusive,
            });
        }
        Some(left)
    }

    fn parse_or_expr(&mut self) -> Option<Expr> {
        let mut expr = self.parse_and_expr()?;
        while self.match_kind(TokenKind::Or) {
            let right = self.parse_and_expr()?;
            expr = Expr::Binary {
                op: BinaryOp::Or,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Some(expr)
    }

    fn parse_and_expr(&mut self) -> Option<Expr> {
        let mut expr = self.parse_cmp_expr()?;
        while self.match_kind(TokenKind::And) {
            let right = self.parse_cmp_expr()?;
            expr = Expr::Binary {
                op: BinaryOp::And,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Some(expr)
    }

    fn parse_cmp_expr(&mut self) -> Option<Expr> {
        let mut expr = self.parse_add_expr()?;
        loop {
            let op = if self.match_kind(TokenKind::EqualEqual) {
                Some(BinaryOp::Eq)
            } else if self.match_kind(TokenKind::BangEqual) {
                Some(BinaryOp::Ne)
            } else if self.match_kind(TokenKind::LtEqual) {
                Some(BinaryOp::Le)
            } else if self.match_kind(TokenKind::GtEqual) {
                Some(BinaryOp::Ge)
            } else if self.match_kind(TokenKind::Lt) {
                Some(BinaryOp::Lt)
            } else if self.match_kind(TokenKind::Gt) {
                Some(BinaryOp::Gt)
            } else {
                None
            };
            if let Some(op) = op {
                let right = self.parse_add_expr()?;
                expr = Expr::Binary {
                    op,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Some(expr)
    }

    fn parse_add_expr(&mut self) -> Option<Expr> {
        let mut expr = self.parse_mul_expr()?;
        loop {
            let op = if self.match_kind(TokenKind::Plus) {
                Some(BinaryOp::Add)
            } else if self.match_kind(TokenKind::Minus) {
                Some(BinaryOp::Sub)
            } else {
                None
            };
            let Some(op) = op else {
                break;
            };
            let right = self.parse_mul_expr()?;
            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Some(expr)
    }

    fn parse_mul_expr(&mut self) -> Option<Expr> {
        let mut expr = self.parse_unary_expr()?;
        loop {
            let op = if self.match_kind(TokenKind::Star) {
                Some(BinaryOp::Mul)
            } else if self.match_kind(TokenKind::Slash) {
                Some(BinaryOp::Div)
            } else if self.match_kind(TokenKind::Percent) {
                Some(BinaryOp::Rem)
            } else {
                None
            };
            let Some(op) = op else {
                break;
            };
            let right = self.parse_unary_expr()?;
            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Some(expr)
    }

    fn parse_unary_expr(&mut self) -> Option<Expr> {
        if self.match_kind(TokenKind::Bang) || self.match_kind(TokenKind::Not) {
            let expr = self.parse_unary_expr()?;
            return Some(Expr::Unary {
                op: UnaryOp::Not,
                expr: Box::new(expr),
            });
        }
        if self.match_kind(TokenKind::Minus) {
            let expr = self.parse_unary_expr()?;
            return Some(Expr::Unary {
                op: UnaryOp::Neg,
                expr: Box::new(expr),
            });
        }
        self.parse_postfix_expr()
    }

    fn parse_match_expr(&mut self) -> Option<Expr> {
        let scrutinee = self.parse_expr()?;
        self.expect(TokenKind::LBrace, "Expected '{' after match expression")?;
        let mut arms = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let pattern = self.parse_pattern()?;
            let guard = if self.match_kind(TokenKind::If) {
                Some(self.parse_expr()?)
            } else {
                None
            };
            self.expect(TokenKind::FatArrow, "Expected `=>` in match arm")?;
            let value = if self.at(TokenKind::LBrace) {
                let body = self.parse_block()?;
                Expr::Call {
                    callee: Box::new(Expr::Closure {
                        params: Vec::new(),
                        return_type: None,
                        body,
                    }),
                    args: Vec::new(),
                }
            } else {
                self.parse_expr()?
            };
            self.expect(
                TokenKind::Semicolon,
                "Expected ';' after match arm expression",
            )?;
            arms.push(MatchArm {
                pattern,
                guard,
                value,
            });
        }
        self.expect(TokenKind::RBrace, "Expected '}' after match arms")?;
        Some(Expr::Match {
            scrutinee: Box::new(scrutinee),
            arms,
        })
    }

    fn parse_pattern(&mut self) -> Option<Pattern> {
        let mut patterns = vec![self.parse_pattern_atom()?];
        while self.match_kind(TokenKind::Pipe) {
            patterns.push(self.parse_pattern_atom()?);
        }
        if patterns.len() == 1 {
            Some(patterns.remove(0))
        } else {
            Some(Pattern::Or(patterns))
        }
    }

    fn parse_pattern_atom(&mut self) -> Option<Pattern> {
        if self.match_kind(TokenKind::Underscore) {
            return Some(Pattern::Wildcard);
        }
        if self.match_kind(TokenKind::DotDot) || self.match_kind(TokenKind::DotDotEq) {
            let inclusive = matches!(self.tokens[self.cursor - 1].kind, TokenKind::DotDotEq);
            let end = if let TokenKind::IntLiteral(value) = self.peek().kind.clone() {
                self.advance();
                Some(value)
            } else {
                None
            };
            return Some(Pattern::Range {
                start: None,
                end,
                inclusive,
            });
        }
        if let TokenKind::IntLiteral(value) = self.peek().kind.clone() {
            self.advance();
            if self.match_kind(TokenKind::DotDot) || self.match_kind(TokenKind::DotDotEq) {
                let inclusive = matches!(self.tokens[self.cursor - 1].kind, TokenKind::DotDotEq);
                let end = if let TokenKind::IntLiteral(end_value) = self.peek().kind.clone() {
                    self.advance();
                    Some(end_value)
                } else {
                    None
                };
                return Some(Pattern::Range {
                    start: Some(value),
                    end,
                    inclusive,
                });
            }
            return Some(Pattern::Int(value));
        }
        if self.match_kind(TokenKind::True) {
            return Some(Pattern::Bool(true));
        }
        if self.match_kind(TokenKind::False) {
            return Some(Pattern::Bool(false));
        }
        if let TokenKind::CharLiteral(value) = self.peek().kind.clone() {
            self.advance();
            return Some(Pattern::Char(value));
        }
        if let TokenKind::StringLiteral(value) = self.peek().kind.clone() {
            self.advance();
            return Some(Pattern::String(value));
        }
        if self.match_kind(TokenKind::LParen) {
            if self.match_kind(TokenKind::RParen) {
                return Some(Pattern::Tuple(Vec::new()));
            }
            let first = self.parse_pattern()?;
            if self.match_kind(TokenKind::Comma) {
                let mut items = vec![first];
                while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
                    items.push(self.parse_pattern()?);
                    if !self.match_kind(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect(TokenKind::RParen, "Expected ')' after tuple pattern")?;
                return Some(Pattern::Tuple(items));
            }
            self.expect(
                TokenKind::RParen,
                "Expected ')' after parenthesized pattern",
            )?;
            return Some(first);
        }
        if self.match_kind(TokenKind::LBracket) {
            return self.parse_slice_pattern();
        }

        let path = self.parse_path("Expected pattern path")?;
        let payload = if self.match_kind(TokenKind::LParen) {
            if self.match_kind(TokenKind::RParen) {
                self.error_current("Variant payload pattern cannot be empty");
                return None;
            }
            let first = self.parse_pattern()?;
            if self.match_kind(TokenKind::Comma) {
                let mut items = vec![first];
                while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
                    items.push(self.parse_pattern()?);
                    if !self.match_kind(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect(
                    TokenKind::RParen,
                    "Expected ')' after variant payload pattern",
                )?;
                Some(Box::new(Pattern::Tuple(items)))
            } else {
                self.expect(
                    TokenKind::RParen,
                    "Expected ')' after variant payload pattern",
                )?;
                Some(Box::new(first))
            }
        } else {
            None
        };
        if self.match_kind(TokenKind::LBrace) {
            let mut fields = Vec::new();
            let mut has_rest = false;
            while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
                if self.match_kind(TokenKind::DotDot) {
                    has_rest = true;
                    self.match_kind(TokenKind::Comma);
                    break;
                }
                let name = self.expect_ident("Expected struct pattern field name")?;
                let pattern = if self.match_kind(TokenKind::Colon) {
                    self.parse_pattern()?
                } else {
                    Pattern::Binding(name.clone())
                };
                fields.push(PatternField { name, pattern });
                if !self.match_kind(TokenKind::Comma) {
                    break;
                }
            }
            self.expect(
                TokenKind::RBrace,
                "Expected '}' after struct pattern fields",
            )?;
            return Some(Pattern::Struct {
                path,
                fields,
                has_rest,
            });
        }
        if path.len() == 1 && payload.is_none() && self.match_kind(TokenKind::At) {
            let inner = self.parse_pattern_atom()?;
            return Some(Pattern::BindingAt {
                name: path[0].clone(),
                pattern: Box::new(inner),
            });
        }
        if path.len() == 1 && payload.is_none() {
            return Some(Pattern::Binding(path[0].clone()));
        }
        Some(Pattern::Variant { path, payload })
    }

    fn parse_slice_pattern(&mut self) -> Option<Pattern> {
        let mut prefix = Vec::new();
        let mut suffix = Vec::new();
        let mut rest: Option<String> = None;
        let mut parsing_suffix = false;

        while !self.at(TokenKind::RBracket) && !self.at(TokenKind::Eof) {
            if self.match_kind(TokenKind::DotDot) {
                if rest.is_some() {
                    self.error_current("Slice pattern can contain at most one `..` rest");
                    return None;
                }
                rest = if let TokenKind::Identifier(name) = self.peek().kind.clone() {
                    self.advance();
                    Some(name)
                } else {
                    None
                };
                parsing_suffix = true;
            } else {
                let pattern = self.parse_pattern()?;
                if parsing_suffix {
                    suffix.push(pattern);
                } else {
                    prefix.push(pattern);
                }
            }

            if !self.match_kind(TokenKind::Comma) {
                break;
            }
        }

        self.expect(TokenKind::RBracket, "Expected ']' after slice pattern")?;
        Some(Pattern::Slice {
            prefix,
            rest,
            suffix,
        })
    }

    fn parse_postfix_expr(&mut self) -> Option<Expr> {
        let mut expr = self.parse_primary_expr()?;
        loop {
            if self.match_kind(TokenKind::Bang) {
                let Expr::Path(path) = &expr else {
                    self.error_current("Macro invocation requires a path before '!'");
                    return None;
                };
                self.expect(TokenKind::LParen, "Expected '(' after macro '!'")?;
                let mut args = Vec::new();
                while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
                    args.push(self.parse_expr()?);
                    if !self.match_kind(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect(TokenKind::RParen, "Expected ')' after macro arguments")?;
                expr = Expr::MacroCall {
                    path: path.clone(),
                    args,
                };
                continue;
            }
            if self.match_kind(TokenKind::LParen) {
                let mut args = Vec::new();
                while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
                    args.push(self.parse_expr()?);
                    if !self.match_kind(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect(TokenKind::RParen, "Expected ')' after function arguments")?;
                expr = Expr::Call {
                    callee: Box::new(expr),
                    args,
                };
                continue;
            }
            if self.match_kind(TokenKind::Dot) {
                let field = self.expect_ident("Expected field name after '.'")?;
                expr = Expr::Field {
                    base: Box::new(expr),
                    field,
                };
                continue;
            }
            if self.match_kind(TokenKind::LBracket) {
                let index = self.parse_expr()?;
                self.expect(TokenKind::RBracket, "Expected ']' after index expression")?;
                expr = Expr::Index {
                    base: Box::new(expr),
                    index: Box::new(index),
                };
                continue;
            }
            if self.match_kind(TokenKind::Question) {
                expr = Expr::Try(Box::new(expr));
                continue;
            }
            break;
        }
        Some(expr)
    }

    fn parse_primary_expr(&mut self) -> Option<Expr> {
        if self.match_kind(TokenKind::Pipe) {
            return self.parse_closure_expr();
        }
        let token = self.peek().clone();
        match token.kind {
            TokenKind::IntLiteral(value) => {
                self.advance();
                Some(Expr::Int(value))
            }
            TokenKind::FloatLiteral(value) => {
                self.advance();
                Some(Expr::Float(value))
            }
            TokenKind::True => {
                self.advance();
                Some(Expr::Bool(true))
            }
            TokenKind::False => {
                self.advance();
                Some(Expr::Bool(false))
            }
            TokenKind::StringLiteral(value) => {
                self.advance();
                Some(Expr::String(value))
            }
            TokenKind::CharLiteral(value) => {
                self.advance();
                Some(Expr::Char(value))
            }
            TokenKind::Identifier(_) => {
                let path_expr = self.parse_expr_path_with_optional_type_args()?;
                let path = match &path_expr {
                    Expr::Path(path) => path.clone(),
                    Expr::PathWithTypeArgs { path, .. } => path.clone(),
                    _ => unreachable!("identifier path parser should only return path expressions"),
                };
                if self.at(TokenKind::LBrace)
                    && path_looks_like_type_name(&path)
                    && self.looks_like_struct_literal_body()
                {
                    self.advance();
                    return self.parse_struct_literal_expr(path);
                }
                Some(path_expr)
            }
            TokenKind::LParen => {
                self.advance();
                if self.match_kind(TokenKind::RParen) {
                    return Some(Expr::Tuple(Vec::new()));
                }
                let first = self.parse_expr()?;
                if self.match_kind(TokenKind::Comma) {
                    let mut items = vec![first];
                    while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
                        items.push(self.parse_expr()?);
                        if !self.match_kind(TokenKind::Comma) {
                            break;
                        }
                    }
                    self.expect(TokenKind::RParen, "Expected ')' after tuple literal")?;
                    Some(Expr::Tuple(items))
                } else {
                    self.expect(
                        TokenKind::RParen,
                        "Expected ')' after parenthesized expression",
                    )?;
                    Some(first)
                }
            }
            TokenKind::LBracket => {
                self.advance();
                let mut items = Vec::new();
                while !self.at(TokenKind::RBracket) && !self.at(TokenKind::Eof) {
                    items.push(self.parse_expr()?);
                    if !self.match_kind(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect(TokenKind::RBracket, "Expected ']' after array literal")?;
                Some(Expr::Array(items))
            }
            TokenKind::LBrace => self.parse_block().map(Expr::Block),
            _ => {
                self.error_current("Expected expression");
                None
            }
        }
    }

    fn parse_type(&mut self) -> Option<Type> {
        let mut ty = self.parse_type_atom()?;
        while self.match_kind(TokenKind::Plus) {
            ty.trait_bounds.push(self.parse_type_atom()?);
        }
        Some(ty)
    }

    fn parse_type_bound(&mut self) -> Option<Type> {
        self.parse_type_atom()
    }

    fn parse_type_atom(&mut self) -> Option<Type> {
        if self.match_kind(TokenKind::Underscore) {
            return Some(inferred_placeholder_type());
        }
        if self.match_kind(TokenKind::LParen) {
            if self.match_kind(TokenKind::RParen) {
                return Some(Type {
                    path: vec!["Tuple".to_string()],
                    args: Vec::new(),
                    trait_bounds: Vec::new(),
                });
            }
            let first = self.parse_type()?;
            if self.match_kind(TokenKind::Comma) {
                let mut items = vec![first];
                while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
                    items.push(self.parse_type()?);
                    if !self.match_kind(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect(TokenKind::RParen, "Expected ')' after tuple type")?;
                return Some(Type {
                    path: vec!["Tuple".to_string()],
                    args: items,
                    trait_bounds: Vec::new(),
                });
            }
            self.expect(TokenKind::RParen, "Expected ')' after parenthesized type")?;
            return Some(first);
        }

        let path = self.parse_path("Expected type name")?;
        let mut args = Vec::new();
        if self.match_kind(TokenKind::Lt) {
            while !self.at(TokenKind::Gt) && !self.at(TokenKind::Eof) {
                args.push(self.parse_type()?);
                if !self.match_kind(TokenKind::Comma) {
                    break;
                }
            }
            self.expect(TokenKind::Gt, "Expected '>' to close generic arguments")?;
        }
        Some(Type {
            path,
            args,
            trait_bounds: Vec::new(),
        })
    }

    fn parse_struct_literal_expr(&mut self, path: Vec<String>) -> Option<Expr> {
        let mut fields = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let name = self.expect_ident("Expected field name in struct literal")?;
            self.expect(
                TokenKind::Colon,
                "Expected ':' after struct literal field name",
            )?;
            let value = self.parse_expr()?;
            fields.push(StructLiteralField { name, value });

            if self.match_kind(TokenKind::Comma) || self.match_kind(TokenKind::Semicolon) {
                continue;
            }
            if !self.at(TokenKind::RBrace) {
                self.error_current("Expected ',' or ';' between struct literal fields");
                return None;
            }
        }
        self.expect(TokenKind::RBrace, "Expected '}' after struct literal")?;
        Some(Expr::StructLiteral { path, fields })
    }

    fn looks_like_struct_literal_body(&self) -> bool {
        let Some(next) = self.tokens.get(self.cursor + 1) else {
            return false;
        };
        match &next.kind {
            TokenKind::RBrace => true,
            TokenKind::Identifier(_) => matches!(
                self.tokens.get(self.cursor + 2).map(|tok| &tok.kind),
                Some(TokenKind::Colon)
            ),
            _ => false,
        }
    }

    fn parse_closure_expr(&mut self) -> Option<Expr> {
        let mut params = Vec::new();
        while !self.at(TokenKind::Pipe) && !self.at(TokenKind::Eof) {
            let name = self.expect_ident("Expected closure parameter name")?;
            self.expect(
                TokenKind::Colon,
                "Expected ':' after closure parameter name",
            )?;
            let ty = self.parse_type()?;
            params.push(Param { name, ty });
            if !self.match_kind(TokenKind::Comma) {
                break;
            }
        }
        self.expect(TokenKind::Pipe, "Expected '|' after closure parameters")?;
        let return_type = if self.match_kind(TokenKind::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };
        let body = self.parse_block()?;
        Some(Expr::Closure {
            params,
            return_type,
            body,
        })
    }

    fn parse_path(&mut self, message: &str) -> Option<Vec<String>> {
        let mut path = Vec::new();
        path.push(self.expect_ident(message)?);
        while self.match_kind(TokenKind::ColonColon) {
            path.push(self.expect_ident("Expected identifier after '::'")?);
        }
        Some(path)
    }

    fn parse_expr_path_with_optional_type_args(&mut self) -> Option<Expr> {
        let mut path = Vec::new();
        path.push(self.expect_ident("Expected identifier path")?);

        let mut type_args = Vec::new();
        if self.at(TokenKind::Lt) && self.looks_like_expr_path_type_args() {
            self.advance();
            while !self.at(TokenKind::Gt) && !self.at(TokenKind::Eof) {
                type_args.push(self.parse_type()?);
                if !self.match_kind(TokenKind::Comma) {
                    break;
                }
            }
            self.expect(TokenKind::Gt, "Expected '>' to close generic arguments")?;
        }

        while self.match_kind(TokenKind::ColonColon) {
            path.push(self.expect_ident("Expected identifier after '::'")?);
        }

        if type_args.is_empty() {
            Some(Expr::Path(path))
        } else {
            Some(Expr::PathWithTypeArgs { path, type_args })
        }
    }

    fn looks_like_expr_path_type_args(&self) -> bool {
        if !self.at(TokenKind::Lt) {
            return false;
        }
        let mut idx = self.cursor;
        let mut depth = 0usize;
        while let Some(token) = self.tokens.get(idx) {
            match token.kind {
                TokenKind::Lt => depth += 1,
                TokenKind::Gt => {
                    if depth == 0 {
                        return false;
                    }
                    depth -= 1;
                    if depth == 0 {
                        return matches!(
                            self.tokens.get(idx + 1).map(|next| &next.kind),
                            Some(TokenKind::ColonColon) | Some(TokenKind::LParen)
                        );
                    }
                }
                TokenKind::Eof => return false,
                _ => {}
            }
            idx += 1;
        }
        false
    }

    fn parse_destructure_pattern_tuple(&mut self) -> Option<DestructurePattern> {
        let mut items = Vec::new();
        while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
            items.push(self.parse_destructure_pattern_atom()?);
            if !self.match_kind(TokenKind::Comma) {
                break;
            }
        }
        self.expect(
            TokenKind::RParen,
            "Expected ')' after destructuring pattern",
        )?;
        Some(DestructurePattern::Tuple(items))
    }

    fn parse_destructure_pattern_atom(&mut self) -> Option<DestructurePattern> {
        if self.match_kind(TokenKind::Underscore) {
            return Some(DestructurePattern::Ignore);
        }
        if self.match_kind(TokenKind::LParen) {
            return self.parse_destructure_pattern_tuple();
        }
        if self.match_kind(TokenKind::LBracket) {
            return self.parse_destructure_pattern_slice();
        }
        let name = self.expect_ident("Expected identifier in destructuring pattern")?;
        Some(DestructurePattern::Name(name))
    }

    fn parse_destructure_pattern_slice(&mut self) -> Option<DestructurePattern> {
        let mut prefix = Vec::new();
        let mut suffix = Vec::new();
        let mut rest: Option<String> = None;
        let mut parsing_suffix = false;

        while !self.at(TokenKind::RBracket) && !self.at(TokenKind::Eof) {
            if self.match_kind(TokenKind::DotDot) {
                if rest.is_some() {
                    self.error_current(
                        "Destructure slice pattern can contain at most one `..` rest",
                    );
                    return None;
                }
                rest = if let TokenKind::Identifier(name) = self.peek().kind.clone() {
                    self.advance();
                    Some(name)
                } else {
                    None
                };
                parsing_suffix = true;
            } else {
                let pattern = self.parse_destructure_pattern_atom()?;
                if parsing_suffix {
                    suffix.push(pattern);
                } else {
                    prefix.push(pattern);
                }
            }

            if !self.match_kind(TokenKind::Comma) {
                break;
            }
        }

        self.expect(
            TokenKind::RBracket,
            "Expected ']' after destructuring slice pattern",
        )?;
        Some(DestructurePattern::Slice {
            prefix,
            rest,
            suffix,
        })
    }

    fn range_rhs_starts_here(&self) -> bool {
        !(self.at(TokenKind::Semicolon)
            || self.at(TokenKind::Comma)
            || self.at(TokenKind::RParen)
            || self.at(TokenKind::RBrace)
            || self.at(TokenKind::FatArrow))
    }

    fn expr_to_assign_target(&mut self, expr: Expr) -> Option<AssignTarget> {
        match expr {
            Expr::Path(path) => {
                if path.len() == 1 {
                    Some(AssignTarget::Path(path[0].clone()))
                } else {
                    self.error_current("Assignment target path must be a local identifier");
                    None
                }
            }
            Expr::Field { base, field } => Some(AssignTarget::Field { base, field }),
            Expr::Index { base, index } => Some(AssignTarget::Index { base, index }),
            Expr::Tuple(items) => {
                let mut targets = Vec::new();
                for item in items {
                    targets.push(self.expr_to_assign_target(item)?);
                }
                Some(AssignTarget::Tuple(targets))
            }
            _ => {
                self.error_current(
                    "Invalid assignment target; expected identifier, field access, index access, or tuple target",
                );
                None
            }
        }
    }

    fn expect_ident(&mut self, message: &str) -> Option<String> {
        let token = self.peek().clone();
        match token.kind {
            TokenKind::Identifier(name) => {
                self.advance();
                Some(name)
            }
            _ => {
                self.error_current(message);
                None
            }
        }
    }

    fn expect(&mut self, kind: TokenKind, message: &str) -> Option<()> {
        if self.at(kind.clone()) {
            self.advance();
            Some(())
        } else {
            self.error_current(message);
            None
        }
    }

    fn match_kind(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn at(&self, kind: TokenKind) -> bool {
        same_variant(&self.peek().kind, &kind)
    }

    fn advance(&mut self) {
        if self.cursor < self.tokens.len().saturating_sub(1) {
            self.cursor += 1;
        }
    }

    fn previous_span_end(&self) -> usize {
        self.tokens
            .get(self.cursor.saturating_sub(1))
            .map(|token| token.span.start)
            .unwrap_or_else(|| self.peek().span.start)
    }

    fn span_from_start(&self, start: usize) -> crate::diag::Span {
        let end = self
            .tokens
            .get(self.cursor.saturating_sub(1))
            .map(|token| token.span.end)
            .unwrap_or(start);
        crate::diag::Span::new(start, end.max(start))
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.cursor]
    }

    fn error_current(&mut self, message: &str) {
        let span = self.peek().span;
        self.diagnostics.push(Diagnostic::new(message, span));
    }

    fn synchronize_top_level(&mut self) {
        while !self.at(TokenKind::Eof) {
            if self.match_kind(TokenKind::Semicolon) {
                return;
            }
            if self.at(TokenKind::Rust)
                || matches!(self.peek().kind, TokenKind::RustBlock(_))
                || self.at(TokenKind::Pub)
                || self.at(TokenKind::Use)
                || self.at(TokenKind::Struct)
                || self.at(TokenKind::Enum)
                || self.at(TokenKind::Trait)
                || self.at(TokenKind::Impl)
                || self.at(TokenKind::Fn)
                || self.at(TokenKind::Const)
                || self.at(TokenKind::Static)
            {
                return;
            }
            self.advance();
        }
    }
}

fn block_ends_with_tail_expr(block: &Block) -> bool {
    matches!(block.statements.last(), Some(Stmt::TailExpr(_)))
}

fn inferred_placeholder_type() -> Type {
    Type {
        path: vec!["_".to_string()],
        args: Vec::new(),
        trait_bounds: Vec::new(),
    }
}

fn path_looks_like_type_name(path: &[String]) -> bool {
    path.last()
        .and_then(|segment| segment.chars().next())
        .is_some_and(|ch| ch.is_ascii_uppercase())
}

fn same_variant(left: &TokenKind, right: &TokenKind) -> bool {
    use TokenKind::*;
    matches!(
        (left, right),
        (Rust, Rust)
            | (RustBlock(_), RustBlock(_))
            | (Use, Use)
            | (Struct, Struct)
            | (Enum, Enum)
            | (Trait, Trait)
            | (Impl, Impl)
            | (Fn, Fn)
            | (Let, Let)
            | (Const, Const)
            | (Static, Static)
            | (Return, Return)
            | (If, If)
            | (Else, Else)
            | (While, While)
            | (For, For)
            | (In, In)
            | (Loop, Loop)
            | (Break, Break)
            | (Continue, Continue)
            | (Pub, Pub)
            | (Match, Match)
            | (And, And)
            | (Or, Or)
            | (Not, Not)
            | (True, True)
            | (False, False)
            | (Underscore, Underscore)
            | (Identifier(_), Identifier(_))
            | (IntLiteral(_), IntLiteral(_))
            | (FloatLiteral(_), FloatLiteral(_))
            | (CharLiteral(_), CharLiteral(_))
            | (StringLiteral(_), StringLiteral(_))
            | (LBrace, LBrace)
            | (RBrace, RBrace)
            | (LParen, LParen)
            | (RParen, RParen)
            | (LBracket, LBracket)
            | (RBracket, RBracket)
            | (Colon, Colon)
            | (ColonColon, ColonColon)
            | (Semicolon, Semicolon)
            | (Comma, Comma)
            | (Dot, Dot)
            | (DotDot, DotDot)
            | (DotDotEq, DotDotEq)
            | (Pipe, Pipe)
            | (At, At)
            | (Bang, Bang)
            | (Plus, Plus)
            | (PlusEqual, PlusEqual)
            | (Minus, Minus)
            | (Star, Star)
            | (Slash, Slash)
            | (Percent, Percent)
            | (Equal, Equal)
            | (EqualEqual, EqualEqual)
            | (BangEqual, BangEqual)
            | (FatArrow, FatArrow)
            | (Arrow, Arrow)
            | (Lt, Lt)
            | (LtEqual, LtEqual)
            | (Gt, Gt)
            | (GtEqual, GtEqual)
            | (Question, Question)
            | (Eof, Eof)
    )
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expr, Item, Stmt, UseTree};
    use crate::lexer::lex;

    use super::parse_module;

    #[test]
    fn parser_rejects_rust_use_alias() {
        let source = r#"
            rust
            use std::fmt;
            fn read_value() -> Result<i64, Error> {
                return get_value()?;
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let diagnostics = parse_module(tokens).expect_err("expected parse error");
        assert!(diagnostics.iter().any(|diag| {
            diag.message
                .contains("`rust` prefix on imports is no longer supported")
        }));
    }

    #[test]
    fn parse_plain_use_and_function() {
        let source = r#"
            use std::fmt;
            fn read_value() -> Result<i64, Error> {
                return get_value()?;
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 2);
    }

    #[test]
    fn parse_grouped_use_imports() {
        let source = r#"
            use std::fmt::{Display, Formatter};
            use std::fmt::{self, Display};
            fn read_value() -> i64 { return 1; }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 3);
        match &module.items[0] {
            Item::RustUse(def) => {
                assert_eq!(
                    def.tree,
                    UseTree::Path {
                        segment: "std".to_string(),
                        next: Box::new(UseTree::Path {
                            segment: "fmt".to_string(),
                            next: Box::new(UseTree::Group(vec![
                                UseTree::Name("Display".to_string()),
                                UseTree::Name("Formatter".to_string())
                            ]))
                        })
                    }
                );
            }
            _ => panic!("expected first item to be a use import"),
        }
        match &module.items[1] {
            Item::RustUse(def) => {
                assert_eq!(
                    def.tree,
                    UseTree::Path {
                        segment: "std".to_string(),
                        next: Box::new(UseTree::Path {
                            segment: "fmt".to_string(),
                            next: Box::new(UseTree::Group(vec![
                                UseTree::Name("self".to_string()),
                                UseTree::Name("Display".to_string())
                            ]))
                        })
                    }
                );
            }
            _ => panic!("expected second item to be a use import"),
        }
    }

    #[test]
    fn parse_nested_grouped_use_imports() {
        let source = r#"
            use std::{fmt::{self, Display}, io};
            fn run() { return; }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 2);
        match &module.items[0] {
            Item::RustUse(def) => {
                assert_eq!(
                    def.tree,
                    UseTree::Path {
                        segment: "std".to_string(),
                        next: Box::new(UseTree::Group(vec![
                            UseTree::Path {
                                segment: "fmt".to_string(),
                                next: Box::new(UseTree::Group(vec![
                                    UseTree::Name("self".to_string()),
                                    UseTree::Name("Display".to_string()),
                                ])),
                            },
                            UseTree::Name("io".to_string()),
                        ]))
                    }
                );
            }
            _ => panic!("expected use item"),
        }
    }

    #[test]
    fn parser_reports_missing_semicolon() {
        let source = "const value = 1";
        let tokens = lex(source).expect("expected lex success");
        let diagnostics = parse_module(tokens).expect_err("expected parse error");
        assert!(!diagnostics.is_empty());
    }

    #[test]
    fn parse_match_expression() {
        let source = r#"
            enum Maybe {
                Some(i64);
                None;
            }

            fn unwrap_or_zero(value: Maybe) -> i64 {
                return match value {
                    Maybe::Some(inner) => inner;
                    Maybe::None => 0;
                    _ => 0;
                };
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 2);
    }

    #[test]
    fn parse_visibility_and_control_flow() {
        let source = r#"
            pub fn choose(flag: bool) -> i64 {
                if flag {
                    return 1;
                } else {
                    return 0;
                }
            }

            fn spin(flag: bool) -> i64 {
                while flag {
                    return 1;
                }
                return 0;
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 2);
    }

    #[test]
    fn parse_loop_break_continue() {
        let source = r#"
            fn spin(flag: bool) -> i64 {
                loop {
                    if flag {
                        break;
                    }
                    continue;
                }
                return 0;
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_for_in_loop() {
        let source = r#"
            fn sum_to(n: i64) -> i64 {
                const total = 0;
                for i in 0..=n {
                    total += i;
                }
                return total;
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_let_bindings() {
        let source = r#"
            fn sum_to(n: i64) -> i64 {
                let total = 0;
                for i in 0..=n {
                    total += i;
                }
                total
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_for_loop_with_tuple_destructure() {
        let source = r#"
            fn sum_pairs(pairs: PairIter) -> i64 {
                const total = 0;
                for (left, right) in pairs {
                    total += left;
                    total += right;
                }
                total
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_slice_destructure_patterns() {
        let source = r#"
            fn demo(values: Vec<i64>) -> i64 {
                const [first, ..rest] = values;
                for [left, right] in [[1, 2], [3, 4]] {
                    std::mem::drop(left);
                    std::mem::drop(right);
                }
                std::mem::drop(rest);
                first
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_impl_methods() {
        let source = r#"
            struct Point { x: i64; }
            impl Point {
                pub fn get_x(p: Point) -> i64 {
                    return p.x;
                }
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 2);
    }

    #[test]
    fn parse_impl_methods_with_self_param() {
        let source = r#"
            struct Point { x: i64; }
            impl Point {
                pub fn bump(self, n: i64) -> Point {
                    self.x += n;
                    self
                }
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 2);
    }

    #[test]
    fn parse_trait_impl_methods() {
        let source = r#"
            trait Printable {
                fn render(self: Self) -> String;
            }
            struct Point { x: i64; }
            impl Printable for Point {
                pub fn render(self) -> String {
                    format!("{}", self.x)
                }
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 3);
    }

    #[test]
    fn parse_tail_expression_and_operators() {
        let source = r#"
            pub fn ok(a: bool, b: bool, x: i64, y: i64) -> bool {
                if not a and b {
                    x <= y
                } else {
                    x != y
                }
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_final_if_else_as_tail_expression() {
        let source = r#"
            fn choose(flag: bool) -> i64 {
                if flag {
                    1
                } else {
                    2
                }
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
        let Item::Function(def) = &module.items[0] else {
            panic!("expected function");
        };
        let last = def.body.statements.last().expect("expected statement");
        match last {
            Stmt::TailExpr(Expr::Match { .. }) => {}
            _ => panic!("expected final if/else to parse as tail-expression match"),
        }
    }

    #[test]
    fn parse_ranges_and_tuple_destructure() {
        let source = r#"
            fn f() -> i64 {
                const (a, b) = (1, 2);
                const r1 = a..b;
                const r2 = a..=b;
                return a;
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_tuple_types_in_signatures() {
        let source = r#"
            fn pair(left: i64, right: String) -> (i64, String) {
                (left, right)
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_placeholder_param_types() {
        let source = r#"
            fn add(left: _, right: _) -> i64 {
                return left + right;
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_omitted_param_types() {
        let source = r#"
            fn add(left, right) -> i64 {
                return left + right;
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_traits_and_trait_object_type_unions() {
        let source = r#"
            pub trait Renderable: Debug + Display {
                fn render(self: Self) -> String;
                fn merge<T: Clone + Debug>(self: Self, other: T) -> String;
            }

            fn take(value: Renderable + Send) -> i64 {
                1
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 2);
    }

    #[test]
    fn parse_expression_paths_with_explicit_type_args() {
        let source = r#"
            fn id<T>(value: T) -> T {
                value
            }

            struct Point<T> { x: T; y: T; }
            impl<T> Point<T> {
                fn new(x: T, y: T) -> Self {
                    Point { x: x; y: y; }
                }
            }

            fn run() {
                const a = id<i64>(1);
                const p = Point<i64>::new(1, 2);
                std::mem::drop(a);
                std::mem::drop(p);
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 4);
    }

    #[test]
    fn parse_index_and_slice_expressions() {
        let source = r#"
            fn head(values: Vec<i64>) -> i64 {
                const first = values[0];
                std::mem::drop(values[1..3]);
                first
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_array_literal_expression() {
        let source = r#"
            fn values() -> Vec<i64> {
                [1, 2, 3]
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_assignment_and_add_assign_statements() {
        let source = r#"
            fn bump(n: i64) -> i64 {
                n = n + 1;
                n += 1;
                return n;
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_unary_neg_and_mul_div_rem_expressions() {
        let source = r#"
            fn calc(a: i64, b: i64, c: i64) -> i64 {
                return -a + b * c / 2 % 5 - 1;
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_index_assignment_target() {
        let source = r#"
            fn set(values: Vec<i64>) {
                values[1] = 3;
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_tuple_assignment_target() {
        let source = r#"
            fn swap(a: i64, b: i64) -> i64 {
                (a, b) = (b, a);
                return a;
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_advanced_match_patterns() {
        let source = r#"
            fn f(a: i64, b: i64) -> i64 {
                return match (a, b) {
                    (0, n) => n;
                    (x, 0) => x;
                    _ => a;
                };
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_multi_value_variant_payload_patterns() {
        let source = r#"
            fn classify(value: Pair) -> i64 {
                return match value {
                    Pair::Both(left, right) => left;
                    _ => 0;
                };
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_match_block_arms() {
        let source = r#"
            fn f(v: i64) -> i64 {
                return match v {
                    0 => { 1 };
                    _ => { 2 };
                };
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_match_or_patterns_and_guards() {
        let source = r#"
            fn f(v: i64) -> i64 {
                return match v {
                    0 | 1 if v == 1 => 10;
                    _ => 0;
                };
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_match_binding_at_patterns() {
        let source = r#"
            fn f(v: i64) -> i64 {
                return match v {
                    n @ 0..=10 => n;
                    _ => 0;
                };
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_match_struct_patterns() {
        let source = r#"
            struct Point { x: i64; y: i64; }
            fn f(p: Point) -> i64 {
                return match p {
                    Point { x, y: 0 } => x;
                    _ => 0;
                };
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 2);
    }

    #[test]
    fn parse_match_range_patterns() {
        let source = r#"
            fn f(v: i64) -> i64 {
                return match v {
                    0..10 => 1;
                    10..=20 => 2;
                    ..0 => 3;
                    _ => 4;
                };
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_match_slice_patterns() {
        let source = r#"
            fn f(v: Vec<i64>) -> i64 {
                return match v {
                    [head, ..tail] => head;
                    [single] => single;
                    [] => 0;
                    _ => 1;
                };
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_closure_expression() {
        let source = r#"
            fn f(x: i64) -> i64 {
                const inc = |y: i64| -> i64 { y };
                return inc(x);
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_block_expression() {
        let source = r#"
            fn f(x: i64) -> i64 {
                const y = { const z = x + 1; z };
                y
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_comments_and_raw_multiline_strings() {
        let source = r##"
            /* block comment */
            fn banner() -> String {
                // line comment
                const msg = r#"hello
"elevate"
world"#;
                return msg;
            }
        "##;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_char_literals_in_exprs_and_patterns() {
        let source = r#"
            fn classify(ch: char) -> i64 {
                return match ch {
                    'a' => 1;
                    _ => 0;
                };
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_float_literals_in_expressions() {
        let source = r#"
            fn value() -> f64 {
                const a = 5.25;
                const b = 1_2.5_0e+1_0;
                const c = 0b10_10;
                const d = 0o7_7;
                const e = 0xF_F;
                return a;
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_inline_rust_blocks() {
        let source = r#"
            rust {
                pub fn bridge(v: i64) -> i64 { v + 1 }
            }

            fn f(v: i64) -> i64 {
                rust { std::mem::drop(v); }
                v
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 2);
    }

    #[test]
    fn parse_macro_invocation_expression() {
        let source = r#"
            fn render(value: i64) -> String {
                return format!("v={}", value);
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn parse_generic_type_params_on_struct_enum_and_impl() {
        let source = r#"
            struct Box<T> { value: T; }
            enum Maybe<T> { Some(T); None; }
            impl<T> Box<T> {
                fn take(self) -> T { self.value }
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 3);

        let crate::ast::Item::Struct(def) = &module.items[0] else {
            panic!("expected struct item");
        };
        assert_eq!(def.type_params.len(), 1);
        assert_eq!(def.type_params[0].name, "T");

        let crate::ast::Item::Enum(def) = &module.items[1] else {
            panic!("expected enum item");
        };
        assert_eq!(def.type_params.len(), 1);
        assert_eq!(def.type_params[0].name, "T");

        let crate::ast::Item::Impl(def) = &module.items[2] else {
            panic!("expected impl item");
        };
        assert_eq!(def.type_params.len(), 1);
        assert_eq!(def.target, "Box");
        assert_eq!(def.target_args.len(), 1);
    }

    #[test]
    fn parse_enum_named_variant_payload_fields() {
        let source = r#"
            enum Message {
                Move { x: i64; y: i64; };
                Quit;
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 1);

        let crate::ast::Item::Enum(def) = &module.items[0] else {
            panic!("expected enum item");
        };
        assert_eq!(def.variants.len(), 2);
        assert_eq!(def.variants[0].name, "Move");
        let crate::ast::EnumVariantFields::Named(fields) = &def.variants[0].fields else {
            panic!("expected named variant fields");
        };
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].name, "x");
        assert_eq!(fields[1].name, "y");

        let crate::ast::EnumVariantFields::Unit = def.variants[1].fields else {
            panic!("expected unit variant");
        };
    }

    #[test]
    fn parse_effect_row_annotations() {
        let source = r#"
            trait Draw {
                fn render(self: Self) -> String ![method::render + ..r];
            }

            fn run(value: i64) -> () ![call::std::mem::drop] {
                std::mem::drop(value);
                return;
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 2);

        let crate::ast::Item::Trait(def) = &module.items[0] else {
            panic!("expected trait item");
        };
        let row = def.methods[0]
            .effect_row
            .as_ref()
            .expect("trait method should carry effect row");
        assert_eq!(
            row.caps[0],
            vec!["method".to_string(), "render".to_string()]
        );
        assert_eq!(row.rest.as_deref(), Some("r"));

        let crate::ast::Item::Function(def) = &module.items[1] else {
            panic!("expected function item");
        };
        let row = def
            .effect_row
            .as_ref()
            .expect("function should carry effect row");
        assert_eq!(
            row.caps[0],
            vec![
                "call".to_string(),
                "std".to_string(),
                "mem".to_string(),
                "drop".to_string()
            ]
        );
        assert!(row.rest.is_none());
    }
}
