use crate::ast::{
    AssignOp, AssignTarget, BinaryOp, Block, ConstDef, DestructurePattern, EnumDef, EnumVariant,
    Expr, Field, FunctionDef, ImplBlock, Item, MatchArm, Module, Param, Pattern, RustUse, StaticDef, Stmt,
    StructLiteralField, StructDef, Type, UnaryOp, Visibility,
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
        let visibility = if self.match_kind(TokenKind::Pub) {
            Visibility::Public
        } else {
            Visibility::Private
        };

        if self.match_kind(TokenKind::Rust) {
            if visibility == Visibility::Public {
                self.error_current("`pub rust use` is not supported; use `rust use`");
            }
            return self.parse_rust_use().map(Item::RustUse);
        }
        if self.match_kind(TokenKind::Struct) {
            return self.parse_struct(visibility).map(Item::Struct);
        }
        if self.match_kind(TokenKind::Enum) {
            return self.parse_enum(visibility).map(Item::Enum);
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
            "Expected top-level item (`rust use`, `struct`, `enum`, `impl`, `fn`, `const`, `static`)",
        );
        None
    }

    fn parse_rust_use(&mut self) -> Option<RustUse> {
        self.expect(TokenKind::Use, "Expected `use` after `rust`")?;
        let path = self.parse_path("Expected import path after `rust use`")?;
        self.expect(TokenKind::Semicolon, "Expected ';' after rust use import")?;
        Some(RustUse { path })
    }

    fn parse_struct(&mut self, visibility: Visibility) -> Option<StructDef> {
        let name = self.expect_ident("Expected struct name")?;
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
            fields,
        })
    }

    fn parse_enum(&mut self, visibility: Visibility) -> Option<EnumDef> {
        let name = self.expect_ident("Expected enum name")?;
        self.expect(TokenKind::LBrace, "Expected '{' after enum name")?;
        let mut variants = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let variant_name = self.expect_ident("Expected enum variant name")?;
            let payload = if self.match_kind(TokenKind::LParen) {
                let ty = self.parse_type()?;
                self.expect(TokenKind::RParen, "Expected ')' after variant payload type")?;
                Some(ty)
            } else {
                None
            };
            self.expect(TokenKind::Semicolon, "Expected ';' after enum variant")?;
            variants.push(EnumVariant {
                name: variant_name,
                payload,
            });
        }
        self.expect(TokenKind::RBrace, "Expected '}' after enum body")?;
        Some(EnumDef {
            visibility,
            name,
            variants,
        })
    }

    fn parse_function(
        &mut self,
        visibility: Visibility,
        impl_target: Option<&str>,
    ) -> Option<FunctionDef> {
        let name = self.expect_ident("Expected function name")?;
        self.expect(TokenKind::LParen, "Expected '(' after function name")?;
        let mut params = Vec::new();
        while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
            let param_name = self.expect_ident("Expected parameter name")?;
            let param_ty = if param_name == "self" && impl_target.is_some() && !self.at(TokenKind::Colon) {
                Type {
                    path: vec![impl_target.expect("impl target checked above").to_string()],
                    args: Vec::new(),
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
        let body = self.parse_block()?;
        Some(FunctionDef {
            visibility,
            name,
            params,
            return_type,
            body,
        })
    }

    fn parse_impl(&mut self) -> Option<ImplBlock> {
        let target = self.expect_ident("Expected type name after `impl`")?;
        self.expect(TokenKind::LBrace, "Expected '{' to start impl block")?;
        let mut methods = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let method_visibility = if self.match_kind(TokenKind::Pub) {
                Visibility::Public
            } else {
                Visibility::Private
            };
            self.expect(TokenKind::Fn, "Expected `fn` in impl block")?;
            methods.push(self.parse_function(method_visibility, Some(&target))?);
        }
        self.expect(TokenKind::RBrace, "Expected '}' after impl block")?;
        Some(ImplBlock { target, methods })
    }

    fn parse_const_item(&mut self, visibility: Visibility) -> Option<ConstDef> {
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
        })
    }

    fn parse_static_item(&mut self, visibility: Visibility) -> Option<StaticDef> {
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
            if self.match_kind(TokenKind::LParen) {
                let pattern = self.parse_destructure_pattern_tuple()?;
                self.expect(
                    TokenKind::Equal,
                    "Expected '=' after destructuring const pattern",
                )?;
                let value = self.parse_expr()?;
                self.expect(
                    TokenKind::Semicolon,
                    "Expected ';' after destructuring const",
                )?;
                return Some(Stmt::DestructureConst { pattern, value });
            }
            let name = self.expect_ident("Expected const name")?;
            let ty = if self.match_kind(TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.expect(TokenKind::Equal, "Expected '=' after const declaration")?;
            let value = self.parse_expr()?;
            self.expect(TokenKind::Semicolon, "Expected ';' after const statement")?;
            return Some(Stmt::Const(ConstDef {
                visibility: Visibility::Private,
                name,
                ty,
                value,
            }));
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

        let expr = self.parse_expr()?;
        if self.match_kind(TokenKind::Equal) || self.match_kind(TokenKind::PlusEqual) {
            let op = if matches!(self.tokens[self.cursor - 1].kind, TokenKind::PlusEqual) {
                AssignOp::AddAssign
            } else {
                AssignOp::Assign
            };
            let value = self.parse_expr()?;
            self.expect(TokenKind::Semicolon, "Expected ';' after assignment statement")?;
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
        let mut expr = self.parse_unary_expr()?;
        while self.match_kind(TokenKind::Plus) {
            let right = self.parse_unary_expr()?;
            expr = Expr::Binary {
                op: BinaryOp::Add,
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
        self.parse_postfix_expr()
    }

    fn parse_match_expr(&mut self) -> Option<Expr> {
        let scrutinee = self.parse_expr()?;
        self.expect(TokenKind::LBrace, "Expected '{' after match expression")?;
        let mut arms = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let pattern = self.parse_pattern()?;
            self.expect(TokenKind::FatArrow, "Expected `=>` in match arm")?;
            let value = self.parse_expr()?;
            self.expect(
                TokenKind::Semicolon,
                "Expected ';' after match arm expression",
            )?;
            arms.push(MatchArm { pattern, value });
        }
        self.expect(TokenKind::RBrace, "Expected '}' after match arms")?;
        Some(Expr::Match {
            scrutinee: Box::new(scrutinee),
            arms,
        })
    }

    fn parse_pattern(&mut self) -> Option<Pattern> {
        if self.match_kind(TokenKind::Underscore) {
            return Some(Pattern::Wildcard);
        }
        if let TokenKind::IntLiteral(value) = self.peek().kind.clone() {
            self.advance();
            return Some(Pattern::Int(value));
        }
        if self.match_kind(TokenKind::True) {
            return Some(Pattern::Bool(true));
        }
        if self.match_kind(TokenKind::False) {
            return Some(Pattern::Bool(false));
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

        let path = self.parse_path("Expected pattern path")?;
        let payload = if self.match_kind(TokenKind::LParen) {
            let inner = self.parse_pattern()?;
            self.expect(
                TokenKind::RParen,
                "Expected ')' after variant payload pattern",
            )?;
            Some(Box::new(inner))
        } else {
            None
        };
        if path.len() == 1 && payload.is_none() {
            return Some(Pattern::Binding(path[0].clone()));
        }
        Some(Pattern::Variant { path, payload })
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
            TokenKind::Identifier(_) => {
                let path = self.parse_path("Expected identifier path")?;
                if self.at(TokenKind::LBrace)
                    && path_looks_like_type_name(&path)
                    && self.looks_like_struct_literal_body()
                {
                    self.advance();
                    return self.parse_struct_literal_expr(path);
                }
                Some(Expr::Path(path))
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
            _ => {
                self.error_current("Expected expression");
                None
            }
        }
    }

    fn parse_type(&mut self) -> Option<Type> {
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
        Some(Type { path, args })
    }

    fn parse_struct_literal_expr(&mut self, path: Vec<String>) -> Option<Expr> {
        let mut fields = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let name = self.expect_ident("Expected field name in struct literal")?;
            self.expect(TokenKind::Colon, "Expected ':' after struct literal field name")?;
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
        let name = self.expect_ident("Expected identifier in destructuring pattern")?;
        Some(DestructurePattern::Name(name))
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
            _ => {
                self.error_current("Invalid assignment target; expected identifier or field access");
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
                || self.at(TokenKind::Pub)
                || self.at(TokenKind::Struct)
                || self.at(TokenKind::Enum)
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
            | (Use, Use)
            | (Struct, Struct)
            | (Enum, Enum)
            | (Impl, Impl)
            | (Fn, Fn)
            | (Const, Const)
            | (Static, Static)
            | (Return, Return)
            | (If, If)
            | (Else, Else)
            | (While, While)
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
            | (StringLiteral(_), StringLiteral(_))
            | (LBrace, LBrace)
            | (RBrace, RBrace)
            | (LParen, LParen)
            | (RParen, RParen)
            | (Colon, Colon)
            | (ColonColon, ColonColon)
            | (Semicolon, Semicolon)
            | (Comma, Comma)
            | (Dot, Dot)
            | (DotDot, DotDot)
            | (DotDotEq, DotDotEq)
            | (Pipe, Pipe)
            | (Bang, Bang)
            | (Plus, Plus)
            | (PlusEqual, PlusEqual)
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
    use crate::lexer::lex;

    use super::parse_module;

    #[test]
    fn parse_rust_use_and_function() {
        let source = r#"
            rust use std::fmt;
            fn read_value() -> Result<i64, Error> {
                return get_value()?;
            }
        "#;
        let tokens = lex(source).expect("expected lex success");
        let module = parse_module(tokens).expect("expected parse success");
        assert_eq!(module.items.len(), 2);
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
}
