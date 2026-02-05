use crate::ast::{
    Block, ConstDef, EnumDef, EnumVariant, Expr, Field, FunctionDef, Item, Module, Param, RustUse,
    StaticDef, Stmt, StructDef, Type,
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
        if self.match_kind(TokenKind::Rust) {
            return self.parse_rust_use().map(Item::RustUse);
        }
        if self.match_kind(TokenKind::Struct) {
            return self.parse_struct().map(Item::Struct);
        }
        if self.match_kind(TokenKind::Enum) {
            return self.parse_enum().map(Item::Enum);
        }
        if self.match_kind(TokenKind::Fn) {
            return self.parse_function().map(Item::Function);
        }
        if self.match_kind(TokenKind::Const) {
            return self.parse_const_item().map(Item::Const);
        }
        if self.match_kind(TokenKind::Static) {
            return self.parse_static_item().map(Item::Static);
        }

        self.error_current("Expected top-level item (`rust use`, `struct`, `enum`, `fn`, `const`, `static`)");
        None
    }

    fn parse_rust_use(&mut self) -> Option<RustUse> {
        self.expect(TokenKind::Use, "Expected `use` after `rust`")?;
        let path = self.parse_path("Expected import path after `rust use`")?;
        self.expect(TokenKind::Semicolon, "Expected ';' after rust use import")?;
        Some(RustUse { path })
    }

    fn parse_struct(&mut self) -> Option<StructDef> {
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
        Some(StructDef { name, fields })
    }

    fn parse_enum(&mut self) -> Option<EnumDef> {
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
        Some(EnumDef { name, variants })
    }

    fn parse_function(&mut self) -> Option<FunctionDef> {
        let name = self.expect_ident("Expected function name")?;
        self.expect(TokenKind::LParen, "Expected '(' after function name")?;
        let mut params = Vec::new();
        while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
            let param_name = self.expect_ident("Expected parameter name")?;
            self.expect(TokenKind::Colon, "Expected ':' after parameter name")?;
            let param_ty = self.parse_type()?;
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
            name,
            params,
            return_type,
            body,
        })
    }

    fn parse_const_item(&mut self) -> Option<ConstDef> {
        let name = self.expect_ident("Expected const name")?;
        let ty = if self.match_kind(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect(TokenKind::Equal, "Expected '=' after const declaration")?;
        let value = self.parse_expr()?;
        self.expect(TokenKind::Semicolon, "Expected ';' after const item")?;
        Some(ConstDef { name, ty, value })
    }

    fn parse_static_item(&mut self) -> Option<StaticDef> {
        let name = self.expect_ident("Expected static name")?;
        self.expect(TokenKind::Colon, "Expected ':' after static name")?;
        let ty = self.parse_type()?;
        self.expect(TokenKind::Equal, "Expected '=' after static type")?;
        let value = self.parse_expr()?;
        self.expect(TokenKind::Semicolon, "Expected ';' after static item")?;
        Some(StaticDef { name, ty, value })
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
            let name = self.expect_ident("Expected const name")?;
            let ty = if self.match_kind(TokenKind::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.expect(TokenKind::Equal, "Expected '=' after const declaration")?;
            let value = self.parse_expr()?;
            self.expect(TokenKind::Semicolon, "Expected ';' after const statement")?;
            return Some(Stmt::Const(ConstDef { name, ty, value }));
        }
        if self.match_kind(TokenKind::Return) {
            if self.match_kind(TokenKind::Semicolon) {
                return Some(Stmt::Return(None));
            }
            let expr = self.parse_expr()?;
            self.expect(TokenKind::Semicolon, "Expected ';' after return value")?;
            return Some(Stmt::Return(Some(expr)));
        }

        let expr = self.parse_expr()?;
        self.expect(TokenKind::Semicolon, "Expected ';' after expression statement")?;
        Some(Stmt::Expr(expr))
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        self.parse_postfix_expr()
    }

    fn parse_postfix_expr(&mut self) -> Option<Expr> {
        let mut expr = self.parse_primary_expr()?;
        loop {
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
                Some(Expr::Path(path))
            }
            TokenKind::LParen => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(TokenKind::RParen, "Expected ')' after parenthesized expression")?;
                Some(expr)
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

    fn parse_path(&mut self, message: &str) -> Option<Vec<String>> {
        let mut path = Vec::new();
        path.push(self.expect_ident(message)?);
        while self.match_kind(TokenKind::ColonColon) {
            path.push(self.expect_ident("Expected identifier after '::'")?);
        }
        Some(path)
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
                || self.at(TokenKind::Struct)
                || self.at(TokenKind::Enum)
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

fn same_variant(left: &TokenKind, right: &TokenKind) -> bool {
    use TokenKind::*;
    matches!(
        (left, right),
        (Rust, Rust)
            | (Use, Use)
            | (Struct, Struct)
            | (Enum, Enum)
            | (Fn, Fn)
            | (Const, Const)
            | (Static, Static)
            | (Return, Return)
            | (True, True)
            | (False, False)
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
            | (Equal, Equal)
            | (Arrow, Arrow)
            | (Lt, Lt)
            | (Gt, Gt)
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
}
