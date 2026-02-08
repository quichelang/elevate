use crate::diag::{Diagnostic, Span};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Rust,
    RustBlock(String),
    Use,
    Struct,
    Enum,
    Trait,
    Impl,
    Fn,
    Let,
    Const,
    Static,
    Return,
    If,
    Else,
    While,
    For,
    In,
    Loop,
    Break,
    Continue,
    Pub,
    Match,
    And,
    Or,
    Not,
    True,
    False,
    Underscore,
    Identifier(String),
    IntLiteral(i64),
    CharLiteral(char),
    StringLiteral(String),
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Colon,
    ColonColon,
    Semicolon,
    Comma,
    Dot,
    DotDot,
    DotDotEq,
    Pipe,
    At,
    Bang,
    Plus,
    PlusEqual,
    Minus,
    Star,
    Slash,
    Percent,
    Equal,
    EqualEqual,
    BangEqual,
    FatArrow,
    Arrow,
    Lt,
    LtEqual,
    Gt,
    GtEqual,
    Question,
    Eof,
}

pub fn lex(source: &str) -> Result<Vec<Token>, Vec<Diagnostic>> {
    let mut lexer = Lexer::new(source);
    lexer.lex_all();
    if lexer.diagnostics.is_empty() {
        Ok(lexer.tokens)
    } else {
        Err(lexer.diagnostics)
    }
}

struct Lexer<'a> {
    source: &'a str,
    chars: Vec<char>,
    cursor: usize,
    tokens: Vec<Token>,
    diagnostics: Vec<Diagnostic>,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars().collect(),
            cursor: 0,
            tokens: Vec::new(),
            diagnostics: Vec::new(),
        }
    }

    fn lex_all(&mut self) {
        while !self.is_at_end() {
            self.skip_whitespace_and_comments();
            if self.is_at_end() {
                break;
            }
            let start = self.cursor;
            let current = self.advance();
            match current {
                '{' => self.push_simple(TokenKind::LBrace, start),
                '}' => self.push_simple(TokenKind::RBrace, start),
                '(' => self.push_simple(TokenKind::LParen, start),
                ')' => self.push_simple(TokenKind::RParen, start),
                '[' => self.push_simple(TokenKind::LBracket, start),
                ']' => self.push_simple(TokenKind::RBracket, start),
                ':' => {
                    if self.peek_char() == Some(':') {
                        self.advance();
                        self.tokens.push(Token {
                            kind: TokenKind::ColonColon,
                            span: Span::new(start, self.cursor),
                        });
                    } else {
                        self.push_simple(TokenKind::Colon, start);
                    }
                }
                ';' => self.push_simple(TokenKind::Semicolon, start),
                ',' => self.push_simple(TokenKind::Comma, start),
                '|' => self.push_simple(TokenKind::Pipe, start),
                '@' => self.push_simple(TokenKind::At, start),
                '+' => {
                    if self.peek_char() == Some('=') {
                        self.advance();
                        self.tokens.push(Token {
                            kind: TokenKind::PlusEqual,
                            span: Span::new(start, self.cursor),
                        });
                    } else {
                        self.push_simple(TokenKind::Plus, start);
                    }
                }
                '*' => self.push_simple(TokenKind::Star, start),
                '/' => self.push_simple(TokenKind::Slash, start),
                '%' => self.push_simple(TokenKind::Percent, start),
                '.' => {
                    if self.peek_char() == Some('.') {
                        self.advance();
                        if self.peek_char() == Some('=') {
                            self.advance();
                            self.tokens.push(Token {
                                kind: TokenKind::DotDotEq,
                                span: Span::new(start, self.cursor),
                            });
                        } else {
                            self.tokens.push(Token {
                                kind: TokenKind::DotDot,
                                span: Span::new(start, self.cursor),
                            });
                        }
                    } else {
                        self.push_simple(TokenKind::Dot, start);
                    }
                }
                '!' => {
                    if self.peek_char() == Some('=') {
                        self.advance();
                        self.tokens.push(Token {
                            kind: TokenKind::BangEqual,
                            span: Span::new(start, self.cursor),
                        });
                    } else {
                        self.push_simple(TokenKind::Bang, start);
                    }
                }
                '=' => {
                    if self.peek_char() == Some('=') {
                        self.advance();
                        self.tokens.push(Token {
                            kind: TokenKind::EqualEqual,
                            span: Span::new(start, self.cursor),
                        });
                    } else if self.peek_char() == Some('>') {
                        self.advance();
                        self.tokens.push(Token {
                            kind: TokenKind::FatArrow,
                            span: Span::new(start, self.cursor),
                        });
                    } else {
                        self.push_simple(TokenKind::Equal, start);
                    }
                }
                '<' => {
                    if self.peek_char() == Some('=') {
                        self.advance();
                        self.tokens.push(Token {
                            kind: TokenKind::LtEqual,
                            span: Span::new(start, self.cursor),
                        });
                    } else {
                        self.push_simple(TokenKind::Lt, start);
                    }
                }
                '>' => {
                    if self.peek_char() == Some('=') {
                        self.advance();
                        self.tokens.push(Token {
                            kind: TokenKind::GtEqual,
                            span: Span::new(start, self.cursor),
                        });
                    } else {
                        self.push_simple(TokenKind::Gt, start);
                    }
                }
                '?' => self.push_simple(TokenKind::Question, start),
                '-' => {
                    if self.peek_char() == Some('>') {
                        self.advance();
                        self.tokens.push(Token {
                            kind: TokenKind::Arrow,
                            span: Span::new(start, self.cursor),
                        });
                    } else {
                        self.push_simple(TokenKind::Minus, start);
                    }
                }
                '"' => self.lex_string(start),
                '\'' => self.lex_char(start),
                'r' => {
                    if self.peek_char() == Some('"') || self.peek_char() == Some('#') {
                        self.lex_raw_string(start);
                    } else {
                        self.lex_identifier(start, 'r');
                    }
                }
                c if c.is_ascii_digit() => self.lex_number(start, c),
                '_' => {
                    if self.peek_char().is_some_and(is_identifier_continue) {
                        self.lex_identifier(start, '_');
                    } else {
                        self.push_simple(TokenKind::Underscore, start);
                    }
                }
                c if is_identifier_start(c) => self.lex_identifier(start, c),
                c => self.diagnostics.push(Diagnostic::new(
                    format!("Unexpected character '{c}'"),
                    Span::new(start, self.cursor),
                )),
            }
        }

        self.tokens.push(Token {
            kind: TokenKind::Eof,
            span: Span::new(self.source.len(), self.source.len()),
        });
    }

    fn lex_string(&mut self, start: usize) {
        let mut value = String::new();
        while let Some(c) = self.peek_char() {
            if c == '"' {
                self.advance();
                self.tokens.push(Token {
                    kind: TokenKind::StringLiteral(value),
                    span: Span::new(start, self.cursor),
                });
                return;
            }
            if c == '\\' {
                self.advance();
                match self.peek_char() {
                    Some('n') => {
                        self.advance();
                        value.push('\n');
                    }
                    Some('r') => {
                        self.advance();
                        value.push('\r');
                    }
                    Some('t') => {
                        self.advance();
                        value.push('\t');
                    }
                    Some('0') => {
                        self.advance();
                        value.push('\0');
                    }
                    Some('\\') => {
                        self.advance();
                        value.push('\\');
                    }
                    Some('"') => {
                        self.advance();
                        value.push('"');
                    }
                    Some('x') => {
                        self.advance();
                        match self.lex_hex_escape(start) {
                            Some(ch) => value.push(ch),
                            None => return,
                        }
                    }
                    Some(other) => {
                        self.advance();
                        self.diagnostics.push(Diagnostic::new(
                            format!("Unsupported string escape '\\{other}'"),
                            Span::new(start, self.cursor),
                        ));
                        value.push(other);
                    }
                    None => {
                        self.diagnostics.push(Diagnostic::new(
                            "Unterminated string literal escape",
                            Span::new(start, self.cursor),
                        ));
                        return;
                    }
                }
                continue;
            }
            value.push(c);
            self.advance();
        }

        self.diagnostics.push(Diagnostic::new(
            "Unterminated string literal",
            Span::new(start, self.cursor),
        ));
    }

    fn lex_hex_escape(&mut self, start: usize) -> Option<char> {
        let hi = match self.peek_char() {
            Some(value) => value,
            None => {
                self.diagnostics.push(Diagnostic::new(
                    "Unterminated string hex escape; expected two hex digits",
                    Span::new(start, self.cursor),
                ));
                return None;
            }
        };
        self.advance();
        let lo = match self.peek_char() {
            Some(value) => value,
            None => {
                self.diagnostics.push(Diagnostic::new(
                    "Unterminated string hex escape; expected two hex digits",
                    Span::new(start, self.cursor),
                ));
                return None;
            }
        };
        self.advance();

        let hi = match hi.to_digit(16) {
            Some(value) => value,
            None => {
                self.diagnostics.push(Diagnostic::new(
                    "Invalid string hex escape; expected two hex digits",
                    Span::new(start, self.cursor),
                ));
                return None;
            }
        };
        let lo = match lo.to_digit(16) {
            Some(value) => value,
            None => {
                self.diagnostics.push(Diagnostic::new(
                    "Invalid string hex escape; expected two hex digits",
                    Span::new(start, self.cursor),
                ));
                return None;
            }
        };
        let byte = (hi << 4) | lo;
        char::from_u32(byte).or_else(|| {
            self.diagnostics.push(Diagnostic::new(
                "Invalid string hex escape; out of ASCII range",
                Span::new(start, self.cursor),
            ));
            None
        })
    }

    fn lex_char(&mut self, start: usize) {
        let value = match self.peek_char() {
            Some('\\') => {
                self.advance();
                match self.peek_char() {
                    Some('n') => {
                        self.advance();
                        '\n'
                    }
                    Some('r') => {
                        self.advance();
                        '\r'
                    }
                    Some('t') => {
                        self.advance();
                        '\t'
                    }
                    Some('0') => {
                        self.advance();
                        '\0'
                    }
                    Some('\\') => {
                        self.advance();
                        '\\'
                    }
                    Some('\'') => {
                        self.advance();
                        '\''
                    }
                    Some(other) => {
                        self.advance();
                        self.diagnostics.push(Diagnostic::new(
                            format!("Unsupported char escape '\\{other}'"),
                            Span::new(start, self.cursor),
                        ));
                        other
                    }
                    None => {
                        self.diagnostics.push(Diagnostic::new(
                            "Unterminated character literal escape",
                            Span::new(start, self.cursor),
                        ));
                        return;
                    }
                }
            }
            Some(value) => {
                self.advance();
                value
            }
            None => {
                self.diagnostics.push(Diagnostic::new(
                    "Unterminated character literal",
                    Span::new(start, self.cursor),
                ));
                return;
            }
        };

        if self.peek_char() != Some('\'') {
            self.diagnostics.push(Diagnostic::new(
                "Character literal must contain exactly one character",
                Span::new(start, self.cursor),
            ));
            return;
        }
        self.advance();
        self.tokens.push(Token {
            kind: TokenKind::CharLiteral(value),
            span: Span::new(start, self.cursor),
        });
    }

    fn lex_raw_string(&mut self, start: usize) {
        let mut hashes = 0usize;
        if self.peek_char() == Some('"') {
            self.advance();
        } else {
            while self.peek_char() == Some('#') {
                hashes += 1;
                self.advance();
            }
            if self.peek_char() != Some('"') {
                self.diagnostics.push(Diagnostic::new(
                    "Invalid raw string literal prefix; expected '\"' after `r` and `#` markers",
                    Span::new(start, self.cursor),
                ));
                return;
            }
            self.advance();
        }

        let mut value = String::new();
        while let Some(c) = self.peek_char() {
            self.advance();
            if c == '"' && self.has_hash_terminator(hashes) {
                for _ in 0..hashes {
                    self.advance();
                }
                self.tokens.push(Token {
                    kind: TokenKind::StringLiteral(value),
                    span: Span::new(start, self.cursor),
                });
                return;
            }
            value.push(c);
        }

        self.diagnostics.push(Diagnostic::new(
            "Unterminated raw string literal",
            Span::new(start, self.cursor),
        ));
    }

    fn lex_number(&mut self, start: usize, first: char) {
        let mut value = String::new();
        value.push(first);
        while let Some(c) = self.peek_char() {
            if c.is_ascii_digit() {
                value.push(c);
                self.advance();
            } else {
                break;
            }
        }
        match value.parse::<i64>() {
            Ok(number) => self.tokens.push(Token {
                kind: TokenKind::IntLiteral(number),
                span: Span::new(start, self.cursor),
            }),
            Err(_) => self.diagnostics.push(Diagnostic::new(
                "Invalid integer literal",
                Span::new(start, self.cursor),
            )),
        }
    }

    fn lex_identifier(&mut self, start: usize, first: char) {
        let mut ident = String::new();
        ident.push(first);
        while let Some(c) = self.peek_char() {
            if is_identifier_continue(c) {
                ident.push(c);
                self.advance();
            } else {
                break;
            }
        }
        if ident == "rust" && self.peek_non_whitespace_char() == Some('{') {
            self.consume_whitespace();
            self.lex_rust_block(start);
            return;
        }
        let kind = match ident.as_str() {
            "rust" => TokenKind::Rust,
            "use" => TokenKind::Use,
            "struct" => TokenKind::Struct,
            "enum" => TokenKind::Enum,
            "trait" => TokenKind::Trait,
            "impl" => TokenKind::Impl,
            "fn" => TokenKind::Fn,
            "let" => TokenKind::Let,
            "const" => TokenKind::Const,
            "static" => TokenKind::Static,
            "return" => TokenKind::Return,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            "loop" => TokenKind::Loop,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "pub" => TokenKind::Pub,
            "match" => TokenKind::Match,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "not" => TokenKind::Not,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Identifier(ident),
        };
        self.tokens.push(Token {
            kind,
            span: Span::new(start, self.cursor),
        });
    }

    fn lex_rust_block(&mut self, start: usize) {
        if self.peek_char() != Some('{') {
            self.tokens.push(Token {
                kind: TokenKind::Rust,
                span: Span::new(start, self.cursor),
            });
            return;
        }
        self.advance();
        let mut depth = 1usize;
        let mut body = String::new();
        while let Some(c) = self.peek_char() {
            self.advance();
            match c {
                '{' => {
                    depth += 1;
                    body.push(c);
                }
                '}' => {
                    depth -= 1;
                    if depth == 0 {
                        self.tokens.push(Token {
                            kind: TokenKind::RustBlock(body),
                            span: Span::new(start, self.cursor),
                        });
                        return;
                    }
                    body.push(c);
                }
                _ => body.push(c),
            }
        }
        self.diagnostics.push(Diagnostic::new(
            "Unterminated `rust { ... }` block",
            Span::new(start, self.cursor),
        ));
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            while let Some(c) = self.peek_char() {
                if c.is_whitespace() {
                    self.advance();
                } else {
                    break;
                }
            }

            if self.peek_char() == Some('/') && self.peek_next_char() == Some('/') {
                while let Some(c) = self.peek_char() {
                    self.advance();
                    if c == '\n' {
                        break;
                    }
                }
                continue;
            }
            if self.peek_char() == Some('/') && self.peek_next_char() == Some('*') {
                let start = self.cursor;
                self.advance();
                self.advance();
                let mut depth = 1usize;
                while let Some(c) = self.peek_char() {
                    if c == '/' && self.peek_next_char() == Some('*') {
                        self.advance();
                        self.advance();
                        depth += 1;
                        continue;
                    }
                    if c == '*' && self.peek_next_char() == Some('/') {
                        self.advance();
                        self.advance();
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                        continue;
                    }
                    self.advance();
                }
                if depth != 0 {
                    self.diagnostics.push(Diagnostic::new(
                        "Unterminated block comment",
                        Span::new(start, self.cursor),
                    ));
                }
                continue;
            }
            break;
        }
    }

    fn push_simple(&mut self, kind: TokenKind, start: usize) {
        self.tokens.push(Token {
            kind,
            span: Span::new(start, self.cursor),
        });
    }

    fn is_at_end(&self) -> bool {
        self.cursor >= self.chars.len()
    }

    fn advance(&mut self) -> char {
        let ch = self.chars[self.cursor];
        self.cursor += 1;
        ch
    }

    fn peek_char(&self) -> Option<char> {
        self.chars.get(self.cursor).copied()
    }

    fn peek_next_char(&self) -> Option<char> {
        self.chars.get(self.cursor + 1).copied()
    }

    fn peek_non_whitespace_char(&self) -> Option<char> {
        let mut index = self.cursor;
        while let Some(ch) = self.chars.get(index).copied() {
            if ch.is_whitespace() {
                index += 1;
                continue;
            }
            return Some(ch);
        }
        None
    }

    fn consume_whitespace(&mut self) {
        while let Some(c) = self.peek_char() {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn peek_n_char(&self, offset: usize) -> Option<char> {
        self.chars.get(self.cursor + offset).copied()
    }

    fn has_hash_terminator(&self, hashes: usize) -> bool {
        for offset in 0..hashes {
            if self.peek_n_char(offset) != Some('#') {
                return false;
            }
        }
        true
    }
}

fn is_identifier_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_identifier_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

#[cfg(test)]
mod tests {
    use super::{TokenKind, lex};

    #[test]
    fn lex_struct_tokens() {
        let source = "struct Point { x: i64; }";
        let tokens = lex(source).expect("expected lex success");

        assert!(matches!(tokens[0].kind, TokenKind::Struct));
        assert!(matches!(tokens[1].kind, TokenKind::Identifier(_)));
        assert!(matches!(tokens[2].kind, TokenKind::LBrace));
        assert!(matches!(tokens[3].kind, TokenKind::Identifier(_)));
    }

    #[test]
    fn lex_path_and_try_tokens() {
        let source = "use std::io; value?;";
        let tokens = lex(source).expect("expected lex success");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Use)));
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t.kind, TokenKind::ColonColon))
        );
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Question)));
    }

    #[test]
    fn lex_match_tokens() {
        let source = "match value { _ => 1; }";
        let tokens = lex(source).expect("expected lex success");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Match)));
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t.kind, TokenKind::Underscore))
        );
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::FatArrow)));
    }

    #[test]
    fn lex_rust_block_token() {
        let source = "rust { fn bridge(v: &str) -> usize { v.len() } }";
        let tokens = lex(source).expect("expected lex success");
        assert!(
            tokens.iter().any(
                |t| matches!(t.kind, TokenKind::RustBlock(ref body) if body.contains("v.len()"))
            )
        );
    }

    #[test]
    fn lex_visibility_and_flow_tokens() {
        let source = "pub fn f() { if true { } else { } while false { } }";
        let tokens = lex(source).expect("expected lex success");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Pub)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::If)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Else)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::While)));
    }

    #[test]
    fn lex_impl_tokens() {
        let source = "impl Point { pub fn make() { return; } }";
        let tokens = lex(source).expect("expected lex success");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Impl)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Pub)));
    }

    #[test]
    fn lex_boolean_and_comparison_ops() {
        let source = "if not a and b or !c { x == y; x != y; x <= y; x >= y; x < y; x > y; }";
        let tokens = lex(source).expect("expected lex success");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Not)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Bang)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::And)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Or)));
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t.kind, TokenKind::EqualEqual))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t.kind, TokenKind::BangEqual))
        );
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::LtEqual)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::GtEqual)));
    }

    #[test]
    fn lex_add_and_add_assign_tokens() {
        let source = "x = a + b; x += 1;";
        let tokens = lex(source).expect("expected lex success");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Plus)));
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t.kind, TokenKind::PlusEqual))
        );
    }

    #[test]
    fn lex_arithmetic_operator_tokens() {
        let source = "x = a - b * c / d % e;";
        let tokens = lex(source).expect("expected lex success");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Minus)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Star)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Slash)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Percent)));
    }

    #[test]
    fn lex_range_tokens() {
        let source = "a..b; a..=b; ..b; ..=b; a..;";
        let tokens = lex(source).expect("expected lex success");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::DotDot)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::DotDotEq)));
    }

    #[test]
    fn lex_bracket_tokens() {
        let source = "match xs { [head, ..tail] => head; _ => 0; }";
        let tokens = lex(source).expect("expected lex success");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::LBracket)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::RBracket)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::DotDot)));
    }

    #[test]
    fn lex_closure_tokens() {
        let source = "const f = |x: i64| -> i64 { x };";
        let tokens = lex(source).expect("expected lex success");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Pipe)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Arrow)));
    }

    #[test]
    fn lex_let_tokens() {
        let source = "let value = 1;";
        let tokens = lex(source).expect("expected lex success");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Let)));
    }

    #[test]
    fn lex_block_comments() {
        let source = "const x = 1; /* comment */ const y = 2;";
        let tokens = lex(source).expect("expected lex success");
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t.kind, TokenKind::Identifier(ref name) if name == "x"))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t.kind, TokenKind::Identifier(ref name) if name == "y"))
        );
    }

    #[test]
    fn lex_leading_underscore_identifiers() {
        let source = "use crate::__elevate_interop;";
        let tokens = lex(source).expect("expected lex success");
        assert!(tokens.iter().any(
            |t| matches!(t.kind, TokenKind::Identifier(ref name) if name == "__elevate_interop")
        ));
    }

    #[test]
    fn lex_raw_multiline_string_literal() {
        let source = r##"const text = r#"first line
"quoted"
last line"#;"##;
        let tokens = lex(source).expect("expected lex success");
        let literal = tokens
            .iter()
            .find_map(|token| match &token.kind {
                TokenKind::StringLiteral(value) => Some(value.as_str()),
                _ => None,
            })
            .expect("expected a string literal token");
        assert!(literal.contains('\n'));
        assert!(literal.contains("\"quoted\""));
        assert!(literal.contains("last line"));
    }

    #[test]
    fn lex_char_literals() {
        let source = r#"const a = 'x'; const b = '\n'; const c = '\'';"#;
        let tokens = lex(source).expect("expected lex success");
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t.kind, TokenKind::CharLiteral('x')))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t.kind, TokenKind::CharLiteral('\n')))
        );
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t.kind, TokenKind::CharLiteral('\'')))
        );
    }

    #[test]
    fn lex_string_escapes_and_hex_escape() {
        let source = r#"const text = "line\n\t\"q\"\\\x1b";"#;
        let tokens = lex(source).expect("expected lex success");
        let literal = tokens
            .iter()
            .find_map(|token| match &token.kind {
                TokenKind::StringLiteral(value) => Some(value),
                _ => None,
            })
            .expect("expected string literal");
        assert!(literal.contains('\n'));
        assert!(literal.contains('\t'));
        assert!(literal.contains('"'));
        assert!(literal.contains('\\'));
        assert!(literal.contains('\u{1b}'));
        assert!(!literal.contains("\\x1b"));
    }
}
