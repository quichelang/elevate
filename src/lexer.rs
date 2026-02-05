use crate::diag::{Diagnostic, Span};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Rust,
    Use,
    Struct,
    Enum,
    Fn,
    Const,
    Static,
    Return,
    True,
    False,
    Identifier(String),
    IntLiteral(i64),
    StringLiteral(String),
    LBrace,
    RBrace,
    LParen,
    RParen,
    Colon,
    ColonColon,
    Semicolon,
    Comma,
    Dot,
    Equal,
    Arrow,
    Lt,
    Gt,
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
                '.' => self.push_simple(TokenKind::Dot, start),
                '=' => self.push_simple(TokenKind::Equal, start),
                '<' => self.push_simple(TokenKind::Lt, start),
                '>' => self.push_simple(TokenKind::Gt, start),
                '?' => self.push_simple(TokenKind::Question, start),
                '-' => {
                    if self.peek_char() == Some('>') {
                        self.advance();
                        self.tokens.push(Token {
                            kind: TokenKind::Arrow,
                            span: Span::new(start, self.cursor),
                        });
                    } else {
                        self.diagnostics.push(Diagnostic::new(
                            "Unexpected '-' (did you mean '->'?)",
                            Span::new(start, self.cursor),
                        ));
                    }
                }
                '"' => self.lex_string(start),
                c if c.is_ascii_digit() => self.lex_number(start, c),
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
            value.push(c);
            self.advance();
        }

        self.diagnostics.push(Diagnostic::new(
            "Unterminated string literal",
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
        let kind = match ident.as_str() {
            "rust" => TokenKind::Rust,
            "use" => TokenKind::Use,
            "struct" => TokenKind::Struct,
            "enum" => TokenKind::Enum,
            "fn" => TokenKind::Fn,
            "const" => TokenKind::Const,
            "static" => TokenKind::Static,
            "return" => TokenKind::Return,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Identifier(ident),
        };
        self.tokens.push(Token {
            kind,
            span: Span::new(start, self.cursor),
        });
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
}

fn is_identifier_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_identifier_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

#[cfg(test)]
mod tests {
    use super::{lex, TokenKind};

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
        let source = "rust use std::io; value?;";
        let tokens = lex(source).expect("expected lex success");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Rust)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Use)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::ColonColon)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Question)));
    }
}
