#![allow(dead_code)]
use super::{KeyWord, Span, TokenKind};

#[derive(Debug, Clone)]
pub struct Token {
    span: Span,
    kind: TokenKind,
    // lexme: String,
}

impl Token {
    // pub fn new(span: Span, kind: TokenKind, lexme: &str) -> Self {
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Self {
            span,
            kind,
            // lexme: lexme.into(),
        }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }

    pub fn is_keyword(&self, expected: KeyWord) -> bool {
        match self.kind {
            TokenKind::KeyWord(keyword) if keyword == expected => true,
            _ => false,
        }
    }
}
