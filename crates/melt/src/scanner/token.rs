#![allow(dead_code)]
use super::{Span, TokenKind};

#[derive(Debug, Clone)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Self { span, kind }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind.clone()
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}
