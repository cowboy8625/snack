use anyhow::Result;
use scanner::{KeyWord, Token, TokenKind};
use std::iter::Peekable;

type Span = std::ops::Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    U64(u64),
    Bool(bool),
    String(String, usize),
    Char(char),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    Grt,
    Les,
    Geq,
    Leq,
    Equ,
    Not,
    Dot,
    Copy,
    Over,
    Rot,
    Swap,
    Drop,
    Max,
    And,
    Or,
    SysCall1,
    SysCall2,
    SysCall3,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Value(Value, Span),
    Builtin(Builtin, Span),
    While(Vec<Self>, Span),
    If(Vec<Self>, Span),
    IfElse(Vec<Self>, Vec<Self>, Span),
    Word(String, Vec<String>, Vec<Self>, Span),
    Error(String, Span),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Self::Value(_, span) => span.clone(),
            Self::Builtin(_, span) => span.clone(),
            Self::While(_, span) => span.clone(),
            Self::If(_, span) => span.clone(),
            Self::IfElse(_, _, span) => span.clone(),
            Self::Word(_, _, _, span) => span.clone(),
            Self::Error(_, span) => span.clone(),
        }
    }
}

pub struct Parser<T>
where
    T: Iterator<Item = Token>,
{
    scan: Peekable<T>,
    current: Option<Token>,
}

impl<T> Parser<T>
where
    T: Iterator<Item = Token>,
{
    pub fn new(scan: Peekable<T>) -> Self {
        let mut p = Self {
            scan,
            current: None,
        };
        p.advance();
        p
    }

    pub fn parse(&mut self) -> Result<Vec<Expr>> {
        let mut result = vec![];
        while let Some(_) = self.current {
            result.push(self.conditional()?);
        }
        Ok(result)
    }

    fn advance(&mut self) {
        self.current = dbg!(self.scan.next());
        if cfg!(feature = "token") {
            eprintln!(
                "{}",
                self.current
                    .clone()
                    .map(|i| format!("{:?}", i.kind()))
                    .unwrap_or("None".to_string())
            );
        }
    }

    fn conditional(&mut self) -> Result<Expr> {
        if self
            .current
            .clone()
            .map(|i| i.is_keyword(KeyWord::If))
            .unwrap_or(false)
        {
            let start = self.current.clone().unwrap().span().start;
            self.advance();
            let mut body = Vec::new();
            while let Some(tok) = &self.current {
                if tok.is_keyword(KeyWord::End) {
                    self.advance();
                    break;
                }
                body.push(self.conditional()?);
                self.advance();
            }
            let end = self.current.clone().map(|i| i.span().end).unwrap_or(start);
            return Ok(Expr::If(body, start..end));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr> {
        let span = self
            .current
            .clone()
            .map(|i| i.clone().span())
            .unwrap_or(0..1);
        match self.current.clone().map(|i| i.kind().clone()) {
            Some(TokenKind::String(s)) => {
                self.advance();
                Ok(Expr::Value(Value::String(s.clone().into(), s.len()), span))
            }
            Some(TokenKind::Int(int)) => {
                self.advance();
                Ok(Expr::Value(Value::U64(int.parse()?), span))
            }
            Some(TokenKind::Op(ref op)) if op == "+" => {
                self.advance();
                Ok(Expr::Builtin(Builtin::Add, span))
            }
            Some(TokenKind::Op(ref op)) if op == "-" => {
                self.advance();
                Ok(Expr::Builtin(Builtin::Sub, span))
            }
            Some(TokenKind::Op(ref op)) if op == "*" => {
                self.advance();
                Ok(Expr::Builtin(Builtin::Mul, span))
            }
            Some(TokenKind::Op(ref op)) if op == "/" => {
                self.advance();
                Ok(Expr::Builtin(Builtin::Div, span))
            }
            Some(TokenKind::KeyWord(KeyWord::True)) => {
                self.advance();
                Ok(Expr::Value(Value::Bool(true), span))
            }
            Some(TokenKind::KeyWord(KeyWord::False)) => {
                self.advance();
                Ok(Expr::Value(Value::Bool(false), span))
            }
            Some(TokenKind::KeyWord(KeyWord::Dot)) => {
                self.advance();
                Ok(Expr::Builtin(Builtin::Dot, span))
            }
            Some(TokenKind::KeyWord(KeyWord::Copy)) => {
                self.advance();
                Ok(Expr::Builtin(Builtin::Copy, span))
            }
            Some(TokenKind::KeyWord(KeyWord::Over)) => {
                self.advance();
                Ok(Expr::Builtin(Builtin::Over, span))
            }
            Some(TokenKind::KeyWord(KeyWord::Rot)) => {
                self.advance();
                Ok(Expr::Builtin(Builtin::Rot, span))
            }
            Some(TokenKind::KeyWord(KeyWord::Swap)) => {
                self.advance();
                Ok(Expr::Builtin(Builtin::Swap, span))
            }
            Some(TokenKind::KeyWord(KeyWord::Drop)) => {
                self.advance();
                Ok(Expr::Builtin(Builtin::Drop, span))
            }
            Some(TokenKind::KeyWord(KeyWord::Max)) => {
                self.advance();
                Ok(Expr::Builtin(Builtin::Max, span))
            }
            Some(TokenKind::KeyWord(KeyWord::And)) => {
                self.advance();
                Ok(Expr::Builtin(Builtin::And, span))
            }
            Some(TokenKind::KeyWord(KeyWord::Or)) => {
                self.advance();
                Ok(Expr::Builtin(Builtin::Or, span))
            }
            Some(TokenKind::KeyWord(KeyWord::SysCall1)) => {
                self.advance();
                Ok(Expr::Builtin(Builtin::SysCall1, span))
            }
            Some(TokenKind::KeyWord(KeyWord::SysCall2)) => {
                self.advance();
                Ok(Expr::Builtin(Builtin::SysCall2, span))
            }
            Some(TokenKind::KeyWord(KeyWord::SysCall3)) => {
                self.advance();
                Ok(Expr::Builtin(Builtin::SysCall3, span))
            }
            i => {
                self.advance();
                Ok(Expr::Error(format!("{:?}, syntax error", i), span))
            }
        }
    }
}

// #[test]
// fn parse_keywords() -> Result<()> {
//     use scanner::Scanner;
//     let src = "swap";
//     let mut parser = Parser::new(Scanner::new(src).peekable());
//     assert!(matches!(
//         parser.parse()?,
//         vec![Expr::Builtin(Builtin::Swap, _)]
//     ));
//     Ok(())
// }
//
// #[test]
// fn parse_int() -> Result<()> {
//     use scanner::Scanner;
//     let src = "444";
//     let mut parser = Parser::new(Scanner::new("TEST", src).peekable());
//     assert_eq!(parser.parse()?, vec![Expr::Value(Value::U64(444))]);
//     Ok(())
// }
