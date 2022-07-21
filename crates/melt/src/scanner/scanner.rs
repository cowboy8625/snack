#![allow(dead_code)]
#![allow(unreachable_code)]
use super::span::Pos;
use super::{MonicKind, Reg, Span, Token, TokenKind};
use anyhow::{bail, Result};
use std::{iter::Peekable, str::Chars};
type Stream<'a> = Peekable<Chars<'a>>;

pub struct Scanner<'a> {
    filename: &'a str,
    stream: Stream<'a>,
    pos: Pos,
    current_char: Option<char>,
    previous_char: Option<char>,
    current_tok: Option<Result<Token>>,
    next_tok: Option<Result<Token>>,
    next_op_tok: Option<Token>,
}

impl<'a> Scanner<'a> {
    pub fn new(filename: &'a str, src: &'a str) -> Self {
        Self {
            filename,
            stream: src.chars().peekable(),
            pos: Pos::default(),
            current_char: None,
            previous_char: None,
            current_tok: None,
            next_tok: None,
            next_op_tok: None,
        }
    }

    pub fn peek(&mut self) -> Option<Token> {
        if let Some(Ok(tok)) = &self.next_tok {
            self.next_op_tok = Some(tok.clone());
        }
        self.next_op_tok.clone()
    }

    pub fn next(&mut self) -> Result<Token> {
        self.current_tok = self.next_tok.take();
        self.next_tok = Some(self.next_tok());
        match &self.current_tok {
            Some(Ok(tok)) => Ok(tok.clone()),
            // Some(Err(err)) => Err(anyhow!(err)),
            _ => self.next(),
        }
    }

    fn next_tok(&mut self) -> Result<Token> {
        if let Some(ch) = self.next_char() {
            match ch {
                '0' if self.peek_char() == Some(&'x') => return todo!("implment hex tokenizing"),
                num if num.is_ascii_digit() => return self.number(),
                ident if ident.is_ascii_alphabetic() => return self.id(),
                ';' => return self.comment(),
                '"' => return self.string(),
                '+' => return Ok(Token::new(self.span_one(), TokenKind::Op(ch.into()))),
                '*' => return Ok(Token::new(self.span_one(), TokenKind::Op(ch.into()))),
                '-' => return Ok(Token::new(self.span_one(), TokenKind::Op(ch.into()))),
                '=' => return Ok(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                ':' => return Ok(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                ',' => return Ok(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                '(' => return Ok(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                ')' => return Ok(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                '[' => return Ok(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                ']' => return Ok(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                '{' => return Ok(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                '}' => return Ok(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                ' ' | '\n' => return self.next_tok(),
                _ => bail!("{}: unknown char '{}'", self.span(self.pos), ch),
            }
        }
        self.eof()
    }

    fn advance(&mut self) {
        match self.current_char {
            Some('\n') => self.pos.newline(),
            _ => self.pos.right_shift(),
        }
    }

    fn next_char(&mut self) -> Option<char> {
        self.previous_char = self.current_char;
        self.current_char = self.stream.next();
        self.advance();
        self.current_char
    }

    fn next_if(&mut self, func: impl FnOnce(&char) -> bool) -> Option<char> {
        match (self.current_char, self.stream.next_if(func)) {
            (Some(current), next @ Some(_)) => {
                self.previous_char = Some(current);
                self.current_char = next;
                self.advance();
            }
            (_, next @ Some(_)) => self.current_char = next,
            _ => return None,
        }
        self.current_char
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.stream.peek()
    }

    fn span(&self, start: Pos) -> Span {
        Span::new(self.filename, start, self.pos)
    }

    fn span_one(&self) -> Span {
        let mut end = self.pos.clone();
        end.right_shift();
        Span::new(self.filename, self.pos, end)
    }

    fn comment(&mut self) -> Result<Token> {
        while let Some(_) = self.next_if(|c| c != &'\n') {}
        self.next_tok()
    }

    fn string(&mut self) -> Result<Token> {
        let mut string = String::new();
        let start = self.pos;
        while let Some(ch) = self.next_if(|c| c != &'"') {
            string.push(ch);
        }
        let _ = self.next_char();
        let span = self.span(start);
        string = string.replace("\\n", "\n").into();
        Ok(Token::new(span, TokenKind::String(string)))
    }

    fn number(&mut self) -> Result<Token> {
        let mut number = self.current_char.unwrap().to_string();
        let start = self.pos;
        while let Some(ch) = self.next_if(|c| c.is_ascii_digit() || c == &'_') {
            number.push(ch);
        }
        let span = self.span(start);
        Ok(Token::new(span, TokenKind::Int(number)))
    }

    fn id(&mut self) -> Result<Token> {
        let mut ident = self.current_char.unwrap().to_string();
        let start = self.pos;
        while let Some(ch) = self.next_if(|c| c.is_ascii_alphanumeric() || c == &'_') {
            ident.push(ch);
        }
        let span = self.span(start);
        let token_type = MonicKind::lookup(&ident).map_or(
            Reg::lookup(&ident).map_or(TokenKind::Id(ident), TokenKind::Reg),
            TokenKind::Monic,
        );
        Ok(Token::new(span, token_type))
    }

    fn eof(&mut self) -> Result<Token> {
        Ok(Token::new(self.span(self.pos), TokenKind::Eof))
    }
}
