use super::{KeyWord, Span, Token, TokenKind};
// use anyhow::{bail, Result};
use std::{iter::Peekable, str::Chars};
type Stream<'a> = Peekable<Chars<'a>>;

pub struct Scanner<'a> {
    stream: Stream<'a>,
    pos: usize,
    current_char: Option<char>,
    previous_char: Option<char>,
}

impl<'a> Scanner<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            stream: src.chars().peekable(),
            pos: 0,
            current_char: None,
            previous_char: None,
        }
    }

    fn advance(&mut self) {
        self.pos += 1;
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

    fn span(&self, start: usize) -> Span {
        start..self.pos
    }

    fn span_one(&self) -> Span {
        self.pos..self.pos + 1
    }

    fn number(&mut self) -> Option<Token> {
        let mut number = self.current_char.unwrap().to_string();
        let start = self.pos;
        while let Some(ch) = self.next_if(|c| c.is_ascii_digit() || c == &'_') {
            number.push(ch);
        }
        let span = self.span(start);
        Some(Token::new(span, TokenKind::Int(number)))
    }

    fn id(&mut self) -> Option<Token> {
        let mut ident = self.current_char.unwrap().to_string();
        let start = self.pos;
        while let Some(ch) = self.next_if(|c| c.is_ascii_alphanumeric() || c == &'_') {
            ident.push(ch);
        }
        let span = self.span(start);
        let token_type = KeyWord::lookup(&ident).map_or(TokenKind::Id(ident), TokenKind::KeyWord);
        Some(Token::new(span, token_type))
    }

    fn string(&mut self) -> Option<Token> {
        let mut string = "".to_string();
        let start = self.pos;
        while let Some(ch) = self.next_if(|c| c != &'"') {
            string.push(ch);
        }
        let _ = self.next_char();
        let span = self.span(start);
        Some(Token::new(span, TokenKind::String(string)))
    }

    fn line_commit(&mut self) {
        while let Some(_) = self.next_if(|c| c != &'\n') {}
    }

    fn return_arrow(&mut self) -> Option<Token> {
        let _ = self.next_char();
        Some(Token::new(self.span(self.pos), TokenKind::RArrow))
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ch) = self.next_char() {
            match ch {
                num if num.is_ascii_digit() => return self.number(),
                ident if ident.is_ascii_alphabetic() => return self.id(),
                '/' if self.peek_char() == Some(&'/') => {
                    self.line_commit();
                    return self.next();
                }
                '-' if self.peek_char() == Some(&'>') => return self.return_arrow(),
                '"' => return self.string(),
                '+' => return Some(Token::new(self.span_one(), TokenKind::Op(ch.into()))),
                '*' => return Some(Token::new(self.span_one(), TokenKind::Op(ch.into()))),
                '/' => return Some(Token::new(self.span_one(), TokenKind::Op(ch.into()))),
                '-' => return Some(Token::new(self.span_one(), TokenKind::Op(ch.into()))),
                '=' => return Some(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                ':' => return Some(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                ';' => return Some(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                ',' => return Some(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                '(' => return Some(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                ')' => return Some(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                '[' => return Some(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                ']' => return Some(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                '{' => return Some(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                '}' => return Some(Token::new(self.span_one(), TokenKind::Ctrl(ch))),
                '.' => {
                    return Some(Token::new(
                        self.span_one(),
                        TokenKind::KeyWord(KeyWord::Dot),
                    ))
                }
                ' ' | '\n' => return self.next(),
                i => unreachable!("unknown char '{}' at '{:?}'", i, self.span_one()),
            }
        }
        None
    }
}
