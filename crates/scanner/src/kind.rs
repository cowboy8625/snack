use super::KeyWord;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    String(String),
    Id(String),
    Int(String),
    Ctrl(char),
    Op(String),
    KeyWord(KeyWord),
    RArrow,
    Start,
    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(i) => write!(f, "{}", i),
            Self::Id(i) => write!(f, "{}", i),
            Self::Int(i) => write!(f, "{}", i),
            Self::KeyWord(kw) => write!(f, "{}", kw),
            Self::Ctrl(c) => write!(f, "{}", c),
            Self::Op(o) => write!(f, "{}", o),
            Self::RArrow => write!(f, "->"),
            Self::Start => write!(f, "Start"),
            Self::Eof => write!(f, "EOF"),
        }
    }
}
