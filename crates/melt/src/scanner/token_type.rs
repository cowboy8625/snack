#![allow(dead_code)]
use super::Reg;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Id(String),
    String(String),
    Int(String),
    Ctrl(char),
    Op(String),
    Monic(MonicKind),
    Reg(Reg),
    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Id(i) => write!(f, "{}", i),
            Self::String(i) => write!(f, "{}", i),
            Self::Int(i) => write!(f, "{}", i),
            Self::Ctrl(c) => write!(f, "{}", c),
            Self::Op(o) => write!(f, "{}", o),
            Self::Monic(kw) => write!(f, "{}", kw),
            Self::Reg(kw) => write!(f, "{}", kw),
            Self::Eof => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MonicKind {
    Mov,
    Push,
    Pop,
    Add,
    Jump,
    JumpEq,
    JumpNotEq,
    Compare,
    Call,
    SysCall,
    Return,
    DB,
}

impl fmt::Display for MonicKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mov => write!(f, "mov"),
            Self::Push => write!(f, "push"),
            Self::Pop => write!(f, "pop"),
            Self::Add => write!(f, "add"),
            Self::Jump => write!(f, "jmp"),
            Self::JumpEq => write!(f, "je"),
            Self::JumpNotEq => write!(f, "jne"),
            Self::Compare => write!(f, "cmp"),
            Self::Call => write!(f, "call"),
            Self::SysCall => write!(f, "syscall"),
            Self::Return => write!(f, "ret"),
            Self::DB => write!(f, "db"),
        }
    }
}

impl MonicKind {
    pub fn lookup(name: &str) -> Option<Self> {
        use MonicKind::*;
        match name {
            "mov" => Some(Mov),
            "push" => Some(Push),
            "pop" => Some(Pop),
            "add" => Some(Add),
            "jmp" => Some(Jump),
            "je" => Some(JumpEq),
            "jne" => Some(JumpNotEq),
            "cmp" => Some(Compare),
            "call" => Some(Call),
            "syscall" => Some(SysCall),
            "ret" => Some(Return),
            "db" => Some(DB),
            _ => None,
        }
    }
}
