use std::collections::HashMap;
// use std::env::args;
use std::fs::OpenOptions;
use std::io::prelude::*;
use std::{fmt, iter::Peekable};

const MEMORY: u32 = 640_000;
type Stream<'a, T> = Peekable<std::slice::Iter<'a, T>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pos {
    pub filename: String,
    pub idx: usize,
    pub row: usize,
    pub col: usize,
}

impl Pos {
    fn addr(&self) -> String {
        format!(
            "{}{}",
            remove_file_extension(&self.filename)
                .replace("-", "")
                .replace("/", ""),
            self.idx,
        )
    }

    fn jump(&self) -> String {
        remove_file_extension(&self.filename)
            .replace("-", "")
            .replace("/", "")
            .to_string()
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}:", self.filename, self.col, self.row)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.start)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeyWord {
    Use,
    Word,
    In,
    Const,
    While,
    Do,
    If,
    ElIf,
    Else,
    End,
    Return,
    And,
    Or,
    True,
    False,
    Dot,
    Copy,
    Over,
    Rot,
    Swap,
    Drop,
    Max,
    Memory,
    SysCall1,
    SysCall2,
    SysCall3,
    SysCall4,
    SysCall5,
    SysCall6,
}

impl KeyWord {
    pub fn lookup(name: &str) -> Option<Self> {
        use KeyWord::*;
        match name {
            "use" => Some(Use),
            "word" => Some(Word),
            "in" => Some(In),
            "const" => Some(Const),
            "while" => Some(While),
            "do" => Some(Do),
            "if" => Some(If),
            "elif" => Some(ElIf),
            "else" => Some(Else),
            "end" => Some(End),
            "return" => Some(Return),
            "and" => Some(And),
            "or" => Some(Or),
            "true" => Some(True),
            "false" => Some(False),
            "." => Some(Dot),
            "copy" => Some(Copy),
            "over" => Some(Over),
            "rot" => Some(Rot),
            "swap" => Some(Swap),
            "drop" => Some(Drop),
            "max" => Some(Max),
            "memory" => Some(Memory),
            "syscall1" => Some(SysCall1),
            "syscall2" => Some(SysCall2),
            "syscall3" => Some(SysCall3),
            "syscall4" => Some(SysCall4),
            "syscall5" => Some(SysCall5),
            "syscall6" => Some(SysCall6),
            _ => None,
        }
    }
}

impl fmt::Display for KeyWord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Use => write!(f, "use"),
            Self::Word => write!(f, "word"),
            Self::In => write!(f, "in"),
            Self::Const => write!(f, "const"),
            Self::While => write!(f, "while"),
            Self::Do => write!(f, "do"),
            Self::If => write!(f, "if"),
            Self::ElIf => write!(f, "elif"),
            Self::Else => write!(f, "else"),
            Self::End => write!(f, "end"),
            Self::Return => write!(f, "return"),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::Dot => write!(f, "."),
            Self::Copy => write!(f, "copy"),
            Self::Over => write!(f, "over"),
            Self::Rot => write!(f, "rot"),
            Self::Swap => write!(f, "swap"),
            Self::Drop => write!(f, "drop"),
            Self::Max => write!(f, "max"),
            Self::Memory => write!(f, "memory"),
            Self::SysCall1 => write!(f, "syscall1"),
            Self::SysCall2 => write!(f, "syscall2"),
            Self::SysCall3 => write!(f, "syscall3"),
            Self::SysCall4 => write!(f, "syscall4"),
            Self::SysCall5 => write!(f, "syscall5"),
            Self::SysCall6 => write!(f, "syscall6"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Oper {
    Store,
    Load,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    Grt,
    Geq,
    Les,
    Leq,
    Neq,
    Eq,
}

impl fmt::Display for Oper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Store => write!(f, "!"),
            Self::Load => write!(f, "@"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Mul => write!(f, "/"),
            Self::Div => write!(f, "*"),
            Self::Mod => write!(f, "%"),
            Self::Grt => write!(f, ">"),
            Self::Geq => write!(f, ">="),
            Self::Les => write!(f, "<"),
            Self::Leq => write!(f, "<="),
            Self::Neq => write!(f, "!="),
            Self::Eq => write!(f, "=="),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Prim {
    Hex(String),
    Int(String),
    Char(char),
    String(String),
}

impl fmt::Display for Prim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Hex(i) => write!(f, "Hex({})", i),
            Self::Int(i) => write!(f, "Int({})", i),
            Self::Char(i) => write!(f, "Char({})", i),
            Self::String(i) => write!(f, "String({:?})", i),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sym {
    RArrow,
}

impl fmt::Display for Sym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::RArrow => write!(f, "->"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    Id(String),
    Prim(Prim),
    Oper(Oper),
    Ctrl(char),
    Sym(Sym),
    KeyWord(KeyWord),
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Id(i) => write!(f, "{}", i),
            Self::Prim(k) => write!(f, "{}", k),
            Self::Oper(a) => write!(f, "{}", a),
            Self::Ctrl(c) => write!(f, "{}", c),
            Self::Sym(sym) => write!(f, "{}", sym),
            Self::KeyWord(k) => write!(f, "{}", k),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: Kind,
    pub span: Span,
    pub jump: Option<usize>,
    pub end: Option<usize>,
    pub ret: bool,
    pub idx: usize,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let jump = self
            .jump
            .map(|j| format!(" jump: {}", j))
            .unwrap_or("".into());
        let end = self
            .end
            .map(|e| format!(" end: {}", e))
            .unwrap_or("".into());
        write!(
            f,
            "{} idx: {} kind: {} ret: {}{}{}",
            self.span, self.span.start.idx, self.kind, self.ret, jump, end
        )
    }
}

impl Token {
    pub fn new(kind: Kind, span: Span, idx: usize) -> Self {
        Self {
            kind,
            span,
            jump: None,
            end: None,
            ret: false,
            idx,
        }
    }

    pub fn with_jump(mut self, jump: Option<usize>) -> Self {
        self.jump = jump;
        self
    }

    pub fn with_ret(mut self, ret: bool) -> Self {
        self.ret = ret;
        self
    }

    pub fn jump_mut(&mut self, jump: Option<usize>) {
        self.jump = jump;
    }

    pub fn end_mut(&mut self, end: Option<usize>) {
        self.end = end;
    }

    pub fn id(&self) -> usize {
        self.span.start.idx
    }

    pub fn addr(&self) -> String {
        self.span.start.addr()
    }

    pub fn jump(&self) -> String {
        self.span.start.jump()
    }
}

pub struct Scanner<'a> {
    stream: Stream<'a, (char, Pos)>,
    pub tokens: Vec<Token>,
    pub errors: Vec<String>,
    block: Vec<(usize, Kind)>,
}

impl<'a> Scanner<'a> {
    pub fn new(stream: Stream<'a, (char, Pos)>) -> Self {
        Self {
            stream,
            tokens: Vec::new(),
            errors: Vec::new(),
            block: Vec::new(),
            // current: None,
        }
    }

    fn advance(&mut self) -> Option<(char, Pos)> {
        self.stream
            .next_if(|_| self.errors.is_empty())
            .map(Clone::clone)
    }

    fn peek_char(&mut self) -> char {
        self.stream
            .peek()
            .unwrap_or(&&(
                '\0',
                Pos {
                    filename: "ERROR".into(),
                    idx: 0,
                    row: 0,
                    col: 0,
                },
            ))
            .0
    }

    pub fn lexer(mut self) -> Self {
        while let Some((c, pos)) = self.advance() {
            match c {
                '-' if self.peek_char() == '>' => {
                    let (_, end) = self.stream.next().unwrap();
                    self.add_tok(Kind::Sym(Sym::RArrow), pos.clone(), end.clone())
                }
                '\'' => self.chr(pos.clone()),
                '/' if self.peek_char() == '/' => {
                    self.line_comment();
                }
                '0' if self.peek_char() == 'x' => self.hex(c, pos.clone()),
                '"' => self.string(pos.clone()),
                n if n.is_ascii_digit() => self.number(c, pos.clone()),
                i if i.is_ascii_alphabetic() => self.identifier(c, pos.clone()),
                '.' => self.add_tok(Kind::KeyWord(KeyWord::Dot), pos.clone(), pos.clone()),
                '+' => self.add_tok(Kind::Oper(Oper::Plus), pos.clone(), pos.clone()),
                '-' => self.add_tok(Kind::Oper(Oper::Minus), pos.clone(), pos.clone()),
                '/' => self.add_tok(Kind::Oper(Oper::Div), pos.clone(), pos.clone()),
                '%' => self.add_tok(Kind::Oper(Oper::Mod), pos.clone(), pos.clone()),
                '*' => self.add_tok(Kind::Oper(Oper::Mul), pos.clone(), pos.clone()),
                '>' if self.peek_char() == '=' => {
                    let (_, end) = self.stream.next().unwrap();
                    self.add_tok(Kind::Oper(Oper::Geq), pos.clone(), end.clone())
                }
                '<' if self.peek_char() == '=' => {
                    let (_, end) = self.stream.next().unwrap();
                    self.add_tok(Kind::Oper(Oper::Leq), pos.clone(), end.clone())
                }
                '>' => self.add_tok(Kind::Oper(Oper::Grt), pos.clone(), pos.clone()),
                '<' => self.add_tok(Kind::Oper(Oper::Les), pos.clone(), pos.clone()),
                '=' if self.peek_char() == '=' => {
                    let (_, end) = self.stream.next().unwrap();
                    self.add_tok(Kind::Oper(Oper::Eq), pos.clone(), end.clone())
                }
                '!' if self.peek_char() == '=' => {
                    let (_, end) = self.stream.next().unwrap();
                    self.add_tok(Kind::Oper(Oper::Neq), pos.clone(), end.clone())
                }
                '!' => self.add_tok(Kind::Oper(Oper::Store), pos.clone(), pos.clone()),
                '@' => self.add_tok(Kind::Oper(Oper::Load), pos.clone(), pos.clone()),
                ':' => self.add_tok(Kind::Ctrl(':'), pos.clone(), pos.clone()),
                ' ' | '\n' => {}
                _ => self.errors.push(format!(
                    "{} ERROR: UnKnown Char: '{}'",
                    Span {
                        start: pos.clone(),
                        end: pos.clone(),
                    },
                    c,
                )),
            }
        }
        self
    }

    fn add_tok(&mut self, kind: Kind, start: Pos, end: Pos) {
        self.tokens
            .push(Token::new(kind, Span { start, end }, self.tokens.len()));
    }

    fn line_comment(&mut self) {
        while let Some((_, _)) = self.stream.next_if(|(c, _)| c != &'\n') {}
    }

    fn chr(&mut self, start: Pos) {
        let mut string = String::new();
        let mut end = ('\0', start.clone());
        while let Some((c, pos)) = self.stream.next_if(|(c, _)| c != &'\'') {
            end.1 = pos.clone();
            string.push(*c);
        }
        let (_, end) = self.stream.next().unwrap_or(&end);
        let chr = match string.as_str() {
            "\\n" => '\n',
            "\\t" => '\t',
            "\\x1b" => '\x1b',
            c => c.chars().collect::<Vec<char>>()[0],
        };
        self.tokens.push(Token::new(
            Kind::Prim(Prim::Char(chr)),
            Span {
                start,
                end: end.clone(),
            },
            self.tokens.len(),
        ));
    }

    fn hex(&mut self, c: char, start: Pos) {
        let mut number = c.to_string();
        let mut end = start.clone();
        while let Some((c, pos)) = self.stream.next_if(|(c, _)| c.is_ascii_alphanumeric()) {
            end = pos.clone();
            number.push(*c);
        }
        self.tokens.push(Token::new(
            Kind::Prim(Prim::Hex(number)),
            Span { start, end },
            self.tokens.len(),
        ));
    }

    fn number(&mut self, c: char, start: Pos) {
        let mut number = c.to_string();
        let mut end = start.clone();
        while let Some((c, pos)) = self.stream.next_if(|(c, _)| c.is_ascii_digit()) {
            end = pos.clone();
            number.push(*c);
        }
        self.tokens.push(Token::new(
            Kind::Prim(Prim::Int(number)),
            Span { start, end },
            self.tokens.len(),
        ));
    }

    fn string(&mut self, start: Pos) {
        let mut string = String::new();
        let mut end = ('\0', start.clone());
        while let Some((c, pos)) = self.stream.next_if(|(c, _)| c != &'"') {
            end.1 = pos.clone();
            string.push(*c);
        }
        let (_, end) = self.stream.next().unwrap_or(&end);
        self.tokens.push(Token::new(
            Kind::Prim(Prim::String(
                string
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\r", "\r")
                    .replace("\\x1b", "\x1b"),
            )),
            Span {
                start,
                end: end.clone(),
            },
            self.tokens.len(),
        ));
    }

    fn identifier(&mut self, c: char, start: Pos) {
        let mut id = c.to_string();
        let mut end = start.clone();
        while let Some((c, pos)) = self
            .stream
            .next_if(|(c, _)| c.is_ascii_alphanumeric() || c == &'_')
        {
            end = pos.clone();
            id.push(*c);
        }
        let kind = KeyWord::lookup(&id).map_or(Kind::Id(id), Kind::KeyWord);
        if let Kind::KeyWord(KeyWord::While | KeyWord::Word | KeyWord::Const | KeyWord::If) = kind {
            self.block.push((start.idx, kind.clone()));
            self.tokens
                .push(Token::new(kind, Span { start, end }, self.tokens.len()));
        } else if let Kind::KeyWord(KeyWord::End) = kind {
            if let Some((i, k)) = self.block.pop() {
                match k {
                    Kind::KeyWord(KeyWord::Word) => {
                        self.tokens.push(
                            Token::new(kind, Span { start, end }, self.tokens.len())
                                .with_jump(Some(i))
                                .with_ret(true),
                        );
                    }
                    Kind::KeyWord(KeyWord::If) => {
                        self.tokens
                            .push(Token::new(kind, Span { start, end }, self.tokens.len()));
                    }
                    _ => {
                        self.tokens.push(
                            Token::new(kind, Span { start, end }, self.tokens.len())
                                .with_jump(Some(i)),
                        );
                    }
                }
            } else {
                self.tokens
                    .push(Token::new(kind, Span { start, end }, self.tokens.len()));
            }
        } else {
            self.tokens
                .push(Token::new(kind, Span { start, end }, self.tokens.len()));
        }
    }

    pub fn link(mut self) -> Self {
        let mut returns: Vec<usize> = Vec::new();
        let mut ends: Vec<usize> = Vec::new();
        let mut end_ret: Vec<(usize, Vec<usize>)> = Vec::new();

        let mut stack: Vec<Token> = Vec::new();
        for (idx, tok) in self.tokens.iter_mut().enumerate().rev() {
            match tok.kind {
                // End needs to know jump if its ending a while loop.
                Kind::KeyWord(KeyWord::End) => {
                    ends.push(idx);
                    stack.push(tok.clone());
                }
                Kind::KeyWord(KeyWord::Return) => {
                    returns.push(idx);
                }
                // Else needs to know end
                Kind::KeyWord(KeyWord::Else) if !stack.is_empty() => {
                    let last = stack.pop().unwrap();
                    if last.kind == Kind::KeyWord(KeyWord::End) {
                        tok.end_mut(Some(last.id()));
                    } else {
                        tok.jump_mut(last.jump);
                        tok.end_mut(last.end);
                    }
                    stack.push(tok.clone());
                }
                // ElIf needs to know just the jump
                Kind::KeyWord(KeyWord::ElIf) if !stack.is_empty() => {
                    let last = stack.pop().unwrap();
                    tok.jump_mut(Some(last.id()));
                    tok.end_mut(last.end);
                    stack.push(tok.clone());
                }
                // Do needs to know end
                Kind::KeyWord(KeyWord::Do) if !stack.is_empty() => {
                    let last = stack.pop().unwrap();
                    tok.end_mut(Some(last.id()));
                    stack.push(tok.clone());
                }
                Kind::KeyWord(KeyWord::If) if !stack.is_empty() => {
                    let last = stack.pop().unwrap();
                    tok.end_mut(Some(last.id()));
                    tok.jump_mut(Some(last.id()));
                }
                Kind::KeyWord(KeyWord::Word | KeyWord::While | KeyWord::Const)
                    if !stack.is_empty() =>
                {
                    if tok.kind == Kind::KeyWord(KeyWord::Word) {
                        end_ret.push((ends[0], returns.clone()));
                        returns.clear();
                        ends.clear();
                    }
                    let last = stack.pop().unwrap();
                    tok.end_mut(Some(last.id()));
                }
                Kind::KeyWord(
                    KeyWord::Const
                    | KeyWord::While
                    | KeyWord::Do
                    | KeyWord::If
                    | KeyWord::ElIf
                    | KeyWord::Else,
                ) => {
                    if stack.is_empty() {
                        self.errors.push(format!(
                            "{} Error: {} is missing 'end' closing block.",
                            tok.span, tok.kind
                        ));
                    }
                }
                _ => {}
            }
        }
        for (end, rets) in end_ret.iter() {
            let end_tok_id = self.tokens.get(*end).unwrap().id();
            for ret in rets.iter() {
                self.tokens
                    .get_mut(*ret)
                    .map(|tok| tok.end_mut(Some(end_tok_id)));
            }
        }
        self
    }
}

pub fn pos_enum<'a>(filename: &str, src: &str) -> Vec<(char, Pos)> {
    let mut pos = Pos {
        filename: filename.into(),
        idx: 0,
        row: 0,
        col: 1,
    };
    let mut spanned: Vec<(char, Pos)> = Vec::new();
    for (i, c) in src.chars().enumerate() {
        pos.idx = i;
        pos.row += if i != 0 { 1 } else { 0 };
        if c == '\n' {
            pos.row = 0;
            pos.col += 1;
        }
        spanned.push((c, pos.clone()));
    }
    spanned.clone()
}

#[derive(Debug, Clone)]
enum Value {
    Const(usize),
    Word(Word),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Const(i) => write!(f, "{}", i),
            Self::Word(word) => write!(f, "{}", word),
        }
    }
}

impl Value {
    fn get_word(&self) -> Option<&Word> {
        match self {
            Self::Word(word) => Some(&word),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeOf {
    U64,
    Char,
    Str, // Int Ptr
    Bool,
    Null,
}

impl TypeOf {
    fn lookup(name: &str) -> Option<Self> {
        use TypeOf::*;
        match name {
            "u64" => Some(U64),
            "char" => Some(Char),
            "str" => Some(Str),
            "bool" => Some(Bool),
            "null" => Some(Null),
            _ => None,
        }
    }
}

// TODO: TypeChecker will remove type annotations in tokens and throw errors if Types do not match
// return Type of word or all branches Types do not match in a if statment
// TypeChecker:
//  1. return type of if statments match up with word return type
//  2. stack has enough items on it to do ops
//  3. definition of variable or word is defined
// pub struct TypeChecker<'a> {
//     global_defs: &'a mut Vec<Token>,
//     word_defs: &'a mut Vec<(String, Vec<Token>)>,
//     main_def: &'a mut Vec<Token>,
//     global: &'a HashMap<String, Value>,
//     tp: usize,
//     stack: Vec<TypeOf>,
//     errors: Vec<String>,
// }
//
// impl<'a> TypeChecker<'a> {
//     fn new(
//         global_defs: &'a mut Vec<Token>,
//         word_defs: &'a mut Vec<(String, Vec<Token>)>,
//         main_def: &'a mut Vec<Token>,
//         global: &'a HashMap<String, Value>,
//     ) -> Self {
//         Self {
//             global_defs,
//             word_defs,
//             main_def,
//             global,
//             tp: 0,
//             stack: Vec::new(),
//             errors: Vec::new(),
//         }
//     }
//
//     // Checks all return types of Word Defs match
//     fn check_word_defs(&mut self) {
//         for (_name, tokens) in self.word_defs.iter_mut() {
//             while self.tp < tokens.len() {
//                 match &tokens[self.tp].kind {
//                     _ => self.advance(),
//                 }
//             }
//         }
//     }
//
//     // Checks all return types of if branches match
//     fn check_if_block(&mut self) {}
//
//     // fn peek(&self) -> Option<&Token> {
//     //     self.tokens.get(self.tp + 1)
//     // }
//
//     fn advance(&mut self) {
//         self.tp += 1;
//     }
//
//     fn check(mut self) -> Self {
//         // while self.tp < self.tokens.len() {
//         //     match &self.tokens[self.tp].kind {
//         //         Kind::Id(_) => self.advance(),
//         //         Kind::Prim(_) => self.advance(),
//         //         Kind::Oper(_) => self.advance(),
//         //         Kind::KeyWord(keyword) => match keyword {
//         //             KeyWord::Use => {}
//         //             KeyWord::Word => {}
//         //             KeyWord::In => {}
//         //             KeyWord::Const => {}
//         //             KeyWord::While => {}
//         //             KeyWord::Do => {}
//         //             KeyWord::If => {}
//         //             KeyWord::ElIf => {}
//         //             KeyWord::Else => {}
//         //             KeyWord::End => {}
//         //             KeyWord::Return => {}
//         //             KeyWord::And => {}
//         //             KeyWord::Or => {}
//         //             KeyWord::True => {}
//         //             KeyWord::False => {}
//         //             KeyWord::Dot => {}
//         //             KeyWord::Copy => {}
//         //             KeyWord::Over => {}
//         //             KeyWord::Rot => {}
//         //             KeyWord::Swap => {}
//         //             KeyWord::Drop => {}
//         //             KeyWord::Max => {}
//         //             KeyWord::Memory => {}
//         //             KeyWord::SysCall1 => {}
//         //             KeyWord::SysCall2 => {}
//         //             KeyWord::SysCall3 => {}
//         //             KeyWord::SysCall4 => {}
//         //             KeyWord::SysCall5 => {}
//         //             KeyWord::SysCall6 => {}
//         //         },
//         //     }
//         // }
//         self
//     }
//
//     fn report(&self) {
//         for error in self.errors.iter() {
//             eprintln!("{}", error);
//         }
//         if !self.errors.is_empty() {
//             std::process::exit(2);
//         }
//     }
// }

struct FasmCompiler {
    out: std::fs::File,
    global_defs: Vec<Token>,
    word_defs: Vec<Word>,
    main_def: Word,
    data: Vec<(String, usize, String)>,
    global: HashMap<String, Value>,
    local: Vec<String>,
    errors: Vec<String>,
    flags: Flags,
}

impl<'a> FasmCompiler {
    const ENTRY_POINT: &'a str = "start";
    fn new(
        flags: Flags,
        global_defs: Vec<Token>,
        word_defs: Vec<Word>,
        main_def: Word,
        global: HashMap<String, Value>,
    ) -> std::io::Result<Self> {
        let filename = format!("{}.asm", flags.name());
        let out = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(filename)?;
        let local = Vec::new();
        let errors = Vec::new();
        let data = Vec::new();
        Ok(Self {
            out,
            global_defs,
            word_defs,
            main_def,
            data,
            global,
            local,
            errors,
            flags,
        })
    }

    fn out(&mut self, msg: &str) -> std::io::Result<()> {
        self.out.write_all(&format!("{}", msg).as_bytes())?;
        Ok(())
    }

    fn fasm_header(&mut self, entry_point: &str) -> std::io::Result<()> {
        self.out("format ELF64 executable 3\n")?;
        self.out("segment readable executable\n")?;
        self.out(&format!("entry __{}__\n", entry_point))?;
        Ok(())
    }
    fn fasm_print(&mut self) -> std::io::Result<()> {
        self.out("dbg_int:\n")?;
        self.out("    mov     r9, -3689348814741910323\n")?;
        self.out("    sub     rsp, 40\n")?;
        self.out("    mov     BYTE [rsp+31], 10\n")?;
        self.out("    lea     rcx, [rsp+30]\n")?;
        self.out(".L2:\n")?;
        self.out("    mov     rax, rdi\n")?;
        self.out("    lea     r8, [rsp+32]\n")?;
        self.out("    mul     r9\n")?;
        self.out("    mov     rax, rdi\n")?;
        self.out("    sub     r8, rcx\n")?;
        self.out("    shr     rdx, 3\n")?;
        self.out("    lea     rsi, [rdx+rdx*4]\n")?;
        self.out("    add     rsi, rsi\n")?;
        self.out("    sub     rax, rsi\n")?;
        self.out("    add     eax, 48\n")?;
        self.out("    mov     BYTE [rcx], al\n")?;
        self.out("    mov     rax, rdi\n")?;
        self.out("    mov     rdi, rdx\n")?;
        self.out("    mov     rdx, rcx\n")?;
        self.out("    sub     rcx, 1\n")?;
        self.out("    cmp     rax, 9\n")?;
        self.out("    ja      .L2\n")?;
        self.out("    lea     rax, [rsp+32]\n")?;
        self.out("    mov     edi, 1\n")?;
        self.out("    sub     rdx, rax\n")?;
        self.out("    xor     eax, eax\n")?;
        self.out("    lea     rsi, [rsp+32+rdx]\n")?;
        self.out("    mov     rdx, r8\n")?;
        self.out("    mov     rax, 1\n")?;
        self.out("    syscall\n")?;
        self.out("    add     sp, 40\n")?;
        self.out("    ret\n")?;
        Ok(())
    }
    fn fasm_exit(&mut self) -> std::io::Result<()> {
        self.out("    mov     eax,1\n")?;
        self.out("    xor     ebx,ebx\n")?;
        self.out("    int     0x80\n")?;
        Ok(())
    }
    fn fasm_memory(&mut self) -> std::io::Result<()> {
        // Filename + Idx of String in file
        let data = self.data.clone();
        for (filename, idx, d) in data {
            self.out(&format!(
                "data{}{} db ",
                filename.replace("-", "").replace("/", ""),
                idx
            ))?;
            for (i, byte) in d.as_bytes().iter().enumerate() {
                if i != 0 {
                    self.out(&format!(" ,{}", byte))?;
                } else {
                    self.out(&format!("{}", byte))?;
                }
            }
            self.out("\n")?;
        }
        Ok(())
    }
    fn fasm_footer(&mut self) -> std::io::Result<()> {
        self.out("segment readable writeable\n")?;
        self.fasm_memory()?;
        self.out(&format!("mem: rb {}\n", MEMORY))?;
        Ok(())
    }

    fn fasm_debug_title(&mut self, token: &Token, comment: &str) -> std::io::Result<()> {
        self.out(&format!("{}  === {} ===\n", comment, token))?;
        Ok(())
    }

    fn fasm_prologue(&mut self, counter: usize, arg_sub: bool) -> std::io::Result<()> {
        self.out("    push     rbp\n")?;
        self.out("    mov      rbp, rsp\n")?;
        if arg_sub {
            self.out(&format!("    sub      rsp,{}\n", 64 * counter))?;
        }
        Ok(())
    }

    fn create_pass_fasm_arguments(&mut self, counter: usize) -> std::io::Result<()> {
        assert!(counter <= 4);
        let args: [&str; 5] = ["rdi", "rsi", "rdx", "rcx", "r8d"];
        for idx in 0..counter {
            self.out(&format!(
                "    mov      qword [rbp-{}],{}\n",
                (idx * 64) + 64,
                args[idx]
            ))?;
        }
        Ok(())
    }

    fn fasm_label(&mut self, name: &str) -> std::io::Result<()> {
        self.out(&format!("__{}__:\n", name))?;
        Ok(())
    }

    fn fasm_push(&mut self, value: &str) -> std::io::Result<()> {
        self.out(&format!("    push    {}\n", value))?;
        Ok(())
    }

    fn fasm_pop(&mut self, value: &str) -> std::io::Result<()> {
        self.out(&format!("    pop    {}\n", value))?;
        Ok(())
    }

    fn fasm_jmp(&mut self, jump: &str, value: &str) -> std::io::Result<()> {
        self.out(&format!("    {}    __{}__\n", jump, value))?;
        Ok(())
    }

    fn fasm_call(&mut self, name: &str) -> std::io::Result<()> {
        self.out(&format!("    call     __{}__\n", name))?;
        Ok(())
    }
}

impl FasmCompiler {
    fn report(&self) {
        for e in self.errors.iter() {
            eprintln!("{}", e);
        }
        if !self.errors.is_empty() {
            std::process::exit(2);
        }
    }

    fn compiler(mut self) -> std::io::Result<Self> {
        self.fasm_header(FasmCompiler::ENTRY_POINT)?;
        self.fasm_print()?;
        self.compile_globels()?;
        self.compile_main()?;
        self.fasm_exit()?;
        self.fasm_footer()?;
        Ok(self)
    }

    fn compile_globels(&mut self) -> std::io::Result<()> {
        self.kind(&self.global_defs.clone())?;

        let words = self.word_defs.clone();
        for word in words.iter() {
            if let Some((prams, pram_len, id_list)) = self
                .global
                .get(word.name())
                .map(|v| v.get_word())
                .flatten()
                .map(|wd| (wd.prams(), wd.pram_len(), wd.id_list()))
            {
                self.fasm_label(word.name())?;
                self.local = prams;
                let arg_sub = id_list
                    .iter()
                    .map(|name| self.global.contains_key(name))
                    .fold(false, |acc, x| acc || x);
                self.fasm_prologue(pram_len, arg_sub)?;
                self.create_pass_fasm_arguments(pram_len)?;
            } else {
                self.errors.push(format!(
                    "{} NameAlreadyDefinedError: {} is already defind.",
                    word.span(),
                    word.name(),
                ));
            }
            self.kind(word.body_with_end())?;
        }
        Ok(())
    }

    fn compile_main(&mut self) -> std::io::Result<()> {
        self.fasm_label(FasmCompiler::ENTRY_POINT)?;
        self.kind(&self.main_def.body().to_vec())?;
        Ok(())
    }

    fn kind(&mut self, tokens: &[Token]) -> std::io::Result<()> {
        let mut stream = tokens.iter().peekable();
        while let Some(token) = stream.next() {
            if self.flags.debug {
                self.fasm_debug_title(&token, ";;")?;
            }
            match &token.kind {
                Kind::Id(id) => self.id(id, &token)?,
                Kind::KeyWord(KeyWord::Const) => self.constant(&mut stream, &token)?,
                Kind::KeyWord(kw) => self.keyword(kw, &token)?,
                Kind::Prim(prim) => self.primitives(&mut stream, prim, &token)?,
                Kind::Oper(op) => self.operators(&mut stream, op)?,
                _ => unreachable!(token),
            }
        }
        Ok(())
    }

    fn id(&mut self, id: &str, token: &Token) -> std::io::Result<()> {
        let value = self.global.get(id).cloned();
        match value {
            Some(v) => match v {
                Value::Const(i) => self.fasm_push(&i.to_string())?,
                Value::Word(word) => {
                    self.call(word.name(), word.pram_len(), word.is_returning())?
                }
            },
            _ => match self
                .local
                .iter()
                .enumerate()
                .find(|(_, name)| name.as_str() == id)
                .map(|(i, _)| i)
            {
                Some(i) => {
                    self.out(&format!("    push     qword [rbp-{}]\n", i * 64 + 64))?;
                }
                _ => self
                    .errors
                    .push(format!("{} Error: UnKnown word `{}`.", token.span, id)),
            },
        }
        Ok(())
    }

    fn call(&mut self, name: &str, count: usize, ret: bool) -> std::io::Result<()> {
        if count > 4 {
            panic!("We don't know how to handle more arguments");
        }
        let args: [&str; 4] = ["rdi", "rsi", "rdx", "rcx"];
        for reg in args[0..count].iter().rev() {
            self.out(&format!("    pop     {}\n", reg))?;
        }
        self.fasm_call(name)?;
        if ret {
            self.fasm_push("rax")?;
        }
        Ok(())
    }

    fn do_keyword(&mut self, token: &Token) -> std::io::Result<()> {
        self.out("    pop      rbx\n")?;
        self.out("    test     rbx,rbx\n")?;
        if let Some(e) = token.end {
            self.fasm_jmp("jz", &format!("{}{}", token.jump(), e))?;
        } else {
            self.errors.push(format!(
                "{} Error: Do block missing `end` closing block. This could be a compiler bug.",
                token.span,
            ));
        }
        Ok(())
    }

    fn if_keyword(&mut self, token: &Token) -> std::io::Result<()> {
        self.out("    pop      rbx\n")?;
        self.out("    test     rbx,rbx\n")?;
        if let Some(j) = token.jump {
            self.fasm_jmp("jz", &format!("{}{}", token.jump(), j))?;
        }
        Ok(())
    }

    fn elif_keyword(&mut self, token: &Token) -> std::io::Result<()> {
        self.fasm_label(&token.addr())?;
        self.out("    pop     rbx\n")?;
        self.out("    test    rbx,rbx\n")?;
        if let Some(j) = token.jump {
            self.fasm_jmp("jz", &format!("{}{}", token.jump(), j))?;
        } else if let Some(e) = token.end {
            self.fasm_jmp("jz", &format!("{}{}", token.jump(), e))?;
        } else {
            self.errors.push(format!(
                "{} Error: elif statement missing `end` closing block.",
                token.span,
            ));
        }
        Ok(())
    }

    fn else_keyword(&mut self, token: &Token) -> std::io::Result<()> {
        if let Some(e) = token.end {
            self.fasm_jmp("jmp", &format!("{}{}", token.jump(), e))?;
        } else {
            self.errors.push(format!(
                "{} Error: Else statement missing `end` closing block.",
                token.span,
            ));
        }
        self.fasm_label(&token.addr())?;
        Ok(())
    }

    fn end_keyword(&mut self, token: &Token) -> std::io::Result<()> {
        if let (Some(j), false) = (token.jump, token.ret) {
            self.fasm_jmp("jmp", &format!("{}{}", token.jump(), j))?;
        } else if let Some(_e) = token.end {
            self.fasm_label(&token.addr())?;
        }
        self.fasm_label(&token.addr())?;
        if token.ret {
            self.local.clear();
            self.out("    pop      rbp\n")?;
            self.out("    ret\n")?;
        }
        Ok(())
    }

    fn return_keyword(&mut self, token: &Token) -> std::io::Result<()> {
        if let Some(e) = token.end {
            self.fasm_pop("rax")?;
            self.fasm_jmp("jmp", &format!("{}{}", token.jump(), e))?;
        } else {
            self.errors.push(format!(
                "{} Error: return missing `end` closing block",
                token.span,
            ));
        }
        Ok(())
    }
    fn and_keyword(&mut self, token: &Token) -> std::io::Result<()> {
        let failed = format!("failed{}", token.id());
        let passed = format!("passed{}", token.id());

        self.out("    pop      rax\n")?;
        self.out("    pop      rbx\n")?;
        self.out("    test     rax,rbx\n")?;
        self.fasm_jmp("jz", &failed)?;

        self.fasm_push("1")?;
        self.fasm_jmp("jmp", &passed)?;

        self.fasm_label(&failed)?;
        self.fasm_push("0")?;

        self.fasm_label(&passed)?;
        Ok(())
    }

    fn or_keyword(&mut self, token: &Token) -> std::io::Result<()> {
        let failed = format!("failed{}", token.id());
        let passed = format!("passed{}", token.id());

        self.out("    pop      rax\n")?;
        self.out("    pop      rbx\n")?;
        self.out("    or       rax,rbx\n")?;
        self.fasm_jmp("jz", &failed)?;

        self.fasm_push("1")?;
        self.fasm_jmp("jmp", &passed)?;

        self.fasm_label(&failed)?;
        self.fasm_push("0")?;

        self.fasm_label(&passed)?;
        Ok(())
    }

    fn dot_keyword(&mut self) -> std::io::Result<()> {
        self.out("    mov      rdx,rdi\n")?;
        self.out("    pop      rdi\n")?;
        self.out("    push     rdx\n")?;
        self.out("    call     dbg_int\n")?;
        self.out("    pop      rdi\n")?;
        Ok(())
    }

    fn copy_keyword(&mut self) -> std::io::Result<()> {
        self.out("    pop      rdi\n")?;
        self.out("    push     rdi\n")?;
        self.out("    push     rdi\n")?;
        Ok(())
    }
    fn over_keyword(&mut self) -> std::io::Result<()> {
        self.out("    pop      rdi\n")?;
        self.out("    pop      rdi\n")?;
        self.out("    pop      rdx\n")?;
        self.out("    push     rdx\n")?;
        self.out("    push     rdi\n")?;
        self.out("    push     rdx\n")?;
        Ok(())
    }

    fn rot_keyword(&mut self) -> std::io::Result<()> {
        self.out("    pop      rdi\n")?;
        self.out("    pop      rdx\n")?;
        self.out("    pop      rsi\n")?;
        self.out("    push     rdi\n")?;
        self.out("    push     rsi\n")?;
        self.out("    push     rdx\n")?;
        Ok(())
    }

    fn swap_keyword(&mut self) -> std::io::Result<()> {
        self.out("    pop      rdi\n")?;
        self.out("    pop      rdx\n")?;
        self.out("    push     rdi\n")?;
        self.out("    push     rdx\n")?;
        Ok(())
    }

    fn drop_keyword(&mut self) -> std::io::Result<()> {
        self.out("    pop      rdx\n")?;
        Ok(())
    }

    fn max_keyword(&mut self) -> std::io::Result<()> {
        self.out("    pop      rdi\n")?;
        self.out("    pop      rsi\n")?;
        self.out("    cmp      rdi,rsi\n")?;
        self.out("    mov      rax,rsi\n")?;
        self.out("    cmovge   rax,rdi\n")?;
        self.out("    push     rax\n")?;
        Ok(())
    }

    fn memory_keyword(&mut self) -> std::io::Result<()> {
        self.out("    push     mem\n")?;
        Ok(())
    }
    fn syscall1_keyword(&mut self) -> std::io::Result<()> {
        self.out("    pop      rax\n")?;
        self.out("    pop      rdi\n")?;
        self.out("    syscall\n")?;
        Ok(())
    }
    fn syscall2_keyword(&mut self) -> std::io::Result<()> {
        self.out("    pop      rax\n")?;
        self.out("    pop      rdi\n")?;
        self.out("    pop      rsi\n")?;
        self.out("    syscall\n")?;
        Ok(())
    }
    fn syscall3_keyword(&mut self) -> std::io::Result<()> {
        self.out("    pop      rax\n")?;
        self.out("    pop      rdi\n")?;
        self.out("    pop      rsi\n")?;
        self.out("    pop      rdx\n")?;
        self.out("    syscall\n")?;
        Ok(())
    }
    fn syscall4_keyword(&mut self) -> std::io::Result<()> {
        self.out("    pop      rax\n")?;
        self.out("    pop      rdi\n")?;
        self.out("    pop      rsi\n")?;
        self.out("    pop      rdx\n")?;
        self.out("    pop      r10\n")?;
        self.out("    syscall\n")?;
        Ok(())
    }
    fn syscall5_keyword(&mut self) -> std::io::Result<()> {
        self.out("    pop      rax\n")?;
        self.out("    pop      rdi\n")?;
        self.out("    pop      rsi\n")?;
        self.out("    pop      rdx\n")?;
        self.out("    pop      r10\n")?;
        self.out("    pop      r8\n")?;
        self.out("    syscall\n")?;
        Ok(())
    }
    fn syscall6_keyword(&mut self) -> std::io::Result<()> {
        self.out("    pop      rax\n")?;
        self.out("    pop      rdi\n")?;
        self.out("    pop      rsi\n")?;
        self.out("    pop      rdx\n")?;
        self.out("    pop      r10\n")?;
        self.out("    pop      r8\n")?;
        self.out("    pop      r9\n")?;
        self.out("    syscall\n")?;
        Ok(())
    }

    fn keyword(&mut self, kw: &KeyWord, token: &Token) -> std::io::Result<()> {
        match kw {
            KeyWord::Use => {
                self.errors
                    .push("Error Use keyword only used a top of file".into());
            }
            KeyWord::Word => {} // self.word(stream, token)?,
            KeyWord::In => {}
            KeyWord::While => self.fasm_label(&token.addr())?,
            KeyWord::Do => self.do_keyword(token)?,
            KeyWord::If => self.if_keyword(token)?,
            KeyWord::ElIf => self.elif_keyword(token)?,
            KeyWord::Else => self.else_keyword(token)?,
            KeyWord::End => self.end_keyword(token)?,
            KeyWord::Return => self.return_keyword(token)?,
            KeyWord::And => self.and_keyword(token)?,
            KeyWord::Or => self.or_keyword(token)?,
            KeyWord::True => self.fasm_push("1")?,
            KeyWord::False => self.fasm_push("0")?,
            KeyWord::Dot => self.dot_keyword()?,
            KeyWord::Copy => self.copy_keyword()?,
            KeyWord::Over => self.over_keyword()?,
            KeyWord::Rot => self.rot_keyword()?,
            KeyWord::Swap => self.swap_keyword()?,
            KeyWord::Drop => self.drop_keyword()?,
            KeyWord::Max => self.max_keyword()?,
            KeyWord::Memory => self.memory_keyword()?,
            KeyWord::SysCall1 => self.syscall1_keyword()?,
            KeyWord::SysCall2 => self.syscall2_keyword()?,
            KeyWord::SysCall3 => self.syscall3_keyword()?,
            KeyWord::SysCall4 => self.syscall4_keyword()?,
            KeyWord::SysCall5 => self.syscall5_keyword()?,
            KeyWord::SysCall6 => self.syscall6_keyword()?,
            _ => unreachable!(),
        }
        Ok(())
    }

    fn number_prim(&mut self, value: &str) -> std::io::Result<()> {
        self.fasm_push(value)?;
        Ok(())
    }

    fn char_prim(&mut self, c: &char) -> std::io::Result<()> {
        self.fasm_push(&(*c as u32).to_string())?;
        Ok(())
    }

    fn string_prim(&mut self, token: &Token, string: &str) -> std::io::Result<()> {
        self.data.push((
            remove_file_extension(&token.span.start.filename),
            token.span.start.idx,
            string.to_string(),
        ));
        self.out(&format!("    push     {}\n", string.len()))?;
        self.out(&format!(
            "    push     data{}{}\n",
            remove_file_extension(&token.span.start.filename)
                .replace("-", "")
                .replace("/", ""),
            token.span.start.idx
        ))?;
        Ok(())
    }

    fn primitives(
        &mut self,
        _stream: &mut Stream<Token>,
        prim: &Prim,
        token: &Token,
    ) -> std::io::Result<()> {
        match prim {
            Prim::Int(v) | Prim::Hex(v) => self.number_prim(v)?,
            Prim::Char(i) => self.char_prim(i)?,
            Prim::String(string) => self.string_prim(token, string)?,
        }
        Ok(())
    }

    fn store_op(&mut self) -> std::io::Result<()> {
        self.out("    pop     rbx\n")?;
        self.out("    pop     rax\n")?;
        self.out("    mov     [rax],bl\n")?;
        Ok(())
    }

    fn load_op(&mut self) -> std::io::Result<()> {
        self.out("    pop     rax\n")?;
        self.out("    xor     rbx,rbx\n")?;
        self.out("    mov     bl,[rax]\n")?;
        self.out("    push    rbx\n")?;
        self.out("    push    mem\n")?;
        Ok(())
    }

    fn add_op(&mut self) -> std::io::Result<()> {
        self.out("    pop      rdi\n")?;
        self.out("    pop      rdx\n")?;
        self.out("    add      rdx,rdi\n")?;
        self.out("    push     rdx\n")?;
        Ok(())
    }

    fn minus_op(&mut self) -> std::io::Result<()> {
        self.out("    pop      rdi\n")?;
        self.out("    pop      rdx\n")?;
        self.out("    sub      rdx,rdi\n")?;
        self.out("    push     rdx\n")?;
        Ok(())
    }

    fn mul_op(&mut self) -> std::io::Result<()> {
        self.out("    pop     rax\n")?;
        self.out("    pop     rbx\n")?;
        self.out("    mul     rbx\n")?;
        self.out("    push    rax\n")?;
        Ok(())
    }

    fn div_op(&mut self) -> std::io::Result<()> {
        self.out("    pop      rbx\n")?;
        self.out("    pop      rax\n")?;
        self.out("    div      rbx\n")?;
        self.out("    push     rax\n")?;
        Ok(())
    }

    fn mod_op(&mut self) -> std::io::Result<()> {
        self.out("    xor      rdx, rdx\n")?;
        self.out("    pop      rbx\n")?;
        self.out("    pop      rax\n")?;
        self.out("    div      rbx\n")?;
        self.out("    push     rdx\n")?;
        Ok(())
    }

    fn grt_op(&mut self) -> std::io::Result<()> {
        self.out("    mov         rcx,0\n")?;
        self.out("    mov         rdx,1\n")?;
        self.out("    pop         rbx\n")?;
        self.out("    pop         rax\n")?;
        self.out("    cmp         rax,rbx\n")?;
        self.out("    cmovg       rcx,rdx\n")?;
        self.out("    push        rcx\n")?;
        Ok(())
    }

    fn geq_op(&mut self) -> std::io::Result<()> {
        self.out("    mov     rcx,0\n")?;
        self.out("    mov     rdx,1\n")?;
        self.out("    pop     rbx\n")?;
        self.out("    pop     rax\n")?;
        self.out("    cmp     rax,rbx\n")?;
        self.out("    cmovge  rcx,rdx\n")?;
        self.out("    push    rcx\n")?;
        Ok(())
    }

    fn les_op(&mut self) -> std::io::Result<()> {
        self.out("    mov     rcx, 0\n")?;
        self.out("    mov     rdx, 1\n")?;
        self.out("    pop     rbx\n")?;
        self.out("    pop     rax\n")?;
        self.out("    cmp     rax, rbx\n")?;
        self.out("    cmovl   rcx, rdx\n")?;
        self.out("    push    rcx\n")?;
        Ok(())
    }

    fn leq_op(&mut self) -> std::io::Result<()> {
        self.out("    mov     rcx,0\n")?;
        self.out("    mov     rdx,1\n")?;
        self.out("    pop     rbx\n")?;
        self.out("    pop     rax\n")?;
        self.out("    cmp     rax,rbx\n")?;
        self.out("    cmovle  rcx,rdx\n")?;
        self.out("    push    rcx\n")?;
        Ok(())
    }

    fn neq_op(&mut self) -> std::io::Result<()> {
        self.out("    mov      rbx,0\n")?;
        self.out("    mov      rdx,1\n")?;
        self.out("    pop      rdi\n")?;
        self.out("    pop      rsi\n")?;
        self.out("    cmp      rsi,rdi\n")?;
        self.out("    cmovne   rbx,rdx\n")?;
        self.out("    push     rbx\n")?;
        Ok(())
    }

    fn eq_op(&mut self) -> std::io::Result<()> {
        self.out("    mov      rbx,0\n")?;
        self.out("    mov      rdx,1\n")?;
        self.out("    pop      rdi\n")?;
        self.out("    pop      rsi\n")?;
        self.out("    cmp      rsi,rdi\n")?;
        self.out("    cmove    rbx,rdx\n")?;
        self.out("    push     rbx\n")?;
        Ok(())
    }

    fn operators(&mut self, _stream: &mut Stream<Token>, op: &Oper) -> std::io::Result<()> {
        match op {
            Oper::Store => self.store_op()?,
            Oper::Load => self.load_op()?,
            Oper::Plus => self.add_op()?,
            Oper::Minus => self.minus_op()?,
            Oper::Mul => self.mul_op()?,
            Oper::Div => self.div_op()?,
            Oper::Mod => self.mod_op()?,
            Oper::Grt => self.grt_op()?,
            Oper::Geq => self.geq_op()?,
            Oper::Les => self.les_op()?,
            Oper::Leq => self.leq_op()?,
            Oper::Neq => self.neq_op()?,
            Oper::Eq => self.eq_op()?,
        }
        Ok(())
    }

    fn constant(&mut self, stream: &mut Stream<Token>, const_token: &Token) -> std::io::Result<()> {
        if let Some(token1) = stream.next() {
            match &token1.kind {
                Kind::Id(id1) => {
                    let mut stack: Vec<usize> = Vec::new();
                    while let Some(token2) =
                        stream.next_if(|t| t.kind != Kind::KeyWord(KeyWord::End))
                    {
                        match &token2.kind {
                            Kind::Id(id2) => {
                                if let Some(&Value::Const(num)) = self.global.get(id2) {
                                    stack.push(num);
                                } else {
                                    self.errors.push(format!(
                                        "{} Error: {} is not defined.",
                                        token2.span, id2
                                    ));
                                }
                            }
                            Kind::KeyWord(KeyWord::Const) => {
                                self.errors
                                        .push(
                                            format!("{} Error: `const` Keyword is not allowed in another `const` definition", token2.span)
                                        );
                                break;
                            }
                            Kind::KeyWord(_) => {}
                            Kind::Prim(prim) => match prim {
                                Prim::Int(i) => stack.push(i.parse().unwrap()),
                                _ => self.errors.push(format!(
                                    "{} Error: Unsupported const Type <String>",
                                    token2.span,
                                )),
                            },
                            Kind::Oper(op) => match op {
                                Oper::Store => {
                                    self.errors.push(format!(
                                                "{} Error: Unsupported {} operation in side a 'const declaration'.",
                                                token2.span, token2.kind
                                        ));
                                    break;
                                }
                                Oper::Load => {
                                    self.errors.push(format!(
                                                "{} Error: Unsupported {} operation in side a 'const declaration'.",
                                                token2.span, token2.kind
                                        ));
                                    break;
                                }
                                Oper::Plus => {
                                    if let (Some(r), Some(l)) = (stack.pop(), stack.pop()) {
                                        stack.push(l + r);
                                    } else {
                                        self.errors.push(format!(
                                            "{} Error: Not enough items on stack to {} to.",
                                            token2.span, token2.kind
                                        ));
                                        break;
                                    }
                                }
                                Oper::Minus => {
                                    if let (Some(r), Some(l)) = (stack.pop(), stack.pop()) {
                                        stack.push(l - r);
                                    } else {
                                        self.errors.push(format!(
                                            "{} Error: Not enough items on stack to {} to.",
                                            token2.span, token2.kind
                                        ));
                                        break;
                                    }
                                }
                                Oper::Mul => {
                                    if let (Some(r), Some(l)) = (stack.pop(), stack.pop()) {
                                        stack.push(l * r);
                                    } else {
                                        self.errors.push(format!(
                                            "{} Error: Not enough items on stack to {} to.",
                                            token2.span, token2.kind
                                        ));
                                        break;
                                    }
                                }
                                Oper::Div => {
                                    if let (Some(r), Some(l)) = (stack.pop(), stack.pop()) {
                                        stack.push(l / r);
                                    } else {
                                        self.errors.push(format!(
                                            "{} Error: Not enough items on stack to {} to.",
                                            token2.span, token2.kind
                                        ));
                                        break;
                                    }
                                }
                                Oper::Mod => {
                                    if let (Some(r), Some(l)) = (stack.pop(), stack.pop()) {
                                        stack.push(l % r);
                                    } else {
                                        self.errors.push(format!(
                                            "{} Error: Not enough items on stack to {} to.",
                                            token2.span, token2.kind
                                        ));
                                        break;
                                    }
                                }
                                Oper::Grt => {
                                    if let (Some(r), Some(l)) = (stack.pop(), stack.pop()) {
                                        stack.push((l > r) as usize);
                                    } else {
                                        self.errors.push(format!(
                                            "{} Error: Not enough items on stack to {} to.",
                                            token2.span, token2.kind
                                        ));
                                        break;
                                    }
                                }
                                Oper::Geq => {
                                    if let (Some(r), Some(l)) = (stack.pop(), stack.pop()) {
                                        stack.push((l >= r) as usize);
                                    } else {
                                        self.errors.push(format!(
                                            "{} Error: Not enough items on stack to {} to.",
                                            token2.span, token2.kind
                                        ));
                                        break;
                                    }
                                }
                                Oper::Les => {
                                    if let (Some(r), Some(l)) = (stack.pop(), stack.pop()) {
                                        stack.push((l < r) as usize);
                                    } else {
                                        self.errors.push(format!(
                                            "{} Error: Not enough items on stack to {} to.",
                                            token2.span, token2.kind
                                        ));
                                        break;
                                    }
                                }
                                Oper::Leq => {
                                    if let (Some(r), Some(l)) = (stack.pop(), stack.pop()) {
                                        stack.push((l < r) as usize);
                                    } else {
                                        self.errors.push(format!(
                                            "{} Error: Not enough items on stack to {} to.",
                                            token2.span, token2.kind
                                        ));
                                        break;
                                    }
                                }
                                Oper::Neq => {
                                    if let (Some(r), Some(l)) = (stack.pop(), stack.pop()) {
                                        stack.push((l != r) as usize);
                                    } else {
                                        self.errors.push(format!(
                                            "{} Error: Not enough items on stack to {} to.",
                                            token2.span, token2.kind
                                        ));
                                        break;
                                    }
                                }
                                Oper::Eq => {
                                    if let (Some(r), Some(l)) = (stack.pop(), stack.pop()) {
                                        stack.push((l == r) as usize);
                                    } else {
                                        self.errors.push(format!(
                                            "{} Error: Not enough items on stack to {} to.",
                                            token2.span, token2.kind
                                        ));
                                        break;
                                    }
                                }
                            },
                            _ => unreachable!(),
                        }
                    }
                    if stack.is_empty() {
                        self.errors.push(format!(
                            "{} Error: Nothing to assine to '{}' const variable.",
                            token1.span, id1
                        ));
                    } else if stack.len() > 1 {
                        self.errors.push(format!(
                                "{} Error: To Many values to assine to '{}' const variable left on stack.",
                                token1.span, id1
                        ));
                    } else {
                        self.global
                            .insert(id1.to_string(), Value::Const(stack.pop().unwrap()));
                    }
                    let _ = stream.next();
                }
                _ => self.errors.push(format!(
                    "{} Error: No const variable name was give.",
                    const_token.span
                )),
            }
        }
        Ok(())
    }
}

fn snack_source_file(filename: &str) -> Result<String, String> {
    if filename.ends_with(".snack") {
        match std::fs::read_to_string(filename) {
            Ok(file) => Ok(file),
            Err(e) => Err(e.to_string()),
        }
    } else {
        Err(format!("`{}` is not `snack` source file.", filename))
    }
}

fn remove_file_extension(filename: &str) -> String {
    filename.split('.').collect::<Vec<&str>>()[0].to_string()
}

#[derive(Debug, Clone)]
pub struct Flags {
    pub program: String,
    pub file: String,
    pub debug: bool,
    pub run: bool,
    pub help: bool,
}

impl Flags {
    fn new() -> Self {
        let arguments: Vec<String> = std::env::args().collect();
        if arguments.len() <= 1 {
            println!("No File given to compile.");
            std::process::exit(2);
        }
        let run = arguments.contains(&"run".into());
        let debug = arguments.contains(&"--debug".into());
        let help = arguments.contains(&"--help".into());
        let mut idx_of_program_name = 0;
        for (i, arg) in arguments.iter().enumerate() {
            match (i, arg.as_str()) {
                (_, "run" | "--debug" | "--help") => {}
                (i, _) if i != 0 => {
                    idx_of_program_name = i;
                    break;
                }
                _ => {}
            }
        }
        let program = arguments[0].to_string();
        let file = arguments[idx_of_program_name].to_string();
        Self {
            program,
            file,
            run,
            debug,
            help,
        }
    }

    pub fn name<'a>(&'a self) -> &'a str {
        &self.file.split('.').collect::<Vec<&'a str>>()[0]
    }

    pub fn name_ext(&self) -> &str {
        &self.file
    }
}

fn printhelp(flags: &Flags) {
    if flags.help {
        println!("--debug               :Puts Debug info into output and 'asm' file.");
        println!("  run                 :Compiles program and runs.");
        println!("--help                :Prints this help message.");
        std::process::exit(2);
    }
}

fn scanner(filename: &str) -> (Vec<Token>, Vec<String>) {
    let src = snack_source_file(filename).expect(&format!(
        "Failed to open file '{}' expected a snack file",
        filename
    ));
    let char_span = pos_enum(filename, &src);
    let stream = char_span.iter().peekable();
    let scanner = Scanner::new(stream).lexer().link();
    (scanner.tokens, scanner.errors)
}

fn fasm_assembler(flags: &Flags) {
    let filename = format!("{}.asm", flags.name());
    let fasm_output = std::process::Command::new("fasm")
        .arg(filename.as_str())
        .output()
        .expect(&format!("Failed to compile [{}].", flags.file));
    if fasm_output.status.success() {
        print!("{}", String::from_utf8(fasm_output.stdout).unwrap());
    } else {
        eprint!(
            "[-----FAIL-----]\nSTDERR: {}STDOUT: {}",
            String::from_utf8(fasm_output.stderr).unwrap(),
            String::from_utf8(fasm_output.stdout).unwrap()
        );
        std::process::exit(0);
    }
}

// TODO: Find a bet way to do this please 😁
fn find_word_definitions(tokens: &mut Vec<Token>) -> (Vec<Word>, Option<Word>) {
    let mut main_def: Option<Word> = None;
    let mut words: Vec<Word> = Vec::new();
    let wordloc: Vec<usize> = tokens
        .iter()
        .enumerate()
        .filter(|(_, tok)| tok.kind == Kind::KeyWord(KeyWord::Word))
        .map(|(start, _)| start)
        .collect();

    let endloc: Vec<usize> = tokens
        .iter()
        .enumerate()
        .filter(|(_, tok)| tok.kind == Kind::KeyWord(KeyWord::End) && tok.ret)
        .map(|(end, _)| end)
        .collect();

    // Taking all Functions out of tokens.
    for (start, end) in wordloc.iter().zip(endloc).rev() {
        let mut wordtok: Vec<Token> = Vec::new();
        let mut wordname = String::new();
        let mut last: Option<Kind> = None;
        for t in tokens.drain(*start..=end) {
            if let (Some(Kind::KeyWord(KeyWord::Word)), Kind::Id(name)) = (&last, &t.kind) {
                wordname = name.to_owned();
            }
            last = Some(t.kind.clone());
            wordtok.push(t.clone());
        }
        if wordname == "main" {
            main_def = Some(Word::from((wordname, wordtok)));
        } else {
            words.push(Word::from((wordname, wordtok)));
        }
    }
    words.reverse();
    (words, main_def)
}

fn create_globals(word_defs: &Vec<Word>) -> HashMap<String, Value> {
    let mut global = HashMap::new();
    for word in word_defs.iter() {
        global.insert(word.name().to_string(), Value::Word(word.clone()));
    }
    global
}

#[derive(Debug, Clone)]
struct Word {
    name: String,
    prams: Vec<String>,
    prams_types: Vec<TypeOf>,
    tokens: Vec<Token>,
    return_type: TypeOf,
}

impl Word {
    fn new(
        name: &str,
        prams: Vec<String>,
        prams_types: Vec<TypeOf>,
        tokens: Vec<Token>,
        return_type: TypeOf,
    ) -> Self {
        Self {
            name: name.to_string(),
            prams,
            prams_types,
            tokens,
            return_type,
        }
    }

    fn span(&self) -> Span {
        let start = self.tokens.first().unwrap().span.start.clone();
        let end = self.tokens.last().unwrap().span.end.clone();
        Span { start, end }
    }
    fn prams(&self) -> Vec<String> {
        self.prams.to_owned()
    }

    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn pram_len(&self) -> usize {
        self.prams.len()
    }

    // fn as_tokens(&self) -> &[Token] {
    //     &self.tokens
    // }
    fn body(&self) -> &[Token] {
        // Body returns just tokings between the In and End.
        let end = self.tokens.len() - 1;
        let start = 3 + self.prams.len();
        &self.tokens[start..end]
    }

    fn body_with_end(&self) -> &[Token] {
        // Body returns just tokings between the In and End.
        let start = 3 + self.prams.len();
        &self.tokens[start..]
    }

    fn id_list(&self) -> Vec<String> {
        self.body()
            .iter()
            .filter(|tok| matches!(&tok.kind, Kind::Id(name) if !self.prams.contains(&name)))
            .map(|tok| match &tok.kind {
                Kind::Id(name) => name.to_string(),
                _ => unreachable!(),
            })
            .collect::<Vec<String>>()
    }

    // TODO: rename to 'is_returning'?
    fn is_returning(&self) -> bool {
        match self.return_type {
            TypeOf::Null => false,
            _ => true,
        }
    }
}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<Word({})>", self.name)
    }
}

impl From<(String, Vec<Token>)> for Word {
    fn from((name, mut body): (String, Vec<Token>)) -> Self {
        // Gets all the arguments for word def
        let word_header = body[1..]
            .iter()
            .take_while(|tok| tok.kind != Kind::KeyWord(KeyWord::In))
            .enumerate()
            .filter(|(i, _)| i != &0)
            .map(|(_, tok)| tok.clone())
            .collect::<Vec<Token>>();
        let mut tp = 0;
        let mut prams = Vec::new();
        let mut pram_types = Vec::new();
        let mut return_type = None;
        let mut loc = Vec::new();
        while tp < word_header.len() {
            match &word_header[tp].kind {
                Kind::Id(id) => {
                    prams.push(id.to_string());
                    tp += 1;
                }
                Kind::Ctrl(':') => {
                    loc.push(tp);
                    tp += 1;
                    loc.push(tp);
                    let tok = &word_header[tp];
                    let type_of = match &tok.kind {
                        Kind::Id(id) => id.to_string(),
                        k => {
                            eprintln!("Error: expected pram type but found '{}'", k);
                            std::process::exit(0);
                        }
                    };
                    if let Some(t) = TypeOf::lookup(&type_of) {
                        pram_types.push(t);
                    } else {
                        eprintln!("Error: unknown type '{}'", type_of);
                        std::process::exit(0);
                    }
                    tp += 1;
                }
                Kind::Sym(Sym::RArrow) => {
                    loc.push(tp);
                    tp += 1;
                    loc.push(tp);
                    let tok = &word_header[tp];
                    let type_of = match &tok.kind {
                        Kind::Id(id) => id.to_string(),
                        k => {
                            eprintln!("error: expected return type but found '{}'", k);
                            std::process::exit(0);
                        }
                    };
                    return_type = TypeOf::lookup(&type_of);
                    tp += 1;
                }
                k => {
                    eprintln!("error: expected return type but found '{}'", k);
                    std::process::exit(0);
                }
            }
        }

        for i in loc.iter().rev() {
            let _ = body.remove(*i + 2);
        }

        Self::new(
            &name,
            prams,
            pram_types,
            body,
            return_type.unwrap_or(TypeOf::Null),
        )
    }
}

impl From<(&str, Vec<Token>)> for Word {
    fn from((name, body): (&str, Vec<Token>)) -> Self {
        Self::from((name, body))
    }
}

impl From<(&String, &Vec<Token>)> for Word {
    fn from((name, body): (&String, &Vec<Token>)) -> Self {
        Self::from((name.to_owned(), body.to_owned()))
    }
}

fn create_imports(tokens: &mut Vec<Token>) -> Vec<Token> {
    let idx = tokens
        .iter()
        .take_while(|tok| matches!(tok.kind, Kind::Id(_) | Kind::KeyWord(KeyWord::Use)))
        .count();
    tokens.drain(0..idx).collect::<Vec<Token>>()
}

// This Function is called before Compiler and before TypeChecker
fn sort_tokens(
    mut tokens: Vec<Token>,
) -> (
    Vec<Token>,
    Vec<Token>,
    Vec<Word>,
    Option<Word>,
    HashMap<String, Value>,
) {
    // Pull out all of the Word defs find the Main.
    let (word_defs, main_def) = find_word_definitions(&mut tokens);
    let global = create_globals(&word_defs);
    let imports = create_imports(&mut tokens);
    // The left over 'toknes' are currently const and memory
    // later will be enums and structs as well.
    (imports, tokens, word_defs, main_def, global)
}

fn timer_start() -> std::time::Instant {
    std::time::Instant::now()
}

fn timer_end(msg: &str, start: std::time::Instant) {
    let now = std::time::Instant::now();
    let time = (now - start).as_secs_f64();
    eprintln!("[{}]: {}s", msg, time);
}

// fn timer<F>(msg: &str, task: &mut F)
// where
//     F: FnMut() -> (),
// {
//     let start = timer_start();
//     task();
//     timer_end(msg, start);
// }

fn import(
    imports: Vec<Token>,
    global_defs: &mut Vec<Token>,
    word_defs: &mut Vec<Word>,
    global: &mut HashMap<String, Value>,
) {
    for name in imports
        .iter()
        .filter(|tok| tok.kind != Kind::KeyWord(KeyWord::Use))
        .map(|tok| match &tok.kind {
            Kind::Id(name) => name.to_string(),
            _ => unreachable!(),
        })
    {
        let filename = format!("{}.snack", name);
        let (import_tokens, errs) = scanner(&filename);
        if !errs.is_empty() {
            println!("------------ Failure to Scan -----------");
            for e in errs.iter() {
                eprintln!("{}", e);
            }
            std::process::exit(2);
        }

        let (imports, iglobal_defs, iword_defs, imain_def, iglobal) = sort_tokens(import_tokens);
        if imain_def.is_some() {
            eprintln!("Multiple-Main-Error: Libraries can not have main word declaration.");
            std::process::exit(0);
        }
        global_defs.extend_from_slice(&iglobal_defs);
        word_defs.extend_from_slice(&iword_defs);
        global.extend(iglobal);
        if !imports.is_empty() {
            import(imports, global_defs, word_defs, global);
        }
    }
}

fn main() -> std::io::Result<()> {
    let total = timer_start();
    let flags = Flags::new();
    printhelp(&flags);

    let start = timer_start();
    let (tokens, errors) = scanner(&flags.name_ext());
    // if flags.debug {
    //     for token in tokens.iter() {
    //         eprintln!("{}", token);
    //     }
    // }

    if !errors.is_empty() {
        println!("------------ Failure to compile -----------");
        for e in errors.iter() {
            eprintln!("{}", e);
        }
        std::process::exit(2);
    }
    timer_end("Scanner", start);

    let start = timer_start();
    let (imports, mut global_defs, mut word_defs, main_def, mut global) = sort_tokens(tokens);
    if main_def.is_none() {
        eprintln!("NoMainError: Main entry point must be declared.");
        std::process::exit(1);
    }
    let main_def = main_def.unwrap();
    timer_end("Sorting", start);

    let start = timer_start();
    import(imports, &mut global_defs, &mut word_defs, &mut global);
    timer_end("Importing", start);

    // let start = timer_start();
    // let type_checker =
    //     TypeChecker::new(&mut global_defs, &mut word_defs, &mut main_def, &global).check();
    // type_checker.report();
    // timer_end("Type Checking", start);

    let start = timer_start();
    let compler =
        FasmCompiler::new(flags.clone(), global_defs, word_defs, main_def, global)?.compiler()?;
    compler.report();
    timer_end("Compiler", start);

    let start = timer_start();
    fasm_assembler(&flags);
    timer_end("Fasm Assembler", start);
    timer_end("Total", total);

    if flags.run {
        std::process::Command::new(&format!("./{}", flags.name()))
            .status()
            .expect(&format!("Failed to run [{}].", flags.name()));
    }
    Ok(())
}

// TODO: Make a Constant struct to simulate Stack and compiling constant at compile time.
