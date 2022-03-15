use std::collections::HashMap;
// use std::env::args;
use std::fs::OpenOptions;
use std::io::prelude::*;
use std::{fmt, iter::Peekable};

const MEMORY: u32 = 640_000;
type Stream<'a, T> = Peekable<std::slice::Iter<'a, T>>;

// #[derive(Debug, Default)]
// struct Streamer<T> {
//     tokens: Vec<T>,
//     tp: usize,
// }
//
// impl<T> Streamer<T>
// where
//     T: Clone,
// {
//     fn new(tokens: Vec<T>) -> Self {
//         Self { tokens, tp: 0 }
//     }
//     fn push(&mut self, item: T) {
//         self.tokens.push(item);
//     }
//     fn insert_from_slice(&mut self, slice: &[T]) {
//         for (i, item) in slice.iter().enumerate() {
//             self.tokens.insert(self.tp + i, item.clone());
//         }
//     }
//     pub fn next_if(&mut self, func: impl FnOnce(&T) -> bool) -> Option<T> {
//         match self.next() {
//             Some(matched) if func(&matched) => Some(matched),
//             _ => None,
//         }
//     }
//
//     pub fn peek(&mut self) -> Option<&T> {
//         self.tokens.get(self.tp + 1)
//     }
// }
//
// impl<T> From<&[T]> for Streamer<T>
// where
//     T: Clone,
// {
//     fn from(items: &[T]) -> Self {
//         Self::new(items.to_vec())
//     }
// }
//
// impl<T> Iterator for Streamer<T>
// where
//     T: Clone,
// {
//     type Item = T;
//     fn next(&mut self) -> Option<Self::Item> {
//         let tp = self.tp;
//         self.tp += 1;
//         self.tokens.get(tp).cloned()
//     }
// }

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
            "{}{}:\n",
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
            "end" => Some(End), // (EndKind::UnBound)),
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
            // Self::End(t) => write!(f, "end {}", t),
            Self::End => write!(f, "end"),
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
    String(String),
}

impl fmt::Display for Prim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Hex(i) => write!(f, "Hex({})", i),
            Self::Int(i) => write!(f, "Int({})", i),
            Self::String(i) => write!(f, "String({:?})", i),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    Id(String),
    Prim(Prim),
    Oper(Oper),
    KeyWord(KeyWord),
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Id(i) => write!(f, "{}", i),
            Self::Prim(k) => write!(f, "{}", k),
            Self::Oper(a) => write!(f, "{}", a),
            Self::KeyWord(k) => write!(f, "{}", k),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: Kind,
    pub span: Span,
    pub jump: Option<usize>,
    pub end: Option<usize>,
    pub ret: bool,
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
    pub fn new(kind: Kind, span: Span) -> Self {
        Self {
            kind,
            span,
            jump: None,
            end: None,
            ret: false,
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
}

pub struct Scanner<'a> {
    stream: Stream<'a, (char, Pos)>,
    pub tokens: Vec<Token>,
    pub errors: Vec<String>,
    block: Vec<(usize, Kind)>,
    //current: Option<(char, Pos)>,
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
                '!' => {
                    let (_, end) = self.stream.next().unwrap();
                    self.add_tok(Kind::Oper(Oper::Store), pos.clone(), end.clone())
                }
                '@' => {
                    let (_, end) = self.stream.next().unwrap();
                    self.add_tok(Kind::Oper(Oper::Load), pos.clone(), end.clone())
                }
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
        self.tokens.push(Token::new(kind, Span { start, end }));
    }

    fn line_comment(&mut self) {
        while let Some((_, _)) = self.stream.next_if(|(c, _)| c != &'\n') {}
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
            self.tokens.push(Token::new(kind, Span { start, end }));
        } else if let Kind::KeyWord(KeyWord::End) = kind {
            if let Some((i, k)) = self.block.pop() {
                match k {
                    Kind::KeyWord(KeyWord::Word) => {
                        self.tokens.push(
                            Token::new(kind, Span { start, end })
                                .with_jump(Some(i))
                                .with_ret(true),
                        );
                    }
                    Kind::KeyWord(KeyWord::If) => {
                        self.tokens.push(Token::new(kind, Span { start, end }));
                    }
                    _ => {
                        self.tokens
                            .push(Token::new(kind, Span { start, end }).with_jump(Some(i)));
                    }
                }
            } else {
                self.tokens.push(Token::new(kind, Span { start, end }));
            }
        } else {
            self.tokens.push(Token::new(kind, Span { start, end }));
        }
    }

    pub fn link(mut self) -> Self {
        let mut stack: Vec<Token> = Vec::new();
        for tok in self.tokens.iter_mut().rev() {
            match tok.kind {
                // End needs to know jump if its ending a while loop.
                Kind::KeyWord(KeyWord::End) => {
                    stack.push(tok.clone());
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
                Kind::KeyWord(KeyWord::If)
                    if !stack.is_empty() =>
                    {
                        let last = stack.pop().unwrap();
                        tok.end_mut(Some(last.id()));
                        tok.jump_mut(Some(last.id()));
                    }
                Kind::KeyWord(KeyWord::Word | KeyWord::While | KeyWord::Const)
                    if !stack.is_empty() =>
                    {
                        let last = stack.pop().unwrap();
                        tok.end_mut(Some(last.id()));
                    }
                Kind::KeyWord(
                    KeyWord::Const
                    | KeyWord::Word
                    // | KeyWord::In
                    | KeyWord::While
                    | KeyWord::Do
                    | KeyWord::If
                    | KeyWord::ElIf
                    | KeyWord::Else,
                ) => if stack.is_empty() {
                    self.errors.push(format!(
                            "{} Error: {} is missing 'end' closing block.",
                            tok.span, tok.kind
                    ));
                }
                _ => {
                }
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
    Word(String, usize),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Const(i) => write!(f, "{}", i),
            Self::Word(name, count) => write!(f, "{}, {}", name, count),
        }
    }
}

fn build_global(tokens: &mut Vec<Token>, words: &mut HashMap<String, Vec<Token>>) -> bool {
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
        words.insert(wordname, wordtok);
    }
    if !words.contains_key("main") {
        return true;
    }
    false
}

pub struct FasmCompiler {
    out: std::fs::File,
    tokens: Vec<Token>,
    words: HashMap<String, Vec<Token>>,
    data: Vec<(String, usize, String)>,
    global: HashMap<String, Value>,
    errors: Vec<String>,
    flags: Flags,
}

impl<'a> FasmCompiler {
    const ENTRY_POINT: &'a str = "start";
    pub fn new(flags: Flags, tokens: Vec<Token>) -> std::io::Result<Self> {
        let filename = format!("{}.asm", flags.name());
        let out = OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open(filename)?;
        // let stream = Streamer::new(tokens);
        let global = HashMap::new();
        let errors = Vec::new();
        let words = HashMap::new();
        let data = Vec::new();
        Ok(Self {
            out,
            tokens,
            // stream,
            words,
            data,
            global,
            errors,
            flags,
        })
    }
    fn fasm_header(&mut self, entry_point: &str) -> std::io::Result<()> {
        self.out.write_all(b"format ELF64 executable 3\n")?;
        self.out.write_all(b"segment readable executable\n")?;
        self.out
            .write_all(format!("entry {}\n", entry_point).as_bytes())?;
        Ok(())
    }
    fn fasm_print(&mut self) -> std::io::Result<()> {
        self.out.write_all(b"print:\n")?;
        self.out
            .write_all(b"    mov     r9, -3689348814741910323\n")?;
        self.out.write_all(b"    sub     rsp, 40\n")?;
        self.out.write_all(b"    mov     BYTE [rsp+31], 10\n")?;
        self.out.write_all(b"    lea     rcx, [rsp+30]\n")?;
        self.out.write_all(b".L2:\n")?;
        self.out.write_all(b"    mov     rax, rdi\n")?;
        self.out.write_all(b"    lea     r8, [rsp+32]\n")?;
        self.out.write_all(b"    mul     r9\n")?;
        self.out.write_all(b"    mov     rax, rdi\n")?;
        self.out.write_all(b"    sub     r8, rcx\n")?;
        self.out.write_all(b"    shr     rdx, 3\n")?;
        self.out.write_all(b"    lea     rsi, [rdx+rdx*4]\n")?;
        self.out.write_all(b"    add     rsi, rsi\n")?;
        self.out.write_all(b"    sub     rax, rsi\n")?;
        self.out.write_all(b"    add     eax, 48\n")?;
        self.out.write_all(b"    mov     BYTE [rcx], al\n")?;
        self.out.write_all(b"    mov     rax, rdi\n")?;
        self.out.write_all(b"    mov     rdi, rdx\n")?;
        self.out.write_all(b"    mov     rdx, rcx\n")?;
        self.out.write_all(b"    sub     rcx, 1\n")?;
        self.out.write_all(b"    cmp     rax, 9\n")?;
        self.out.write_all(b"    ja      .L2\n")?;
        self.out.write_all(b"    lea     rax, [rsp+32]\n")?;
        self.out.write_all(b"    mov     edi, 1\n")?;
        self.out.write_all(b"    sub     rdx, rax\n")?;
        self.out.write_all(b"    xor     eax, eax\n")?;
        self.out.write_all(b"    lea     rsi, [rsp+32+rdx]\n")?;
        self.out.write_all(b"    mov     rdx, r8\n")?;
        self.out.write_all(b"    mov     rax, 1\n")?;
        self.out.write_all(b"    syscall\n")?;
        self.out.write_all(b"    add     sp, 40\n")?;
        self.out.write_all(b"    ret\n")?;
        Ok(())
    }
    fn fasm_exit(&mut self) -> std::io::Result<()> {
        self.out.write_all(b"    mov     eax,1\n")?;
        self.out.write_all(b"    xor     ebx,ebx\n")?;
        self.out.write_all(b"    int     0x80\n")?;
        Ok(())
    }
    fn fasm_memory(&mut self) -> std::io::Result<()> {
        // Filename + Idx of String in file
        for (filename, idx, d) in self.data.iter() {
            self.out.write_all(
                format!(
                    "data{}{} db ",
                    filename.replace("-", "").replace("/", ""),
                    idx
                )
                .as_bytes(),
            )?;
            for (i, byte) in d.as_bytes().iter().enumerate() {
                if i != 0 {
                    self.out.write_all(format!(" ,{}", byte).as_bytes())?;
                } else {
                    self.out.write_all(format!("{}", byte).as_bytes())?;
                }
            }
            self.out.write_all(b"\n")?;
        }
        Ok(())
    }
    fn fasm_footer(&mut self) -> std::io::Result<()> {
        self.out.write_all(b"segment readable writeable\n")?;
        self.fasm_memory()?;
        self.out
            .write_all(format!("mem: rb {}\n", MEMORY).as_bytes())?;
        Ok(())
    }

    fn fasm_debug_title(&mut self, token: &Token, comment: &str) -> std::io::Result<()> {
        self.out
            .write_all(format!("{}  === {} ===\n", comment, token).as_bytes())?;
        Ok(())
    }

    fn fasm_prologue(&mut self) -> std::io::Result<()> {
        self.out.write_all(b"    push     rbp\n")?;
        self.out.write_all(b"    mov      rbp, rsp\n")?;
        Ok(())
    }
    fn create_pass_fasm_arguments(&mut self, counter: usize) -> std::io::Result<()> {
        assert!(counter <= 2);
        let args: [&str; 2] = ["edi", "esi"];
        let ret: [&str; 2] = ["ax", "bx"];
        for idx in 0..counter {
            self.out.write_all(
                format!("    mov      dword [rbp-{}],{}\n", (idx * 4) + 4, args[idx]).as_bytes(),
            )?;
            self.out.write_all(
                format!("    mov      e{},dword [rbp-{}]\n", ret[idx], (idx * 4) + 4).as_bytes(),
            )?;
        }
        for idx in 0..counter {
            self.out
                .write_all(format!("    push      r{}\n", ret[idx]).as_bytes())?;
        }
        Ok(())
    }
    fn label(&mut self, name: &str) -> std::io::Result<()> {
        self.out.write_all(format!("{}:\n", name).as_bytes())?;
        Ok(())
    }
}

impl FasmCompiler {
    fn compiler(&mut self) -> std::io::Result<Vec<String>> {
        if build_global(&mut self.tokens, &mut self.words) {
            self.errors
                .push("NoMainError: Main entry point must be declared.".into());
        }

        self.fasm_header(FasmCompiler::ENTRY_POINT)?;
        self.fasm_print()?;

        self.kind(&self.tokens.clone())?;

        let words = self.words.clone();
        for (name, tokens) in words.iter() {
            if name != "main" {
                self.kind(tokens)?;
            }
        }

        self.label(FasmCompiler::ENTRY_POINT)?;

        let main_word = self.words.get("main").cloned();
        if let Some(main) = main_word {
            let body = &main[3..main.len() - 1];
            self.kind(body)?;
        }

        self.fasm_exit()?;
        self.fasm_footer()?;
        Ok(self.errors.clone())
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
                Kind::KeyWord(kw) => self.keyword(&mut stream, kw, &token)?,
                Kind::Prim(prim) => self.primitives(&mut stream, prim, &token)?,
                Kind::Oper(op) => self.operators(&mut stream, op)?,
            }
        }
        Ok(())
    }

    fn word(&mut self, stream: &mut Stream<Token>, wordtok: &Token) -> std::io::Result<()> {
        if let Some(tokenid) = stream.next() {
            match &tokenid.kind {
                Kind::Id(name) => {
                    let mut arg_count = 0;
                    if !self.global.contains_key(name) {
                        self.out.write_all(format!("{}:\n", name).as_bytes())?;
                    } else {
                        self.errors.push(format!(
                            "{} NameAlreadyDefinedError: {} is already defind.",
                            tokenid.span, tokenid.kind
                        ));
                    }
                    while let Some(_name) =
                        stream.next_if(|tok| tok.kind != Kind::KeyWord(KeyWord::In))
                    {
                        arg_count += 1;
                    }
                    self.global
                        .insert(name.clone(), Value::Word(name.clone(), arg_count));
                    self.fasm_prologue()?;
                    self.create_pass_fasm_arguments(arg_count)?;
                }
                _ => self.errors.push(format!(
                    "{} NoWordIdError: found {} ",
                    tokenid.span, tokenid.kind
                )),
            }
        } else {
            self.errors.push(format!(
                "{} NoWordIdError: Name and Body of Word not defined.",
                wordtok.span
            ));
        }
        Ok(())
    }

    fn id(&mut self, id: &str, token: &Token) -> std::io::Result<()> {
        let value = self.global.get(id).cloned();
        match value {
            Some(v) => match v {
                Value::Const(i) => self
                    .out
                    .write_all(format!("    push     {}\n", i).as_bytes())?,
                Value::Word(name, count) => self.call(&name, count)?,
            },
            _ => self
                .errors
                .push(format!("{} Error: UnKnown variable `{}`.", token.span, id)),
        }
        Ok(())
    }

    fn call(&mut self, name: &str, count: usize) -> std::io::Result<()> {
        if count > 2 {
            panic!("We don't know how to handle more arguments");
        }
        let args: [&str; 2] = ["rdi", "rsi"];
        for reg in args[0..count].iter().rev() {
            self.out
                .write_all(format!("    pop     {}\n", reg).as_bytes())?;
        }
        self.out
            .write_all(format!("    call     {}\n", name).as_bytes())?;
        Ok(())
    }

    fn keyword(
        &mut self,
        stream: &mut Stream<Token>,
        kw: &KeyWord,
        token: &Token,
    ) -> std::io::Result<()> {
        match kw {
            KeyWord::Use => {
                if let Some(Token {
                    kind: Kind::Id(name),
                    ..
                }) = stream.next()
                {
                    // Tokenizes file and adds tokens to stream.
                    // Adds Words to global Word list.
                    let filename = format!("{}.snack", name);
                    let (mut import_tokens, errs) = scanner(&filename);
                    if !errs.is_empty() {
                        self.errors.extend_from_slice(&errs);
                        return Ok(());
                    }

                    let mut words: HashMap<String, Vec<Token>> = HashMap::new();
                    if !build_global(&mut import_tokens, &mut words) {
                        self.errors.push(
                            "Multiple-Main-Error: Libraries can not have main word declaration."
                                .into(),
                        );
                    }
                    self.kind(&import_tokens)?;
                    for (_, tokens) in words.iter() {
                        self.kind(tokens)?;
                    }
                } else {
                    self.errors
                        .push(format!("{} Error: Use requires path", token.span,));
                }
            }
            KeyWord::Word => {
                self.word(stream, token)?;
            }
            KeyWord::In => {}
            KeyWord::While => {
                self.out.write_all(token.span.start.addr().as_bytes())?;
            }
            KeyWord::Do => {
                self.out.write_all(b"    pop      rbx\n")?;
                self.out.write_all(b"    test     rbx,rbx\n")?;
                if let Some(e) = token.end {
                    self.out.write_all(
                        format!("    jz      {}{}\n", token.span.start.jump(), e).as_bytes(),
                    )?;
                } else {
                    self.errors.push(format!(
                            "{} Error: Do block missing `end` closing block. This could be a compiler bug.",
                            token.span,
                    ));
                }
            }
            KeyWord::If => {
                self.out.write_all(b"    pop      rbx\n")?;
                self.out.write_all(b"    test     rbx,rbx\n")?;
                if let Some(j) = token.jump {
                    self.out.write_all(
                        format!("    jz     {}{}\n", token.span.start.jump(), j).as_bytes(),
                    )?;
                }
            }
            KeyWord::ElIf => {
                self.out.write_all(token.span.start.addr().as_bytes())?;
                self.out.write_all(b"    pop     rbx\n")?;
                self.out.write_all(b"    test    rbx,rbx\n")?;
                if let Some(j) = token.jump {
                    self.out.write_all(
                        format!("    jz       {}{}\n", token.span.start.jump(), j).as_bytes(),
                    )?;
                } else if let Some(e) = token.end {
                    self.out.write_all(
                        format!("    jz       {}{}\n", token.span.start.jump(), e).as_bytes(),
                    )?;
                } else {
                    self.errors.push(format!(
                        "{} Error: elif statement missing `end` closing block.",
                        token.span,
                    ));
                }
            }
            KeyWord::Else => {
                if let Some(e) = token.end {
                    self.out.write_all(
                        format!("    jmp      {}{}\n", token.span.start.jump(), e).as_bytes(),
                    )?;
                } else {
                    self.errors.push(format!(
                        "{} Error: Else statement missing `end` closing block.",
                        token.span,
                    ));
                }
                self.out.write_all(token.span.start.addr().as_bytes())?;
            }
            KeyWord::End => {
                if let (Some(j), false) = (token.jump, token.ret) {
                    self.out.write_all(
                        format!("    jmp      {}{}\n", token.span.start.jump(), j).as_bytes(),
                    )?;
                } else if let Some(_e) = token.end {
                    self.out.write_all(token.span.start.addr().as_bytes())?;
                }
                self.out.write_all(token.span.start.addr().as_bytes())?;
                if token.ret {
                    self.out.write_all(b"    pop      rbp\n")?;
                    self.out.write_all(b"    ret\n")?;
                }
            }
            KeyWord::Dot => {
                self.out.write_all(b"    pop      rdi\n")?;
                self.out.write_all(b"    call     print\n")?;
            }
            KeyWord::Copy => {
                self.out.write_all(b"    pop      rdi\n")?;
                self.out.write_all(b"    push     rdi\n")?;
                self.out.write_all(b"    push     rdi\n")?;
            }
            KeyWord::Over => {
                self.out.write_all(b"    pop      rdi\n")?;
                self.out.write_all(b"    pop      rdx\n")?;
                self.out.write_all(b"    push     rdx\n")?;
                self.out.write_all(b"    push     rdi\n")?;
                self.out.write_all(b"    push     rdx\n")?;
            }
            KeyWord::Rot => {
                self.out.write_all(b"    pop      rdi\n")?; // 1
                self.out.write_all(b"    pop      rdx\n")?; // 2
                self.out.write_all(b"    pop      rsi\n")?; // 3
                self.out.write_all(b"    push     rdi\n")?; // 1
                self.out.write_all(b"    push     rsi\n")?; // 3
                self.out.write_all(b"    push     rdx\n")?; // 2
            }
            KeyWord::Swap => {
                self.out.write_all(b"    pop      rdi\n")?; // 1
                self.out.write_all(b"    pop      rdx\n")?; // 2
                self.out.write_all(b"    push     rdi\n")?; // 2
                self.out.write_all(b"    push     rdx\n")?; // 1
            }
            KeyWord::Drop => {
                self.out.write_all(b"    pop      rdx\n")?;
            }
            KeyWord::Max => {
                self.out.write_all(b"    pop      rdi\n")?;
                self.out.write_all(b"    pop      rsi\n")?;
                self.out.write_all(b"    cmp      rdi,rsi\n")?;
                self.out.write_all(b"    mov      rax,rsi\n")?;
                self.out.write_all(b"    cmovge   rax,rdi\n")?;
                self.out.write_all(b"    push     rax\n")?;
            }
            KeyWord::Memory => {
                self.out.write_all(b"    push     mem\n")?;
            }
            KeyWord::SysCall1 => self.errors.push(format!(
                "{} ERROR: {} is not yet implemented yet.",
                token.span, kw
            )),
            KeyWord::SysCall2 => self.errors.push(format!(
                "{} ERROR: {} is not yet implemented yet.",
                token.span, kw
            )),
            KeyWord::SysCall3 => {
                self.out.write_all(b"    pop      rax\n")?;
                self.out.write_all(b"    pop      rdi\n")?;
                self.out.write_all(b"    pop      rsi\n")?;
                self.out.write_all(b"    pop      rdx\n")?;
                self.out.write_all(b"    syscall\n")?;
            }
            KeyWord::SysCall4 => self.errors.push(format!(
                "{} ERROR: {} is not yet implemented yet.",
                token.span, kw
            )),
            KeyWord::SysCall5 => self.errors.push(format!(
                "{} ERROR: {} is not yet implemented yet.",
                token.span, kw
            )),
            KeyWord::SysCall6 => self.errors.push(format!(
                "{} ERROR: {} is not yet implemented yet.",
                token.span, kw
            )),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn primitives(
        &mut self,
        _stream: &mut Stream<Token>,
        prim: &Prim,
        token: &Token,
    ) -> std::io::Result<()> {
        match prim {
            Prim::Int(v) | Prim::Hex(v) => {
                self.out
                    .write_all(format!("    push     {}\n", v).as_bytes())?;
            }
            Prim::String(string) => {
                self.data.push((
                    remove_file_extension(&token.span.start.filename),
                    token.span.start.idx,
                    string.to_string(),
                ));
                self.out
                    .write_all(format!("    push     {}\n", string.len()).as_bytes())?;
                self.out.write_all(
                    format!(
                        "    push     data{}{}\n",
                        remove_file_extension(&token.span.start.filename)
                            .replace("-", "")
                            .replace("/", ""),
                        token.span.start.idx
                    )
                    .as_bytes(),
                )?;
            }
        }
        Ok(())
    }

    fn operators(&mut self, _stream: &mut Stream<Token>, op: &Oper) -> std::io::Result<()> {
        match op {
            Oper::Store => {
                self.out.write_all(b"    pop     rbx\n")?;
                self.out.write_all(b"    pop     rax\n")?;
                self.out.write_all(b"    mov     [rax],bl\n")?;
            }
            Oper::Load => {
                self.out.write_all(b"    pop     rax\n")?;
                self.out.write_all(b"    xor     rbx,rbx\n")?;
                self.out.write_all(b"    mov     bl,[rax]\n")?;
            }
            Oper::Plus => {
                self.out.write_all(b"    pop      rdi\n")?;
                self.out.write_all(b"    pop      rdx\n")?;
                self.out.write_all(b"    add      rdx,rdi\n")?;
                self.out.write_all(b"    push     rdx\n")?;
            }
            Oper::Minus => {
                self.out.write_all(b"    pop      rdi\n")?;
                self.out.write_all(b"    pop      rdx\n")?;
                self.out.write_all(b"    sub      rdx,rdi\n")?;
                self.out.write_all(b"    push     rdx\n")?;
            }
            Oper::Mul => {
                self.out.write_all(b"    pop     rax\n")?;
                self.out.write_all(b"    pop     rbx\n")?;
                self.out.write_all(b"    mul     rbx\n")?;
                self.out.write_all(b"    push    rax\n")?;
            }
            Oper::Div => {
                self.out.write_all(b"    pop      rbx\n")?;
                self.out.write_all(b"    pop      rax\n")?;
                self.out.write_all(b"    div      rbx\n")?;
                self.out.write_all(b"    push     rax\n")?;
            }
            Oper::Mod => {
                self.out.write_all(b"    xor      rdx, rdx\n")?;
                self.out.write_all(b"    pop      rbx\n")?;
                self.out.write_all(b"    pop      rax\n")?;
                self.out.write_all(b"    div      rbx\n")?;
                self.out.write_all(b"    push     rdx\n")?;
            }
            Oper::Grt => {
                self.out.write_all(b"    mov      rbx,0\n")?;
                self.out.write_all(b"    mov      rdx,1\n")?;
                self.out.write_all(b"    pop      rdi\n")?;
                self.out.write_all(b"    pop      rsi\n")?;
                self.out.write_all(b"    cmp      rsi,rdi\n")?;
                self.out.write_all(b"    cmovg    rbx,rdx\n")?;
                self.out.write_all(b"    push     rbx\n")?;
            }
            Oper::Geq => {
                self.out.write_all(b"    mov      rbx,0\n")?;
                self.out.write_all(b"    mov      rdx,1\n")?;
                self.out.write_all(b"    pop      rdi\n")?;
                self.out.write_all(b"    pop      rsi\n")?;
                self.out.write_all(b"    cmp      rsi,rdi\n")?;
                self.out.write_all(b"    cmovge   rbx,rdx\n")?;
                self.out.write_all(b"    push     rbx\n")?;
            }
            Oper::Les => {
                self.out.write_all(b"    mov      rbx,0\n")?;
                self.out.write_all(b"    mov      rdx,1\n")?;
                self.out.write_all(b"    pop      rdi\n")?;
                self.out.write_all(b"    pop      rsi\n")?;
                self.out.write_all(b"    cmp      rsi,rdi\n")?;
                self.out.write_all(b"    cmovl    rbx,rdx\n")?;
                self.out.write_all(b"    push     rbx\n")?;
            }
            Oper::Leq => {
                self.out.write_all(b"    mov      rbx,0\n")?;
                self.out.write_all(b"    mov      rdx,1\n")?;
                self.out.write_all(b"    pop      rdi\n")?;
                self.out.write_all(b"    pop      rsi\n")?;
                self.out.write_all(b"    cmp      rsi,rdi\n")?;
                self.out.write_all(b"    cmovle   rbx,rdx\n")?;
                self.out.write_all(b"    push     rbx\n")?;
            }
            Oper::Neq => {
                self.out.write_all(b"    mov      rbx,0\n")?;
                self.out.write_all(b"    mov      rdx,1\n")?;
                self.out.write_all(b"    pop      rdi\n")?;
                self.out.write_all(b"    pop      rsi\n")?;
                self.out.write_all(b"    cmp      rsi,rdi\n")?;
                self.out.write_all(b"    cmovne   rbx,rdx\n")?;
                self.out.write_all(b"    push     rbx\n")?;
            }
            Oper::Eq => {
                self.out.write_all(b"    mov      rbx,0\n")?;
                self.out.write_all(b"    mov      rdx,1\n")?;
                self.out.write_all(b"    pop      rdi\n")?;
                self.out.write_all(b"    pop      rsi\n")?;
                self.out.write_all(b"    cmp      rsi,rdi\n")?;
                self.out.write_all(b"    cmove    rbx,rdx\n")?;
                self.out.write_all(b"    push     rbx\n")?;
            }
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
    let src = snack_source_file(filename).expect("Failed to open file.  Expected a Snack File.");
    let char_span = pos_enum(filename, &src);
    let stream = char_span.iter().peekable();
    let scanner = Scanner::new(stream).lexer().link();
    (scanner.tokens, scanner.errors)
}

fn main() -> std::io::Result<()> {
    let flags = Flags::new();
    printhelp(&flags);
    let (tokens, errors) = scanner(&flags.name_ext());
    if flags.debug {
        for token in tokens.iter() {
            eprintln!("{}", token);
        }
    }

    if !errors.is_empty() {
        println!("------------ Failure to compile -----------");
        for e in errors.iter() {
            eprintln!("{}", e);
        }
        std::process::exit(2);
    }
    let mut compler = FasmCompiler::new(flags.clone(), tokens)?;
    // match compile_to_fasm_x86_64(&flags, &mut tokens) {
    match compler.compiler() {
        Ok(errors) => {
            for e in errors.iter() {
                eprintln!("{}", e);
                std::process::exit(2);
            }
        }
        Err(error) => {
            eprintln!("{}", error);
            std::process::exit(2);
        }
    }
    let filename = format!("{}.asm", flags.name());
    let fasm_output = std::process::Command::new("fasm")
        .arg(&filename)
        .output()
        .expect(&format!("Failed to compile [{}].", flags.file));
    if fasm_output.status.success() {
        print!("{}", String::from_utf8(fasm_output.stdout).unwrap());
        if flags.run {
            let program_name = remove_file_extension(&filename);
            std::process::Command::new(&format!("./{}", program_name))
                .status()
                // .spawn()
                .expect(&format!("Failed to run [{}].", program_name));
        }
    } else {
        eprint!(
            "[-----FAIL-----]\n{}",
            String::from_utf8(fasm_output.stderr).unwrap()
        );
        std::process::exit(2);
    }
    Ok(())
}
