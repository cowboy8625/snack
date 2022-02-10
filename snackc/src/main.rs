#![allow(unused)]
use std::collections::HashMap;
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
            "{}{}:\n",
            remove_file_extension(&self.filename)
                .replace("-", "")
                .replace("/", ""),
            self.idx,
        )
    }

    fn jump(&self) -> String {
        format!(
            "{}",
            remove_file_extension(&self.filename)
                .replace("-", "")
                .replace("/", ""),
        )
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

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// pub enum EndKind {
//     Const(usize),
//     Word(usize),
//     While(usize),
//     Do(usize),
//     If(usize),
//     ElIf(usize),
//     Else(usize),
//     UnBound,
// }
// impl fmt::Display for EndKind {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Self::Word(i) => write!(f, "to word at {}", i),
//             Self::Const(i) => write!(f, "to const at {}", i),
//             Self::While(i) => write!(f, "to while at {}", i),
//             Self::Do(i) => write!(f, "to do at {}", i),
//             Self::If(i) => write!(f, "to if at {}", i),
//             Self::ElIf(i) => write!(f, "to elif at {}", i),
//             Self::Else(i) => write!(f, "to else at {}", i),
//             Self::UnBound => write!(f, "unbound"),
//         }
//     }
// }

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
                    .replace("\\x1b", "\x1b")
                    .into(),
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

fn debug_title(out: &mut std::fs::File, token: &Token, comment: &str) -> std::io::Result<()> {
    out.write_all(format!("{}  === {} ===\n", comment, token).as_bytes())?;
    Ok(())
}

#[derive(Debug, Clone, Hash)]
enum Value {
    Const(usize),
    Word(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Const(i) => write!(f, "{}", i),
            Self::Word(name) => write!(f, "{}", name),
        }
    }
}

fn compile_to_fams_x86_64(flags: &Flags, tokens: &mut Vec<Token>) -> std::io::Result<Vec<String>> {
    let filename = format!("{}.asm", flags.name());
    let mut out = OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(filename)?;

    out.write_all(b"format ELF64 executable 3\n")?;
    out.write_all(b"segment readable executable\n")?;
    out.write_all(b"entry start\n")?;
    out.write_all(b"print:\n")?;
    out.write_all(b"    mov     r9, -3689348814741910323\n")?;
    out.write_all(b"    sub     rsp, 40\n")?;
    out.write_all(b"    mov     BYTE [rsp+31], 10\n")?;
    out.write_all(b"    lea     rcx, [rsp+30]\n")?;
    out.write_all(b".L2:\n")?;
    out.write_all(b"    mov     rax, rdi\n")?;
    out.write_all(b"    lea     r8, [rsp+32]\n")?;
    out.write_all(b"    mul     r9\n")?;
    out.write_all(b"    mov     rax, rdi\n")?;
    out.write_all(b"    sub     r8, rcx\n")?;
    out.write_all(b"    shr     rdx, 3\n")?;
    out.write_all(b"    lea     rsi, [rdx+rdx*4]\n")?;
    out.write_all(b"    add     rsi, rsi\n")?;
    out.write_all(b"    sub     rax, rsi\n")?;
    out.write_all(b"    add     eax, 48\n")?;
    out.write_all(b"    mov     BYTE [rcx], al\n")?;
    out.write_all(b"    mov     rax, rdi\n")?;
    out.write_all(b"    mov     rdi, rdx\n")?;
    out.write_all(b"    mov     rdx, rcx\n")?;
    out.write_all(b"    sub     rcx, 1\n")?;
    out.write_all(b"    cmp     rax, 9\n")?;
    out.write_all(b"    ja      .L2\n")?;
    out.write_all(b"    lea     rax, [rsp+32]\n")?;
    out.write_all(b"    mov     edi, 1\n")?;
    out.write_all(b"    sub     rdx, rax\n")?;
    out.write_all(b"    xor     eax, eax\n")?;
    out.write_all(b"    lea     rsi, [rsp+32+rdx]\n")?;
    out.write_all(b"    mov     rdx, r8\n")?;
    out.write_all(b"    mov     rax, 1\n")?;
    out.write_all(b"    syscall\n")?;
    out.write_all(b"    add     sp, 40\n")?;
    out.write_all(b"    ret\n")?;

    // Memory to be allocated.
    let mut data: Vec<(String, usize, String)> = Vec::new();
    // A list of all computational errors
    let mut errors: Vec<String> = Vec::new();
    // Find all Word's/Functions/Procedures in tokens and remove the definitions
    // Place in Assembly file all function definitions above main
    // Put them into words hash and

    // Find all Word Tokens
    // FIXME: JUST CLEAN THIS UP.................
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
    let mut words: HashMap<String, Vec<Token>> = HashMap::new();

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
    // Pull out const variable then check.
    // if !tokens.is_empty() {
    //     for token in tokens.iter() {
    //         match token.kind {
    //             Kind::Id(_) => {
    //                 errors
    //                     .push(
    //                         format!(
    //                             "{} No{}OutOfMainError: The <{}> Type is not allowed to be out of a word declaration.",
    //                             token.span,
    //                             token.kind
    //                             .to_string()
    //                             .to_uppercase(),
    //                             token.kind
    //                         ));
    //             }
    //             _ => {}
    //         }
    //     }
    //     if !errors.is_empty() {
    //         return Ok(errors);
    //     }
    // }
    if !words.contains_key("main") {
        errors.push("NoMainError: Main entry point must be declared.".into());
    }
    let mut stream = tokens.iter().peekable();

    // Write all Word declaration above main label.

    let mut global_dec: HashMap<String, Value> = HashMap::new();
    kind(
        &mut out,
        flags,
        tokens,
        &mut errors,
        &mut data,
        &mut global_dec,
    )?;
    for (name, tokens) in words.iter()
    // .filter(|(name, _)| name != &&"main".to_string())
    {
        if name != &"main".to_string() {
            let mut stream = tokens.iter().peekable();
            // Look up table for all Const values.
            kind(
                &mut out,
                &flags,
                tokens,
                &mut errors,
                &mut data,
                &mut global_dec,
            )?;
        }
    }
    out.write_all(b"start:\n")?;
    if let Some(main) = words.get("main") {
        // Remove data to be local allocated
        let body = &main[3..main.len() - 1];
        kind(
            &mut out,
            &flags,
            body,
            &mut errors,
            &mut data,
            &mut global_dec,
        )?;
    }
    out.write_all(b"    mov     eax,1\n")?;
    out.write_all(b"    xor     ebx,ebx\n")?;
    out.write_all(b"    int     0x80\n")?;
    out.write_all(b"segment readable writeable\n")?;
    reserve_data(&mut out, &mut data)?;
    out.write_all(format!("mem: rb {}\n", MEMORY).as_bytes())?;
    Ok(errors)
}

fn word<'a>(
    out: &mut std::fs::File,
    wordtok: &Token,
    stream: &mut Stream<'a, Token>,
    global_dec: &mut HashMap<String, Value>,
    errors: &mut Vec<String>,
) -> std::io::Result<()> {
    if let Some(tokenid) = stream.next() {
        match &tokenid.kind {
            Kind::Id(name) => {
                if !global_dec.contains_key(name) {
                    out.write_all(format!("{}:\n", name).as_bytes())?;
                    global_dec.insert(name.clone(), Value::Word(name.clone()));
                } else {
                    errors.push(format!(
                        "{} NameAlreadyDefinedError: {} is already defind.",
                        tokenid.span, tokenid.kind
                    ));
                }
            }
            _ => errors.push(format!(
                "{} NoWordIdError: found {} ",
                tokenid.span, tokenid.kind
            )),
        }
    } else {
        errors.push(format!(
            "{} NoWordIdError: Name and Body of Word not defined.",
            wordtok.span
        ));
    }
    Ok(())
}

fn kind<'a>(
    out: &mut std::fs::File,
    flags: &Flags,
    tokens: &[Token],
    errors: &mut Vec<String>,
    data: &mut Vec<(String, usize, String)>,
    global_dec: &mut HashMap<String, Value>,
) -> std::io::Result<()> {
    let mut stream = tokens.iter().peekable();
    while let Some(token) = stream.next() {
        if flags.debug {
            debug_title(out, &token, ";;")?;
        }
        match &token.kind {
            // Turn this into a Value to put on the sack.
            Kind::Id(id) => match global_dec.get(id) {
                Some(v) => match v {
                    Value::Const(i) => out.write_all(format!("    push     {}\n", i).as_bytes())?,
                    Value::Word(i) => out.write_all(format!("    call     {}\n", id).as_bytes())?,
                },
                _ => errors.push(format!("{} Error: UnKnown variable `{}`.", token.span, id)),
            },
            Kind::KeyWord(KeyWord::Const) => constant(&token, &mut stream, global_dec, errors),
            Kind::KeyWord(kw) => keyword(
                out,
                &token,
                &mut stream,
                global_dec,
                errors,
                kw,
                token.span.clone(),
                token.jump,
                token.end,
            )?,
            Kind::Prim(prim) => primitives(out, prim, token.span.clone(), data)?,
            Kind::Oper(op) => operators(out, op, token.span.clone())?,
        }
    }
    Ok(())
}

fn constant<'a>(
    const_token: &Token,
    stream: &mut Stream<'a, Token>,
    global_dec: &mut HashMap<String, Value>,
    errors: &mut Vec<String>,
) {
    if let Some(token1) = stream.next() {
        match &token1.kind {
            Kind::Id(id1) => {
                let mut stack: Vec<usize> = Vec::new();
                while let Some(token2) = stream.next_if(|t| t.kind != Kind::KeyWord(KeyWord::End)) {
                    match &token2.kind {
                        Kind::Id(id2) => {
                            if let Some(&Value::Const(num)) = global_dec.get(id2) {
                                stack.push(num);
                            } else {
                                errors.push(format!(
                                    "{} Error: {} is not defined.",
                                    token2.span, id2
                                ));
                            }
                        }
                        Kind::KeyWord(KeyWord::Const) => {
                            errors
                                .push(
                                    format!("{} Error: `const` Keyword is not allowed in another `const` definition", token2.span)
                                );
                            break;
                        }
                        Kind::KeyWord(_) => {}
                        Kind::Prim(prim) => match prim {
                            Prim::Int(i) => stack.push(i.parse().unwrap()),
                            _ => errors.push(format!(
                                "{} Error: Unsupported const Type <String>",
                                token2.span,
                            )),
                        },
                        Kind::Oper(op) => match op {
                            Oper::Store => {
                                errors.push(format!(
                                        "{} Error: Unsupported {} operation in side a 'const declaration'.",
                                        token2.span, token2.kind
                                ));
                                break;
                            }
                            Oper::Load => {
                                errors.push(format!(
                                        "{} Error: Unsupported {} operation in side a 'const declaration'.",
                                        token2.span, token2.kind
                                ));
                                break;
                            }
                            Oper::Plus => {
                                if let (Some(r), Some(l)) = (stack.pop(), stack.pop()) {
                                    stack.push(l + r);
                                } else {
                                    errors.push(format!(
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
                                    errors.push(format!(
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
                                    errors.push(format!(
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
                                    errors.push(format!(
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
                                    errors.push(format!(
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
                                    errors.push(format!(
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
                                    errors.push(format!(
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
                                    errors.push(format!(
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
                                    errors.push(format!(
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
                                    errors.push(format!(
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
                                    errors.push(format!(
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
                    errors.push(format!(
                        "{} Error: Nothing to assine to '{}' const variable.",
                        token1.span, id1
                    ));
                } else if stack.len() > 1 {
                    errors.push(format!(
                        "{} Error: To Many values to assine to '{}' const variable left on stack.",
                        token1.span, id1
                    ));
                } else {
                    global_dec.insert(id1.to_string(), Value::Const(stack.pop().unwrap()));
                }
                let _ = stream.next();
            }
            _ => errors.push(format!(
                "{} Error: No const variable name was give.",
                const_token.span
            )),
        }
    }
}

fn keyword<'a>(
    out: &mut std::fs::File,
    token: &Token,
    stream: &mut Stream<'a, Token>,
    variable: &mut HashMap<String, Value>,
    errors: &mut Vec<String>,
    kw: &KeyWord,
    span: Span,
    jump: Option<usize>,
    end: Option<usize>,
) -> std::io::Result<()> {
    match kw {
        KeyWord::Use => {
            errors.push(format!(
                "{} CompilerUseError: This part of the compiler should not see this token.",
                span
            ));
        }
        KeyWord::Word => {
            word(out, token, stream, variable, errors)?;
            // errors.push(format!(
            //     "{} CompilerWordError: keyword word not reachable.",
            //     span
            // ));
        }
        KeyWord::In => {
            //errors.push(format!("{} Error: in is not implemented yet.", span));
        }
        KeyWord::While => {
            out.write_all(span.start.addr().as_bytes())?;
        }
        KeyWord::Do => {
            out.write_all(b"    pop      rbx\n")?;
            out.write_all(b"    test     rbx,rbx\n")?;
            if let Some(e) = end {
                out.write_all(format!("    jz      {}{}\n", span.start.jump(), e).as_bytes())?;
            } else {
                errors.push(format!(
                    "{} Error: Do block missing `end` closing block. This could be a compiler bug.",
                    span,
                ));
            }
        }
        KeyWord::If => {
            out.write_all(b"    pop      rbx\n")?;
            out.write_all(b"    test     rbx,rbx\n")?;
            if let Some(j) = jump {
                out.write_all(format!("    jz     {}{}\n", span.start.jump(), j).as_bytes())?;
            }
        }
        KeyWord::ElIf => {
            out.write_all(span.start.addr().as_bytes())?;
            out.write_all(b"    pop     rbx\n")?;
            out.write_all(b"    test    rbx,rbx\n")?;
            if let Some(j) = jump {
                out.write_all(format!("    jz       {}{}\n", span.start.jump(), j).as_bytes())?;
            } else if let Some(e) = end {
                out.write_all(format!("    jz       {}{}\n", span.start.jump(), e).as_bytes())?;
            } else {
                errors.push(format!(
                    "{} Error: elif statement missing `end` closing block.",
                    span,
                ));
            }
        }
        KeyWord::Else => {
            if let Some(e) = end {
                out.write_all(format!("    jmp      {}{}\n", span.start.jump(), e).as_bytes())?;
            } else {
                errors.push(format!(
                    "{} Error: Else statement missing `end` closing block.",
                    span,
                ));
            }
            out.write_all(span.start.addr().as_bytes())?;
        }
        KeyWord::End => {
            if let (Some(j), false) = (jump, token.ret) {
                out.write_all(format!("    jmp      {}{}\n", span.start.jump(), j).as_bytes())?;
            } else if let Some(e) = end {
                out.write_all(span.start.addr().as_bytes())?;
            }
            out.write_all(span.start.addr().as_bytes())?;
            if token.ret {
                out.write_all(b"    ret\n")?;
            }
        }
        KeyWord::Dot => {
            out.write_all(b"    pop      rdi\n")?;
            out.write_all(b"    call     print\n")?;
        }
        KeyWord::Copy => {
            out.write_all(b"    pop      rdi\n")?;
            out.write_all(b"    push     rdi\n")?;
            out.write_all(b"    push     rdi\n")?;
        }
        KeyWord::Over => {
            out.write_all(b"    pop      rdi\n")?;
            out.write_all(b"    pop      rdx\n")?;
            out.write_all(b"    push     rdx\n")?;
            out.write_all(b"    push     rdi\n")?;
            out.write_all(b"    push     rdx\n")?;
        }
        KeyWord::Rot => {
            out.write_all(b"    pop      rdi\n")?; // 1
            out.write_all(b"    pop      rdx\n")?; // 2
            out.write_all(b"    pop      rsi\n")?; // 3
            out.write_all(b"    push     rdi\n")?; // 1
            out.write_all(b"    push     rsi\n")?; // 3
            out.write_all(b"    push     rdx\n")?; // 2
        }
        KeyWord::Swap => {
            out.write_all(b"    pop      rdi\n")?; // 1
            out.write_all(b"    pop      rdx\n")?; // 2
            out.write_all(b"    push     rdi\n")?; // 2
            out.write_all(b"    push     rdx\n")?; // 1
        }
        KeyWord::Drop => {
            out.write_all(b"    pop      rdx\n")?;
        }
        KeyWord::Max => {
            out.write_all(b"    pop      rdi\n")?;
            out.write_all(b"    pop      rsi\n")?;
            out.write_all(b"    cmp      rdi,rsi\n")?;
            out.write_all(b"    mov      rax,rsi\n")?;
            out.write_all(b"    cmovge   rax,rdi\n")?;
            out.write_all(b"    push     rax\n")?;
        }
        KeyWord::Memory => {
            out.write_all(b"    push     mem\n")?;
        }
        KeyWord::SysCall1 => errors.push(format!(
            "{} ERROR: {} is not yet implemented yet.",
            span, kw
        )),
        KeyWord::SysCall2 => errors.push(format!(
            "{} ERROR: {} is not yet implemented yet.",
            span, kw
        )),
        KeyWord::SysCall3 => {
            out.write_all(b"    pop      rax\n")?;
            out.write_all(b"    pop      rdi\n")?;
            out.write_all(b"    pop      rsi\n")?;
            out.write_all(b"    pop      rdx\n")?;
            out.write_all(b"    syscall\n")?;
        }
        KeyWord::SysCall4 => errors.push(format!(
            "{} ERROR: {} is not yet implemented yet.",
            span, kw
        )),
        KeyWord::SysCall5 => errors.push(format!(
            "{} ERROR: {} is not yet implemented yet.",
            span, kw
        )),
        KeyWord::SysCall6 => errors.push(format!(
            "{} ERROR: {} is not yet implemented yet.",
            span, kw
        )),
        _ => unreachable!(),
    }
    Ok(())
}

fn primitives(
    out: &mut std::fs::File,
    prim: &Prim,
    span: Span,
    data: &mut Vec<(String, usize, String)>,
) -> std::io::Result<()> {
    match prim {
        Prim::Int(v) | Prim::Hex(v) => {
            out.write_all(format!("    push     {}\n", v).as_bytes())?;
        }
        Prim::String(string) => {
            data.push((
                remove_file_extension(&span.start.filename),
                span.start.idx,
                string.to_string(),
            ));
            out.write_all(format!("    push     {}\n", string.len()).as_bytes())?;
            out.write_all(
                format!(
                    "    push     {}\n",
                    format!(
                        "data{}{}",
                        remove_file_extension(&span.start.filename)
                            .replace("-", "")
                            .replace("/", ""),
                        span.start.idx
                    )
                )
                .as_bytes(),
            )?;
        }
    }
    Ok(())
}

fn operators(out: &mut std::fs::File, op: &Oper, _span: Span) -> std::io::Result<()> {
    match op {
        Oper::Store => {
            out.write_all(b"    pop     rbx\n")?;
            out.write_all(b"    pop     rax\n")?;
            out.write_all(b"    mov     [rax],bl\n")?;
        }
        Oper::Load => {
            out.write_all(b"    pop     rax\n")?;
            out.write_all(b"    xor     rbx,rbx\n")?;
            out.write_all(b"    mov     bl,[rax]\n")?;
        }
        Oper::Plus => {
            out.write_all(b"    pop      rdi\n")?;
            out.write_all(b"    pop      rdx\n")?;
            out.write_all(b"    add      rdx,rdi\n")?;
            out.write_all(b"    push     rdx\n")?;
        }
        Oper::Minus => {
            out.write_all(b"    pop      rdi\n")?;
            out.write_all(b"    pop      rdx\n")?;
            out.write_all(b"    sub      rdx,rdi\n")?;
            out.write_all(b"    push     rdi\n")?;
        }
        Oper::Mul => {
            out.write_all(b"    pop     rax\n")?;
            out.write_all(b"    pop     rbx\n")?;
            out.write_all(b"    mul     rbx\n")?;
            out.write_all(b"    push    rax\n")?;
        }
        Oper::Div => {
            out.write_all(b"    pop      rbx\n")?;
            out.write_all(b"    pop      rax\n")?;
            out.write_all(b"    div      rbx\n")?;
            out.write_all(b"    push     rax\n")?;
        }
        Oper::Mod => {
            out.write_all(b"    xor      rdx, rdx\n")?;
            out.write_all(b"    pop      rbx\n")?;
            out.write_all(b"    pop      rax\n")?;
            out.write_all(b"    div      rbx\n")?;
            out.write_all(b"    push     rdx\n")?;
        }
        Oper::Grt => {
            out.write_all(b"    mov      rbx,0\n")?;
            out.write_all(b"    mov      rdx,1\n")?;
            out.write_all(b"    pop      rdi\n")?;
            out.write_all(b"    pop      rsi\n")?;
            out.write_all(b"    cmp      rsi,rdi\n")?;
            out.write_all(b"    cmovg    rbx,rdx\n")?;
            out.write_all(b"    push     rbx\n")?;
        }
        Oper::Geq => {
            out.write_all(b"    mov      rbx,0\n")?;
            out.write_all(b"    mov      rdx,1\n")?;
            out.write_all(b"    pop      rdi\n")?;
            out.write_all(b"    pop      rsi\n")?;
            out.write_all(b"    cmp      rsi,rdi\n")?;
            out.write_all(b"    cmovge   rbx,rdx\n")?;
            out.write_all(b"    push     rbx\n")?;
        }
        Oper::Les => {
            out.write_all(b"    mov      rbx,0\n")?;
            out.write_all(b"    mov      rdx,1\n")?;
            out.write_all(b"    pop      rdi\n")?;
            out.write_all(b"    pop      rsi\n")?;
            out.write_all(b"    cmp      rsi,rdi\n")?;
            out.write_all(b"    cmovl    rbx,rdx\n")?;
            out.write_all(b"    push     rbx\n")?;
        }
        Oper::Leq => {
            out.write_all(b"    mov      rbx,0\n")?;
            out.write_all(b"    mov      rdx,1\n")?;
            out.write_all(b"    pop      rdi\n")?;
            out.write_all(b"    pop      rsi\n")?;
            out.write_all(b"    cmp      rsi,rdi\n")?;
            out.write_all(b"    cmovle   rbx,rdx\n")?;
            out.write_all(b"    push     rbx\n")?;
        }
        Oper::Neq => {
            out.write_all(b"    mov      rbx,0\n")?;
            out.write_all(b"    mov      rdx,1\n")?;
            out.write_all(b"    pop      rdi\n")?;
            out.write_all(b"    pop      rsi\n")?;
            out.write_all(b"    cmp      rsi,rdi\n")?;
            out.write_all(b"    cmovne   rbx,rdx\n")?;
            out.write_all(b"    push     rbx\n")?;
        }
        Oper::Eq => {
            out.write_all(b"    mov      rbx,0\n")?;
            out.write_all(b"    mov      rdx,1\n")?;
            out.write_all(b"    pop      rdi\n")?;
            out.write_all(b"    pop      rsi\n")?;
            out.write_all(b"    cmp      rsi,rdi\n")?;
            out.write_all(b"    cmove    rbx,rdx\n")?;
            out.write_all(b"    push     rbx\n")?;
        }
    }
    Ok(())
}
//                                                Filename, idx, Data
fn reserve_data(out: &mut std::fs::File, data: &[(String, usize, String)]) -> std::io::Result<()> {
    // Filename + Idx of String in file
    for (filename, idx, d) in data.iter() {
        out.write_all(
            format!(
                "data{}{} db ",
                filename.replace("-", "").replace("/", ""),
                idx
            )
            .as_bytes(),
        )?;
        for (i, byte) in d.as_bytes().iter().enumerate() {
            if i != 0 {
                out.write_all(format!(" ,{}", byte).as_bytes())?;
            } else {
                out.write_all(format!("{}", byte).as_bytes())?;
            }
        }
        out.write_all(b"\n")?;
    }
    Ok(())
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

fn main() {
    let flags = Flags::new();
    printhelp(&flags);
    let src =
        snack_source_file(&flags.name_ext()).expect("Failed to open file.  Expected a Snack File.");
    let char_span = pos_enum(&flags.name_ext(), &src);
    let stream = char_span.iter().peekable();
    let mut scanner = Scanner::new(stream).lexer().link();

    if flags.debug {
        for token in scanner.tokens.iter() {
            eprintln!("{}", token);
        }
    }

    if !scanner.errors.is_empty() {
        println!("------------ Failure to compile -----------");
        for e in scanner.errors.iter() {
            eprintln!("{}", e);
        }
        std::process::exit(2);
    }
    match compile_to_fams_x86_64(&flags, &mut scanner.tokens) {
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
}
