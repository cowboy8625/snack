use std::fs::OpenOptions;
use std::io::prelude::*;
use std::{fmt, iter::Peekable};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pos {
    pub idx: usize,
    pub row: usize,
    pub col: usize,
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.idx, self.row, self.col)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeyWord {
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
    Drop,
    Max,
    Memory,
}

impl KeyWord {
    pub fn lookup(name: &str) -> Option<Self> {
        use KeyWord::*;
        match name {
            "while" => Some(While),
            "do" => Some(Do),
            "if" => Some(If),
            "elif" => Some(ElIf),
            "else" => Some(Else),
            "end" => Some(End),
            "." => Some(Dot),
            "copy" => Some(Copy),
            "over" => Some(Over),
            "rot" => Some(Rot),
            "drop" => Some(Drop),
            "max" => Some(Max),
            "memory" => Some(Memory),
            _ => None,
        }
    }
}

impl fmt::Display for KeyWord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::While => write!(f, "while"),
            Self::Do => write!(f, "do"),
            Self::If => write!(f, "if"),
            Self::ElIf => write!(f, "elif"),
            Self::Else => write!(f, "else"),
            Self::End => write!(f, "end"),
            Self::Dot => write!(f, "."),
            Self::Copy => write!(f, "copy"),
            Self::Over => write!(f, "over"),
            Self::Rot => write!(f, "rot"),
            Self::Drop => write!(f, "drop"),
            Self::Max => write!(f, "max"),
            Self::Memory => write!(f, "memory"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Oper {
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
    Int(String),
}

impl fmt::Display for Prim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "Int({})", i),
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
}

impl Token {
    pub fn new(kind: Kind, span: Span) -> Self {
        Self {
            kind,
            span,
            jump: None,
            end: None,
        }
    }

    pub fn with_jump(mut self, jump: usize) -> Self {
        self.jump = Some(jump);
        self
    }

    pub fn jump_mut(&mut self, jump: Option<usize>) {
        self.jump = jump;
    }

    pub fn end_mut(&mut self, end: Option<usize>) {
        self.end = end;
    }
}

pub struct Scanner<'a> {
    stream: Peekable<std::slice::Iter<'a, (char, Pos)>>,
    pub tokens: Vec<Token>,
    pub errors: Vec<String>,
    whileloop: Vec<usize>,
}

impl<'a> Scanner<'a> {
    pub fn new(stream: Peekable<std::slice::Iter<'a, (char, Pos)>>) -> Self {
        Self {
            stream,
            tokens: Vec::new(),
            errors: Vec::new(),
            whileloop: Vec::new(),
        }
    }

    pub fn lexer(mut self) -> Self {
        let unwrap = (
            '\0',
            Pos {
                idx: 0,
                row: 0,
                col: 0,
            },
        );
        while let Some((c, pos)) = self.stream.next() {
            match c {
                '/' if self.stream.peek().unwrap_or(&&&unwrap).0 == '/' => {
                    self.line_comment();
                }
                n if n.is_ascii_digit() => self.number(*c, *pos),
                i if i.is_ascii_alphabetic() => self.identifier(*c, *pos),
                '.' => self.add_tok(Kind::KeyWord(KeyWord::Dot), pos.clone(), *pos),
                '+' => self.add_tok(Kind::Oper(Oper::Plus), pos.clone(), *pos),
                '-' => self.add_tok(Kind::Oper(Oper::Minus), pos.clone(), *pos),
                '/' => self.add_tok(Kind::Oper(Oper::Div), pos.clone(), *pos),
                '%' => self.add_tok(Kind::Oper(Oper::Mod), pos.clone(), *pos),
                '*' => self.add_tok(Kind::Oper(Oper::Mul), pos.clone(), *pos),
                '>' if self.stream.peek().unwrap_or(&&unwrap).0 == '=' => {
                    let (_, end) = self.stream.next().unwrap();
                    self.add_tok(Kind::Oper(Oper::Geq), pos.clone(), *end)
                }
                '<' if self.stream.peek().unwrap_or(&&unwrap).0 == '=' => {
                    let (_, end) = self.stream.next().unwrap();
                    self.add_tok(Kind::Oper(Oper::Leq), pos.clone(), *end)
                }
                '>' => self.add_tok(Kind::Oper(Oper::Grt), pos.clone(), *pos),
                '<' => self.add_tok(Kind::Oper(Oper::Les), pos.clone(), *pos),
                '=' if self.stream.peek().unwrap_or(&&unwrap).0 == '=' => {
                    let (_, end) = self.stream.next().unwrap();
                    self.add_tok(Kind::Oper(Oper::Eq), pos.clone(), *end)
                }
                '!' if self.stream.peek().unwrap_or(&&unwrap).0 == '=' => {
                    let (_, end) = self.stream.next().unwrap();
                    self.add_tok(Kind::Oper(Oper::Neq), pos.clone(), *end)
                }
                ' ' | '\n' => {}
                _ => self.errors.push(format!(
                    "[{}:ERROR]: UnKnown Char: '{}'",
                    c,
                    Span {
                        start: *pos,
                        end: *pos,
                    }
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

    fn number(&mut self, c: char, start: Pos) {
        let mut number = c.to_string();
        let mut end = start;
        while let Some((c, pos)) = self.stream.next_if(|(c, _)| c.is_ascii_digit()) {
            end = *pos;
            number.push(*c);
        }
        self.tokens.push(Token::new(
            Kind::Prim(Prim::Int(number)),
            Span { start, end },
        ));
    }
    fn identifier(&mut self, c: char, start: Pos) {
        let mut id = c.to_string();
        let mut end = start;
        while let Some((c, pos)) = self.stream.next_if(|(c, _)| c.is_ascii_alphanumeric()) {
            end = *pos;
            id.push(*c);
        }
        let kind = KeyWord::lookup(&id).map_or(Kind::Id(id), Kind::KeyWord);
        if let Kind::KeyWord(KeyWord::While) = kind {
            self.whileloop.push(start.idx);
            self.tokens.push(Token::new(kind, Span { start, end }));
        } else if let Kind::KeyWord(KeyWord::End) = kind {
            self.tokens.push(
                Token::new(kind, Span { start, end }).with_jump(self.whileloop.pop().unwrap()),
            );
        } else {
            self.tokens.push(Token::new(kind, Span { start, end }));
        }
    }

    pub fn link(mut self) -> Self {
        let mut ends: Vec<usize> = Vec::new();
        let mut next: Vec<usize> = Vec::new();
        for tok in self.tokens.iter_mut().rev() {
            let i = tok.span.start.idx;
            match tok.kind {
                Kind::KeyWord(KeyWord::End) => {
                    ends.push(i);
                }
                // Else Needs to know end.
                Kind::KeyWord(KeyWord::Else) if !ends.is_empty() => {
                    tok.end_mut(ends.last().map(Clone::clone));
                    next.push(i);
                }
                Kind::KeyWord(KeyWord::ElIf) if !ends.is_empty() => {
                    if ends.len() == next.len() {
                        tok.jump_mut(next.last().map(Clone::clone));
                        tok.end_mut(ends.last().map(Clone::clone));
                        next.last_mut().replace(&mut i.clone());
                        let mut last = next.pop();
                        last.replace(i);
                        next.push(last.unwrap());
                    } else {
                        next.push(i);
                    }
                }
                Kind::KeyWord(KeyWord::If) if !ends.is_empty() => {
                    tok.jump_mut(next.pop());
                    tok.end_mut(ends.pop());
                }
                Kind::KeyWord(KeyWord::Do) if !ends.is_empty() => {
                    tok.end_mut(ends.last().map(Clone::clone));
                }
                Kind::KeyWord(KeyWord::While) if !ends.is_empty() => {
                    tok.jump_mut(next.pop());
                    tok.end_mut(ends.pop());
                }
                _ => {}
            }
        }
        self
    }
}

pub fn pos_enum<'a>(src: &'a str) -> Vec<(char, Pos)> {
    let mut pos = Pos {
        idx: 0,
        row: 0,
        col: 0,
    };
    let mut spanned: Vec<(char, Pos)> = Vec::new();
    for (i, c) in src.chars().enumerate() {
        pos.idx = i;
        pos.row += if i != 0 { 1 } else { 0 };
        if c == '\n' {
            pos.row = 0;
            pos.col += 1;
        }
        spanned.push((c, pos));
    }
    spanned.clone()
}

#[cfg(feature = "debug")]
fn debug_title(out: &mut std::fs::File, token: &Token, comment: &str) -> std::io::Result<()> {
    out.write_all(format!("{}  === {} ===\n", comment, token.kind).as_bytes())?;
    Ok(())
}

fn compile_to_fams_x86_64(tokens: &[Token], filename: &str) -> std::io::Result<()> {
    let mut stream = tokens.iter().peekable();
    let filename = format!("{}.asm", &filename.split('.').collect::<Vec<&str>>()[0]);
    let mut out = OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(filename)?;

    out.write_all(b"format ELF64 executable 3\n")?;
    out.write_all(b"segment readable executable\n")?;
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
    out.write_all(b"entry start\n")?;
    out.write_all(b"start:\n")?;
    while let Some(token) = stream.next() {
        #[cfg(feature = "debug")]
        debug_title(&mut out, &token, ";;")?;
        match &token.kind {
            Kind::Id(id) => panic!("{}: UnKnown KeyWord `{}`.", token.span, id),
            Kind::KeyWord(kw) => keyword(&mut out, kw, token.span, token.jump, token.end)?,
            Kind::Prim(prim) => primitives(&mut out, prim, token.span)?,
            Kind::Oper(op) => operators(&mut out, op, token.span)?,
        }
    }
    out.write_all(b"    mov     eax,1\n")?;
    out.write_all(b"    xor     ebx,ebx\n")?;
    out.write_all(b"    int     0x80\n")?;
    out.write_all(b"segment readable writeable\n")?;
    Ok(())
}

fn keyword(
    out: &mut std::fs::File,
    kw: &KeyWord,
    span: Span,
    jump: Option<usize>,
    end: Option<usize>,
) -> std::io::Result<()> {
    match kw {
        KeyWord::While => {
            out.write_all(format!("address{}:\n", span.start.idx).as_bytes())?;
        }
        KeyWord::Do => {
            out.write_all(b"    pop      rbx\n")?;
            out.write_all(b"    test     rbx,rbx\n")?;
            if let Some(e) = end {
                out.write_all(format!("    jz      address{}\n", e).as_bytes())?;
            }
        }
        KeyWord::If => {
            out.write_all(b"    pop      rbx\n")?;
            out.write_all(b"    test     rbx,rbx\n")?;
            if let Some(j) = jump {
                out.write_all(format!("    jz     address{}\n", j).as_bytes())?;
            }
        }
        KeyWord::ElIf => {
            if let Some(e) = end {
                out.write_all(format!("    jmp     address{}\n", e).as_bytes())?;
            } else {
                panic!("{}, If statement missing `end` closing block.", span);
            }
            out.write_all(format!("address{}:\n", span.start.idx).as_bytes())?;
            out.write_all(b"    pop     rbx\n")?;
            out.write_all(b"    test    rbx,rbx\n")?;
            if let Some(j) = jump {
                out.write_all(format!("    jz    address{}\n", j).as_bytes())?;
            } else if let Some(e) = end {
                out.write_all(format!("    jz    address{}\n", e).as_bytes())?;
            } else {
                panic!("{}, If statement missing `end` closing block.", span);
            }
        }
        KeyWord::Else => {
            if let Some(e) = end {
                out.write_all(format!("    jmp      address{}\n", e).as_bytes())?;
            } else {
                panic!("{}, If statement missing `end` closing block.", span);
            }
            out.write_all(format!("address{}:\n", span.start.idx).as_bytes())?;
        }
        KeyWord::End => {
            if let Some(j) = jump {
                out.write_all(format!("    jmp      address{}\n", j).as_bytes())?;
            }
            out.write_all(format!("address{}:\n", span.start.idx).as_bytes())?;
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
            out.write_all(b"   pop       rdi\n")?;
            out.write_all(b"   pop       rdx\n")?;
            out.write_all(b"   push      rdx\n")?;
            out.write_all(b"   push      rdi\n")?;
            out.write_all(b"   push      rdx\n")?;
        }
        KeyWord::Rot => {
            out.write_all(b"    pop      rdi\n")?;
            out.write_all(b"    pop      rdx\n")?;
            out.write_all(b"    push     rdx\n")?;
            out.write_all(b"    push     rdi\n")?;
            out.write_all(b"    push     rdx\n")?;
        }
        KeyWord::Drop => out.write_all(b"    pop    rdx\n")?,
        KeyWord::Max => {
            out.write_all(b"    pop      rdi\n")?;
            out.write_all(b"    pop      rsi\n")?;
            out.write_all(b"    cmp      rdi,rsi\n")?;
            out.write_all(b"    mov      rax,rsi\n")?;
            out.write_all(b"    cmovge   rax,rdi\n")?;
            out.write_all(b"    push     rax\n")?;
        }
        KeyWord::Memory => panic!("{}, Else is not implemeted.", span),
    }
    Ok(())
}

fn primitives(out: &mut std::fs::File, prim: &Prim, _span: Span) -> std::io::Result<()> {
    match prim {
        Prim::Int(v) => {
            out.write_all(format!("    push   {}\n", v).as_bytes())?;
        }
    }
    Ok(())
}

fn operators(out: &mut std::fs::File, op: &Oper, _span: Span) -> std::io::Result<()> {
    match op {
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
            out.write_all(b"    pop      rdi\n")?;
            out.write_all(b"    pop      rdx\n")?;
            out.write_all(b"    sub      rdx,rdi\n")?;
            out.write_all(b"    push     rdi\n")?;
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

fn snack_source_file(filename: &str) -> Result<String, String> {
    if filename.ends_with(".snack") {
        match std::fs::read_to_string(filename) {
            Ok(file) => Ok(file),
            Err(e) => Err(e.to_string()),
        }
    } else {
        Err("This is not `snack` source file.".into())
    }
}

fn main() {
    let arguments: Vec<String> = dbg!(std::env::args().collect());
    let _source_file = arguments[0].to_string();
    if arguments.len() < 2 {
        println!("No File given to compile.");
        std::process::exit(1);
    }
    let filename = arguments[1].to_string();
    let src = snack_source_file(&filename).expect("Failed to open file.  Expected a Snack File.");
    let char_span = pos_enum(&src);
    let stream = char_span.iter().peekable();
    let scanner = Scanner::new(stream).lexer().link();
    for token in scanner.tokens.iter() {
        println!("{:?}", token);
    }
    if !scanner.errors.is_empty() {
        for e in scanner.errors.iter() {
            println!("{}", e);
        }
        std::process::exit(1);
    }
    compile_to_fams_x86_64(&scanner.tokens, &filename).unwrap();
}
