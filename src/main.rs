#![allow(unused)]
use std::env::args;
use std::fs::OpenOptions;
use std::io::Write;
use std::process::Command;

macro_rules! is_a {
    ($kind:ident, $id:ident, $name:ident) => {
        pub fn $name(&self) -> bool {
            match self {
                $kind::$id(..) => true,
                _ => false,
            }
        }
    };
}
mod lexer {
    use std::iter::Peekable;
    use std::str::Chars;
    #[derive(Debug, Default, Clone, Copy)]
    pub struct Span {
        pub start: usize,
        pub end: usize,
        pub line: usize,
    }

    impl Span {
        pub fn new(line: usize, start: usize, end: usize) -> Self {
            Self { start, end, line }
        }
        pub fn right_shift(&mut self, ch: char) {
            if ch == '\0' {
                return;
            }
            if ch == '\n' {
                self.line += 1;
            }
            self.end += ch.to_string().as_bytes().len();
        }

        pub fn range(&self) -> std::ops::Range<usize> {
            self.start..self.end
        }
    }

    impl From<&Span> for Span {
        fn from(other: &Self) -> Self {
            Self {
                start: other.end,
                end: other.end,
                line: other.line,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum Token {
        KeyWord(String, Span),
        Int(String, Span),
        Id(String, Span),
        Op(String, Span),
        String(String, Span),
        Char(char, Span),
        Eof(Span),
    }

    impl Token {
        pub fn span(&self) -> Span {
            match self {
                Self::KeyWord(.., span) => *span,
                Self::Int(.., span) => *span,
                Self::Id(.., span) => *span,
                Self::Op(.., span) => *span,
                Self::String(.., span) => *span,
                Self::Char(.., span) => *span,
                Self::Eof(span) => *span,
            }
        }
        pub fn is_keyword_of(&self, expect: &str) -> bool {
            match self {
                Token::KeyWord(word, ..) if expect == word => true,
                _ => false,
            }
        }
        pub fn is_op_of(&self, expect: &str) -> bool {
            match self {
                Token::Op(op, ..) if expect == op => true,
                _ => false,
            }
        }
        is_a!(Token, KeyWord, is_keyword);
        is_a!(Token, Int, is_int);
        is_a!(Token, Id, is_id);
        is_a!(Token, Op, is_op);
        is_a!(Token, Eof, is_eof);
        fn lookup(ident: &str, span: Span) -> Option<Self> {
            match ident {
                "word" | "struct" | "do" | "in" | "if" | "elif" | "else" | "use" | "return"
                | "end" => Some(Self::KeyWord(ident.into(), span)),
                _ => None,
            }
        }
    }

    pub struct Lexer<'a> {
        src: Peekable<Chars<'a>>,
        len: usize,
        span: Span,
        last_chr_len: usize,
    }

    impl<'a> Lexer<'a> {
        pub fn new(src: &'a str) -> Self {
            Self {
                len: src.len(),
                src: src.chars().peekable(),
                span: Span::default(),
                last_chr_len: 0,
            }
        }

        fn tp(&self) -> usize {
            self.span.end
        }

        fn is_end(&self) -> bool {
            self.tp() >= self.len.saturating_sub(1)
        }

        fn peek(&mut self) -> char {
            self.src.peek().cloned().unwrap_or('\0')
        }

        fn next(&mut self) -> char {
            let ch = self.src.next().unwrap_or('\0');
            self.span.right_shift(ch);
            self.last_chr_len = ch.to_string().as_bytes().len();
            ch
        }

        fn next_if<F: FnOnce(char) -> bool>(&mut self, func: F) -> Option<char> {
            let c = self.peek();
            if func(c) {
                assert_eq!(c, self.next());
                return Some(c);
            }
            None
        }

        fn span(&mut self) -> Span {
            let span = self.span;
            self.span.start = self.span.end;
            span
        }

        fn number(&mut self, c: char) -> Token {
            let mut number = c.to_string();
            while let Some(c) = self.next_if(|c| c.is_ascii_digit() || c == '_') {
                number.push(c);
            }
            Token::Int(number, self.span())
        }

        fn ident(&mut self, c: char) -> Token {
            let mut id = c.to_string();
            while let Some(c) = self.next_if(|c| c.is_ascii_alphanumeric() || c == '_') {
                id.push(c);
            }
            let span = self.span();
            Token::lookup(&id, span).map_or_else(|| Token::Id(id, span), |t| t)
        }

        fn string(&mut self) -> Token {
            let mut string = String::new();
            while let Some(c) = self.next_if(|c| c != '"') {
                string.push(c);
            }
            self.next();
            Token::String(string, self.span())
        }

        fn chr(&mut self, ch: char) -> Token {
            self.next();
            Token::Char(ch, self.span())
        }

        fn comment(&mut self) -> Token {
            while let Some(_) = self.next_if(|c| c != '\n') {}
            let ch = self.next();
            self.parse(ch)
        }

        fn arrow(&mut self) -> Token {
            Token::Op("->".into(), self.span())
        }

        fn op_token(&mut self, op: &str) -> Token {
            for _ in 0..op.chars().count().saturating_sub(self.last_chr_len) {
                self.next();
            }
            Token::Op(op.into(), self.span())
        }

        fn parse(&mut self, ch: char) -> Token {
            match ch {
                n @ '0'..='9' => self.number(n),
                i @ ('a'..='z' | 'A'..='Z') => self.ident(i),
                '"' => self.string(),
                '\'' => self.chr(ch),
                '/' if self.peek() == '/' => self.comment(),
                '-' if self.peek() == '>' => self.op_token("->"),
                '>' if self.peek() == '=' => self.op_token(">="),
                '>' if self.peek() == '>' => self.op_token(">>"),
                '<' if self.peek() == '=' => self.op_token("<="),
                '<' if self.peek() == '<' => self.op_token("<<"),
                '=' if self.peek() == '=' => self.op_token("=="),
                '|' if self.peek() == '|' => self.op_token("||"),
                '&' if self.peek() == '&' => self.op_token("&&"),
                '|' => self.op_token("|"),
                '&' => self.op_token("&"),
                '-' => self.op_token("-"),
                '+' => self.op_token("+"),
                '*' => self.op_token("*"),
                '/' => self.op_token("/"),
                '>' => self.op_token(">"),
                '<' => self.op_token("<"),
                '=' => self.op_token("="),
                '!' => self.op_token("!"),
                '%' => self.op_token("%"),
                '.' => self.op_token("."),
                ',' => self.op_token(","),
                '(' => self.op_token("("),
                ')' => self.op_token(")"),
                '{' => self.op_token("{"),
                '}' => self.op_token("}"),
                '[' => self.op_token("{"),
                ']' => self.op_token("}"),
                ':' => self.op_token(":"),
                'λ' => self.op_token("λ"),
                '\n' | ' ' => {
                    let ch = self.next();
                    self.span.start = self.span.end.saturating_sub(self.last_chr_len);
                    self.parse(ch)
                }
                '\0' => Token::Eof(self.span()),
                c => panic!(
                    "unknown char '{}' '{}' l n: {}, ip: {}",
                    c,
                    self.peek(),
                    self.len,
                    self.span.end
                ),
            }
        }

        pub fn lex(mut self) -> Vec<Token> {
            let mut tokens = vec![];
            while !self.is_end() {
                let ch = self.next();
                tokens.push(self.parse(ch));
            }
            tokens
        }
    }
}

mod ast {
    use crate::lexer::Span;
    use std::fmt;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum Op {
        Plus,
        Minus,
        Mult,
        Div,
        Grt,
        Les,
        GrtEq,
        LesEq,
        Eq,
        Neq,
        EqEq,
    }

    impl From<&str> for Op {
        fn from(op: &str) -> Self {
            match op {
                "+" => Self::Plus,
                "-" => Self::Minus,
                "*" => Self::Mult,
                "/" => Self::Div,
                ">" => Self::Grt,
                "<" => Self::Les,
                ">=" => Self::GrtEq,
                "<=" => Self::LesEq,
                "=" => Self::Eq,
                "!=" => Self::Neq,
                "==" => Self::EqEq,
                _ => unreachable!(),
            }
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum Builtin {
        Copy,
        Over,
        Rot,
        Swap,
        Drop,
        Max,
        And,
        Or,
        Not,
        // FIXME: remove these and replace DbgInt with just PrintInt
        DbgInt,
        SysCall1,
        SysCall2,
        SysCall3,
        // ----
    }

    #[derive(Debug, Clone)]
    pub enum AtomKind {
        Id(Name),
        Int(u64),
        String(String),
        Char(char),
        Ptr(usize),
        Builtin(Builtin),
        Null,
    }

    #[derive(Debug, Clone)]
    pub struct Atom {
        pub item: AtomKind,
        pub span: Span,
    }

    #[derive(Debug, Clone)]
    pub struct Binary {
        pub lhs: Expr,
        pub rhs: Expr,
        pub op: Op,
        pub span: Span,
    }

    #[derive(Debug, Clone)]
    pub struct Return {
        pub item: Expr,
        pub span: Span,
    }

    #[derive(Debug, Clone)]
    pub struct Conditional {
        pub condition: Expr,
        pub branch1: Expr,
        pub branch2: Option<Expr>,
        pub span: Span,
    }

    #[derive(Debug, Clone)]
    pub struct While {
        pub condition: Expr,
        pub body: Block,
        pub span: Span,
    }

    #[derive(Debug, Clone)]
    pub enum Expr {
        Atom(Atom),
        Binary(Box<Binary>),
        Return(Box<Return>),
        Conditional(Box<Conditional>),
        While(Box<While>),
        Block(Box<Block>),
    }

    impl Expr {
        pub fn span(&self) -> Span {
            match self {
                Self::Atom(i) => i.span.clone(),
                Self::Binary(i) => i.span.clone(),
                Self::Return(i) => i.span.clone(),
                Self::Conditional(i) => i.span.clone(),
                Self::While(i) => i.span.clone(),
                Self::Block(i) => i.span.clone(),
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub enum TypeKind {
        Int,
        Str,
        Char,
        Ptr,
        Null,
    }

    impl TypeKind {
        pub fn size(&self) -> usize {
            match self {
                Self::Int => 4,
                Self::Str => 4,
                Self::Char => 2,
                Self::Ptr => 4,
                Self::Null => 0,
            }
        }
    }

    impl From<&str> for TypeKind {
        fn from(op: &str) -> Self {
            match op {
                "int" => Self::Int,
                "str" => Self::Str,
                "char" => Self::Char,
                "ptr" => Self::Ptr,
                "null" => Self::Null,
                _ => panic!("unknown type '{}'", op),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Type {
        pub ty: TypeKind,
        pub span: Span,
    }

    impl Type {
        pub fn size(&self) -> usize {
            self.ty.size()
        }
    }

    #[derive(Debug, Clone)]
    pub struct Block {
        pub stmts: Vec<Expr>,
        pub span: Span,
    }

    #[derive(Debug, Clone)]
    pub struct Name {
        pub name: String,
        pub span: Span,
    }

    impl fmt::Display for Name {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.name)
        }
    }

    impl PartialEq for Name {
        fn eq(&self, other: &Self) -> bool {
            self.name == other.name
        }
    }

    #[derive(Debug, Clone)]
    pub struct Param {
        pub name: Name,
        pub ty: Type,
    }

    #[derive(Debug, Clone)]
    pub struct Word {
        pub name: Name,
        pub params: Vec<Param>,
        pub ret_ty: Option<Type>,
        pub body: Block,
        pub span: Span,
    }

    impl Word {
        pub fn has_func_call(&self) -> bool {
            true
            // for expr in self.body.stmts.iter() {
            //     match expr {
            //         Expr::Atom(ref a) => {
            //             if self.params.iter().find(|p| match &a.item {
            //                 AtomKind::Id(name) => name.name == p.name.name,
            //                 _ => false,
            //             }).is_some() {
            //                 return true;
            //             }
            //         }
            //         Expr::Binary(i) => todo!(),
            //         Expr::Return(i) => todo!(),
            //         Expr::Conditional(i) => todo!(),
            //         Expr::While(i) => todo!(),
            //         Expr::Block(i) => todo!(),
            //     }
            // }
            // false
        }
    }

    #[derive(Debug, Clone)]
    pub enum Item {
        // Use(Use),
        // Struct(Struct),
        // Const(Const),
        Word(Word),
    }
}

mod parser {
    use crate::ast::{
        Atom, AtomKind, Binary, Block, Builtin, Conditional, Expr, Item, Name, Op, Param, Return,
        Type, TypeKind, Word,
    };
    use crate::lexer::{Span, Token};
    use std::iter::Peekable;
    use std::slice::Iter;

    pub struct Parser<'a> {
        stream: Peekable<Iter<'a, Token>>,
        stack: Vec<Expr>,
    }

    impl<'a> Parser<'a> {
        pub fn new(stream: Peekable<Iter<'a, Token>>) -> Self {
            Self {
                stream,
                stack: vec![],
            }
        }

        fn is_end(&mut self) -> bool {
            self.stream
                .peek()
                .map(|i| matches!(i, Token::Eof(..)))
                .unwrap_or(true)
        }

        fn peek(&mut self) -> Token {
            self.stream
                .peek()
                .cloned()
                .cloned()
                .unwrap_or(Token::Eof(Span::default()))
        }

        fn next(&mut self) -> Token {
            let Some(token) = self.stream.next() else {
                return Token::Eof(Span::default());
            };
            token.clone()
        }

        fn next_if<F: FnOnce(Token) -> bool>(&mut self, func: F) -> Option<Token> {
            if func(self.peek()) {
                return Some(self.next());
            }
            None
        }

        fn declaration(&mut self) -> Item {
            match self.next() {
                Token::KeyWord(word, span) if word == "word" => self.word(span),
                t => panic!("not allowed in global scope '{:?}'", t),
            }
        }

        fn word(&mut self, span: Span) -> Item {
            let name = self.name();
            let params = self.params();
            let ret_ty = self.return_type();
            let body = self.block();
            let span = Span::new(span.line, span.start, body.span.end);
            Item::Word(Word {
                name,   // : Name,
                params, // : Vec<Param>,
                ret_ty, // : Option<Type>,
                body,   // : Block,
                span,   // : Span,
            })
        }

        fn return_type(&mut self) -> Option<Type> {
            if !self.next().is_op_of("->") {
                return None;
            }
            let Token::Id(name, span) = self.next() else {
                panic!("missing return Type");
            };
            Some(Type {
                ty: TypeKind::from(name.as_str()),
                span,
            })
        }

        fn params(&mut self) -> Vec<Param> {
            let mut params = vec![];
            while !self.peek().is_op_of("->") && !self.peek().is_keyword_of("do") {
                let Token::Id(name, span) = self.next() else {
                    panic!("{:?}: {:?}, expected a ident in word param", self.peek(), self.stack);
                };

                let name = Name { name, span };

                let next = self.next();
                if !next.is_op_of(":") {
                    panic!("expected ':' but found '{:?}'", next);
                }

                let Token::Id(ty, span) = self.next() else {
                    panic!("expected a Type in word param");
                };
                let ty = Type {
                    ty: TypeKind::from(ty.as_str()),
                    span,
                };
                let param = Param { name, ty };
                params.push(param);
            }
            params
        }

        fn block(&mut self) -> Block {
            let do_tok = self.next();
            if !do_tok.is_keyword_of("do") {
                panic!("missing do key word");
            }
            let start_span = do_tok.span();
            while !self.peek().is_keyword_of("end") {
                self.expr();
            }
            let end_span = self.next().span();

            let span = Span::new(start_span.line, start_span.start, end_span.end);
            let stmts = self.stack.clone();
            self.stack.clear();
            Block { stmts, span }
        }

        fn binary(&mut self, op: &str, span: Span) -> Expr {
            let Some(rhs) = self.stack.pop() else {
                panic!("not enough items on stack to do '{}' with", op);
            };
            let Some(lhs) = self.stack.pop() else {
                panic!("not enough items on stack to do '{}' with", op);
            };
            let span = Span::new(span.line, span.start, lhs.span().end);
            let op = Op::from(op);
            Expr::Binary(Box::new(Binary { rhs, lhs, op, span }))
        }

        fn return_expr(&mut self, span: Span) -> Expr {
            let Some(item) = self.stack.pop() else {
                panic!("nothing to return on stack");
            };
            Expr::Return(Box::new(Return { item, span }))
        }

        fn conditional(&mut self, span: Span) -> Expr {
            let Some(condition) = self.stack.pop() else {
                panic!("expected a condition on stack");
            };
            self.expr();
            let Some(branch1) = self.stack.pop() else {
                panic!("expected branch on stack");
            };
            let peek_token = self.peek();
            let branch2 = if peek_token.is_keyword_of("elif") | peek_token.is_keyword_of("else") {
                self.next();
                self.expr();
                let Some(b) = self.stack.pop() else {
                    panic!("expected branch on stack");
                };
                Some(b)
            } else {
                None
            };
            Expr::Conditional(Box::new(Conditional {
                condition,
                branch1,
                branch2,
                span,
            }))
        }

        fn expr(&mut self) {
            let e = match self.next() {
                Token::KeyWord(kw, span) if kw == "return" => self.return_expr(span),
                Token::KeyWord(kw, span) if kw == "if" => self.conditional(span),
                Token::Id(name, span) if name == "dbg_int" => Expr::Atom(Atom {
                    item: AtomKind::Builtin(Builtin::DbgInt),
                    span,
                }),
                Token::Id(name, span) if name == "drop" => Expr::Atom(Atom {
                    item: AtomKind::Builtin(Builtin::Drop),
                    span,
                }),
                Token::Id(name, span) if name == "copy" => Expr::Atom(Atom {
                    item: AtomKind::Builtin(Builtin::Copy),
                    span,
                }),
                Token::Id(name, span) if name == "over" => Expr::Atom(Atom {
                    item: AtomKind::Builtin(Builtin::Over),
                    span,
                }),
                Token::Id(name, span) if name == "rot" => Expr::Atom(Atom {
                    item: AtomKind::Builtin(Builtin::Rot),
                    span,
                }),
                Token::Id(name, span) if name == "swap" => Expr::Atom(Atom {
                    item: AtomKind::Builtin(Builtin::Swap),
                    span,
                }),
                Token::Id(name, span) if name == "max" => Expr::Atom(Atom {
                    item: AtomKind::Builtin(Builtin::Max),
                    span,
                }),
                Token::Id(name, span) => Expr::Atom(Atom {
                    item: AtomKind::Id(Name {
                        name: name.into(),
                        span,
                    }),
                    span,
                }),
                Token::Int(int, span) => Expr::Atom(Atom {
                    item: AtomKind::Int(int.parse().unwrap()),
                    span,
                }),
                Token::String(string, span) => Expr::Atom(Atom {
                    item: AtomKind::String(string.into()),
                    span,
                }),
                Token::Char(ch, span) => Expr::Atom(Atom {
                    item: AtomKind::Char(ch),
                    span,
                }),
                Token::Op(ref op, span) => self.binary(op, span),

                t => unimplemented!("{t:?}"),
                // Expr::While(Box<While>),
                // Expr::Block(Box<Block>),
            };
            self.stack.push(e);
        }

        fn name(&mut self) -> Name {
            let Token::Id(name, span) = self.next() else {
                panic!("failed to get name syntax error");
            };
            Name { name, span }
        }

        pub fn parse(mut self) -> Vec<Item> {
            let mut items = vec![];
            while !self.is_end() {
                items.push(self.declaration());
            }
            items
        }
    }
}

mod code_gen {
    use super::ast::*;
    use std::fs::OpenOptions;

    pub trait IrGenerator {
        fn into_ir(self, ctx: Option<Context>) -> String;
    }

    #[derive(Debug, Clone)]
    enum Reg {
        Rdi,
        Rsi,
        Rdx,
        Rcx,
        R8,
        R9,
        Xmm0,
        Xmm1,
    }

    impl From<usize> for Reg {
        fn from(value: usize) -> Self {
            vec![
                Self::Rdi,
                Self::Rsi,
                Self::Rdx,
                Self::Rcx,
                Self::R8,
                Self::R9,
                Self::Xmm0,
                Self::Xmm1,
            ][value]
                .clone()
        }
    }

    #[derive(Debug, Clone)]
    pub struct Context {
        regs: [bool; 8],
        local_var: Vec<Param>,
        ast: Vec<Item>,
    }

    impl Context {
        pub fn with_ast(mut self, ast: Vec<Item>) -> Self {
            self.ast = ast;
            self
        }

        fn params_of(&self, name: &Name) -> Option<Vec<Param>> {
            self.ast
                .iter()
                .find(|item| matches!(item, Item::Word(word) if &word.name == name))
                .map(|i| match i {
                    Item::Word(word) => word.params.clone(),
                    _ => unreachable!(),
                })
        }

        fn return_value_of(&self, name: &Name) -> Option<Type> {
            self.ast
                .iter()
                .find(|item| matches!(item, Item::Word(word) if &word.name == name))
                .map(|i| match i {
                    Item::Word(word) => word.ret_ty.clone(),
                    _ => unreachable!(),
                })
                .flatten()
        }

        fn get_reg(&mut self) -> Reg {
            Reg::from(self.regs.iter().enumerate().find(|(_, i)| !**i).unwrap().0)
        }

        fn add_params(&mut self, params: Vec<Param>) {
            self.local_var = params
        }

        fn is_param(&self, item: &str) -> bool {
            self.local_var
                .iter()
                .find(|i| i.name.name == item)
                .map(|_| true)
                .unwrap_or(false)
        }

        fn idx_of_param(&self, item: &str) -> usize {
            self.local_var
                .iter()
                .enumerate()
                .find(|(idx, i)| i.name.name == item)
                .unwrap()
                .0
        }
    }

    impl Default for Context {
        fn default() -> Self {
            Self {
                regs: [false; 8],
                local_var: vec![],
                ast: vec![],
            }
        }
    }

    impl IrGenerator for Reg {
        fn into_ir(self, _: Option<Context>) -> String {
            format!("{:?}", self).to_lowercase()
        }
    }

    impl IrGenerator for Op {
        fn into_ir(self, _: Option<Context>) -> String {
            match self {
                Self::Plus => "add".to_string(),
                Self::Minus => "sub".to_string(),
                Self::Mult => "imul".to_string(),
                Self::Div => "idiv".to_string(),
                Self::Grt => "setg".into(),
                Self::Les => "setl".into(),
                Self::GrtEq => "setge".into(),
                Self::LesEq => "setle".into(),
                Self::Eq => unimplemented!(),
                Self::Neq => "setne".into(),
                Self::EqEq => "sete".into(),
            }
        }
    }

    impl IrGenerator for TypeKind {
        fn into_ir(self, _: Option<Context>) -> String {
            match self {
                Self::Int => "i32".to_string(),
                Self::Str => unimplemented!(),
                Self::Char => unimplemented!(),
                Self::Ptr => unimplemented!(),
                Self::Null => "void".to_string(),
            }
        }
    }

    impl IrGenerator for Conditional {
        fn into_ir(self, _: Option<Context>) -> String {
        // condition: Expr,
        // branch1: Expr,
        // branch2: Option<Expr>,
        // span: Span,
        format!("
; conditional
{}
; branch 1
{}
; branch 2
{}
", self.condition.into_ir(None),
   self.branch1.into_ir(None),
   self.branch2.map(|i|i.into_ir(None)).unwrap_or("; no else".into()))
        }
    }

    impl IrGenerator for Type {
        fn into_ir(self, _: Option<Context>) -> String {
            self.ty.into_ir(None)
        }
    }

    impl IrGenerator for Builtin {
        fn into_ir(self, _: Option<Context>) -> String {
            match self {
                Self::Copy => {
                    format!(
                        "
  pop     rdi
  push    rdi
  push    rdi
"
                    )
                }
                Self::Over => {
                    format!(
                        "
  pop     rdi
  pop     rsi
  push    rsi
  push    rdi
  push    rsi
"
                    )
                }
                Self::Rot => {
                    format!(
                        "
  ; a b c
  pop     rdi ; c
  pop     rsi ; b
  pop     rdx ; a

  ; b c a
  push    rsi ; b
  push    rdi ; c
  push    rdx ; a
"
                    )
                }
                Self::Swap => {
                    format!(
                        "
  pop     rdi
  pop     rsi
  push    rdi
  push    rsi
"
                    )
                }
                Self::Drop => {
                    format!("  pop    rdi\n")
                }
                Self::Max => {
                    format!(
                        "
  pop     rsi
  pop     rdi
  cmp     rsi, rdi
  cmovg  rdi, rsi
  push    rdi
"
                    )
                }
                Self::And => unimplemented!(),
                Self::Or => unimplemented!(),
                Self::Not => unimplemented!(),
                Self::DbgInt => unimplemented!(),
                Self::SysCall1 => unimplemented!(),
                Self::SysCall2 => unimplemented!(),
                Self::SysCall3 => unimplemented!(),
            }
        }
    }

    impl IrGenerator for AtomKind {
        fn into_ir(self, ctx: Option<Context>) -> String {
            let Some(ctx) = ctx else {
                return "ATOMKIND NO CTX".into();
            };
            match self {
                Self::Id(i) => {
                    if ctx.is_param(&i.name) {
                        let idx = ctx.idx_of_param(&i.name);
                        let offset = 8 * (idx + 1);
                        let reg = Reg::from(idx).into_ir(None);
                        format!("  push qword [rbp-{offset}]\n")
                    } else {
                        let returning = match ctx.return_value_of(&i) {
                            None => "",
                            _ => "  push rax\n",
                        };
                        let setup_args = ctx
                            .params_of(&i)
                            .map(|i| {
                                i.iter()
                                    .enumerate()
                                    .rev()
                                    .map(|(idx, _)| {
                                        format!("  pop {}\n", Reg::from(idx).into_ir(None))
                                    })
                                    .collect::<String>()
                            })
                            .unwrap_or("".into());
                        format!(
                            "
{setup_args}
  call __{i}__
  {returning}"
                        )
                    }
                }
                Self::Int(i) => format!("push {i}\n"),
                Self::String(i) => i.into(),
                Self::Char(i) => i.into(),
                Self::Ptr(i) => i.to_string(),
                Self::Builtin(i) => i.into_ir(None),
                Self::Null => "null".into(),
            }
        }
    }

    impl IrGenerator for Atom {
        fn into_ir(self, ctx: Option<Context>) -> String {
            self.item.into_ir(ctx)
        }
    }

    impl IrGenerator for Expr {
        fn into_ir(self, ctx: Option<Context>) -> String {
            match self {
                Self::Atom(atom) => atom.into_ir(ctx),
                Self::Binary(binary) => binary.into_ir(ctx),
                Self::Return(ret) => ret.into_ir(ctx),
                Self::Conditional(conditional) => "".to_string(),
                Self::While(while_loop) => "".to_string(),
                Self::Block(block) => "".to_string(),
            }
        }
    }

    impl IrGenerator for Return {
        fn into_ir(self, ctx: Option<Context>) -> String {
            let ty = TypeKind::Int.into_ir(ctx.clone());
            let expr = self.item.into_ir(ctx);
            format!(
                "{expr}
  pop rax
  jmp .RETURN
"
            )
        }
    }

    impl IrGenerator for Binary {
        fn into_ir(self, ctx: Option<Context>) -> String {
            match self.op {
                Op::Plus | Op::Minus | Op::Mult | Op::Div => {
                    format!(
                        "{}
{}
  pop rbx
  pop rax
  {} rax, rbx
  push rax
",
                        self.lhs.into_ir(ctx.clone()),
                        self.rhs.into_ir(ctx),
                        self.op.into_ir(None)
                    )
                }
                _ => {
                    format!(
                        "
{}
{}
  pop   rsi
  pop   rdi
  cmp   rdi, rsi
  {}  al
  movzx rax, al
  push rax
",
                        self.lhs.into_ir(ctx.clone()),
                        self.rhs.into_ir(ctx),
                        self.op.into_ir(None)
                    )
                }
            }
        }
    }

    impl IrGenerator for Block {
        fn into_ir(self, ctx: Option<Context>) -> String {
            self.stmts
                .into_iter()
                .map(|i| i.into_ir(ctx.clone()))
                .collect()
        }
    }

    impl IrGenerator for Word {
        fn into_ir(self, mut ctx: Option<Context>) -> String {
            let has_func_call = self.has_func_call();
            let Self { name, params, .. } = self;
            let sub = if has_func_call {
                let offset = params.len() * 8;
                format!("sub rsp, {offset}")
            } else {
                "".into()
            };
            // let ret_type = self
            //     .ret_ty
            //     .map(IrGenerator::into_ir)
            //     .unwrap_or("void".into());
            let setup_params = params
                .iter()
                .enumerate()
                .map(|(idx, _)| {
                    let offset = 8 * (idx + 1);
                    let reg = Reg::from(idx).into_ir(None);
                    format!("  mov qword [rbp-{offset}], {reg}\n")
                })
                .collect::<String>();
            ctx.as_mut().map(|mut i| {
                i.add_params(params.clone());
                i
            });
            let body = self.body.into_ir(ctx);
            format!(
                "__{name}__:
  push rbp
  mov  rbp, rsp
  ;; {sub} 
{setup_params}
{body}
.RETURN:
  leave
  ret
"
            )
        }
    }

    impl IrGenerator for Item {
        fn into_ir(self, ctx: Option<Context>) -> String {
            match self {
                Self::Word(word) => word.into_ir(ctx),
            }
        }
    }
}

fn compile(filename: String, print_tokens: bool, print_ast: bool) {
    use code_gen::IrGenerator;
    let src = std::fs::read_to_string(filename).expect("failed to load file");
    let tokens = crate::lexer::Lexer::new(&src).lex();
    if print_tokens {
        dbg!(&tokens);
    }

    let ast = crate::parser::Parser::new(tokens.iter().peekable()).parse();
    if print_ast {
        dbg!(&ast);
    }
    let mut ctx = code_gen::Context::default().with_ast(ast.clone());
    let ir = ast
        .into_iter()
        .map(|i| format!("{}\n", i.into_ir(Some(ctx.clone()))))
        .collect::<String>();
    let filename = "output.asm";
    let mut file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(filename)
        .unwrap();
    file.write_all(boiler_plate_fasm(ir).as_bytes()).unwrap();
    compile_with_fasm();
}

fn boiler_plate_fasm(code: impl Into<String>) -> String {
    format!(
        "
format ELF64 executable 3
segment readable executable

entry _start

{}

_start:
  push rbp
  mov  rbp, rsp
  call __main__
  mov  rdi, rax
  mov rax, 60
  syscall

segment readable writable",
        code.into()
    )
}

fn compile_with_fasm() {
    Command::new("fasm")
        .arg("output.asm")
        .output()
        .expect("failed to compile");
}

fn main() {
    let print_tokens = args()
        .nth(2)
        .map(|i| i == String::from("-dt"))
        .unwrap_or(false);

    let print_ast = args()
        .nth(2)
        .map(|i| i == String::from("-da"))
        .unwrap_or(false);
    args().nth(1).map_or_else(
        || println!("expected a file"),
        |filename| {
            compile(filename, print_tokens, print_ast);
        },
    );
}

// TODO: fix the calling and setup function params
//      missing call function
//      missing passing args
//      missing handling of return value
