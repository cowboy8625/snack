#![allow(dead_code)]

use anyhow::Result;
use melt::{
    // ops, reg,
    scanner::{MonicKind, Scanner, Span, Token, TokenKind},
    Monic,
    Program,
    Reg,
    Value,
};
use std::fs::OpenOptions;
use std::io::Write;

struct Parser<'a> {
    scan: Scanner<'a>,
    program: Program,
}

impl<'a> Parser<'a> {
    fn new(scan: Scanner<'a>) -> Self {
        Self {
            scan,
            program: Program::new(),
        }
    }

    fn parse(&mut self) -> Result<()> {
        while let Ok(Token { kind, span }) = self.scan.next() {
            match kind {
                TokenKind::Id(i)
                    if self.scan.peek().map(|i| i.kind()).unwrap() == TokenKind::Ctrl(':') =>
                {
                    let _ = self.scan.next();
                    self.label(&i, &span)?
                }
                TokenKind::Monic(i) => self.nmemonic(&i, &span)?,
                TokenKind::Eof => break,
                t => unreachable!("{}: unexpected '{}' instruction", span, t),
            }
        }
        Ok(())
    }

    fn label(&mut self, name: &str, _span: &Span) -> Result<()> {
        self.program.label(name);
        Ok(())
    }

    fn nmemonic(&mut self, monic: &MonicKind, span: &Span) -> Result<()> {
        match monic {
            MonicKind::Mov | MonicKind::Compare => self.double(monic, span)?,
            MonicKind::SysCall => self.program.op(Monic::Interrupt(Value::U8(0x80))),
            MonicKind::Return => self.program.op(Monic::Return),
            MonicKind::DB => self.data(span)?,
            _ => self.single(monic, span)?,
        }
        Ok(())
    }

    fn data(&mut self, _span: &Span) -> Result<()> {
        let name = self.scan.next()?;
        match name.kind() {
            TokenKind::Id(name) => {
                let data = self.scan.next()?;
                match data.kind() {
                    TokenKind::String(data) => {
                        self.program.data(&name, &data);
                    }
                    _ => unreachable!("TODO make an error for wrong type"),
                }
            }
            _ => unreachable!("TODO make an error for wrong type"),
        }
        Ok(())
    }

    fn double(&mut self, monic: &MonicKind, _span: &Span) -> Result<()> {
        let arg1 = self.scan.next()?;
        let arg2 = self.scan.next()?;
        match (monic, arg1.kind().clone(), arg2.kind().clone()) {
            (MonicKind::Mov, TokenKind::Reg(from), TokenKind::Reg(to)) => {
                self.program
                    .op(Monic::Move(Value::Reg(to), Value::Reg(from)));
            }
            (MonicKind::Mov, TokenKind::Int(from), TokenKind::Reg(to)) => {
                self.program
                    .op(Monic::Move(Value::Reg(to), Value::U32(from.parse()?)));
            }
            (MonicKind::Mov, TokenKind::Id(from), TokenKind::Reg(to)) => {
                self.program
                    .op(Monic::Move(Value::Reg(to), Value::Var(from)));
            }
            (MonicKind::Compare, TokenKind::Reg(lhs), TokenKind::Reg(rhs)) => {
                self.program
                    .op(Monic::Cmp(Value::Reg(rhs), Value::Reg(lhs)));
            }
            t => unreachable!("should not reach this point '{:?}'", t),
        }
        Ok(())
    }

    fn single(&mut self, monic: &MonicKind, _span: &Span) -> Result<()> {
        let arg = self.scan.next()?;
        match (monic, arg.kind()) {
            (MonicKind::Push, TokenKind::Reg(reg)) => {
                self.program.op(Monic::Push(Value::Reg(reg)));
            }
            (MonicKind::Push, TokenKind::Int(num)) => {
                self.program.op(Monic::Push(Value::U32(num.parse()?)));
            }
            (MonicKind::Pop, TokenKind::Reg(reg)) => {
                self.program.op(Monic::Pop(Value::Reg(reg)));
            }
            (MonicKind::Jump, TokenKind::Id(to)) => {
                self.program.op(Monic::Jump(to));
            }
            (MonicKind::JumpEq, TokenKind::Id(to)) => {
                // FIXME: Jump Eq
                self.program.op(Monic::Jump(to));
            }
            (MonicKind::JumpNotEq, TokenKind::Id(to)) => {
                // FIXME: Jump Not Eq
                self.program.op(Monic::Jump(to));
            }
            (MonicKind::Call, TokenKind::Id(to)) => {
                self.program.op(Monic::Call(Value::Var(to)));
            }
            t => unreachable!("should not reach this point in single '{:?}'", t),
        }
        Ok(())
    }

    fn build(self, filename: &str) -> Result<()> {
        let filename = filename.split('.').collect::<Vec<&str>>()[0];
        let mut file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(true)
            .open(filename)
            .unwrap();
        file.write_all(&self.program.build()).unwrap();
        std::process::Command::new("chmod")
            .args(&["+x", filename])
            .spawn()?;
        Ok(())
    }
}

fn main() -> Result<()> {
    if let Some(filename) = std::env::args().nth(1) {
        let src = std::fs::read_to_string(&filename).unwrap();
        let mut parser = Parser::new(Scanner::new(&filename, &src));
        parser.parse()?;
        parser.build(filename.as_str())?;
    } else {
        let filename = "forever";
        let mut file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(true)
            .open(filename)
            .unwrap();
        file.write_all(
            &Program::new()
                .with_label("main")
                .with_op(Monic::Move(Value::Reg(Reg::EAX), Value::U32(4)))
                .with_op(Monic::Move(Value::Reg(Reg::EBX), Value::U32(1)))
                .with_op(Monic::Move(
                    Value::Reg(Reg::ECX),
                    Value::Var("hello".into()),
                ))
                .with_op(Monic::Move(Value::Reg(Reg::EDX), Value::U32(6)))
                .with_op(Monic::Interrupt(Value::U8(0x80)))
                // Change Value in memory
                .with_op(Monic::Move(Value::Reg(Reg::BPL), Value::U8(97)))
                .with_op(Monic::Move(Value::Mem(Reg::ECX), Value::Reg(Reg::BPL)))
                // Second Print
                .with_op(Monic::Move(Value::Reg(Reg::EAX), Value::U32(4)))
                .with_op(Monic::Move(Value::Reg(Reg::EBX), Value::U32(1)))
                .with_op(Monic::Move(
                    Value::Reg(Reg::ECX),
                    Value::Var("hello".into()),
                ))
                .with_op(Monic::Move(Value::Reg(Reg::EDX), Value::U32(6)))
                .with_op(Monic::Interrupt(Value::U8(0x80)))
                // Exit
                .with_op(Monic::Move(Value::Reg(Reg::EAX), Value::U32(1)))
                .with_op(Monic::Move(Value::Reg(Reg::EBX), Value::U32(0)))
                .with_op(Monic::Interrupt(Value::U8(0x80)))
                .with_data("hello", "Hello\n")
                .build(),
        )
        .unwrap();
        std::process::Command::new("chmod")
            .args(&["+x", filename])
            .spawn()?;
    }
    Ok(())
}
