mod arguments;
use anyhow::Result;
use arguments::cargs; //{cargs, Assember, Settings, Target};
                      // use melt::{Monic, Program, Reg, Value as AValue};
                      // use parser::{Builtin, Expr, Parser, Value as PValue};
use parser::{Expr, Parser};
use scanner::Scanner;
// use std::fs::{File, OpenOptions};
// use std::io::Write;
use std::iter::Peekable;
use typechecker::TypedExpr;

// struct CustomCompiler<'a> {
//     ast: &'a [Expr],
//     idx: usize,
// }
//
// impl<'a> CustomCompiler<'a> {
//     pub fn new(ast: &'a [Expr]) -> Self {
//         Self { ast, idx: 0 }
//     }
//
//     fn add(&self, program: &mut Program) {
//         program.op(Monic::Pop(AValue::Reg(Reg::EAX)));
//         program.op(Monic::Pop(AValue::Reg(Reg::EBX)));
//         program.op(Monic::Add(AValue::Reg(Reg::EAX), AValue::Reg(Reg::EBX)));
//         program.op(Monic::Push(AValue::Reg(Reg::EAX)));
//     }
//
//     fn sub(&self, program: &mut Program) {
//         program.op(Monic::Pop(AValue::Reg(Reg::EAX)));
//         program.op(Monic::Pop(AValue::Reg(Reg::EBX)));
//         program.op(Monic::Sub(AValue::Reg(Reg::EBX), AValue::Reg(Reg::EAX)));
//         program.op(Monic::Push(AValue::Reg(Reg::EBX)));
//     }
//
//     fn mul(&self, program: &mut Program) {
//         program.op(Monic::Pop(AValue::Reg(Reg::EAX)));
//         program.op(Monic::Pop(AValue::Reg(Reg::EDX)));
//         program.op(Monic::Mul(AValue::Reg(Reg::EDX)));
//         program.op(Monic::Push(AValue::Reg(Reg::EAX)));
//     }
//
//     fn div(&self, program: &mut Program) {
//         program.op(Monic::Pop(AValue::Reg(Reg::EBX)));
//         program.op(Monic::Pop(AValue::Reg(Reg::EAX)));
//         program.op(Monic::Div(AValue::Reg(Reg::EBX)));
//         program.op(Monic::Push(AValue::Reg(Reg::EAX)));
//     }
//
//     fn exit(&self, program: &mut Program) {
//         program.op(Monic::Move(AValue::Reg(Reg::EAX), AValue::U32(1)));
//         program.op(Monic::Pop(AValue::Reg(Reg::EBX)));
//         // program.op(Monic::Move(AValue::Reg(Reg::EBX), AValue::U32(10)));
//         program.op(Monic::Interrupt(AValue::U8(0x80)));
//     }
//
//     fn syscall1(&self, program: &mut Program) {
//         program.op(Monic::Pop(AValue::Reg(Reg::EAX)));
//         program.op(Monic::Pop(AValue::Reg(Reg::EBX)));
//         program.op(Monic::Interrupt(AValue::U8(0x80)));
//     }
//
//     fn syscall2(&self, program: &mut Program) {
//         program.op(Monic::Pop(AValue::Reg(Reg::EAX)));
//         program.op(Monic::Pop(AValue::Reg(Reg::EBX)));
//         program.op(Monic::Pop(AValue::Reg(Reg::ECX)));
//         program.op(Monic::Interrupt(AValue::U8(0x80)));
//     }
//
//     fn syscall3(&self, program: &mut Program) {
//         program.op(Monic::Pop(AValue::Reg(Reg::EAX)));
//         program.op(Monic::Pop(AValue::Reg(Reg::EBX)));
//         program.op(Monic::Pop(AValue::Reg(Reg::ECX)));
//         program.op(Monic::Pop(AValue::Reg(Reg::EDX)));
//         program.op(Monic::Interrupt(AValue::U8(0x80)));
//     }
//
//     fn parse(&mut self, program: &mut Program) {
//         while self.idx < self.ast.len() {
//             match &self.ast[self.idx] {
//                 Expr::Value(PValue::U64(i), _) => {
//                     program.op(Monic::Push(AValue::U32(*i as u32)));
//                     self.idx += 1;
//                 }
//                 Expr::Value(PValue::String(string, len), _) => {
//                     program.data("one", string);
//                     program.op(Monic::Push(AValue::U32(*len as u32)));
//                     program.op(Monic::Push(AValue::Var("one".into())));
//                     self.idx += 1;
//                 }
//                 Expr::Builtin(Builtin::Add, _) => {
//                     self.add(program);
//                     self.idx += 1;
//                 }
//                 Expr::Builtin(Builtin::Sub, _) => {
//                     self.sub(program);
//                     self.idx += 1;
//                 }
//                 Expr::Builtin(Builtin::Mul, _) => {
//                     self.mul(program);
//                     self.idx += 1;
//                 }
//                 Expr::Builtin(Builtin::Div, _) => {
//                     self.div(program);
//                     self.idx += 1;
//                 }
//                 Expr::Builtin(Builtin::SysCall1, _) => {
//                     self.syscall1(program);
//                     self.idx += 1;
//                 }
//                 Expr::Builtin(Builtin::SysCall2, _) => {
//                     self.syscall2(program);
//                     self.idx += 1;
//                 }
//                 Expr::Builtin(Builtin::SysCall3, _) => {
//                     self.syscall3(program);
//                     self.idx += 1;
//                 }
//                 _ => unreachable!(),
//             }
//         }
//         self.exit(program);
//     }
//
//     pub fn compile(&mut self) -> Vec<u8> {
//         let mut program = Program::new().with_label("main");
//         self.parse(&mut program);
//         program.build()
//     }
// }
//
// struct Compiler {
//     out: File,
//     filename: String,
//     ast: Vec<Expr>,
// }
//
// impl Compiler {
//     fn new(filename: &str, ast: Vec<Expr>) -> Self {
//         let out = match OpenOptions::new()
//             .create(true)
//             .truncate(true)
//             .write(true)
//             .open(filename)
//         {
//             Ok(f) => f,
//             Err(err) => {
//                 eprintln!("{}", err);
//                 std::process::exit(1);
//             }
//         };
//         Self {
//             out,
//             filename: filename.into(),
//             ast,
//         }
//     }
//
//     fn premissions(&self) -> Result<()> {
//         std::process::Command::new("chmod")
//             .args(&["+x", &self.filename])
//             .spawn()?;
//         Ok(())
//     }
//
//     fn fasm(&mut self) -> Result<()> {
//         Ok(())
//     }
//
//     fn custom(&mut self) -> Result<()> {
//         self.out
//             .write_all(&CustomCompiler::new(&self.ast).compile())?;
//         self.premissions()?;
//         Ok(())
//     }
// }

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

fn _remove_file_extension(filename: &str) -> String {
    filename.split('.').collect::<Vec<&str>>()[0].to_string()
}

fn timer<T, F>(msg: &str, func: F) -> T
where
    F: FnOnce() -> T,
{
    let start = std::time::Instant::now();
    let out = func();
    let now = std::time::Instant::now();
    let time = (now - start).as_secs_f64();
    let w = msg.len() + (13 - msg.len());
    let msg = format!("{:>w$}", msg);
    eprintln!("{msg} {time}s");
    out
}

fn get_file<'a>(filename: &'a str) -> String {
    match snack_source_file(filename) {
        Ok(f) => f,
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
}
fn get_scanner<'a>(src: &'a str) -> Peekable<Scanner<'a>> {
    timer("Scanner", || Scanner::new(src).peekable())
}

fn get_ast<'a>(scanner: Peekable<Scanner<'a>>) -> Vec<Expr> {
    let ast: Vec<Expr> = timer("Parser", || match Parser::new(scanner).parse() {
        Ok(a) => a,
        Err(err) => {
            eprintln!("{:?}", err);
            std::process::exit(1);
        }
    });
    if cfg!(feature = "ast") {
        dbg!(&ast);
    }
    ast
}

fn get_type_checked_ast(ast: &[Expr]) -> Vec<TypedExpr> {
    timer("Type Checking", || typechecker::type_check_ast(ast))
}

fn main() {
    let setting = cargs();
    let src = get_file(setting.filename.as_str());
    println!("{}", src);
    let scanner = get_scanner(src.as_str());
    let ast = get_ast(scanner);
    println!("{:#?}", ast);
    let _type_checked_ast = get_type_checked_ast(&ast);
    println!("{:#?}", _type_checked_ast);
    // let no_extension_filename = remove_file_extension(&setting.filename);
    // let _: Result<()> = timer("Compiling", || {
    //     let mut compiler = Compiler::new(&no_extension_filename, ast);
    //     match (setting.target, setting.assembler) {
    //         (Target::X86_64Lunux, Assember::Custom) => compiler.custom()?,
    //         (Target::X86_64Lunux, Assember::Fasm) => compiler.fasm()?,
    //         // (t, a) => unimplemented!("for assember: '{:?}' on target: '{:?}'", a, t),
    //     }
    //     Ok(())
    // });
}
