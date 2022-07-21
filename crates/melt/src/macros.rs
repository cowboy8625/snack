// #[macro_export]
// macro_rules! ops {
//     ( $id:ident, $args:ident ) => {
//         use melt::scanner::lookup_reg;
//         use melt::Monic;
//         use Monic::*;
//         match name {
//             "push" => Push,
//             "pop" => Pop,
//             "mov" => Move,
//             "add" => Add,
//             "jmp" => Jump,
//             "syscall" => Interrupt,
//         }
//     }; // println!("{:?}", lookup_reg(stringify!($id)));
// }
//
// #[macro_export]
// macro_rules! reg {
//     ( $id:ident ) => {
//         use melt::scanner::lookup_reg;
//         if let Some(reg) = lookup_reg(stringify!($id)) {
//             return Ok(reg);
//         }
//         // return Err(format!("{} is not a register", stringify!($id)));
//     };
// }

#[macro_export]
macro_rules! val {
    ($val:expr) => {
        eprintln!("{}", $val);
        crate::monics::Value::from(stringify!($val))
    };
}
