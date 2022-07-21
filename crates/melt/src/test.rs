use super::*;
use pretty_assertions::assert_eq;

#[test]
fn test_macro_value() {
    assert_eq!(val!(), ());
}

// #[test]
// fn exit() {
//     let program = Program::new()
//         // Exit
//         .with_op(Monic::MoveImm(Reg::EAX, 60))
//         .with_op(Monic::MoveImm(Reg::EBX, 0))
//         .with_op(Monic::Interrupt(0x80))
//         .build();
//     eprintln!("{:?}", program);
//     assert_eq!(
//         program,
//         vec![
//             127, 69, 76, 70, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 3, 0, 1, 0, 0, 0, 84, 128,
//             4, 8, 52, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 52, 0, 32, 0, 1, 0, 40, 0, 0, 0, 0, 0, 1, 0,
//             0, 0, 84, 0, 0, 0, 84, 128, 4, 8, 0, 0, 0, 0, 47, 0, 0, 0, 47, 0, 0, 0, 5, 0, 0, 0, 0,
//             16, 0, 0, 184, 4, 0, 0, 0, 187, 1, 0, 0, 0, 185, 118, 128, 4, 8, 186, 13, 0, 0, 0, 205,
//             128, 184, 1, 0, 0, 0, 187, 0, 0, 0, 0, 205, 128, 72, 101, 108, 108, 111, 32, 87, 111,
//             114, 108, 100, 33, 10
//         ]
//     );
// }
//
// #[test]
// fn hello_world() {
//     let program = Program::new()
//         // Write SysCall
//         .with_op(Monic::MoveImm(Reg::EAX, 4))
//         // Stdout
//         .with_op(Monic::MoveImm(Reg::EBX, 1))
//         // Buffer Location
//         .with_op(Monic::MoveVar(Reg::ECX, "string_name".into()))
//         // Size of Buffer
//         .with_op(Monic::MoveImm(Reg::EDX, 13))
//         .with_op(Monic::Interrupt(0x80))
//         // Exit
//         .with_op(Monic::MoveImm(Reg::EAX, 1))
//         .with_op(Monic::MoveImm(Reg::EBX, 0))
//         .with_op(Monic::Interrupt(0x80))
//         .with_data("string_name", "Hello World!\n")
//         .with_data("string_name_2", "lsdkjflksdfjsd?\n")
//         .build();
//     eprintln!("{:?}", program);
//     assert_eq!(
//         program,
//         vec![
//             127, 69, 76, 70, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 3, 0, 1, 0, 0, 0, 84, 128,
//             4, 8, 52, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 52, 0, 32, 0, 1, 0, 40, 0, 0, 0, 0, 0, 1, 0,
//             0, 0, 84, 0, 0, 0, 84, 128, 4, 8, 0, 0, 0, 0, 63, 0, 0, 0, 63, 0, 0, 0, 5, 0, 0, 0, 0,
//             16, 0, 0, 184, 4, 0, 0, 0, 187, 1, 0, 0, 0, 185, 118, 128, 4, 8, 186, 13, 0, 0, 0, 205,
//             128, 184, 1, 0, 0, 0, 187, 0, 0, 0, 0, 205, 128, 72, 101, 108, 108, 111, 32, 87, 111,
//             114, 108, 100, 33, 10, 108, 115, 100, 107, 106, 102, 108, 107, 115, 100, 102, 1, 06,
//             115, 100, 63, 10
//         ]
//     );
