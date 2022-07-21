mod header;
mod macros;
mod monics;
mod op_codes;
mod program;
mod reg;
pub mod scanner;
#[cfg(test)]
mod test;
pub use header::Header;
pub use monics::{Monic, Value};
pub use op_codes::{Mod, Ops};
pub use program::Program;
pub use reg::Reg;

fn calculate_jump(to: u32, from: u32) -> u32 {
    1 + 0xFF - (from - to)
}
