use super::{calculate_jump, Mod, Ops};
use std::collections::HashMap;

use super::Reg;
#[derive(Debug, Clone)]
pub enum Monic {
    Push(Value),
    Pop(Value),
    Move(Value, Value),
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value),
    Div(Value),
    Call(Value),
    Jump(String),
    JumpEq(String),
    JumpNotEq(String),
    Cmp(Value, Value),
    Interrupt(Value),
    Return,
}

impl Monic {
    pub fn size(&self) -> u32 {
        let get_size = |l, r| match (l, r) {
            (&Value::Reg(reg), &Value::Var(_)) => reg.size(),
            (_, _) => r.size(),
        };
        match self {
            Self::Push(Value::Reg(_)) => 1,
            Self::Push(v) => 1 + v.size(),
            Self::Pop(Value::Reg(_)) => 1,
            Self::Pop(v) => v.size(),
            Self::Move(l, r) => l.size() + get_size(l, r),
            Self::Add(l, r) => l.size() + r.size(),
            Self::Sub(l, r) => l.size() + r.size(),
            Self::Mul(v) => 1 + v.size(),
            Self::Div(v) => 1 + v.size(),
            Self::Call(v) => 1 + v.size(),
            Self::Jump(_) => 2,
            // FIXME jumps are not sized right
            Self::JumpEq(_) => 2,
            Self::JumpNotEq(_) => 2,
            Self::Cmp(l, r) => l.size() + r.size(),

            Self::Interrupt(v) => v.size() + 1,
            Self::Return => 1,
        }
    }

    pub fn code(
        &self,
        current: u32,
        labels: &HashMap<String, u32>,
        data_loc: &HashMap<String, u32>,
    ) -> Vec<u8> {
        match self {
            Self::Push(v) => self.push(v, &data_loc),
            Self::Pop(v) => self.pop(v),
            Self::Move(l, r) => self.mov(l, r, &data_loc),
            Self::Add(l, r) => self.add(l, r),
            Self::Sub(l, r) => self.sub(l, r),
            Self::Mul(v) => self.mul(v),
            Self::Div(v) => self.div(v),
            Self::Call(v) => self.call(v, current, &labels),
            Self::Jump(name) => self.jump(*labels.get(name).unwrap(), current),
            Self::JumpEq(name) => self.jump(*labels.get(name).unwrap(), current),
            Self::JumpNotEq(name) => self.jump(*labels.get(name).unwrap(), current),
            Self::Cmp(l, r) => self.cmp(l, r),
            Self::Interrupt(v) => self.int(v),
            Self::Return => self.ret(),
        }
    }
    fn push(&self, value: &Value, data_loc: &HashMap<String, u32>) -> Vec<u8> {
        match value {
            Value::Var(var) => {
                let mut bytes = vec![Ops::PushImm32.value()];
                if let Some(size) = data_loc.get(&var.to_string()) {
                    bytes.extend_from_slice(&size.to_le_bytes());
                } else {
                    panic!("'{}' is not a variable name", var);
                }
                bytes
            }
            Value::U32(imm) => {
                let mut bytes = vec![Ops::PushImm32.value()];
                bytes.extend_from_slice(&imm.to_le_bytes());
                bytes
            }
            Value::Reg(reg) => vec![Ops::PushReg.value() + reg.value()],
            _ => unreachable!("\nPUSH instruction '{:?}' is not implemented yet\n", value),
        }
    }
    fn pop(&self, value: &Value) -> Vec<u8> {
        match value {
            Value::U32(imm) => {
                let mut bytes = vec![Ops::PushImm32.value()];
                bytes.extend_from_slice(&imm.to_le_bytes());
                bytes
            }
            Value::Reg(reg) => vec![Ops::PopReg.value() + reg.value()],
            _ => unreachable!("\nPOP instruction '{:?}' is not implemented yet\n", value),
        }
    }
    fn mov(&self, lhs: &Value, rhs: &Value, data_loc: &HashMap<String, u32>) -> Vec<u8> {
        match (lhs, rhs) {
            (Value::Reg(reg), Value::Var(var)) => {
                let mut bytes = vec![Ops::MoveRegImm32.value() + reg.value()];
                if let Some(size) = data_loc.get(&var.to_string()) {
                    bytes.extend_from_slice(&size.to_le_bytes());
                } else {
                    panic!("'{}' is not a variable name", var);
                }
                bytes
            }
            (Value::Reg(reg), Value::U8(imm)) => {
                assert_eq!(reg.size(), 1);
                let mut bytes = vec![Ops::MoveRegImm8.value() + reg.value()];
                bytes.push(*imm);
                bytes
            }
            (Value::Reg(reg), Value::U16(imm)) => {
                assert_eq!(reg.size(), 2);
                let mut bytes = vec![Ops::MoveRegImm16.value() + reg.value()];
                bytes.extend_from_slice(&imm.to_le_bytes());
                bytes
            }
            (Value::Reg(reg), Value::U32(imm)) => {
                assert_eq!(reg.size(), 4);
                let mut bytes = vec![Ops::MoveRegImm32.value() + reg.value()];
                bytes.extend_from_slice(&imm.to_le_bytes());
                bytes
            }
            (Value::Reg(reg1), Value::Reg(reg2)) => {
                let left = Mod::Reg as u8;
                let middle = reg1.value();
                let right = reg2.value();
                assert_eq!(reg1.size(), reg2.size());
                let op = match reg1.size() {
                    1 => Ops::MoveRegReg8,
                    2 => Ops::MoveRegReg16,
                    4 => Ops::MoveRegReg32,
                    _ => unreachable!(),
                };
                vec![op.value(), (left << 6) | (middle << 3) | right]
            }
            (Value::Reg(reg1), Value::Mem(reg2)) => {
                // NOTE: Mod would change depending on what mem is.
                let left = Mod::Mem as u8;
                let middle = reg1.value();
                let right = reg2.value();
                let op = match reg1.size() {
                    1 => Ops::MoveRegMem8,
                    2 => Ops::MoveRegMem16,
                    4 => Ops::MoveRegMem32,
                    _ => unreachable!(),
                };
                vec![op.value(), (left << 6) | (middle << 3) | right]
            }
            (Value::Mem(reg1), Value::Reg(reg2)) => {
                // NOTE: Mod would change depending on what mem is.
                let left = Mod::Mem as u8;
                let middle = reg2.value();
                let right = reg1.value();
                let op = match reg2.size() {
                    1 => Ops::MoveMemReg8,
                    2 => Ops::MoveMemReg16,
                    4 => Ops::MoveMemReg32,
                    _ => unreachable!(),
                };
                vec![op.value(), (left << 6) | (middle << 3) | right]
            }
            _ => unreachable!(
                "\nMOV instruction between '{:?}' and '{:?}' is not implemented yet\n",
                rhs, lhs
            ),
        }
    }
    fn add(&self, lhs: &Value, rhs: &Value) -> Vec<u8> {
        match (lhs, rhs) {
            (Value::Reg(reg), Value::U32(imm)) => {
                let left = Mod::Reg as u8;
                let middle = 0b_0000_0000;
                let right = reg.value();
                let modrm = (left << 6) | (middle << 3) | right;
                let mut bytes = vec![Ops::AddRegImm32.value(), modrm];
                bytes.extend_from_slice(&imm.to_le_bytes());
                bytes
            }
            (Value::Reg(lhs), Value::Reg(rhs)) => {
                let left = Mod::Reg as u8;
                let modrm = (left << 6) | (lhs.value() << 3) | rhs.value();
                vec![Ops::AddRegReg32.value(), modrm]
            }
            _ => unreachable!("\nADD is not implemented yet '{:?}' '{:?}'\n", rhs, lhs),
        }
    }
    fn sub(&self, lhs: &Value, rhs: &Value) -> Vec<u8> {
        match (lhs, rhs) {
            (Value::Reg(reg), Value::U32(imm)) => {
                let left = Mod::Reg as u8;
                let middle = 0b_0000_0000;
                let right = reg.value();
                let modrm = (left << 6) | (middle << 3) | right;
                let mut bytes = vec![Ops::SubRegImm32.value(), modrm];
                bytes.extend_from_slice(&imm.to_le_bytes());
                bytes
            }
            (Value::Reg(lhs), Value::Reg(rhs)) => {
                let left = Mod::Reg as u8;
                let modrm = (left << 6) | (lhs.value() << 3) | rhs.value();
                vec![Ops::SubRegReg32.value(), modrm]
            }
            _ => unreachable!("\nSub is not implemented yet '{:?}' '{:?}'\n", rhs, lhs),
        }
    }
    fn mul(&self, value: &Value) -> Vec<u8> {
        match value {
            // (Value::Reg(reg), Value::U32(imm)) => {
            //     let left = Mod::Reg as u8;
            //     let middle = 0b_0000_0000;
            //     let right = reg.value();
            //     let modrm = (left << 6) | (middle << 3) | right;
            //     let mut bytes = vec![Ops::SubRegImm32.value(), modrm];
            //     bytes.extend_from_slice(&imm.to_le_bytes());
            //     bytes
            // }
            Value::Reg(reg) => {
                let left = Mod::Reg as u8;
                // let middle = 0b_0000_0000;
                let modrm = (left << 6) | (Reg::ESP.value() << 3) | reg.value();
                vec![Ops::MulRegReg32.value(), modrm]
            }
            _ => unreachable!("\nMul is not implemented yet '{:?}'\n", value),
        }
    }
    fn div(&self, value: &Value) -> Vec<u8> {
        match value {
            // (Value::Reg(reg), Value::U32(imm)) => {
            //     let left = Mod::Reg as u8;
            //     let middle = 0b_0000_0000;
            //     let right = reg.value();
            //     let modrm = (left << 6) | (middle << 3) | right;
            //     let mut bytes = vec![Ops::SubRegImm32.value(), modrm];
            //     bytes.extend_from_slice(&imm.to_le_bytes());
            //     bytes
            // }
            Value::Reg(reg) => {
                let left = Mod::Reg as u8;
                let modrm = (left << 6) | (Reg::ESI.value() << 3) | reg.value();
                vec![Ops::DivRegReg32.value(), modrm]
            }
            _ => unreachable!("\nMul is not implemented yet '{:?}'\n", value),
        }
    }
    fn call(&self, value: &Value, current: u32, labels: &HashMap<String, u32>) -> Vec<u8> {
        match value {
            Value::Var(label) => {
                if let Some(to) = labels.get(label) {
                    let mut bytes = vec![Ops::CallRel32.value()];
                    bytes.extend_from_slice(&(to - current).to_le_bytes());
                    bytes
                } else {
                    panic!("'{}' is not a known label", label);
                }
            }
            e => todo!("'{}': is not implemented yet", e),
        }
    }
    fn jump(&self, jump_to: u32, current: u32) -> Vec<u8> {
        vec![Ops::Jump8.value(), calculate_jump(jump_to, current) as u8]
    }

    fn cmp(&self, lhs: &Value, _rhs: &Value) -> Vec<u8> {
        let mut bytes = vec![Ops::Int.value()];
        bytes.extend_from_slice(&lhs.to_le_bytes());
        bytes
    }

    fn int(&self, value: &Value) -> Vec<u8> {
        let mut bytes = vec![Ops::Int.value()];
        bytes.extend_from_slice(&value.to_le_bytes());
        bytes
    }
    fn ret(&self) -> Vec<u8> {
        vec![Ops::Ret.value()]
    }
}
impl std::fmt::Display for Monic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let instruction = match self {
            Self::Push(v) => format!("push {}", v),
            Self::Pop(v) => format!("pop {}", v),
            Self::Move(l, r) => format!("mov {}, {}", l, r),
            Self::Add(l, r) => format!("add {}, {}", l, r),
            Self::Sub(l, r) => format!("sub {}, {}", l, r),
            Self::Mul(v) => format!("mul {}", v),
            Self::Div(v) => format!("div {}", v),
            Self::Call(l) => format!("call {}", l),
            Self::Jump(s) => format!("jmp {}", s),
            Self::JumpEq(s) => format!("je {}", s),
            Self::JumpNotEq(s) => format!("jne {}", s),
            Self::Cmp(l, r) => format!("cmp {}, {}", l, r),
            Self::Interrupt(v) => format!("int {}", v),
            Self::Return => "ret".into(),
        };
        write!(f, "{}", instruction)
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    Reg(Reg),
    Var(String),
    Mem(Reg),
}

impl Value {
    pub fn to_le_bytes(&self) -> Vec<u8> {
        match self {
            Self::U8(i) => vec![*i],
            Self::U16(i) => i.to_le_bytes().to_vec(),
            Self::U32(i) => i.to_le_bytes().to_vec(),
            Self::U64(i) => i.to_le_bytes().to_vec(),
            Self::Reg(_) => vec![],
            // FIXME: Var for now is 4 bytes
            Self::Var(_) => vec![],
            Self::Mem(_) => unimplemented!("Value size is not implemented yet"),
        }
    }
    pub fn size(&self) -> u32 {
        match self {
            Self::U8(_) => 1,
            Self::U16(_) => 2,
            Self::U32(_) => 4,
            Self::U64(_) => 8,
            Self::Reg(_) => 1,
            // FIXME: Var for now is 4 bytes
            Self::Var(_) => 4,
            Self::Mem(_) => 1,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U8(v) => write!(f, "{}", v),
            Self::U16(v) => write!(f, "{}", v),
            Self::U32(v) => write!(f, "{}", v),
            Self::U64(v) => write!(f, "{}", v),
            Self::Reg(v) => write!(f, "{}", v),
            Self::Var(v) => write!(f, "{}", v),
            Self::Mem(v) => write!(f, "[{}]", v),
        }
    }
}
