#![allow(dead_code)]
// NOTE:
//     For more info look into
//     V2 sec 2.1.5 Table 2-2
#[derive(Debug, Clone, Copy)]
pub enum Mod {
    Mem = 0x00,
    Reg = 0x03,
}

#[derive(Debug, Clone, Copy)]
pub enum Ops {
    Jump8,
    PushImm32,
    // PushImm8 = 0x6A,
    PushReg,
    PopReg,
    AddRegImm32,
    AddRegReg32,
    SubRegImm32,
    SubRegReg32,
    MulRegReg32,
    DivRegReg32,
    // AddReg = 0x03,

    // mov AL, imm8 none B0 2 2 1 1
    MoveRegImm8,
    // mov AX, imm16 none B8 3 2 1 1
    //     EAX, imm32 5
    MoveRegImm16,
    MoveRegImm32,
    // mov mem8, imm8 none C6 3+ 2 1 1
    MovMemImm8,
    // mov mem16,imm16 none C7 4+ 2 1 1
    //     mem32,imm32 6+
    MoveMemImm32,
    // mov reg8,reg8 none 8A 2 2 1 1
    MoveRegReg8,
    // mov reg16,reg16 none 8B 2 2 1 1
    //     reg32,reg32
    MoveRegReg16,
    MoveRegReg32,
    // mov reg8,mem8 none 8A 2+ 4 1 1
    MoveRegMem8,
    // mov reg16,mem16 none 8B 2+ 4 1 1
    //     reg32,mem32
    MoveRegMem16,
    MoveRegMem32,
    // mov mem8,reg8 none 88 2+ 2 1 1
    MoveMemReg8,
    // mov mem16,reg16 none 89 2+ 2 1 1
    //     mem32,reg32
    MoveMemReg16,
    MoveMemReg32,
    Int,
    Ret,
    CallRel32,
}

impl Ops {
    pub fn value(&self) -> u8 {
        match self {
            Self::Jump8 => 0xEB,
            Self::PushImm32 => 0x68,
            // PushImm8 = 0x6A,
            Self::PushReg => 0x50,
            Self::PopReg => 0x58,
            Self::AddRegImm32 => 0x81,
            Self::AddRegReg32 => 0x03,
            Self::SubRegImm32 => 0x81,
            Self::SubRegReg32 => 0x2B,
            Self::MulRegReg32 => 0xF7,
            Self::DivRegReg32 => 0xF7,
            Self::MoveRegImm8 => 0xB0,
            Self::MoveRegImm16 => 0xB8,
            Self::MoveRegImm32 => 0xB8,
            Self::MovMemImm8 => 0xC6,
            Self::MoveMemImm32 => 0xC7,
            Self::MoveRegReg8 => 0x8A,
            Self::MoveRegReg16 => 0x8B,
            Self::MoveRegReg32 => 0x8B,
            Self::MoveRegMem8 => 0x8A,
            Self::MoveRegMem16 => 0x8B,
            Self::MoveRegMem32 => 0x8B,
            Self::MoveMemReg8 => 0x88,
            Self::MoveMemReg16 => 0x89,
            Self::MoveMemReg32 => 0x89,
            Self::Int => 0xCD,
            Self::Ret => 0xC3,
            Self::CallRel32 => 0xE8,
        }
    }
}

/*
direct is what?
sreg   is what?
mov AL, direct none A0 5 4 1 1
mov AX, direct none A1 5 4 1 1
EAX, direct

mov direct ,AL none A2 5 2 1 1
mov direct, AX none A3 5 2 1 1
direct, EAX

mov sreg, reg16 none 8E 2 2 3 1
mov reg16, sreg none 8C 2 2 3 1
mov sreg,mem16 none 8E 2+ 2 3[*] 2[*]
mov mem16,sreg none 8C 2+ 2 3 1
*/
