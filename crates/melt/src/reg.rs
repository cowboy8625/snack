/*
Intel x86 64
RAX     EAX     AX      AH      AL      Accumulator
RBX     EBX     BX      BH      BL      Base
RCX     ECX     CX      CH      CL      Counter
RDX     EDX     DX      DH      DL      Data (commonly extends the A register)
RSI     ESI     SI      N/A     SIL     Source index for string operations
RDI     EDI     DI      N/A     DIL     Destination index for string operations
RSP     ESP     SP      N/A     SPL     Stack Pointer
RBP     EBP     BP      N/A     BPL     Base Pointer (meant for stack frames)
R8      R8D     R8W     N/A     R8B     General purpose
R9      R9D     R9W     N/A     R9B     General purpose
R10     R10D    R10W    N/A     R10B    General purpose
R11     R11D    R11W    N/A     R11B    General purpose
R12     R12D    R12W    N/A     R12B    General purpose
R13     R13D    R13W    N/A     R13B    General purpose
R14     R14D    R14W    N/A     R14B    General purpose
R15     R15D    R15W    N/A     R15B    General purpose
*/

#![allow(dead_code)]

use variant_count::VariantCount;

#[derive(Debug, Clone, PartialEq, Eq, Copy, VariantCount)]
pub enum Reg {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    RSP,
    RBP,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    // 32
    EAX,
    ECX,
    EDX,
    EBX,
    ESP,
    EDP,
    ESI,
    EDI,
    R8D,
    R9D,
    R10D,
    R11D,
    R12D,
    R13D,
    R14D,
    R15D,
    // 16
    AX,
    BX,
    CX,
    DX,
    SI,
    DI,
    SP,
    BP,
    R8W,
    R9W,
    R10W,
    R11W,
    R12W,
    R13W,
    R14W,
    R15W,
    // 8
    AL,
    BL,
    CL,
    DL,
    SIL,
    DIL,
    SPL,
    BPL,
    R8B,
    R9B,
    R10B,
    R11B,
    R12B,
    R13B,
    R14B,
    R15B,
}

impl Reg {
    pub fn value(&self) -> u8 {
        match self {
            // 64       32          16         8
            Self::RAX | Self::EAX | Self::AX | Self::AL => 0,
            Self::RBX | Self::ECX | Self::BX | Self::BL => 1,
            Self::RCX | Self::EDX | Self::CX | Self::CL => 2,
            Self::RDX | Self::EBX | Self::DX | Self::DL => 3,
            Self::RSI | Self::ESP | Self::SI | Self::SIL => 4,
            Self::RDI | Self::EDP | Self::DI | Self::DIL => 5,
            Self::RSP | Self::ESI | Self::SP | Self::SPL => 6,
            Self::RBP | Self::EDI | Self::BP | Self::BPL => 7,
            Self::R8 | Self::R8D | Self::R8W | Self::R8B => 8,
            Self::R9 | Self::R9D | Self::R9W | Self::R9B => 9,
            Self::R10 | Self::R10D | Self::R10W | Self::R10B => 10,
            Self::R11 | Self::R11D | Self::R11W | Self::R11B => 11,
            Self::R12 | Self::R12D | Self::R12W | Self::R12B => 12,
            Self::R13 | Self::R13D | Self::R13W | Self::R13B => 13,
            Self::R14 | Self::R14D | Self::R14W | Self::R14B => 14,
            Self::R15 | Self::R15D | Self::R15W | Self::R15B => 15,
        }
    }
    pub fn size(&self) -> u32 {
        match self {
            // 64
            Self::RAX
            | Self::RBX
            | Self::RCX
            | Self::RDX
            | Self::RSI
            | Self::RDI
            | Self::RSP
            | Self::RBP
            | Self::R8
            | Self::R9
            | Self::R10
            | Self::R11
            | Self::R12
            | Self::R13
            | Self::R14
            | Self::R15 => 8,
            // 32
            Self::EAX
            | Self::ECX
            | Self::EDX
            | Self::EBX
            | Self::ESP
            | Self::EDP
            | Self::ESI
            | Self::EDI
            | Self::R8D
            | Self::R9D
            | Self::R10D
            | Self::R11D
            | Self::R12D
            | Self::R13D
            | Self::R14D
            | Self::R15D => 4,
            // 16
            Self::AX
            | Self::BX
            | Self::CX
            | Self::DX
            | Self::SI
            | Self::DI
            | Self::SP
            | Self::BP
            | Self::R8W
            | Self::R9W
            | Self::R10W
            | Self::R11W
            | Self::R12W
            | Self::R13W
            | Self::R14W
            | Self::R15W => 2,
            // 8
            Self::AL
            | Self::BL
            | Self::CL
            | Self::DL
            | Self::SIL
            | Self::DIL
            | Self::SPL
            | Self::BPL
            | Self::R8B
            | Self::R9B
            | Self::R10B
            | Self::R11B
            | Self::R12B
            | Self::R13B
            | Self::R14B
            | Self::R15B => 1,
        }
    }
    pub fn lookup(name: &str) -> Option<Reg> {
        use Reg::*;
        match name {
            "rax" => Some(RAX),
            "rbx" => Some(RBX),
            "rcx" => Some(RCX),
            "rdx" => Some(RDX),
            "rsi" => Some(RSI),
            "rdi" => Some(RDI),
            "rsp" => Some(RSP),
            "rbp" => Some(RBP),
            "r8" => Some(R8),
            "r9" => Some(R9),
            "r10" => Some(R10),
            "r11" => Some(R11),
            "r12" => Some(R12),
            "r13" => Some(R13),
            "r14" => Some(R14),
            "r15" => Some(R15),
            // 32
            "eax" => Some(EAX),
            "ecx" => Some(ECX),
            "edx" => Some(EDX),
            "ebx" => Some(EBX),
            "esp" => Some(ESP),
            "edp" => Some(EDP),
            "esi" => Some(ESI),
            "edi" => Some(EDI),
            "r8d" => Some(R8D),
            "r9d" => Some(R9D),
            "r10d" => Some(R10D),
            "r11d" => Some(R11D),
            "r12d" => Some(R12D),
            "r13d" => Some(R13D),
            "r14d" => Some(R14D),
            "r15d" => Some(R15D),
            // 16
            "ax" => Some(AX),
            "bx" => Some(BX),
            "cx" => Some(CX),
            "dx" => Some(DX),
            "si" => Some(SI),
            "di" => Some(DI),
            "sp" => Some(SP),
            "bp" => Some(BP),
            "r8w" => Some(R8W),
            "r9w" => Some(R9W),
            "r10w" => Some(R10W),
            "r11w" => Some(R11W),
            "r12w" => Some(R12W),
            "r13w" => Some(R13W),
            "r14w" => Some(R14W),
            "r15w" => Some(R15W),
            // 8
            "al" => Some(AL),
            "bl" => Some(BL),
            "cl" => Some(CL),
            "dl" => Some(DL),
            "sil" => Some(SIL),
            "dil" => Some(DIL),
            "spl" => Some(SPL),
            "bpl" => Some(BPL),
            "r8b" => Some(R8B),
            "r9b" => Some(R9B),
            "r10b" => Some(R10B),
            "r11b" => Some(R11B),
            "r12b" => Some(R12B),
            "r13b" => Some(R13B),
            "r14b" => Some(R14B),
            "r15b" => Some(R15B),
            _ => None,
        }
    }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
