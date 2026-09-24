//! Assembly intermediate-representation types needed by the backend.
#![allow(non_snake_case)]

use super::Initializers::StaticInit;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AsmReg {
    AX,
    BX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    SP,
    BP,
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM8,
    XMM9,
    XMM10,
    XMM11,
    XMM12,
    XMM13,
    XMM14,
    XMM15,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AsmIndexedOperand {
    pub base: AsmReg,
    pub index: AsmReg,
    pub scale: i32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AsmOperand {
    Imm(i64),
    Reg(AsmReg),
    Pseudo(String),
    Memory(AsmReg, i32),
    Data(String, i32),
    PseudoMem(String, i32),
    Indexed(AsmIndexedOperand),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AsmUnaryOperator {
    Neg,
    Not,
    Shr,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AsmBinaryOperator {
    Add,
    Sub,
    Mult,
    DivDouble,
    And,
    Or,
    Xor,
    Shl,
    ShrBinop,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AsmCondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
    A,
    AE,
    B,
    BE,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AsmByteArrayInfo {
    pub size: i32,
    pub alignment: i32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AsmType {
    Byte,
    Longword,
    Quadword,
    Double,
    ByteArray(AsmByteArrayInfo),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AsmMovsxInfo {
    pub src_type: AsmType,
    pub dst_type: AsmType,
    pub src: AsmOperand,
    pub dst: AsmOperand,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AsmMovZeroExtendInfo {
    pub src_type: AsmType,
    pub dst_type: AsmType,
    pub src: AsmOperand,
    pub dst: AsmOperand,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AsmBinaryInfo {
    pub op: AsmBinaryOperator,
    pub t: AsmType,
    pub src: AsmOperand,
    pub dst: AsmOperand,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AsmInstruction {
    Mov(AsmType, AsmOperand, AsmOperand),
    Movsx(AsmMovsxInfo),
    MovZeroExtend(AsmMovZeroExtendInfo),
    Lea(AsmOperand, AsmOperand),
    Cvttsd2si(AsmType, AsmOperand, AsmOperand),
    Cvtsi2sd(AsmType, AsmOperand, AsmOperand),
    Unary(AsmUnaryOperator, AsmType, AsmOperand),
    Binary(AsmBinaryInfo),
    Cmp(AsmType, AsmOperand, AsmOperand),
    Idiv(AsmType, AsmOperand),
    Div(AsmType, AsmOperand),
    Cdq(AsmType),
    Jmp(String),
    JmpCC(AsmCondCode, String),
    SetCC(AsmCondCode, AsmOperand),
    Label(String),
    Push(AsmOperand),
    Pop(AsmReg),
    Call(String),
    Ret,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AsmFunctionDef {
    pub name: String,
    pub global: bool,
    pub instructions: Vec<AsmInstruction>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AsmStaticVariableDef {
    pub name: String,
    pub alignment: i32,
    pub global: bool,
    pub init: Vec<StaticInit>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AsmStaticConstantDef {
    pub name: String,
    pub alignment: i32,
    pub init: StaticInit,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AsmTopLevel {
    Function(AsmFunctionDef),
    StaticVariable(AsmStaticVariableDef),
    StaticConstant(AsmStaticConstantDef),
}

#[derive(Clone, Debug, PartialEq)]
pub enum AsmProgram {
    Program(Vec<AsmTopLevel>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn represents_every_top_level_definition() {
        let program = AsmProgram::Program(vec![
            AsmTopLevel::Function(AsmFunctionDef {
                name: "main".into(),
                global: true,
                instructions: vec![AsmInstruction::Ret],
            }),
            AsmTopLevel::StaticVariable(AsmStaticVariableDef {
                name: "counter".into(),
                alignment: 4,
                global: false,
                init: vec![StaticInit::IntInit(0)],
            }),
            AsmTopLevel::StaticConstant(AsmStaticConstantDef {
                name: "message".into(),
                alignment: 1,
                init: StaticInit::StringInit("hello".into(), true),
            }),
        ]);

        let AsmProgram::Program(definitions) = program;
        assert_eq!(definitions.len(), 3);
    }
}
