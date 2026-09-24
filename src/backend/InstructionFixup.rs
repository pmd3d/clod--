//! Rewrite assembly IR instructions that violate x86-64 operand constraints.
#![allow(non_snake_case)]
use super::Assembly::*;
use super::{AssemblySymbols, Rounding};

pub fn isLarge(i: i64) -> bool {
    i > i32::MAX as i64 || i < i32::MIN as i64
}
pub fn isLargerThanUint(i: i64) -> bool {
    i > u32::MAX as i64 || i < i32::MIN as i64
}
pub fn isLargerThanByte(i: i64) -> bool {
    !(-128..256).contains(&i)
}
pub fn isConstant(o: &AsmOperand) -> bool {
    matches!(o, AsmOperand::Imm(_))
}
pub fn isMemory(o: &AsmOperand) -> bool {
    matches!(
        o,
        AsmOperand::Memory(..) | AsmOperand::Data(..) | AsmOperand::Indexed(_)
    )
}
pub fn isXmm(r: AsmReg) -> bool {
    matches!(
        r,
        AsmReg::XMM0
            | AsmReg::XMM1
            | AsmReg::XMM2
            | AsmReg::XMM3
            | AsmReg::XMM4
            | AsmReg::XMM5
            | AsmReg::XMM6
            | AsmReg::XMM7
            | AsmReg::XMM8
            | AsmReg::XMM9
            | AsmReg::XMM10
            | AsmReg::XMM11
            | AsmReg::XMM12
            | AsmReg::XMM13
            | AsmReg::XMM14
            | AsmReg::XMM15
    )
}
fn mem(o: &AsmOperand) -> bool {
    matches!(o, AsmOperand::Memory(..) | AsmOperand::Data(..))
}
fn reg(r: AsmReg) -> AsmOperand {
    AsmOperand::Reg(r)
}

pub fn fixupInstruction(callee: &[AsmReg], i: AsmInstruction) -> Vec<AsmInstruction> {
    use AsmBinaryOperator::*;
    use AsmInstruction::*;
    use AsmOperand::*;
    use AsmReg::*;
    use AsmType::*;
    match i {
        Mov(t, src, dst) if mem(&src) && mem(&dst) => {
            let s = if t == Double { reg(XMM14) } else { reg(R10) };
            vec![Mov(t.clone(), src, s.clone()), Mov(t, s, dst)]
        }
        Mov(Quadword, src @ Imm(n), dst) if isLarge(n) && mem(&dst) => {
            vec![Mov(Quadword, src, reg(R10)), Mov(Quadword, reg(R10), dst)]
        }
        Mov(Longword, Imm(n), dst) if isLargerThanUint(n) => {
            vec![Mov(Longword, Imm(n & 0xffff_ffff), dst)]
        }
        Mov(Byte, Imm(n), dst) if isLargerThanByte(n) => {
            vec![Mov(Byte, Imm((n as i8) as i64), dst)]
        }
        Movsx(x) if matches!(x.src, Imm(_)) && mem(&x.dst) => vec![
            Mov(x.src_type.clone(), x.src, reg(R10)),
            Movsx(AsmMovsxInfo {
                src_type: x.src_type,
                dst_type: x.dst_type.clone(),
                src: reg(R10),
                dst: reg(R11),
            }),
            Mov(x.dst_type, reg(R11), x.dst),
        ],
        Movsx(x) if matches!(x.src, Imm(_)) => vec![
            Mov(x.src_type.clone(), x.src, reg(R10)),
            Movsx(AsmMovsxInfo {
                src_type: x.src_type,
                dst_type: x.dst_type,
                src: reg(R10),
                dst: x.dst,
            }),
        ],
        Movsx(x) if mem(&x.dst) => vec![
            Movsx(AsmMovsxInfo {
                src_type: x.src_type,
                dst_type: x.dst_type.clone(),
                src: x.src,
                dst: reg(R11),
            }),
            Mov(x.dst_type, reg(R11), x.dst),
        ],
        MovZeroExtend(x) if x.src_type == Byte && matches!(x.src, Imm(_)) && isMemory(&x.dst) => {
            vec![
                Mov(Byte, x.src, reg(R10)),
                MovZeroExtend(AsmMovZeroExtendInfo {
                    src_type: Byte,
                    dst_type: x.dst_type.clone(),
                    src: reg(R10),
                    dst: reg(R11),
                }),
                Mov(x.dst_type, reg(R11), x.dst),
            ]
        }
        MovZeroExtend(x) if x.src_type == Byte && matches!(x.src, Imm(_)) => vec![
            Mov(Byte, x.src, reg(R10)),
            MovZeroExtend(AsmMovZeroExtendInfo {
                src_type: Byte,
                dst_type: x.dst_type,
                src: reg(R10),
                dst: x.dst,
            }),
        ],
        MovZeroExtend(x) if x.src_type == Byte && isMemory(&x.dst) => vec![
            MovZeroExtend(AsmMovZeroExtendInfo {
                src_type: Byte,
                dst_type: x.dst_type.clone(),
                src: x.src,
                dst: reg(R11),
            }),
            Mov(x.dst_type, reg(R11), x.dst),
        ],
        MovZeroExtend(x) if x.src_type == Longword && isMemory(&x.dst) => vec![
            Mov(Longword, x.src, reg(R11)),
            Mov(x.dst_type, reg(R11), x.dst),
        ],
        MovZeroExtend(x) if x.src_type == Longword => vec![Mov(Longword, x.src, x.dst)],
        Idiv(t, Imm(n)) => vec![Mov(t.clone(), Imm(n), reg(R10)), Idiv(t, reg(R10))],
        Div(t, Imm(n)) => vec![Mov(t.clone(), Imm(n), reg(R10)), Div(t, reg(R10))],
        Lea(src, dst) if isMemory(&dst) => vec![Lea(src, reg(R11)), Mov(Quadword, reg(R11), dst)],
        Binary(b) if b.t == Double && !matches!(b.dst, Reg(_)) => vec![
            Mov(Double, b.dst.clone(), reg(XMM15)),
            Binary(AsmBinaryInfo {
                op: b.op,
                t: Double,
                src: b.src,
                dst: reg(XMM15),
            }),
            Mov(Double, reg(XMM15), b.dst),
        ],
        Binary(b)
            if matches!(b.op, Add | Sub | And | Or)
                && b.t == Quadword
                && matches!(b.src,Imm(n) if isLarge(n)) =>
        {
            vec![
                Mov(Quadword, b.src, reg(R10)),
                Binary(AsmBinaryInfo { src: reg(R10), ..b }),
            ]
        }
        Binary(b) if matches!(b.op, Add | Sub | And | Or) && mem(&b.src) && mem(&b.dst) => vec![
            Mov(b.t.clone(), b.src, reg(R10)),
            Binary(AsmBinaryInfo { src: reg(R10), ..b }),
        ],
        Binary(b)
            if b.op == Mult
                && b.t == Quadword
                && matches!(b.src,Imm(n) if isLarge(n))
                && mem(&b.dst) =>
        {
            vec![
                Mov(Quadword, b.src, reg(R10)),
                Mov(Quadword, b.dst.clone(), reg(R11)),
                Binary(AsmBinaryInfo {
                    op: Mult,
                    t: Quadword,
                    src: reg(R10),
                    dst: reg(R11),
                }),
                Mov(Quadword, reg(R11), b.dst),
            ]
        }
        Binary(b) if b.op == Mult && b.t == Quadword && matches!(b.src,Imm(n) if isLarge(n)) => {
            vec![
                Mov(Quadword, b.src, reg(R10)),
                Binary(AsmBinaryInfo { src: reg(R10), ..b }),
            ]
        }
        Binary(b) if b.op == Mult && mem(&b.dst) => vec![
            Mov(b.t.clone(), b.dst.clone(), reg(R11)),
            Binary(AsmBinaryInfo {
                dst: reg(R11),
                ..b.clone()
            }),
            Mov(b.t, reg(R11), b.dst),
        ],
        Cmp(Double, src, dst) if !matches!(dst, Reg(_)) => vec![
            Mov(Double, dst.clone(), reg(XMM15)),
            Cmp(Double, src, reg(XMM15)),
        ],
        Cmp(t, src, dst) if mem(&src) && mem(&dst) => {
            vec![Mov(t.clone(), src, reg(R10)), Cmp(t, reg(R10), dst)]
        }
        Cmp(Quadword, src @ Imm(n), dst @ Imm(_)) if isLarge(n) => vec![
            Mov(Quadword, src, reg(R10)),
            Mov(Quadword, dst, reg(R11)),
            Cmp(Quadword, reg(R10), reg(R11)),
        ],
        Cmp(Quadword, src @ Imm(n), dst) if isLarge(n) => {
            vec![Mov(Quadword, src, reg(R10)), Cmp(Quadword, reg(R10), dst)]
        }
        Cmp(t, src, Imm(n)) => vec![Mov(t.clone(), Imm(n), reg(R11)), Cmp(t, src, reg(R11))],
        Push(Reg(r)) if isXmm(r) => vec![
            Binary(AsmBinaryInfo {
                op: Sub,
                t: Quadword,
                src: Imm(8),
                dst: reg(SP),
            }),
            Mov(Double, reg(r), Memory(SP, 0)),
        ],
        Push(src @ Imm(n)) if isLarge(n) => vec![Mov(Quadword, src, reg(R10)), Push(reg(R10))],
        Cvttsd2si(t, src, dst) if mem(&dst) => {
            vec![Cvttsd2si(t.clone(), src, reg(R11)), Mov(t, reg(R11), dst)]
        }
        Cvtsi2sd(t, src, dst) if isConstant(&src) && isMemory(&dst) => vec![
            Mov(t.clone(), src, reg(R10)),
            Cvtsi2sd(t, reg(R10), reg(XMM15)),
            Mov(Double, reg(XMM15), dst),
        ],
        Cvtsi2sd(t, src, dst) if isConstant(&src) => {
            vec![Mov(t.clone(), src, reg(R10)), Cvtsi2sd(t, reg(R10), dst)]
        }
        Cvtsi2sd(t, src, dst) if isMemory(&dst) => {
            vec![Cvtsi2sd(t, src, reg(XMM15)), Mov(Double, reg(XMM15), dst)]
        }
        Ret => callee
            .iter()
            .rev()
            .copied()
            .map(Pop)
            .chain(std::iter::once(Ret))
            .collect(),
        other => vec![other],
    }
}

pub fn emitStackAdjustment(bytes: i32, callee_count: usize) -> AsmInstruction {
    let saved = 8 * callee_count as i32;
    let total = saved + bytes;
    let adjusted = Rounding::round_away_from_zero(16, total.into()) as i32;
    AsmInstruction::Binary(AsmBinaryInfo {
        op: AsmBinaryOperator::Sub,
        t: AsmType::Quadword,
        src: AsmOperand::Imm((adjusted - saved).into()),
        dst: AsmOperand::Reg(AsmReg::SP),
    })
}
pub fn fixupTl(top: AsmTopLevel) -> AsmTopLevel {
    match top {
        AsmTopLevel::Function(mut f) => {
            let bytes = -AssemblySymbols::getBytesRequired(&f.name);
            let regs: Vec<_> = AssemblySymbols::getCalleeSavedRegsUsed(&f.name)
                .into_iter()
                .collect();
            let mut out = vec![emitStackAdjustment(bytes, regs.len())];
            out.extend(
                regs.iter()
                    .copied()
                    .map(|r| AsmInstruction::Push(AsmOperand::Reg(r))),
            );
            out.extend(
                f.instructions
                    .into_iter()
                    .flat_map(|i| fixupInstruction(&regs, i)),
            );
            f.instructions = out;
            AsmTopLevel::Function(f)
        }
        x => x,
    }
}
pub fn fixupProgram(p: AsmProgram) -> AsmProgram {
    let AsmProgram::Program(t) = p;
    AsmProgram::Program(t.into_iter().map(fixupTl).collect())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn rewrites_illegal_memory_move_and_large_immediate() {
        let memory = AsmOperand::Memory(AsmReg::BP, -8);
        assert_eq!(
            fixupInstruction(
                &[],
                AsmInstruction::Mov(
                    AsmType::Quadword,
                    memory.clone(),
                    AsmOperand::Data("x".into(), 0)
                )
            )
            .len(),
            2
        );
        assert_eq!(
            fixupInstruction(&[], AsmInstruction::Push(AsmOperand::Imm(i64::MAX))),
            vec![
                AsmInstruction::Mov(
                    AsmType::Quadword,
                    AsmOperand::Imm(i64::MAX),
                    AsmOperand::Reg(AsmReg::R10)
                ),
                AsmInstruction::Push(AsmOperand::Reg(AsmReg::R10)),
            ]
        );
    }

    #[test]
    fn restores_callee_saved_registers_in_reverse_order() {
        assert_eq!(
            fixupInstruction(&[AsmReg::BX, AsmReg::R12], AsmInstruction::Ret),
            vec![
                AsmInstruction::Pop(AsmReg::R12),
                AsmInstruction::Pop(AsmReg::BX),
                AsmInstruction::Ret
            ]
        );
    }
}
