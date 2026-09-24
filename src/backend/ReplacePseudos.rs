//! Replace symbolic pseudo-register operands with stack or static-data locations.
#![allow(non_snake_case)]

use super::Assembly::*;
use super::{AssemblySymbols, Rounding};
use std::collections::BTreeMap;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ReplacementState {
    pub current_offset: i32,
    pub offset_map: BTreeMap<String, i32>,
}

pub fn calculateOffset(mut state: ReplacementState, name: &str) -> (ReplacementState, i32) {
    let size = AssemblySymbols::getSize(name);
    let alignment = AssemblySymbols::getAlignment(name);
    let offset =
        Rounding::round_away_from_zero(alignment.into(), (state.current_offset - size).into())
            as i32;
    state.current_offset = offset;
    state.offset_map.insert(name.to_owned(), offset);
    (state, offset)
}

pub fn replaceOperand(
    state: ReplacementState,
    operand: AsmOperand,
) -> (ReplacementState, AsmOperand) {
    match operand {
        AsmOperand::Pseudo(name) if AssemblySymbols::isStatic(&name) => {
            (state, AsmOperand::Data(name, 0))
        }
        AsmOperand::Pseudo(name) => match state.offset_map.get(&name).copied() {
            Some(offset) => (state, AsmOperand::Memory(AsmReg::BP, offset)),
            None => {
                let (state, offset) = calculateOffset(state, &name);
                (state, AsmOperand::Memory(AsmReg::BP, offset))
            }
        },
        AsmOperand::PseudoMem(name, offset) if AssemblySymbols::isStatic(&name) => {
            (state, AsmOperand::Data(name, offset))
        }
        AsmOperand::PseudoMem(name, offset) => match state.offset_map.get(&name).copied() {
            Some(var_offset) => (state, AsmOperand::Memory(AsmReg::BP, offset + var_offset)),
            None => {
                let (state, var_offset) = calculateOffset(state, &name);
                (state, AsmOperand::Memory(AsmReg::BP, offset + var_offset))
            }
        },
        other => (state, other),
    }
}

pub fn replacePseudosInInstruction(
    state: ReplacementState,
    instruction: AsmInstruction,
) -> (ReplacementState, AsmInstruction) {
    use AsmInstruction::*;
    match instruction {
        Mov(t, src, dst) => {
            let (s, src) = replaceOperand(state, src);
            let (s, dst) = replaceOperand(s, dst);
            (s, Mov(t, src, dst))
        }
        Movsx(mut x) => {
            let (s, src) = replaceOperand(state, x.src);
            let (s, dst) = replaceOperand(s, x.dst);
            x.src = src;
            x.dst = dst;
            (s, Movsx(x))
        }
        MovZeroExtend(mut x) => {
            let (s, src) = replaceOperand(state, x.src);
            let (s, dst) = replaceOperand(s, x.dst);
            x.src = src;
            x.dst = dst;
            (s, MovZeroExtend(x))
        }
        Lea(src, dst) => {
            let (s, src) = replaceOperand(state, src);
            let (s, dst) = replaceOperand(s, dst);
            (s, Lea(src, dst))
        }
        Unary(op, t, dst) => {
            let (s, dst) = replaceOperand(state, dst);
            (s, Unary(op, t, dst))
        }
        Binary(mut x) => {
            let (s, src) = replaceOperand(state, x.src);
            let (s, dst) = replaceOperand(s, x.dst);
            x.src = src;
            x.dst = dst;
            (s, Binary(x))
        }
        Cmp(t, a, b) => {
            let (s, a) = replaceOperand(state, a);
            let (s, b) = replaceOperand(s, b);
            (s, Cmp(t, a, b))
        }
        Idiv(t, x) => {
            let (s, x) = replaceOperand(state, x);
            (s, Idiv(t, x))
        }
        Div(t, x) => {
            let (s, x) = replaceOperand(state, x);
            (s, Div(t, x))
        }
        SetCC(c, x) => {
            let (s, x) = replaceOperand(state, x);
            (s, SetCC(c, x))
        }
        Push(x) => {
            let (s, x) = replaceOperand(state, x);
            (s, Push(x))
        }
        Cvttsd2si(t, a, b) => {
            let (s, a) = replaceOperand(state, a);
            let (s, b) = replaceOperand(s, b);
            (s, Cvttsd2si(t, a, b))
        }
        Cvtsi2sd(t, a, b) => {
            let (s, a) = replaceOperand(state, a);
            let (s, b) = replaceOperand(s, b);
            (s, Cvtsi2sd(t, a, b))
        }
        Pop(_) => panic!("Internal error"),
        other => (state, other),
    }
}

pub fn replacePseudosInTl(top: AsmTopLevel) -> AsmTopLevel {
    match top {
        AsmTopLevel::Function(mut f) => {
            let mut state = ReplacementState {
                current_offset: if AssemblySymbols::returnsOnStack(&f.name) {
                    -8
                } else {
                    0
                },
                offset_map: BTreeMap::new(),
            };
            f.instructions = f
                .instructions
                .into_iter()
                .map(|i| {
                    let (s, i) = replacePseudosInInstruction(std::mem::take(&mut state), i);
                    state = s;
                    i
                })
                .collect();
            AssemblySymbols::setBytesRequired(&f.name, state.current_offset);
            AsmTopLevel::Function(f)
        }
        other => other,
    }
}

pub fn replacePseudos(program: AsmProgram) -> AsmProgram {
    let AsmProgram::Program(tops) = program;
    AsmProgram::Program(tops.into_iter().map(replacePseudosInTl).collect())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn reuses_aligned_stack_slots() {
        let name = "replace_pseudos_test_local";
        AssemblySymbols::addVar(name, AsmType::Quadword, false);
        let initial = ReplacementState::default();
        let (state, first) = replaceOperand(initial, AsmOperand::Pseudo(name.into()));
        let (state, second) = replaceOperand(state, AsmOperand::PseudoMem(name.into(), 4));
        assert_eq!(state.current_offset, -8);
        assert_eq!(first, AsmOperand::Memory(AsmReg::BP, -8));
        assert_eq!(second, AsmOperand::Memory(AsmReg::BP, -4));
    }
}
