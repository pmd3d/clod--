//! Render the assembly IR as GNU-style x86-64 assembly.
#![allow(non_snake_case)]

use std::fmt::Write as _;
use std::io;
use std::path::Path;

use super::Assembly::{
    AsmBinaryOperator::*, AsmCondCode::*, AsmInstruction::*, AsmOperand::*, AsmProgram, AsmReg,
    AsmReg::*, AsmTopLevel, AsmType, AsmType::*, AsmUnaryOperator::*,
};
use super::Initializers::StaticInit;
use super::Settings::Target;

pub fn suffix(t: &AsmType) -> &'static str {
    match t {
        Byte => "b",
        Longword => "l",
        Quadword => "q",
        Double => "sd",
        ByteArray(_) => panic!("Internal error: found instruction w/ non-scalar operand type"),
    }
}

pub fn alignDirective(platform: Target) -> &'static str {
    match platform {
        Target::OS_X => ".balign",
        Target::Linux => ".align",
    }
}
pub fn showLabel(platform: Target, name: &str) -> String {
    match platform {
        Target::OS_X => format!("_{name}"),
        Target::Linux => name.into(),
    }
}
pub fn showLocalLabel(platform: Target, label: &str) -> String {
    match platform {
        Target::OS_X => format!("L{label}"),
        Target::Linux => format!(".L{label}"),
    }
}
pub fn showFunName(platform: Target, name: &str) -> String {
    match platform {
        Target::OS_X => format!("_{name}"),
        Target::Linux => {
            if super::AssemblySymbols::isDefined(name) {
                name.into()
            } else {
                format!("{name}@PLT")
            }
        }
    }
}

pub fn showLongReg(r: AsmReg) -> &'static str {
    match r {
        AX => "%eax",
        BX => "%ebx",
        CX => "%ecx",
        DX => "%edx",
        DI => "%edi",
        SI => "%esi",
        R8 => "%r8d",
        R9 => "%r9d",
        R10 => "%r10d",
        R11 => "%r11d",
        R12 => "%r12d",
        R13 => "%r13d",
        R14 => "%r14d",
        R15 => "%r15d",
        SP => panic!("Internal error: no 32-bit RSP"),
        BP => panic!("Internal error: no 32-bit RBP"),
        _ => panic!("Internal error: can't store longword type in XMM register"),
    }
}
pub fn showQuadwordReg(r: AsmReg) -> &'static str {
    match r {
        AX => "%rax",
        BX => "%rbx",
        CX => "%rcx",
        DX => "%rdx",
        DI => "%rdi",
        SI => "%rsi",
        R8 => "%r8",
        R9 => "%r9",
        R10 => "%r10",
        R11 => "%r11",
        R12 => "%r12",
        R13 => "%r13",
        R14 => "%r14",
        R15 => "%r15",
        SP => "%rsp",
        BP => "%rbp",
        _ => panic!("Internal error: can't store quadword type in XMM register"),
    }
}
pub fn showDoubleReg(r: AsmReg) -> &'static str {
    match r {
        XMM0 => "%xmm0",
        XMM1 => "%xmm1",
        XMM2 => "%xmm2",
        XMM3 => "%xmm3",
        XMM4 => "%xmm4",
        XMM5 => "%xmm5",
        XMM6 => "%xmm6",
        XMM7 => "%xmm7",
        XMM8 => "%xmm8",
        XMM9 => "%xmm9",
        XMM10 => "%xmm10",
        XMM11 => "%xmm11",
        XMM12 => "%xmm12",
        XMM13 => "%xmm13",
        XMM14 => "%xmm14",
        XMM15 => "%xmm15",
        _ => panic!("Internal error: can't store double type in general-purpose register"),
    }
}
pub fn showByteReg(r: AsmReg) -> &'static str {
    match r {
        AX => "%al",
        BX => "%bl",
        CX => "%cl",
        DX => "%dl",
        DI => "%dil",
        SI => "%sil",
        R8 => "%r8b",
        R9 => "%r9b",
        R10 => "%r10b",
        R11 => "%r11b",
        R12 => "%r12b",
        R13 => "%r13b",
        R14 => "%r14b",
        R15 => "%r15b",
        SP => panic!("Internal error: no one-byte RSP"),
        BP => panic!("Internal error: no one-byte RBP"),
        _ => panic!("Internal error: can't store byte type in XMM register"),
    }
}

pub fn showOperand(platform: Target, t: &AsmType, operand: &super::Assembly::AsmOperand) -> String {
    match operand {
        Reg(r) => match t {
            Byte => showByteReg(*r),
            Longword => showLongReg(*r),
            Quadword => showQuadwordReg(*r),
            Double => showDoubleReg(*r),
            ByteArray(_) => panic!("Internal error: can't store non-scalar operand in register"),
        }
        .into(),
        Imm(i) => format!("${i}"),
        Memory(r, 0) => format!("({})", showQuadwordReg(*r)),
        Memory(r, i) => format!("{i}({})", showQuadwordReg(*r)),
        Data(name, offset) => {
            let label = if super::AssemblySymbols::isConstant(name) {
                showLocalLabel(platform, name)
            } else {
                showLabel(platform, name)
            };
            if *offset == 0 {
                format!("{label}(%rip)")
            } else {
                format!("{label}+{offset}(%rip)")
            }
        }
        Indexed(i) => format!(
            "({}, {}, {})",
            showQuadwordReg(i.base),
            showQuadwordReg(i.index),
            i.scale
        ),
        Pseudo(n) => format!("%{n}"),
        PseudoMem(n, o) => format!("{o}(%{n})"),
    }
}
pub fn showByteOperand(platform: Target, op: &super::Assembly::AsmOperand) -> String {
    match op {
        Reg(r) => showByteReg(*r).into(),
        _ => showOperand(platform, &Longword, op),
    }
}
pub fn showUnaryInstruction(op: super::Assembly::AsmUnaryOperator) -> &'static str {
    match op {
        Neg => "neg",
        Not => "not",
        Shr => "shr",
    }
}
pub fn showBinaryInstruction(op: super::Assembly::AsmBinaryOperator) -> &'static str {
    match op {
        Add => "add",
        Sub => "sub",
        Mult => "imul",
        DivDouble => "div",
        And => "and",
        Or => "or",
        Shl => "shl",
        ShrBinop => "shr",
        Xor => panic!("Internal error, should handle xor as special case"),
    }
}
pub fn showCondCode(c: super::Assembly::AsmCondCode) -> &'static str {
    match c {
        E => "e",
        NE => "ne",
        G => "g",
        GE => "ge",
        L => "l",
        LE => "le",
        A => "a",
        AE => "ae",
        B => "b",
        BE => "be",
    }
}

pub fn emitInstruction(
    platform: Target,
    out: &mut String,
    instruction: &super::Assembly::AsmInstruction,
) {
    match instruction {
        Mov(t, s, d) => writeln!(
            out,
            "\tmov{} {}, {}",
            suffix(t),
            showOperand(platform, t, s),
            showOperand(platform, t, d)
        )
        .unwrap(),
        Unary(op, t, d) => writeln!(
            out,
            "\t{}{} {}",
            showUnaryInstruction(*op),
            suffix(t),
            showOperand(platform, t, d)
        )
        .unwrap(),
        Binary(i) if i.op == Xor && i.t == Double => writeln!(
            out,
            "\txorpd {}, {}",
            showOperand(platform, &Double, &i.src),
            showOperand(platform, &Double, &i.dst)
        )
        .unwrap(),
        Binary(i) if i.op == Mult && i.t == Double => writeln!(
            out,
            "\tmulsd {}, {}",
            showOperand(platform, &Double, &i.src),
            showOperand(platform, &Double, &i.dst)
        )
        .unwrap(),
        Binary(i) => writeln!(
            out,
            "\t{}{} {}, {}",
            showBinaryInstruction(i.op),
            suffix(&i.t),
            showOperand(platform, &i.t, &i.src),
            showOperand(platform, &i.t, &i.dst)
        )
        .unwrap(),
        Cmp(Double, s, d) => writeln!(
            out,
            "\tcomisd {}, {}",
            showOperand(platform, &Double, s),
            showOperand(platform, &Double, d)
        )
        .unwrap(),
        Cmp(t, s, d) => writeln!(
            out,
            "\tcmp{} {}, {}",
            suffix(t),
            showOperand(platform, t, s),
            showOperand(platform, t, d)
        )
        .unwrap(),
        Idiv(t, o) => writeln!(out, "\tidiv{} {}", suffix(t), showOperand(platform, t, o)).unwrap(),
        Div(t, o) => writeln!(out, "\tdiv{} {}", suffix(t), showOperand(platform, t, o)).unwrap(),
        Lea(s, d) => writeln!(
            out,
            "\tleaq {}, {}",
            showOperand(platform, &Quadword, s),
            showOperand(platform, &Quadword, d)
        )
        .unwrap(),
        Cdq(Longword) => out.push_str("\tcdq\n"),
        Cdq(Quadword) => out.push_str("\tcqo\n"),
        Cdq(_) => panic!("Internal error: can't apply cdq to a byte or non-integer type"),
        Jmp(l) => writeln!(out, "\tjmp {}", showLocalLabel(platform, l)).unwrap(),
        JmpCC(c, l) => writeln!(
            out,
            "\tj{} {}",
            showCondCode(*c),
            showLocalLabel(platform, l)
        )
        .unwrap(),
        SetCC(c, o) => writeln!(
            out,
            "\tset{} {}",
            showCondCode(*c),
            showByteOperand(platform, o)
        )
        .unwrap(),
        Label(l) => writeln!(out, "{}:", showLocalLabel(platform, l)).unwrap(),
        Push(o) => writeln!(out, "\tpushq {}", showOperand(platform, &Quadword, o)).unwrap(),
        Pop(r) => writeln!(out, "\tpopq {}", showQuadwordReg(*r)).unwrap(),
        Call(f) => writeln!(out, "\tcall {}", showFunName(platform, f)).unwrap(),
        Movsx(i) => writeln!(
            out,
            "\tmovs{}{} {}, {}",
            suffix(&i.src_type),
            suffix(&i.dst_type),
            showOperand(platform, &i.src_type, &i.src),
            showOperand(platform, &i.dst_type, &i.dst)
        )
        .unwrap(),
        MovZeroExtend(i) => writeln!(
            out,
            "\tmovz{}{} {}, {}",
            suffix(&i.src_type),
            suffix(&i.dst_type),
            showOperand(platform, &i.src_type, &i.src),
            showOperand(platform, &i.dst_type, &i.dst)
        )
        .unwrap(),
        Cvtsi2sd(t, s, d) => writeln!(
            out,
            "\tcvtsi2sd{} {}, {}",
            suffix(t),
            showOperand(platform, t, s),
            showOperand(platform, &Double, d)
        )
        .unwrap(),
        Cvttsd2si(t, s, d) => writeln!(
            out,
            "\tcvttsd2si{} {}, {}",
            suffix(t),
            showOperand(platform, &Double, s),
            showOperand(platform, t, d)
        )
        .unwrap(),
        Ret => out.push_str("\n\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret\n"),
    }
}

pub fn escape(s: &str) -> String {
    s.chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() {
                c.to_string()
            } else {
                format!("\\{:03o}", c as u32)
            }
        })
        .collect()
}
pub fn emitInit(platform: Target, out: &mut String, init: &StaticInit) {
    match init {
        StaticInit::IntInit(v) => writeln!(out, "\t.long {v}").unwrap(),
        StaticInit::LongInit(v) => writeln!(out, "\t.quad {v}").unwrap(),
        StaticInit::UIntInit(v) => writeln!(out, "\t.long {v}").unwrap(),
        StaticInit::ULongInit(v) => writeln!(out, "\t.quad {v}").unwrap(),
        StaticInit::CharInit(v) => writeln!(out, "\t.byte {v}").unwrap(),
        StaticInit::UCharInit(v) => writeln!(out, "\t.byte {v}").unwrap(),
        StaticInit::DoubleInit(v) => writeln!(out, "\t.quad {}", v.to_bits() as i64).unwrap(),
        StaticInit::ZeroInit(v) => writeln!(out, "\t.zero {v}").unwrap(),
        StaticInit::StringInit(s, true) => writeln!(out, "\t.asciz \"{}\"", escape(s)).unwrap(),
        StaticInit::StringInit(s, false) => writeln!(out, "\t.ascii \"{}\"", escape(s)).unwrap(),
        StaticInit::PointerInit(l) => {
            writeln!(out, "\t.quad {}", showLocalLabel(platform, l)).unwrap()
        }
    }
}
fn is_zero(i: &StaticInit) -> bool {
    matches!(
        i,
        StaticInit::CharInit(0)
            | StaticInit::UCharInit(0)
            | StaticInit::IntInit(0)
            | StaticInit::LongInit(0)
            | StaticInit::UIntInit(0)
            | StaticInit::ULongInit(0)
            | StaticInit::ZeroInit(_)
    )
}
pub fn emitConstant(
    platform: Target,
    out: &mut String,
    name: &str,
    alignment: i32,
    init: &StaticInit,
) {
    let section = match (platform, init) {
        (Target::Linux, _) => ".section .rodata",
        (Target::OS_X, StaticInit::StringInit(..)) => ".cstring",
        (Target::OS_X, _) if alignment == 8 => ".literal8",
        (Target::OS_X, _) if alignment == 16 => ".literal16",
        _ => panic!("Internal error: found constant with bad alignment"),
    };
    write!(
        out,
        "\n\t{section}\n\t{} {alignment}\n  {}:\n",
        alignDirective(platform),
        showLocalLabel(platform, name)
    )
    .unwrap();
    emitInit(platform, out, init);
    if section == ".literal16" {
        emitInit(platform, out, &StaticInit::LongInit(0));
    }
}
pub fn emitTl(platform: Target, out: &mut String, tl: &AsmTopLevel) {
    match tl {
        AsmTopLevel::Function(f) => {
            let l = showLabel(platform, &f.name);
            if f.global {
                writeln!(out, "\t.globl {l}").unwrap()
            }
            write!(out, "\n\t.text\n{l}:\n\tpushq %rbp\n\tmovq %rsp, %rbp\n").unwrap();
            for i in &f.instructions {
                emitInstruction(platform, out, i)
            }
        }
        AsmTopLevel::StaticVariable(v) => {
            let l = showLabel(platform, &v.name);
            if v.global {
                writeln!(out, "\t.globl {l}").unwrap()
            }
            let section = if v.init.iter().all(is_zero) {
                ".bss"
            } else {
                ".data"
            };
            write!(
                out,
                "\n\t{section}\n\t{} {}\n{l}:\n",
                alignDirective(platform),
                v.alignment
            )
            .unwrap();
            for i in &v.init {
                emitInit(platform, out, i)
            }
        }
        AsmTopLevel::StaticConstant(c) => {
            emitConstant(platform, out, &c.name, c.alignment, &c.init)
        }
    }
}
pub fn emitStackNote(platform: Target, out: &mut String) {
    if platform == Target::Linux {
        out.push_str("\t.section .note.GNU-stack,\"\",@progbits\n")
    }
}
pub fn emitToString(platform: Target, program: &AsmProgram) -> String {
    let mut out = String::new();
    let AsmProgram::Program(tls) = program;
    for tl in tls {
        emitTl(platform, &mut out, tl)
    }
    emitStackNote(platform, &mut out);
    out
}
pub fn emit(
    platform: Target,
    assembly_file: impl AsRef<Path>,
    program: &AsmProgram,
) -> io::Result<()> {
    std::fs::write(assembly_file, emitToString(platform, program))
}

#[cfg(test)]
mod tests {
    use super::super::Assembly::{AsmFunctionDef, AsmInstruction, AsmOperand};
    use super::*;
    #[test]
    fn emits_linux_function() {
        super::super::AssemblySymbols::addFun("main", true, false, vec![], vec![]);
        let p = AsmProgram::Program(vec![AsmTopLevel::Function(AsmFunctionDef {
            name: "main".into(),
            global: true,
            instructions: vec![
                AsmInstruction::Mov(Longword, AsmOperand::Imm(42), AsmOperand::Reg(AX)),
                AsmInstruction::Ret,
            ],
        })]);
        assert_eq!(emitToString(Target::Linux,&p),"\t.globl main\n\n\t.text\nmain:\n\tpushq %rbp\n\tmovq %rsp, %rbp\n\tmovl $42, %eax\n\n\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret\n\t.section .note.GNU-stack,\"\",@progbits\n");
    }
    #[test]
    fn escapes_like_fsharp() {
        assert_eq!(escape("hello 1\n"), "hello\\0401\\012");
    }
}
