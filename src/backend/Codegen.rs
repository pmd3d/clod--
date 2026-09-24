//! Lowering from the TACKY IR to the x86-64 assembly IR.
//! This preserves the System V ABI classification and instruction sequences of
//! the original `Codegen.fs` implementation.
#![allow(non_snake_case)]

use super::{
    Assembly::*,
    AssemblySymbols,
    Const::Constant,
    Initializers::StaticInit,
    Symbols::{self, IdentifierAttrs, SymbolEntry},
    Tacky::*,
    TypeTable, TypeUtils,
    Types::Type,
    UniqueIds::{self, Counter},
};
use std::collections::BTreeMap;

const INT_PARAM_REGS: [AsmReg; 6] = [
    AsmReg::DI,
    AsmReg::SI,
    AsmReg::DX,
    AsmReg::CX,
    AsmReg::R8,
    AsmReg::R9,
];
const DBL_PARAM_REGS: [AsmReg; 8] = [
    AsmReg::XMM0,
    AsmReg::XMM1,
    AsmReg::XMM2,
    AsmReg::XMM3,
    AsmReg::XMM4,
    AsmReg::XMM5,
    AsmReg::XMM6,
    AsmReg::XMM7,
];
fn zero() -> AsmOperand {
    AsmOperand::Imm(0)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ParamClass {
    Mem,
    SSE,
    Integer,
}
type TypedOperand = (AsmType, AsmOperand);

#[derive(Default)]
struct Context {
    constants: BTreeMap<i64, (String, i32)>,
    classified: BTreeMap<String, Vec<ParamClass>>,
    counter: Counter,
}
impl Context {
    fn next_label(&mut self, prefix: &str) -> String {
        let (c, n) = UniqueIds::make_label(prefix, self.counter);
        self.counter = c;
        n
    }
    fn add_constant(&mut self, alignment: Option<i32>, value: f64) -> String {
        let alignment = alignment.unwrap_or(8);
        let key = value.to_bits() as i64;
        if let Some((name, old)) = self.constants.get_mut(&key) {
            *old = (*old).max(alignment);
            return name.clone();
        }
        let name = self.next_label("dbl");
        self.constants.insert(key, (name.clone(), alignment));
        name
    }
    fn classify_structure(&mut self, tag: &str) -> Vec<ParamClass> {
        if let Some(c) = self.classified.get(tag) {
            return c.clone();
        }
        let size = TypeTable::find(tag).size;
        let classes = if size > 16 {
            vec![ParamClass::Mem; size.div_ceil(8)]
        } else {
            fn flatten(t: &Type, out: &mut Vec<Type>) {
                match t {
                    Type::Struct(s) => {
                        for t in TypeTable::get_member_types(s) {
                            flatten(&t, out)
                        }
                    }
                    Type::Array(t, n) => {
                        for _ in 0..*n {
                            flatten(t, out)
                        }
                    }
                    t => out.push(t.clone()),
                }
            }
            let mut scalar = vec![];
            flatten(&Type::Struct(tag.into()), &mut scalar);
            let first = scalar
                .first()
                .expect("Internal error: empty scalar types for structure");
            let last = scalar.last().unwrap();
            let class = |t: &Type| {
                if *t == Type::Double {
                    ParamClass::SSE
                } else {
                    ParamClass::Integer
                }
            };
            if size > 8 {
                vec![class(first), class(last)]
            } else {
                vec![class(first)]
            }
        };
        self.classified.insert(tag.into(), classes.clone());
        classes
    }
}

pub fn getEightbyteType(index: usize, total_size: usize) -> AsmType {
    match total_size - index * 8 {
        n if n >= 8 => AsmType::Quadword,
        4 => AsmType::Longword,
        1 => AsmType::Byte,
        n => AsmType::ByteArray(AsmByteArrayInfo {
            size: n as i32,
            alignment: 8,
        }),
    }
}
pub fn addOffset(n: i32, value: &AsmOperand) -> AsmOperand {
    match value {
        AsmOperand::PseudoMem(b, o) => AsmOperand::PseudoMem(b.clone(), o + n),
        AsmOperand::Memory(r, o) => AsmOperand::Memory(*r, o + n),
        _ => panic!("Internal error: trying to copy data to or from non-memory operand"),
    }
}
pub fn copyBytes(src: AsmOperand, dst: AsmOperand, mut count: usize) -> Vec<AsmInstruction> {
    let mut out = vec![];
    let (mut s, mut d) = (src, dst);
    while count > 0 {
        let (t, n) = if count < 4 {
            (AsmType::Byte, 1)
        } else if count < 8 {
            (AsmType::Longword, 4)
        } else {
            (AsmType::Quadword, 8)
        };
        out.push(AsmInstruction::Mov(t, s.clone(), d.clone()));
        s = addOffset(n, &s);
        d = addOffset(n, &d);
        count -= n as usize;
    }
    out
}
pub fn copyBytesToReg(src: AsmOperand, reg: AsmReg, count: usize) -> Vec<AsmInstruction> {
    let mut out = vec![];
    for i in (0..count).rev() {
        out.push(AsmInstruction::Mov(
            AsmType::Byte,
            addOffset(i as i32, &src),
            AsmOperand::Reg(reg),
        ));
        if i != 0 {
            out.push(binary(
                AsmBinaryOperator::Shl,
                AsmType::Quadword,
                AsmOperand::Imm(8),
                AsmOperand::Reg(reg),
            ));
        }
    }
    out
}
pub fn copyBytesFromReg(reg: AsmReg, dst: AsmOperand, count: usize) -> Vec<AsmInstruction> {
    let mut out = vec![];
    for i in 0..count {
        out.push(AsmInstruction::Mov(
            AsmType::Byte,
            AsmOperand::Reg(reg),
            addOffset(i as i32, &dst),
        ));
        if i + 1 < count {
            out.push(binary(
                AsmBinaryOperator::ShrBinop,
                AsmType::Quadword,
                AsmOperand::Imm(8),
                AsmOperand::Reg(reg),
            ));
        }
    }
    out
}
fn binary(op: AsmBinaryOperator, t: AsmType, src: AsmOperand, dst: AsmOperand) -> AsmInstruction {
    AsmInstruction::Binary(AsmBinaryInfo { op, t, src, dst })
}

fn convertVal(ctx: &mut Context, v: &TackyVal) -> AsmOperand {
    match v {
        TackyVal::Constant(c) => match c {
            Constant::Char(x) => AsmOperand::Imm(*x as i64),
            Constant::UChar(x) => AsmOperand::Imm(*x as i64),
            Constant::Int(x) => AsmOperand::Imm(*x as i64),
            Constant::Long(x) => AsmOperand::Imm(*x),
            Constant::UInt(x) => AsmOperand::Imm(*x as i64),
            Constant::ULong(x) => AsmOperand::Imm(*x as i64),
            Constant::Double(x) => AsmOperand::Data(ctx.add_constant(None, *x), 0),
        },
        TackyVal::Var(n) => {
            if TypeUtils::is_scalar(&Symbols::get(n).symType) {
                AsmOperand::Pseudo(n.clone())
            } else {
                AsmOperand::PseudoMem(n.clone(), 0)
            }
        }
    }
}
pub fn convertType(t: &Type) -> AsmType {
    match t {
        Type::Int | Type::UInt => AsmType::Longword,
        Type::Long | Type::ULong | Type::Pointer(_) => AsmType::Quadword,
        Type::Char | Type::SChar | Type::UChar => AsmType::Byte,
        Type::Double => AsmType::Double,
        Type::Array(_, _) | Type::Struct(_) => AsmType::ByteArray(AsmByteArrayInfo {
            size: TypeUtils::get_size(t) as i32,
            alignment: TypeUtils::get_alignment(t) as i32,
        }),
        Type::Function(_, _) | Type::Void => {
            panic!("Internal error, converting type to assembly: {t:?}")
        }
    }
}
fn asm_type(v: &TackyVal) -> AsmType {
    convertType(&typeOfVal(v))
}
fn cond(op: TackyBinaryOperator, signed: bool) -> AsmCondCode {
    match op {
        TackyBinaryOperator::Equal => AsmCondCode::E,
        TackyBinaryOperator::NotEqual => AsmCondCode::NE,
        TackyBinaryOperator::GreaterThan => {
            if signed {
                AsmCondCode::G
            } else {
                AsmCondCode::A
            }
        }
        TackyBinaryOperator::GreaterOrEqual => {
            if signed {
                AsmCondCode::GE
            } else {
                AsmCondCode::AE
            }
        }
        TackyBinaryOperator::LessThan => {
            if signed {
                AsmCondCode::L
            } else {
                AsmCondCode::B
            }
        }
        TackyBinaryOperator::LessOrEqual => {
            if signed {
                AsmCondCode::LE
            } else {
                AsmCondCode::BE
            }
        }
        _ => panic!("Internal error: not a condition code"),
    }
}

fn classify_params(
    ctx: &mut Context,
    vals: &[(Type, AsmOperand)],
    stack_ret: bool,
) -> (Vec<TypedOperand>, Vec<AsmOperand>, Vec<TypedOperand>) {
    let available = if stack_ret { 5 } else { 6 };
    let (mut ints, mut dbls, mut stack) = (vec![], vec![], vec![]);
    for (tacky_t, op) in vals {
        let typed = (convertType(tacky_t), op.clone());
        match tacky_t {
            Type::Struct(tag) => {
                let name = match op {
                    AsmOperand::PseudoMem(n, 0) => n.clone(),
                    _ => panic!("Bad structure operand"),
                };
                let size = TypeUtils::get_size(tacky_t) as usize;
                let classes = ctx.classify_structure(tag);
                let (mut ti, mut td) = (ints.clone(), dbls.clone());
                for (i, c) in classes.iter().enumerate() {
                    let o = AsmOperand::PseudoMem(name.clone(), (i * 8) as i32);
                    match c {
                        ParamClass::SSE => td.push(o),
                        ParamClass::Integer => ti.push((getEightbyteType(i, size), o)),
                        ParamClass::Mem => {}
                    }
                }
                let use_stack = classes.first() == Some(&ParamClass::Mem)
                    || ti.len() > available
                    || td.len() > 8;
                if use_stack {
                    for i in 0..classes.len() {
                        stack.push((
                            getEightbyteType(i, size),
                            AsmOperand::PseudoMem(name.clone(), (i * 8) as i32),
                        ));
                    }
                } else {
                    ints = ti;
                    dbls = td;
                }
            }
            Type::Double => {
                if dbls.len() < 8 {
                    dbls.push(op.clone())
                } else {
                    stack.push(typed)
                }
            }
            _ => {
                if ints.len() < available {
                    ints.push(typed)
                } else {
                    stack.push(typed)
                }
            }
        }
    }
    (ints, dbls, stack)
}
fn classify_parameters(
    ctx: &mut Context,
    vals: &[TackyVal],
    r: bool,
) -> (Vec<TypedOperand>, Vec<AsmOperand>, Vec<TypedOperand>) {
    let x: Vec<_> = vals
        .iter()
        .map(|v| (typeOfVal(v), convertVal(ctx, v)))
        .collect();
    classify_params(ctx, &x, r)
}
fn classify_return(
    ctx: &mut Context,
    t: &Type,
    op: AsmOperand,
) -> (Vec<TypedOperand>, Vec<AsmOperand>, bool) {
    match t {
        Type::Struct(tag) => {
            let c = ctx.classify_structure(tag);
            if c.first() == Some(&ParamClass::Mem) {
                return (vec![], vec![], true);
            }
            let name = match op {
                AsmOperand::PseudoMem(n, 0) => n,
                _ => panic!("Internal error: invalid assembly operand for structure type"),
            };
            let (mut i, mut d) = (vec![], vec![]);
            for (idx, c) in c.iter().enumerate() {
                let o = AsmOperand::PseudoMem(name.clone(), (idx * 8) as i32);
                match c {
                    ParamClass::SSE => d.push(o),
                    ParamClass::Integer => {
                        i.push((getEightbyteType(idx, TypeUtils::get_size(t) as usize), o))
                    }
                    ParamClass::Mem => unreachable!(),
                }
            }
            (i, d, false)
        }
        Type::Double => (vec![], vec![op], false),
        _ => (vec![(convertType(t), op)], vec![], false),
    }
}
fn classify_return_value(
    ctx: &mut Context,
    v: &TackyVal,
) -> (Vec<TypedOperand>, Vec<AsmOperand>, bool) {
    let operand = convertVal(ctx, v);
    classify_return(ctx, &typeOfVal(v), operand)
}

fn convert_call(
    ctx: &mut Context,
    f: &str,
    args: &[TackyVal],
    dst: &Option<TackyVal>,
) -> Vec<AsmInstruction> {
    let (ir, dr, stack_ret) = dst
        .as_ref()
        .map(|d| classify_return_value(ctx, d))
        .unwrap_or_default();
    let (mut out, first) = if stack_ret {
        (
            vec![AsmInstruction::Lea(
                convertVal(ctx, dst.as_ref().unwrap()),
                AsmOperand::Reg(AsmReg::DI),
            )],
            1,
        )
    } else {
        (vec![], 0)
    };
    let (ints, dbls, stack) = classify_parameters(ctx, args, stack_ret);
    let pad = if stack.len() % 2 == 0 { 0 } else { 8 };
    if pad != 0 {
        out.push(binary(
            AsmBinaryOperator::Sub,
            AsmType::Quadword,
            AsmOperand::Imm(pad),
            AsmOperand::Reg(AsmReg::SP),
        ))
    }
    for (i, (t, a)) in ints.into_iter().enumerate() {
        let r = INT_PARAM_REGS[i + first];
        match t {
            AsmType::ByteArray(x) => out.extend(copyBytesToReg(a, r, x.size as usize)),
            _ => out.push(AsmInstruction::Mov(t, a, AsmOperand::Reg(r))),
        }
    }
    for (i, a) in dbls.into_iter().enumerate() {
        out.push(AsmInstruction::Mov(
            AsmType::Double,
            a,
            AsmOperand::Reg(DBL_PARAM_REGS[i]),
        ))
    }
    for (t, a) in stack.iter().rev().cloned() {
        match (&a, &t) {
            (AsmOperand::Imm(_) | AsmOperand::Reg(_), _)
            | (_, AsmType::Quadword | AsmType::Double) => out.push(AsmInstruction::Push(a)),
            (_, AsmType::ByteArray(x)) => {
                out.push(binary(
                    AsmBinaryOperator::Sub,
                    AsmType::Quadword,
                    AsmOperand::Imm(8),
                    AsmOperand::Reg(AsmReg::SP),
                ));
                out.extend(copyBytes(
                    a,
                    AsmOperand::Memory(AsmReg::SP, 0),
                    x.size as usize,
                ))
            }
            _ => {
                out.push(AsmInstruction::Mov(t, a, AsmOperand::Reg(AsmReg::AX)));
                out.push(AsmInstruction::Push(AsmOperand::Reg(AsmReg::AX)))
            }
        }
    }
    out.push(AsmInstruction::Call(f.into()));
    let remove = stack.len() as i64 * 8 + pad;
    if remove != 0 {
        out.push(binary(
            AsmBinaryOperator::Add,
            AsmType::Quadword,
            AsmOperand::Imm(remove),
            AsmOperand::Reg(AsmReg::SP),
        ))
    }
    if dst.is_some() && !stack_ret {
        for (i, (t, a)) in ir.into_iter().enumerate() {
            let r = [AsmReg::AX, AsmReg::DX][i];
            match t {
                AsmType::ByteArray(x) => out.extend(copyBytesFromReg(r, a, x.size as usize)),
                _ => out.push(AsmInstruction::Mov(t, AsmOperand::Reg(r), a)),
            }
        }
        for (i, a) in dr.into_iter().enumerate() {
            out.push(AsmInstruction::Mov(
                AsmType::Double,
                AsmOperand::Reg([AsmReg::XMM0, AsmReg::XMM1][i]),
                a,
            ))
        }
    }
    out
}
fn convert_return(ctx: &mut Context, v: &Option<TackyVal>) -> Vec<AsmInstruction> {
    let Some(v) = v else {
        return vec![AsmInstruction::Ret];
    };
    let (ints, dbls, stack) = classify_return_value(ctx, v);
    let mut out = vec![];
    if stack {
        out.push(AsmInstruction::Mov(
            AsmType::Quadword,
            AsmOperand::Memory(AsmReg::BP, -8),
            AsmOperand::Reg(AsmReg::AX),
        ));
        out.extend(copyBytes(
            convertVal(ctx, v),
            AsmOperand::Memory(AsmReg::AX, 0),
            TypeUtils::get_size(&typeOfVal(v)) as usize,
        ));
    } else {
        for (i, (t, a)) in ints.into_iter().enumerate() {
            let r = [AsmReg::AX, AsmReg::DX][i];
            match t {
                AsmType::ByteArray(x) => out.extend(copyBytesToReg(a, r, x.size as usize)),
                _ => out.push(AsmInstruction::Mov(t, a, AsmOperand::Reg(r))),
            }
        }
        for (i, a) in dbls.into_iter().enumerate() {
            out.push(AsmInstruction::Mov(
                AsmType::Double,
                a,
                AsmOperand::Reg([AsmReg::XMM0, AsmReg::XMM1][i]),
            ))
        }
    }
    out.push(AsmInstruction::Ret);
    out
}
fn unop(op: TackyUnaryOperator) -> AsmUnaryOperator {
    match op {
        TackyUnaryOperator::Complement => AsmUnaryOperator::Not,
        TackyUnaryOperator::Negate => AsmUnaryOperator::Neg,
        TackyUnaryOperator::Not => {
            panic!("Internal error, can't convert TACKY not directly to assembly")
        }
    }
}
fn binop(op: TackyBinaryOperator) -> AsmBinaryOperator {
    match op {
        TackyBinaryOperator::Add => AsmBinaryOperator::Add,
        TackyBinaryOperator::Subtract => AsmBinaryOperator::Sub,
        TackyBinaryOperator::Multiply => AsmBinaryOperator::Mult,
        TackyBinaryOperator::Divide => AsmBinaryOperator::DivDouble,
        _ => panic!("Internal error: not a binary assembly instruction"),
    }
}
fn convert_instruction(ctx: &mut Context, ins: &TackyInstruction) -> Vec<AsmInstruction> {
    use TackyInstruction::*;
    match ins {
        Copy(x) => {
            let s = convertVal(ctx, &x.src);
            let d = convertVal(ctx, &x.dst);
            if TypeUtils::is_scalar(&typeOfVal(&x.src)) {
                vec![AsmInstruction::Mov(asm_type(&x.src), s, d)]
            } else {
                copyBytes(s, d, TypeUtils::get_size(&typeOfVal(&x.src)) as usize)
            }
        }
        Return(v) => convert_return(ctx, v),
        Unary(x) if x.op == TackyUnaryOperator::Not => {
            let st = asm_type(&x.src);
            let dt = asm_type(&x.dst);
            let s = convertVal(ctx, &x.src);
            let d = convertVal(ctx, &x.dst);
            let mut o = vec![];
            if st == AsmType::Double {
                o.push(binary(
                    AsmBinaryOperator::Xor,
                    AsmType::Double,
                    AsmOperand::Reg(AsmReg::XMM0),
                    AsmOperand::Reg(AsmReg::XMM0),
                ));
                o.push(AsmInstruction::Cmp(st, s, AsmOperand::Reg(AsmReg::XMM0)))
            } else {
                o.push(AsmInstruction::Cmp(st, zero(), s))
            }
            o.push(AsmInstruction::Mov(dt, zero(), d.clone()));
            o.push(AsmInstruction::SetCC(AsmCondCode::E, d));
            o
        }
        Unary(x) if x.op == TackyUnaryOperator::Negate && typeOfVal(&x.src) == Type::Double => {
            let s = convertVal(ctx, &x.src);
            let d = convertVal(ctx, &x.dst);
            let z = ctx.add_constant(Some(16), -0.0);
            vec![
                AsmInstruction::Mov(AsmType::Double, s, d.clone()),
                binary(
                    AsmBinaryOperator::Xor,
                    AsmType::Double,
                    AsmOperand::Data(z, 0),
                    d,
                ),
            ]
        }
        Unary(x) => {
            let t = asm_type(&x.src);
            let s = convertVal(ctx, &x.src);
            let d = convertVal(ctx, &x.dst);
            vec![
                AsmInstruction::Mov(t.clone(), s, d.clone()),
                AsmInstruction::Unary(unop(x.op), t, d),
            ]
        }
        Binary(x) => {
            let st = asm_type(&x.src1);
            let dt = asm_type(&x.dst);
            let a = convertVal(ctx, &x.src1);
            let b = convertVal(ctx, &x.src2);
            let d = convertVal(ctx, &x.dst);
            match x.op {
                TackyBinaryOperator::Equal
                | TackyBinaryOperator::NotEqual
                | TackyBinaryOperator::GreaterThan
                | TackyBinaryOperator::GreaterOrEqual
                | TackyBinaryOperator::LessThan
                | TackyBinaryOperator::LessOrEqual => {
                    let c = cond(
                        x.op,
                        st != AsmType::Double && TypeUtils::is_signed(&typeOfVal(&x.src1)),
                    );
                    vec![
                        AsmInstruction::Cmp(st, b, a),
                        AsmInstruction::Mov(dt, zero(), d.clone()),
                        AsmInstruction::SetCC(c, d),
                    ]
                }
                TackyBinaryOperator::Divide | TackyBinaryOperator::Mod if st != AsmType::Double => {
                    let r = if x.op == TackyBinaryOperator::Divide {
                        AsmReg::AX
                    } else {
                        AsmReg::DX
                    };
                    let mut o = vec![AsmInstruction::Mov(
                        st.clone(),
                        a,
                        AsmOperand::Reg(AsmReg::AX),
                    )];
                    if TypeUtils::is_signed(&typeOfVal(&x.src1)) {
                        o.push(AsmInstruction::Cdq(st.clone()));
                        o.push(AsmInstruction::Idiv(st.clone(), b))
                    } else {
                        o.push(AsmInstruction::Mov(
                            st.clone(),
                            zero(),
                            AsmOperand::Reg(AsmReg::DX),
                        ));
                        o.push(AsmInstruction::Div(st.clone(), b))
                    }
                    o.push(AsmInstruction::Mov(st, AsmOperand::Reg(r), d));
                    o
                }
                _ => vec![
                    AsmInstruction::Mov(st.clone(), a, d.clone()),
                    binary(binop(x.op), st, b, d),
                ],
            }
        }
        Load(x) => {
            let p = convertVal(ctx, &x.src_ptr);
            let d = convertVal(ctx, &x.dst);
            let mut o = vec![AsmInstruction::Mov(
                AsmType::Quadword,
                p,
                AsmOperand::Reg(AsmReg::R9),
            )];
            if TypeUtils::is_scalar(&typeOfVal(&x.dst)) {
                o.push(AsmInstruction::Mov(
                    asm_type(&x.dst),
                    AsmOperand::Memory(AsmReg::R9, 0),
                    d,
                ))
            } else {
                o.extend(copyBytes(
                    AsmOperand::Memory(AsmReg::R9, 0),
                    d,
                    TypeUtils::get_size(&typeOfVal(&x.dst)) as usize,
                ))
            }
            o
        }
        Store(x) => {
            let s = convertVal(ctx, &x.src);
            let p = convertVal(ctx, &x.dst_ptr);
            let mut o = vec![AsmInstruction::Mov(
                AsmType::Quadword,
                p,
                AsmOperand::Reg(AsmReg::R9),
            )];
            if TypeUtils::is_scalar(&typeOfVal(&x.src)) {
                o.push(AsmInstruction::Mov(
                    asm_type(&x.src),
                    s,
                    AsmOperand::Memory(AsmReg::R9, 0),
                ))
            } else {
                o.extend(copyBytes(
                    s,
                    AsmOperand::Memory(AsmReg::R9, 0),
                    TypeUtils::get_size(&typeOfVal(&x.src)) as usize,
                ))
            }
            o
        }
        GetAddress(x) => vec![AsmInstruction::Lea(
            convertVal(ctx, &x.src),
            convertVal(ctx, &x.dst),
        )],
        Jump(x) => vec![AsmInstruction::Jmp(x.clone())],
        Label(x) => vec![AsmInstruction::Label(x.clone())],
        JumpIfZero(v, l) | JumpIfNotZero(v, l) => {
            let t = asm_type(v);
            let a = convertVal(ctx, v);
            let c = if matches!(ins, JumpIfZero(..)) {
                AsmCondCode::E
            } else {
                AsmCondCode::NE
            };
            let mut o = vec![];
            if t == AsmType::Double {
                o.push(binary(
                    AsmBinaryOperator::Xor,
                    AsmType::Double,
                    AsmOperand::Reg(AsmReg::XMM0),
                    AsmOperand::Reg(AsmReg::XMM0),
                ));
                o.push(AsmInstruction::Cmp(t, a, AsmOperand::Reg(AsmReg::XMM0)))
            } else {
                o.push(AsmInstruction::Cmp(t, zero(), a))
            }
            o.push(AsmInstruction::JmpCC(c, l.clone()));
            o
        }
        FunCall(x) => convert_call(ctx, &x.f, &x.args, &x.dst),
        SignExtend(x) => vec![AsmInstruction::Movsx(AsmMovsxInfo {
            src_type: asm_type(&x.src),
            dst_type: asm_type(&x.dst),
            src: convertVal(ctx, &x.src),
            dst: convertVal(ctx, &x.dst),
        })],
        ZeroExtend(x) => vec![AsmInstruction::MovZeroExtend(AsmMovZeroExtendInfo {
            src_type: asm_type(&x.src),
            dst_type: asm_type(&x.dst),
            src: convertVal(ctx, &x.src),
            dst: convertVal(ctx, &x.dst),
        })],
        Truncate(x) => vec![AsmInstruction::Mov(
            asm_type(&x.dst),
            convertVal(ctx, &x.src),
            convertVal(ctx, &x.dst),
        )],
        IntToDouble(x) => {
            let s = convertVal(ctx, &x.src);
            let d = convertVal(ctx, &x.dst);
            let t = asm_type(&x.src);
            if t == AsmType::Byte {
                vec![
                    AsmInstruction::Movsx(AsmMovsxInfo {
                        src_type: AsmType::Byte,
                        dst_type: AsmType::Longword,
                        src: s,
                        dst: AsmOperand::Reg(AsmReg::R9),
                    }),
                    AsmInstruction::Cvtsi2sd(AsmType::Longword, AsmOperand::Reg(AsmReg::R9), d),
                ]
            } else {
                vec![AsmInstruction::Cvtsi2sd(t, s, d)]
            }
        }
        DoubleToInt(x) => {
            let s = convertVal(ctx, &x.src);
            let d = convertVal(ctx, &x.dst);
            let t = asm_type(&x.dst);
            if t == AsmType::Byte {
                vec![
                    AsmInstruction::Cvttsd2si(AsmType::Longword, s, AsmOperand::Reg(AsmReg::R9)),
                    AsmInstruction::Mov(AsmType::Byte, AsmOperand::Reg(AsmReg::R9), d),
                ]
            } else {
                vec![AsmInstruction::Cvttsd2si(t, s, d)]
            }
        }
        UIntToDouble(x) => uint_to_double(ctx, x),
        DoubleToUInt(x) => double_to_uint(ctx, x),
        CopyToOffset(x) => {
            let s = convertVal(ctx, &x.src);
            let d = AsmOperand::PseudoMem(x.dst.clone(), x.offset);
            if TypeUtils::is_scalar(&typeOfVal(&x.src)) {
                vec![AsmInstruction::Mov(asm_type(&x.src), s, d)]
            } else {
                copyBytes(s, d, TypeUtils::get_size(&typeOfVal(&x.src)) as usize)
            }
        }
        CopyFromOffset(x) => {
            let s = AsmOperand::PseudoMem(x.src.clone(), x.offset);
            let d = convertVal(ctx, &x.dst);
            if TypeUtils::is_scalar(&typeOfVal(&x.dst)) {
                vec![AsmInstruction::Mov(asm_type(&x.dst), s, d)]
            } else {
                copyBytes(s, d, TypeUtils::get_size(&typeOfVal(&x.dst)) as usize)
            }
        }
        AddPtr(x) => add_ptr(ctx, x),
    }
}
fn uint_to_double(ctx: &mut Context, x: &TackySrcDst) -> Vec<AsmInstruction> {
    let s = convertVal(ctx, &x.src);
    let d = convertVal(ctx, &x.dst);
    match typeOfVal(&x.src) {
        Type::UChar => vec![
            AsmInstruction::MovZeroExtend(AsmMovZeroExtendInfo {
                src_type: AsmType::Byte,
                dst_type: AsmType::Longword,
                src: s,
                dst: AsmOperand::Reg(AsmReg::R9),
            }),
            AsmInstruction::Cvtsi2sd(AsmType::Longword, AsmOperand::Reg(AsmReg::R9), d),
        ],
        Type::UInt => vec![
            AsmInstruction::MovZeroExtend(AsmMovZeroExtendInfo {
                src_type: AsmType::Longword,
                dst_type: AsmType::Quadword,
                src: s,
                dst: AsmOperand::Reg(AsmReg::R9),
            }),
            AsmInstruction::Cvtsi2sd(AsmType::Quadword, AsmOperand::Reg(AsmReg::R9), d),
        ],
        _ => {
            let oob = ctx.next_label("ulong2dbl.oob");
            let end = ctx.next_label("ulong2dbl.end");
            let (r1, r2) = (AsmOperand::Reg(AsmReg::R8), AsmOperand::Reg(AsmReg::R9));
            vec![
                AsmInstruction::Cmp(AsmType::Quadword, zero(), s.clone()),
                AsmInstruction::JmpCC(AsmCondCode::L, oob.clone()),
                AsmInstruction::Cvtsi2sd(AsmType::Quadword, s.clone(), d.clone()),
                AsmInstruction::Jmp(end.clone()),
                AsmInstruction::Label(oob),
                AsmInstruction::Mov(AsmType::Quadword, s, r1.clone()),
                AsmInstruction::Mov(AsmType::Quadword, r1.clone(), r2.clone()),
                AsmInstruction::Unary(AsmUnaryOperator::Shr, AsmType::Quadword, r2.clone()),
                binary(
                    AsmBinaryOperator::And,
                    AsmType::Quadword,
                    AsmOperand::Imm(1),
                    r1.clone(),
                ),
                binary(AsmBinaryOperator::Or, AsmType::Quadword, r1, r2.clone()),
                AsmInstruction::Cvtsi2sd(AsmType::Quadword, r2, d.clone()),
                binary(AsmBinaryOperator::Add, AsmType::Double, d.clone(), d),
                AsmInstruction::Label(end),
            ]
        }
    }
}
fn double_to_uint(ctx: &mut Context, x: &TackySrcDst) -> Vec<AsmInstruction> {
    let s = convertVal(ctx, &x.src);
    let d = convertVal(ctx, &x.dst);
    match typeOfVal(&x.dst) {
        Type::UChar => vec![
            AsmInstruction::Cvttsd2si(AsmType::Longword, s, AsmOperand::Reg(AsmReg::R9)),
            AsmInstruction::Mov(AsmType::Byte, AsmOperand::Reg(AsmReg::R9), d),
        ],
        Type::UInt => vec![
            AsmInstruction::Cvttsd2si(AsmType::Quadword, s, AsmOperand::Reg(AsmReg::R9)),
            AsmInstruction::Mov(AsmType::Longword, AsmOperand::Reg(AsmReg::R9), d),
        ],
        _ => {
            let oob = ctx.next_label("dbl2ulong.oob");
            let end = ctx.next_label("dbl2ulong.end");
            let upper = ctx.add_constant(None, 9223372036854775808.0);
            let data = AsmOperand::Data(upper, 0);
            let r = AsmOperand::Reg(AsmReg::R9);
            let xmm = AsmOperand::Reg(AsmReg::XMM7);
            vec![
                AsmInstruction::Cmp(AsmType::Double, data.clone(), s.clone()),
                AsmInstruction::JmpCC(AsmCondCode::AE, oob.clone()),
                AsmInstruction::Cvttsd2si(AsmType::Quadword, s.clone(), d.clone()),
                AsmInstruction::Jmp(end.clone()),
                AsmInstruction::Label(oob),
                AsmInstruction::Mov(AsmType::Double, s, xmm.clone()),
                binary(AsmBinaryOperator::Sub, AsmType::Double, data, xmm.clone()),
                AsmInstruction::Cvttsd2si(AsmType::Quadword, xmm, d.clone()),
                AsmInstruction::Mov(AsmType::Quadword, AsmOperand::Imm(i64::MIN), r.clone()),
                binary(AsmBinaryOperator::Add, AsmType::Quadword, r, d),
                AsmInstruction::Label(end),
            ]
        }
    }
}
fn add_ptr(ctx: &mut Context, x: &TackyAddPtrInfo) -> Vec<AsmInstruction> {
    let p = convertVal(ctx, &x.ptr);
    let d = convertVal(ctx, &x.dst);
    if let TackyVal::Constant(Constant::Long(i)) = &x.index {
        return vec![
            AsmInstruction::Mov(AsmType::Quadword, p, AsmOperand::Reg(AsmReg::R9)),
            AsmInstruction::Lea(AsmOperand::Memory(AsmReg::R9, *i as i32 * x.scale), d),
        ];
    }
    let i = convertVal(ctx, &x.index);
    let mut o = vec![
        AsmInstruction::Mov(AsmType::Quadword, p, AsmOperand::Reg(AsmReg::R8)),
        AsmInstruction::Mov(AsmType::Quadword, i, AsmOperand::Reg(AsmReg::R9)),
    ];
    let scale = if [1, 2, 4, 8].contains(&x.scale) {
        x.scale
    } else {
        o.push(binary(
            AsmBinaryOperator::Mult,
            AsmType::Quadword,
            AsmOperand::Imm(x.scale as i64),
            AsmOperand::Reg(AsmReg::R9),
        ));
        1
    };
    o.push(AsmInstruction::Lea(
        AsmOperand::Indexed(AsmIndexedOperand {
            base: AsmReg::R8,
            index: AsmReg::R9,
            scale,
        }),
        d,
    ));
    o
}

fn pass_params(ctx: &mut Context, params: &[TackyVal], stack_ret: bool) -> Vec<AsmInstruction> {
    let (ints, dbls, stack) = classify_parameters(ctx, params, stack_ret);
    let mut out = vec![];
    let regs = if stack_ret {
        out.push(AsmInstruction::Mov(
            AsmType::Quadword,
            AsmOperand::Reg(AsmReg::DI),
            AsmOperand::Memory(AsmReg::BP, -8),
        ));
        &INT_PARAM_REGS[1..]
    } else {
        &INT_PARAM_REGS[..]
    };
    for (i, (t, p)) in ints.into_iter().enumerate() {
        match t {
            AsmType::ByteArray(x) => out.extend(copyBytesFromReg(regs[i], p, x.size as usize)),
            _ => out.push(AsmInstruction::Mov(t, AsmOperand::Reg(regs[i]), p)),
        }
    }
    for (i, p) in dbls.into_iter().enumerate() {
        out.push(AsmInstruction::Mov(
            AsmType::Double,
            AsmOperand::Reg(DBL_PARAM_REGS[i]),
            p,
        ))
    }
    for (i, (t, p)) in stack.into_iter().enumerate() {
        let s = AsmOperand::Memory(AsmReg::BP, 16 + i as i32 * 8);
        match t {
            AsmType::ByteArray(x) => out.extend(copyBytes(s, p, x.size as usize)),
            _ => out.push(AsmInstruction::Mov(t, s, p)),
        }
    }
    out
}
fn returns_on_stack(ctx: &mut Context, name: &str) -> bool {
    match Symbols::get(name).symType {
        Type::Function(_, ret) => match *ret {
            Type::Struct(tag) => ctx.classify_structure(&tag).first() == Some(&ParamClass::Mem),
            _ => false,
        },
        _ => panic!("Internal error: not a function name"),
    }
}
fn var_alignment(t: &Type) -> i32 {
    if matches!(t, Type::Array(..)) && TypeUtils::get_size(t) >= 16 {
        16
    } else {
        TypeUtils::get_alignment(t) as i32
    }
}
fn var_type(t: &Type) -> AsmType {
    if matches!(t, Type::Array(..)) {
        AsmType::ByteArray(AsmByteArrayInfo {
            size: TypeUtils::get_size(t) as i32,
            alignment: var_alignment(t),
        })
    } else {
        convertType(t)
    }
}
fn convert_top(ctx: &mut Context, top: &TackyTopLevel) -> AsmTopLevel {
    match top {
        TackyTopLevel::Function(f) => {
            let stack = returns_on_stack(ctx, &f.name);
            let params: Vec<_> = f.params.iter().cloned().map(TackyVal::Var).collect();
            let mut instructions = pass_params(ctx, &params, stack);
            for i in &f.body {
                instructions.extend(convert_instruction(ctx, i))
            }
            AsmTopLevel::Function(AsmFunctionDef {
                name: f.name.clone(),
                global: f.global,
                instructions,
            })
        }
        TackyTopLevel::StaticVariable(v) => AsmTopLevel::StaticVariable(AsmStaticVariableDef {
            name: v.name.clone(),
            global: v.global,
            alignment: var_alignment(&v.t),
            init: v.init.clone(),
        }),
        TackyTopLevel::StaticConstant(v) => AsmTopLevel::StaticConstant(AsmStaticConstantDef {
            name: v.name.clone(),
            alignment: TypeUtils::get_alignment(&v.t) as i32,
            init: v.init.clone(),
        }),
    }
}
fn classify_return_type(ctx: &mut Context, t: &Type) -> (Vec<AsmReg>, bool) {
    if *t == Type::Void {
        return (vec![], false);
    }
    let op = if TypeUtils::is_scalar(t) {
        AsmOperand::Pseudo("dummy".into())
    } else {
        AsmOperand::PseudoMem("dummy".into(), 0)
    };
    let (i, d, s) = classify_return(ctx, t, op);
    if s {
        (vec![AsmReg::AX], true)
    } else {
        (
            [AsmReg::AX, AsmReg::DX][..i.len()]
                .iter()
                .chain([AsmReg::XMM0, AsmReg::XMM1][..d.len()].iter())
                .copied()
                .collect(),
            false,
        )
    }
}
fn convert_symbol(ctx: &mut Context, name: &str, e: &SymbolEntry) {
    match (&e.symType, &e.attrs) {
        (Type::Function(params, ret), IdentifierAttrs::FunAttr(a))
            if (*ret.as_ref() == Type::Void || TypeUtils::is_complete(ret))
                && params.iter().all(TypeUtils::is_complete) =>
        {
            let (rr, s) = classify_return_type(ctx, ret);
            let vals: Vec<_> = params
                .iter()
                .map(|t| {
                    (
                        t.clone(),
                        if TypeUtils::is_scalar(t) {
                            AsmOperand::Pseudo("dummy".into())
                        } else {
                            AsmOperand::PseudoMem("dummy".into(), 0)
                        },
                    )
                })
                .collect();
            let (i, d, _) = classify_params(ctx, &vals, s);
            let pr = INT_PARAM_REGS[..i.len()]
                .iter()
                .chain(DBL_PARAM_REGS[..d.len()].iter())
                .copied()
                .collect();
            AssemblySymbols::addFun(name, a.defined, returns_on_stack(ctx, name), pr, rr)
        }
        (Type::Function(..), IdentifierAttrs::FunAttr(a)) => {
            assert!(!a.defined);
            AssemblySymbols::addFun(name, a.defined, false, vec![], vec![])
        }
        (t, IdentifierAttrs::ConstAttr(_)) => AssemblySymbols::addConstant(name, convertType(t)),
        (t, IdentifierAttrs::StaticAttr(_)) => AssemblySymbols::addVar(
            name,
            if TypeUtils::is_complete(t) {
                var_type(t)
            } else {
                AsmType::Byte
            },
            true,
        ),
        (t, _) => AssemblySymbols::addVar(name, var_type(t), false),
    }
}

/// Generate assembly and return the updated unique-id counter.
pub fn gen(counter: Counter, program: TackyProgram) -> (Counter, AsmProgram) {
    let mut ctx = Context {
        counter,
        ..Default::default()
    };
    let tops: Vec<_> = program.0.iter().map(|t| convert_top(&mut ctx, t)).collect();
    let mut constants = Vec::with_capacity(ctx.constants.len());
    for (key, (name, alignment)) in &ctx.constants {
        let value = f64::from_bits(*key as u64);
        AssemblySymbols::addConstant(name, AsmType::Double);
        constants.push(AsmTopLevel::StaticConstant(AsmStaticConstantDef {
            name: name.clone(),
            alignment: *alignment,
            init: StaticInit::DoubleInit(value),
        }));
    }
    for (name, e) in Symbols::bindings() {
        convert_symbol(&mut ctx, &name, &e)
    }
    constants.extend(tops);
    (ctx.counter, AsmProgram::Program(constants))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chooses_the_original_eightbyte_move_types() {
        assert_eq!(getEightbyteType(0, 16), AsmType::Quadword);
        assert_eq!(getEightbyteType(1, 12), AsmType::Longword);
        assert_eq!(getEightbyteType(1, 9), AsmType::Byte);
        assert_eq!(
            getEightbyteType(1, 11),
            AsmType::ByteArray(AsmByteArrayInfo {
                size: 3,
                alignment: 8,
            })
        );
    }

    #[test]
    fn copies_uneven_objects_using_largest_legal_moves() {
        assert_eq!(
            copyBytes(
                AsmOperand::PseudoMem("source".into(), 0),
                AsmOperand::PseudoMem("dest".into(), 0),
                13,
            ),
            vec![
                AsmInstruction::Mov(
                    AsmType::Quadword,
                    AsmOperand::PseudoMem("source".into(), 0),
                    AsmOperand::PseudoMem("dest".into(), 0),
                ),
                AsmInstruction::Mov(
                    AsmType::Longword,
                    AsmOperand::PseudoMem("source".into(), 8),
                    AsmOperand::PseudoMem("dest".into(), 8),
                ),
                AsmInstruction::Mov(
                    AsmType::Byte,
                    AsmOperand::PseudoMem("source".into(), 12),
                    AsmOperand::PseudoMem("dest".into(), 12),
                ),
            ]
        );
    }
}
