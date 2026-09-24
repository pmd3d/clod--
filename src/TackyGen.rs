//! Lowering from the typed AST to the three-address TACKY IR.
#![allow(non_snake_case)]

use super::{
    Ast::*,
    Const::{Constant, INT_ONE, INT_ZERO},
    Initializers::StaticInit,
    Symbols::{self, IdentifierAttrs, InitialValue},
    Tacky::*,
    TypeTable, TypeUtils,
    Types::Type,
    UniqueIds::{self, Counter},
};

pub fn breakLabel(id: &str) -> String {
    format!("break.{id}")
}
pub fn continueLabel(id: &str) -> String {
    format!("continue.{id}")
}
fn dummy_operand() -> TackyVal {
    TackyVal::Constant(INT_ZERO)
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExpResult {
    PlainOperand(TackyVal),
    DereferencedPointer(TackyVal),
    SubObject(String, i32),
}

struct Generator {
    counter: Counter,
}
impl Generator {
    fn label(&mut self, prefix: &str) -> String {
        let (c, n) = UniqueIds::make_label(prefix, self.counter);
        self.counter = c;
        n
    }
    fn tmp(&mut self, t: Type) -> TackyVal {
        let (c, n) = UniqueIds::make_temporary(self.counter);
        self.counter = c;
        Symbols::addAutomaticVar(n.clone(), t);
        TackyVal::Var(n)
    }
    fn ptr_scale(t: &Type) -> i32 {
        match t {
            Type::Pointer(r) => TypeUtils::get_size(r) as i32,
            _ => panic!("Internal error: tried to get scale of non-pointer type: {t:?}"),
        }
    }
    fn member_offset(member: &str, t: &Type) -> i32 {
        match t { Type::Struct(tag) => TypeTable::find(tag).members.get(member).unwrap_or_else(|| panic!("Internal error: failed to find member {member} in structure {tag}")).offset as i32, _ => panic!("Internal error: tried to get offset of member {member} within non-structure type {t:?}") }
    }
    fn member_ptr_offset(member: &str, t: &Type) -> i32 {
        match t { Type::Pointer(inner) => Self::member_offset(member, inner), _ => panic!("Internal error: trying to get member through pointer but {t:?} is not a pointer type") }
    }

    fn exp(&mut self, exp: &TypedExp) -> (Vec<TackyInstruction>, ExpResult) {
        use TypedInnerExp as E;
        match &exp.e {
            E::Constant(c) => (vec![], ExpResult::PlainOperand(TackyVal::Constant(*c))),
            E::Var(v) => (vec![], ExpResult::PlainOperand(TackyVal::Var(v.clone()))),
            E::String(s) => {
                let mut ids = super::UniqueIds::UniqueIds::new(self.counter);
                let name = Symbols::addStringWithCounter(&mut ids, s.clone());
                self.counter = ids.counter();
                (vec![], ExpResult::PlainOperand(TackyVal::Var(name)))
            }
            E::Cast(target, inner) => self.cast(target, inner),
            E::Unary(op, inner) => {
                let (mut code, src) = self.convert(inner);
                let dst = self.tmp(exp.t.clone());
                code.push(TackyInstruction::Unary(TackyUnaryInfo {
                    op: match op {
                        UnaryOperator::Complement => TackyUnaryOperator::Complement,
                        UnaryOperator::Negate => TackyUnaryOperator::Negate,
                        UnaryOperator::Not => TackyUnaryOperator::Not,
                    },
                    src,
                    dst: dst.clone(),
                }));
                (code, ExpResult::PlainOperand(dst))
            }
            E::Binary(BinaryOperator::And, a, b) => self.logical(a, b, false),
            E::Binary(BinaryOperator::Or, a, b) => self.logical(a, b, true),
            E::Binary(BinaryOperator::Add, a, b) if TypeUtils::is_pointer(&exp.t) => {
                self.pointer_add(&exp.t, a, b)
            }
            E::Binary(BinaryOperator::Subtract, p, i) if TypeUtils::is_pointer(&exp.t) => {
                self.pointer_sub(&exp.t, p, i)
            }
            E::Binary(BinaryOperator::Subtract, a, b) if TypeUtils::is_pointer(&a.t) => {
                self.pointer_diff(&exp.t, a, b)
            }
            E::Binary(op, a, b) => self.binary(&exp.t, *op, a, b),
            E::Assignment(lhs, rhs) => self.assignment(lhs, rhs),
            E::Conditional(c, a, b) => self.conditional(&exp.t, c, a, b),
            E::FunCall(f, args) => self.call(&exp.t, f, args),
            E::Dereference(inner) => {
                let (code, ptr) = self.convert(inner);
                (code, ExpResult::DereferencedPointer(ptr))
            }
            E::AddrOf(inner) => self.addr_of(&exp.t, inner),
            E::Subscript(a, b) => {
                let (code, result) =
                    self.pointer_add(&Type::Pointer(Box::new(exp.t.clone())), a, b);
                match result {
                    ExpResult::PlainOperand(v) => (code, ExpResult::DereferencedPointer(v)),
                    _ => unreachable!(),
                }
            }
            E::SizeOfT(t) => (
                vec![],
                ExpResult::PlainOperand(TackyVal::Constant(Constant::ULong(
                    TypeUtils::get_size(t) as u64,
                ))),
            ),
            E::SizeOf(inner) => (
                vec![],
                ExpResult::PlainOperand(TackyVal::Constant(Constant::ULong(TypeUtils::get_size(
                    &inner.t,
                )
                    as u64))),
            ),
            E::Dot(inner, member) => self.dot(&exp.t, inner, member),
            E::Arrow(inner, member) => self.arrow(&exp.t, inner, member),
        }
    }
    fn convert(&mut self, e: &TypedExp) -> (Vec<TackyInstruction>, TackyVal) {
        let (mut code, result) = self.exp(e);
        match result {
            ExpResult::PlainOperand(v) => (code, v),
            ExpResult::DereferencedPointer(src_ptr) => {
                let dst = self.tmp(e.t.clone());
                code.push(TackyInstruction::Load(TackyLoadInfo {
                    src_ptr,
                    dst: dst.clone(),
                }));
                (code, dst)
            }
            ExpResult::SubObject(src, offset) => {
                let dst = self.tmp(e.t.clone());
                code.push(TackyInstruction::CopyFromOffset(TackyCopyFromOffsetInfo {
                    src,
                    offset,
                    dst: dst.clone(),
                }));
                (code, dst)
            }
        }
    }
    fn cast(&mut self, target: &Type, inner: &TypedExp) -> (Vec<TackyInstruction>, ExpResult) {
        let (mut code, src) = self.convert(inner);
        if target == &inner.t || target == &Type::Void {
            return (code, ExpResult::PlainOperand(src));
        }
        let dst = self.tmp(target.clone());
        let pair = TackySrcDst {
            src,
            dst: dst.clone(),
        };
        let ins = if target == &Type::Double {
            if TypeUtils::is_signed(&inner.t) {
                TackyInstruction::IntToDouble(pair)
            } else {
                TackyInstruction::UIntToDouble(pair)
            }
        } else if inner.t == Type::Double {
            if TypeUtils::is_signed(target) {
                TackyInstruction::DoubleToInt(pair)
            } else {
                TackyInstruction::DoubleToUInt(pair)
            }
        } else {
            let a = TypeUtils::get_size(target);
            let b = TypeUtils::get_size(&inner.t);
            if a == b {
                TackyInstruction::Copy(pair)
            } else if a < b {
                TackyInstruction::Truncate(pair)
            } else if TypeUtils::is_signed(&inner.t) {
                TackyInstruction::SignExtend(pair)
            } else {
                TackyInstruction::ZeroExtend(pair)
            }
        };
        code.push(ins);
        (code, ExpResult::PlainOperand(dst))
    }
    fn pointer_add(
        &mut self,
        t: &Type,
        a: &TypedExp,
        b: &TypedExp,
    ) -> (Vec<TackyInstruction>, ExpResult) {
        let (mut x, v1) = self.convert(a);
        let (y, v2) = self.convert(b);
        x.extend(y);
        let dst = self.tmp(t.clone());
        let (ptr, index) = if &a.t == t { (v1, v2) } else { (v2, v1) };
        x.push(TackyInstruction::AddPtr(TackyAddPtrInfo {
            ptr,
            index,
            scale: Self::ptr_scale(t),
            dst: dst.clone(),
        }));
        (x, ExpResult::PlainOperand(dst))
    }
    fn pointer_sub(
        &mut self,
        t: &Type,
        p: &TypedExp,
        i: &TypedExp,
    ) -> (Vec<TackyInstruction>, ExpResult) {
        let (mut code, ptr) = self.convert(p);
        let (c, index) = self.convert(i);
        code.extend(c);
        let dst = self.tmp(t.clone());
        let neg = self.tmp(Type::Long);
        code.push(TackyInstruction::Unary(TackyUnaryInfo {
            op: TackyUnaryOperator::Negate,
            src: index,
            dst: neg.clone(),
        }));
        code.push(TackyInstruction::AddPtr(TackyAddPtrInfo {
            ptr,
            index: neg,
            scale: Self::ptr_scale(t),
            dst: dst.clone(),
        }));
        (code, ExpResult::PlainOperand(dst))
    }
    fn pointer_diff(
        &mut self,
        t: &Type,
        a: &TypedExp,
        b: &TypedExp,
    ) -> (Vec<TackyInstruction>, ExpResult) {
        let (mut code, v1) = self.convert(a);
        let (c, v2) = self.convert(b);
        code.extend(c);
        let diff = self.tmp(Type::Long);
        let dst = self.tmp(t.clone());
        code.push(TackyInstruction::Binary(TackyBinaryInfo {
            op: TackyBinaryOperator::Subtract,
            src1: v1,
            src2: v2,
            dst: diff.clone(),
        }));
        code.push(TackyInstruction::Binary(TackyBinaryInfo {
            op: TackyBinaryOperator::Divide,
            src1: diff,
            src2: TackyVal::Constant(Constant::Long(Self::ptr_scale(&a.t) as i64)),
            dst: dst.clone(),
        }));
        (code, ExpResult::PlainOperand(dst))
    }
    fn binary(
        &mut self,
        t: &Type,
        op: BinaryOperator,
        a: &TypedExp,
        b: &TypedExp,
    ) -> (Vec<TackyInstruction>, ExpResult) {
        let (mut code, v1) = self.convert(a);
        let (c, v2) = self.convert(b);
        code.extend(c);
        let dst = self.tmp(t.clone());
        let op = match op {
            BinaryOperator::Add => TackyBinaryOperator::Add,
            BinaryOperator::Subtract => TackyBinaryOperator::Subtract,
            BinaryOperator::Multiply => TackyBinaryOperator::Multiply,
            BinaryOperator::Divide => TackyBinaryOperator::Divide,
            BinaryOperator::Mod => TackyBinaryOperator::Mod,
            BinaryOperator::Equal => TackyBinaryOperator::Equal,
            BinaryOperator::NotEqual => TackyBinaryOperator::NotEqual,
            BinaryOperator::LessThan => TackyBinaryOperator::LessThan,
            BinaryOperator::LessOrEqual => TackyBinaryOperator::LessOrEqual,
            BinaryOperator::GreaterThan => TackyBinaryOperator::GreaterThan,
            BinaryOperator::GreaterOrEqual => TackyBinaryOperator::GreaterOrEqual,
            _ => panic!("cannot directly convert logical operator"),
        };
        code.push(TackyInstruction::Binary(TackyBinaryInfo {
            op,
            src1: v1,
            src2: v2,
            dst: dst.clone(),
        }));
        (code, ExpResult::PlainOperand(dst))
    }
    fn logical(
        &mut self,
        a: &TypedExp,
        b: &TypedExp,
        is_or: bool,
    ) -> (Vec<TackyInstruction>, ExpResult) {
        let (mut code, v1) = self.convert(a);
        let (c, v2) = self.convert(b);
        let branch = self.label(if is_or { "or_true" } else { "and_false" });
        let end = self.label(if is_or { "or_end" } else { "and_end" });
        let dst = self.tmp(Type::Int);
        code.push(if is_or {
            TackyInstruction::JumpIfNotZero(v1, branch.clone())
        } else {
            TackyInstruction::JumpIfZero(v1, branch.clone())
        });
        code.extend(c);
        code.push(if is_or {
            TackyInstruction::JumpIfNotZero(v2, branch.clone())
        } else {
            TackyInstruction::JumpIfZero(v2, branch.clone())
        });
        code.push(TackyInstruction::Copy(TackySrcDst {
            src: TackyVal::Constant(if is_or { INT_ZERO } else { INT_ONE }),
            dst: dst.clone(),
        }));
        code.push(TackyInstruction::Jump(end.clone()));
        code.push(TackyInstruction::Label(branch));
        code.push(TackyInstruction::Copy(TackySrcDst {
            src: TackyVal::Constant(if is_or { INT_ONE } else { INT_ZERO }),
            dst: dst.clone(),
        }));
        code.push(TackyInstruction::Label(end));
        (code, ExpResult::PlainOperand(dst))
    }
    fn assignment(&mut self, lhs: &TypedExp, rhs: &TypedExp) -> (Vec<TackyInstruction>, ExpResult) {
        let (mut code, lval) = self.exp(lhs);
        let (c, rval) = self.convert(rhs);
        code.extend(c);
        match lval {
            ExpResult::PlainOperand(dst) => {
                code.push(TackyInstruction::Copy(TackySrcDst {
                    src: rval,
                    dst: dst.clone(),
                }));
                (code, ExpResult::PlainOperand(dst))
            }
            ExpResult::DereferencedPointer(dst_ptr) => {
                code.push(TackyInstruction::Store(TackyStoreInfo {
                    src: rval.clone(),
                    dst_ptr,
                }));
                (code, ExpResult::PlainOperand(rval))
            }
            ExpResult::SubObject(dst, offset) => {
                code.push(TackyInstruction::CopyToOffset(TackyCopyToOffsetInfo {
                    src: rval.clone(),
                    dst,
                    offset,
                }));
                (code, ExpResult::PlainOperand(rval))
            }
        }
    }
    fn conditional(
        &mut self,
        t: &Type,
        cond: &TypedExp,
        a: &TypedExp,
        b: &TypedExp,
    ) -> (Vec<TackyInstruction>, ExpResult) {
        let (mut code, c) = self.convert(cond);
        let (ca, va) = self.convert(a);
        let (cb, vb) = self.convert(b);
        let els = self.label("conditional_else");
        let end = self.label("conditional_end");
        let dst = if t == &Type::Void {
            dummy_operand()
        } else {
            self.tmp(t.clone())
        };
        code.push(TackyInstruction::JumpIfZero(c, els.clone()));
        code.extend(ca);
        if t != &Type::Void {
            code.push(TackyInstruction::Copy(TackySrcDst {
                src: va,
                dst: dst.clone(),
            }));
        }
        code.push(TackyInstruction::Jump(end.clone()));
        code.push(TackyInstruction::Label(els));
        code.extend(cb);
        if t != &Type::Void {
            code.push(TackyInstruction::Copy(TackySrcDst {
                src: vb,
                dst: dst.clone(),
            }));
        }
        code.push(TackyInstruction::Label(end));
        (code, ExpResult::PlainOperand(dst))
    }
    fn call(&mut self, t: &Type, f: &str, args: &[TypedExp]) -> (Vec<TackyInstruction>, ExpResult) {
        let dst = if t == &Type::Void {
            None
        } else {
            Some(self.tmp(t.clone()))
        };
        let mut code = vec![];
        let mut vals = vec![];
        for a in args {
            let (c, v) = self.convert(a);
            code.extend(c);
            vals.push(v)
        }
        code.push(TackyInstruction::FunCall(TackyFunCallInfo {
            f: f.into(),
            args: vals,
            dst: dst.clone(),
        }));
        (
            code,
            ExpResult::PlainOperand(dst.unwrap_or_else(dummy_operand)),
        )
    }
    fn dot(
        &mut self,
        t: &Type,
        obj: &TypedExp,
        member: &str,
    ) -> (Vec<TackyInstruction>, ExpResult) {
        let off = Self::member_offset(member, &obj.t);
        let (mut code, r) = self.exp(obj);
        match r {
            ExpResult::PlainOperand(TackyVal::Var(v)) => (code, ExpResult::SubObject(v, off)),
            ExpResult::SubObject(v, o) => (code, ExpResult::SubObject(v, o + off)),
            ExpResult::DereferencedPointer(ptr) => {
                if off == 0 {
                    return (code, ExpResult::DereferencedPointer(ptr));
                }
                let dst = self.tmp(Type::Pointer(Box::new(t.clone())));
                code.push(TackyInstruction::AddPtr(TackyAddPtrInfo {
                    ptr,
                    index: TackyVal::Constant(Constant::Long(off as i64)),
                    scale: 1,
                    dst: dst.clone(),
                }));
                (code, ExpResult::DereferencedPointer(dst))
            }
            ExpResult::PlainOperand(TackyVal::Constant(_)) => {
                panic!("Internal error: dot operator applied to constant")
            }
        }
    }
    fn arrow(
        &mut self,
        t: &Type,
        obj: &TypedExp,
        member: &str,
    ) -> (Vec<TackyInstruction>, ExpResult) {
        let off = Self::member_ptr_offset(member, &obj.t);
        let (mut code, ptr) = self.convert(obj);
        if off == 0 {
            return (code, ExpResult::DereferencedPointer(ptr));
        }
        let dst = self.tmp(Type::Pointer(Box::new(t.clone())));
        code.push(TackyInstruction::AddPtr(TackyAddPtrInfo {
            ptr,
            index: TackyVal::Constant(Constant::Long(off as i64)),
            scale: 1,
            dst: dst.clone(),
        }));
        (code, ExpResult::DereferencedPointer(dst))
    }
    fn addr_of(&mut self, t: &Type, inner: &TypedExp) -> (Vec<TackyInstruction>, ExpResult) {
        let (mut code, r) = self.exp(inner);
        match r {
            ExpResult::PlainOperand(src) => {
                let dst = self.tmp(t.clone());
                code.push(TackyInstruction::GetAddress(TackySrcDst {
                    src,
                    dst: dst.clone(),
                }));
                (code, ExpResult::PlainOperand(dst))
            }
            ExpResult::DereferencedPointer(ptr) => (code, ExpResult::PlainOperand(ptr)),
            ExpResult::SubObject(base, off) => {
                let dst = self.tmp(t.clone());
                code.push(TackyInstruction::GetAddress(TackySrcDst {
                    src: TackyVal::Var(base),
                    dst: dst.clone(),
                }));
                if off != 0 {
                    code.push(TackyInstruction::AddPtr(TackyAddPtrInfo {
                        ptr: dst.clone(),
                        index: TackyVal::Constant(Constant::Long(off as i64)),
                        scale: 1,
                        dst: dst.clone(),
                    }))
                }
                (code, ExpResult::PlainOperand(dst))
            }
        }
    }

    fn string_init(dst: &str, mut offset: i32, bytes: &[u8]) -> Vec<TackyInstruction> {
        let mut out = vec![];
        let mut p = 0;
        while p < bytes.len() {
            let remain = bytes.len() - p;
            let (n, c) = if remain >= 8 {
                (
                    8,
                    Constant::Long(i64::from_le_bytes(bytes[p..p + 8].try_into().unwrap())),
                )
            } else if remain >= 4 {
                (
                    4,
                    Constant::Int(i32::from_le_bytes(bytes[p..p + 4].try_into().unwrap())),
                )
            } else {
                (1, Constant::Char(bytes[p] as i8))
            };
            out.push(TackyInstruction::CopyToOffset(TackyCopyToOffsetInfo {
                src: TackyVal::Constant(c),
                dst: dst.into(),
                offset,
            }));
            p += n;
            offset += n as i32
        }
        out
    }
    fn compound_init(
        &mut self,
        name: &str,
        offset: i32,
        init: &TypedInitializer,
    ) -> Vec<TackyInstruction> {
        match init {
            TypedInitializer::SingleInit(TypedExp {
                e: TypedInnerExp::String(s),
                t: Type::Array(_, size),
            }) => {
                let mut bytes = s.as_bytes().to_vec();
                // Match F#'s `String.length`: padding is based on UTF-16 code
                // units, while `Bytes.ofString` contributes UTF-8 bytes.
                let padding = size.saturating_sub(s.encode_utf16().count());
                bytes.extend(std::iter::repeat_n(0, padding));
                Self::string_init(name, offset, &bytes)
            }
            TypedInitializer::SingleInit(e) => {
                let (mut c, v) = self.convert(e);
                c.push(TackyInstruction::CopyToOffset(TackyCopyToOffsetInfo {
                    src: v,
                    dst: name.into(),
                    offset,
                }));
                c
            }
            TypedInitializer::CompoundInit(Type::Array(elem, _), xs) => {
                let mut out = vec![];
                for (i, x) in xs.iter().enumerate() {
                    out.extend(self.compound_init(
                        name,
                        offset + i as i32 * TypeUtils::get_size(elem) as i32,
                        x,
                    ))
                }
                out
            }
            TypedInitializer::CompoundInit(Type::Struct(tag), xs) => {
                let members = TypeTable::get_members(tag);
                let mut out = vec![];
                for (m, x) in members.iter().zip(xs) {
                    out.extend(self.compound_init(name, offset + m.offset as i32, x))
                }
                out
            }
            TypedInitializer::CompoundInit(_, _) => {
                panic!("Internal error: compound init has non-array type!")
            }
        }
    }
    fn declaration(&mut self, d: &TypedDeclaration) -> Vec<TackyInstruction> {
        match d {
            TypedDeclaration::VarDecl(v) if v.storageClass.is_none() => match &v.init {
                None => vec![],
                Some(
                    i @ TypedInitializer::SingleInit(TypedExp {
                        e: TypedInnerExp::String(_),
                        t: Type::Array(_, _),
                    }),
                ) => self.compound_init(&v.name, 0, i),
                Some(TypedInitializer::SingleInit(e)) => {
                    let lhs = TypedExp {
                        e: TypedInnerExp::Var(v.name.clone()),
                        t: v.varType.clone(),
                    };
                    self.assignment(&lhs, e).0
                }
                Some(i) => self.compound_init(&v.name, 0, i),
            },
            _ => vec![],
        }
    }
    fn block(&mut self, b: &TypedBlock) -> Vec<TackyInstruction> {
        let mut out = vec![];
        for item in &b.0 {
            out.extend(match item {
                TypedBlockItem::Stmt(s) => self.statement(s),
                TypedBlockItem::Decl(d) => self.declaration(d),
            })
        }
        out
    }
    fn statement(&mut self, s: &TypedStatement) -> Vec<TackyInstruction> {
        match s {
            TypedStatement::Return(e) => {
                let (mut c, v) = match e {
                    Some(e) => {
                        let (c, v) = self.convert(e);
                        (c, Some(v))
                    }
                    None => (vec![], None),
                };
                c.push(TackyInstruction::Return(v));
                c
            }
            TypedStatement::Expression(e) => self.exp(e).0,
            TypedStatement::Compound(b) => self.block(b),
            TypedStatement::Break(id) => vec![TackyInstruction::Jump(breakLabel(id))],
            TypedStatement::Continue(id) => vec![TackyInstruction::Jump(continueLabel(id))],
            TypedStatement::Null => vec![],
            TypedStatement::If(c, a, b) => {
                let (mut out, v) = self.convert(c);
                let el = self.label(if b.is_some() { "else" } else { "if_end" });
                let end = if b.is_some() {
                    self.label("")
                } else {
                    el.clone()
                };
                out.push(TackyInstruction::JumpIfZero(v, el.clone()));
                out.extend(self.statement(a));
                if let Some(b) = b {
                    out.push(TackyInstruction::Jump(end.clone()));
                    out.push(TackyInstruction::Label(el));
                    out.extend(self.statement(b))
                }
                out.push(TackyInstruction::Label(end));
                out
            }
            TypedStatement::DoWhile(body, cond, id) => {
                let start = self.label("do_loop_start");
                let mut out = vec![TackyInstruction::Label(start.clone())];
                out.extend(self.statement(body));
                out.push(TackyInstruction::Label(continueLabel(id)));
                let (c, v) = self.convert(cond);
                out.extend(c);
                out.push(TackyInstruction::JumpIfNotZero(v, start));
                out.push(TackyInstruction::Label(breakLabel(id)));
                out
            }
            TypedStatement::While(cond, body, id) => {
                let cont = continueLabel(id);
                let br = breakLabel(id);
                let (mut c, v) = self.convert(cond);
                let mut out = vec![TackyInstruction::Label(cont.clone())];
                out.append(&mut c);
                out.push(TackyInstruction::JumpIfZero(v, br.clone()));
                out.extend(self.statement(body));
                out.push(TackyInstruction::Jump(cont));
                out.push(TackyInstruction::Label(br));
                out
            }
            TypedStatement::For(init, cond, post, body, id) => {
                let start = self.label("for_start");
                let cont = continueLabel(id);
                let br = breakLabel(id);
                let mut out = match init {
                    TypedForInit::InitDecl(d) => {
                        self.declaration(&TypedDeclaration::VarDecl(d.clone()))
                    }
                    TypedForInit::InitExp(Some(e)) => self.exp(e).0,
                    TypedForInit::InitExp(None) => vec![],
                };
                out.push(TackyInstruction::Label(start.clone()));
                if let Some(c) = cond {
                    let (x, v) = self.convert(c);
                    out.extend(x);
                    out.push(TackyInstruction::JumpIfZero(v, br.clone()))
                }
                out.extend(self.statement(body));
                out.push(TackyInstruction::Label(cont));
                if let Some(p) = post {
                    out.extend(self.exp(p).0)
                }
                out.push(TackyInstruction::Jump(start));
                out.push(TackyInstruction::Label(br));
                out
            }
        }
    }
}

pub fn convertSymbolsToTacky(symbols: Vec<(String, Symbols::SymbolEntry)>) -> Vec<TackyTopLevel> {
    symbols
        .into_iter()
        .filter_map(|(name, e)| match e.attrs {
            IdentifierAttrs::StaticAttr(a) => match a.init {
                InitialValue::Initial(init) => {
                    Some(TackyTopLevel::StaticVariable(TackyStaticVariableDef {
                        name,
                        t: e.symType,
                        global: a.global,
                        init,
                    }))
                }
                InitialValue::Tentative => {
                    Some(TackyTopLevel::StaticVariable(TackyStaticVariableDef {
                        name,
                        t: e.symType.clone(),
                        global: a.global,
                        init: zero_initializer(&e.symType),
                    }))
                }
                InitialValue::NoInitializer => None,
            },
            IdentifierAttrs::ConstAttr(init) => {
                Some(TackyTopLevel::StaticConstant(TackyStaticConstantDef {
                    name,
                    t: e.symType,
                    init,
                }))
            }
            _ => None,
        })
        .collect()
}
fn zero_initializer(t: &Type) -> Vec<StaticInit> {
    vec![StaticInit::ZeroInit(TypeUtils::get_size(t) as i32)]
}
pub fn gen(counter: Counter, program: TypedProgram) -> (Counter, TackyProgram) {
    let mut g = Generator { counter };
    let mut funcs = vec![];
    for d in program.0 {
        if let TypedDeclaration::FunDecl(f) = d {
            if let Some(body) = f.body {
                let mut code = g.block(&body);
                code.push(TackyInstruction::Return(Some(TackyVal::Constant(INT_ZERO))));
                funcs.push(TackyTopLevel::Function(TackyFunctionDef {
                    name: f.name.clone(),
                    global: Symbols::isGlobal(&f.name),
                    params: f.params,
                    body: code,
                }))
            }
        }
    }
    let mut defs = convertSymbolsToTacky(Symbols::bindings());
    defs.extend(funcs);
    (g.counter, TackyProgram(defs))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    fn int(e: TypedInnerExp) -> TypedExp {
        TypedExp { e, t: Type::Int }
    }

    #[test]
    fn lowers_short_circuit_expression_and_function() {
        Symbols::setTable(BTreeMap::new());
        Symbols::addFun(
            "main",
            Type::Function(vec![], Box::new(Type::Int)),
            true,
            true,
        );
        let expression = int(TypedInnerExp::Binary(
            BinaryOperator::And,
            Box::new(int(TypedInnerExp::Constant(Constant::Int(1)))),
            Box::new(int(TypedInnerExp::Constant(Constant::Int(0)))),
        ));
        let program = TypedProgram(vec![TypedDeclaration::FunDecl(TypedFunctionDeclaration {
            name: "main".into(),
            funType: Type::Function(vec![], Box::new(Type::Int)),
            params: vec![],
            body: Some(TypedBlock(vec![TypedBlockItem::Stmt(
                TypedStatement::Return(Some(expression)),
            )])),
            storageClass: None,
        })]);
        let (_, TackyProgram(definitions)) = gen(0, program);
        let TackyTopLevel::Function(function) = &definitions[0] else {
            panic!("expected function")
        };
        assert!(function
            .body
            .iter()
            .any(|i| matches!(i, TackyInstruction::JumpIfZero(_, _))));
        assert!(function
            .body
            .iter()
            .any(|i| matches!(i, TackyInstruction::Label(l) if l.starts_with("and_false."))));
    }

    #[test]
    fn converts_tentative_static_to_zero_initializer() {
        Symbols::setTable(BTreeMap::new());
        Symbols::addStaticVar("object", Type::Long, false, InitialValue::Tentative);
        let definitions = convertSymbolsToTacky(Symbols::bindings());
        assert!(
            matches!(&definitions[0], TackyTopLevel::StaticVariable(v) if v.init == vec![StaticInit::ZeroInit(8)])
        );
    }
}
