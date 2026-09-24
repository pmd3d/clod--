//! Three-address ("TACKY") intermediate representation.
#![allow(non_snake_case)]

use std::cmp::Ordering;
use std::io::{self, Write};

use super::{
    Const::{self, ConstValue},
    Initializers::StaticInit,
    Symbols,
    Types::Type,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TackyUnaryOperator {
    Complement,
    Negate,
    Not,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TackyBinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Mod,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TackyVal {
    Constant(ConstValue),
    Var(String),
}

/// Compare constants as map keys, keeping positive and negative zero distinct.
///
/// The ordinary constant comparison deliberately considers the two zeroes
/// equal.  TACKY optimizations use this comparison when the distinction is
/// observable (for example, after division by zero).
pub fn constCompare(a: &ConstValue, b: &ConstValue) -> Ordering {
    match (*a, *b) {
        (ConstValue::Double(left), ConstValue::Double(right)) if left == right => {
            left.signum().total_cmp(&right.signum())
        }
        _ => a.partial_cmp(b).expect("constants have a total ordering"),
    }
}

pub fn showTackyVal(value: &TackyVal) -> String {
    match value {
        TackyVal::Constant(constant) => Const::show(*constant),
        TackyVal::Var(name) => name.clone(),
    }
}

pub fn ppTackyVal(out: &mut impl Write, value: &TackyVal) -> io::Result<()> {
    out.write_all(showTackyVal(value).as_bytes())
}

pub fn typeOfVal(value: &TackyVal) -> Type {
    match value {
        TackyVal::Constant(value) => Const::type_of_const(*value),
        TackyVal::Var(name) => Symbols::get(name).symType,
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TackySrcDst {
    pub src: TackyVal,
    pub dst: TackyVal,
}
#[derive(Clone, Debug, PartialEq)]
pub struct TackyUnaryInfo {
    pub op: TackyUnaryOperator,
    pub src: TackyVal,
    pub dst: TackyVal,
}
#[derive(Clone, Debug, PartialEq)]
pub struct TackyBinaryInfo {
    pub op: TackyBinaryOperator,
    pub src1: TackyVal,
    pub src2: TackyVal,
    pub dst: TackyVal,
}
#[derive(Clone, Debug, PartialEq)]
pub struct TackyAddPtrInfo {
    pub ptr: TackyVal,
    pub index: TackyVal,
    pub scale: i32,
    pub dst: TackyVal,
}
#[derive(Clone, Debug, PartialEq)]
pub struct TackyCopyToOffsetInfo {
    pub src: TackyVal,
    pub dst: String,
    pub offset: i32,
}
#[derive(Clone, Debug, PartialEq)]
pub struct TackyCopyFromOffsetInfo {
    pub src: String,
    pub offset: i32,
    pub dst: TackyVal,
}
#[derive(Clone, Debug, PartialEq)]
pub struct TackyFunCallInfo {
    pub f: String,
    pub args: Vec<TackyVal>,
    pub dst: Option<TackyVal>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct TackyLoadInfo {
    pub src_ptr: TackyVal,
    pub dst: TackyVal,
}
#[derive(Clone, Debug, PartialEq)]
pub struct TackyStoreInfo {
    pub src: TackyVal,
    pub dst_ptr: TackyVal,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TackyInstruction {
    Return(Option<TackyVal>),
    SignExtend(TackySrcDst),
    ZeroExtend(TackySrcDst),
    DoubleToInt(TackySrcDst),
    IntToDouble(TackySrcDst),
    DoubleToUInt(TackySrcDst),
    UIntToDouble(TackySrcDst),
    Truncate(TackySrcDst),
    Unary(TackyUnaryInfo),
    Binary(TackyBinaryInfo),
    Copy(TackySrcDst),
    GetAddress(TackySrcDst),
    Load(TackyLoadInfo),
    Store(TackyStoreInfo),
    AddPtr(TackyAddPtrInfo),
    CopyToOffset(TackyCopyToOffsetInfo),
    CopyFromOffset(TackyCopyFromOffsetInfo),
    Jump(String),
    JumpIfZero(TackyVal, String),
    JumpIfNotZero(TackyVal, String),
    Label(String),
    FunCall(TackyFunCallInfo),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TackyFunctionDef {
    pub name: String,
    pub global: bool,
    pub params: Vec<String>,
    pub body: Vec<TackyInstruction>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct TackyStaticVariableDef {
    pub name: String,
    pub t: Type,
    pub global: bool,
    pub init: Vec<StaticInit>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct TackyStaticConstantDef {
    pub name: String,
    pub t: Type,
    pub init: StaticInit,
}
#[derive(Clone, Debug, PartialEq)]
pub enum TackyTopLevel {
    Function(TackyFunctionDef),
    StaticVariable(TackyStaticVariableDef),
    StaticConstant(TackyStaticConstantDef),
}
#[derive(Clone, Debug, PartialEq)]
pub struct TackyProgram(pub Vec<TackyTopLevel>);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constant_comparison_distinguishes_signed_zero() {
        assert_eq!(
            constCompare(&ConstValue::Double(-0.0), &ConstValue::Double(0.0)),
            Ordering::Less
        );
        assert_eq!(
            constCompare(&ConstValue::Double(0.0), &ConstValue::Double(-0.0)),
            Ordering::Greater
        );
    }

    #[test]
    fn tacky_values_use_constant_debug_syntax() {
        assert_eq!(showTackyVal(&TackyVal::Constant(ConstValue::UInt(42))), "42U");
        assert_eq!(showTackyVal(&TackyVal::Var("answer".into())), "answer");
    }
}
