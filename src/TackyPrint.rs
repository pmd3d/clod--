//! Pretty-printing for the TACKY intermediate representation.
#![allow(non_snake_case)]

use std::fs::File;
use std::io::{self, BufWriter, Write};
use std::path::Path;

use super::Const::ConstValue;
use super::Initializers::StaticInit;
use super::Tacky::*;
use super::Types::Type;
use super::UniqueIds::{self, Counter};

pub fn ppUnaryOperator(out: &mut impl Write, op: TackyUnaryOperator) -> io::Result<()> {
    write!(out, "{}", match op { TackyUnaryOperator::Complement => "~", TackyUnaryOperator::Negate => "-", TackyUnaryOperator::Not => "!" })
}

pub fn ppBinaryOperator(escape_brackets: bool, out: &mut impl Write, op: TackyBinaryOperator) -> io::Result<()> {
    let text = match op {
        TackyBinaryOperator::Add => "+", TackyBinaryOperator::Subtract => "-",
        TackyBinaryOperator::Multiply => "*", TackyBinaryOperator::Divide => "/",
        TackyBinaryOperator::Mod => "%", TackyBinaryOperator::Equal => "==",
        TackyBinaryOperator::NotEqual => "!=",
        TackyBinaryOperator::LessThan if escape_brackets => "&lt;",
        TackyBinaryOperator::LessOrEqual if escape_brackets => "&lt;=",
        TackyBinaryOperator::GreaterThan if escape_brackets => "&gt;",
        TackyBinaryOperator::GreaterOrEqual if escape_brackets => "&gt;=",
        TackyBinaryOperator::LessThan => "<", TackyBinaryOperator::LessOrEqual => "<=",
        TackyBinaryOperator::GreaterThan => ">", TackyBinaryOperator::GreaterOrEqual => ">=",
    };
    out.write_all(text.as_bytes())
}

pub fn constToString(value: ConstValue) -> String {
    match value {
        ConstValue::Int(v) => v.to_string(), ConstValue::Long(v) => format!("{v}l"),
        ConstValue::UInt(v) => format!("{v}u"), ConstValue::ULong(v) => format!("{v}ul"),
        ConstValue::Double(v) => format!("{v}"), ConstValue::Char(v) => v.to_string(),
        ConstValue::UChar(v) => v.to_string(),
    }
}

pub fn ppTackyVal(out: &mut (impl Write + ?Sized), value: &TackyVal) -> io::Result<()> {
    match value { TackyVal::Constant(c) => write!(out, "{}", constToString(*c)), TackyVal::Var(v) => out.write_all(v.as_bytes()) }
}

fn commaList<T>(out: &mut impl Write, values: &[T], mut pp: impl FnMut(&mut dyn Write, &T) -> io::Result<()>) -> io::Result<()> {
    for (index, value) in values.iter().enumerate() { if index != 0 { write!(out, ", ")?; } pp(out, value)?; }
    Ok(())
}

fn showType(t: &Type) -> String {
    match t {
        Type::Char => "Char".into(), Type::SChar => "SChar".into(), Type::UChar => "UChar".into(),
        Type::Int => "Int".into(), Type::Long => "Long".into(), Type::UInt => "UInt".into(),
        Type::ULong => "ULong".into(), Type::Double => "Double".into(), Type::Void => "Void".into(),
        Type::Pointer(inner) => format!("{}*", showType(inner)),
        Type::Array(inner, size) => format!("({}, {})", showType(inner), size),
        Type::Function(params, ret) => format!("(FunType (param_types = [{}], ret_type = {}))", params.iter().map(showType).collect::<Vec<_>>().join("; "), showType(ret)),
        Type::Struct(tag) => format!("(Structure {tag})"),
    }
}

fn showInit(init: &StaticInit) -> String {
    match init {
        StaticInit::CharInit(v) => v.to_string(), StaticInit::UCharInit(v) => v.to_string(),
        StaticInit::IntInit(v) => v.to_string(), StaticInit::LongInit(v) => format!("{v}l"),
        StaticInit::UIntInit(v) => format!("{v}u"), StaticInit::ULongInit(v) => format!("{v}ul"),
        StaticInit::DoubleInit(v) => v.to_string(), StaticInit::ZeroInit(v) => format!("zero[{v}]"),
        StaticInit::StringInit(s, nul) => format!("\"{s}{}\"", if *nul { "\\0" } else { "" }),
        StaticInit::PointerInit(s) => format!("&{s}"),
    }
}

pub fn ppInstruction(escape_brackets: bool, out: &mut impl Write, instruction: &TackyInstruction) -> io::Result<()> {
    use TackyInstruction::*;
    match instruction {
        Return(None) => write!(out, "Return"), Return(Some(v)) => { write!(out, "Return(")?; ppTackyVal(out, v)?; write!(out, ")") }
        Unary(v) => { ppTackyVal(out, &v.dst)?; write!(out, " = ")?; ppUnaryOperator(out, v.op)?; ppTackyVal(out, &v.src) }
        Binary(v) => { ppTackyVal(out, &v.dst)?; write!(out, " = ")?; ppTackyVal(out, &v.src1)?; write!(out, " ")?; ppBinaryOperator(escape_brackets, out, v.op)?; write!(out, " ")?; ppTackyVal(out, &v.src2) }
        Copy(v) => assignment(out, &v.dst, "", &v.src),
        Jump(s) => write!(out, "Jump({s})"), JumpIfZero(v, s) => conditional(out, "JumpIfZero", v, s),
        JumpIfNotZero(v, s) => conditional(out, "JumpIfNotZero", v, s), Label(s) => write!(out, "\n{s}:"),
        FunCall(v) => { if let Some(dst) = &v.dst { ppTackyVal(out, dst)?; write!(out, " = ")?; } write!(out, "{}(", v.f)?; commaList(out, &v.args, |o, x| ppTackyVal(o, x))?; write!(out, ")") }
        SignExtend(v) => conversion(out, "SignExtend", v), ZeroExtend(v) => conversion(out, "ZeroExtend", v),
        Truncate(v) => conversion(out, "Truncate", v), DoubleToInt(v) => conversion(out, "DoubleToInt", v),
        DoubleToUInt(v) => conversion(out, "DoubleToUInt", v), IntToDouble(v) => conversion(out, "IntToDouble", v),
        UIntToDouble(v) => conversion(out, "UIntToDouble", v), GetAddress(v) => conversion(out, "GetAddress", v),
        Load(v) => { ppTackyVal(out, &v.dst)?; write!(out, " = Load(")?; ppTackyVal(out, &v.src_ptr)?; write!(out, ")") }
        Store(v) => { write!(out, "*(")?; ppTackyVal(out, &v.dst_ptr)?; write!(out, ") = ")?; ppTackyVal(out, &v.src) }
        AddPtr(v) => { ppTackyVal(out, &v.dst)?; write!(out, " = ")?; ppTackyVal(out, &v.ptr)?; write!(out, " + ")?; ppTackyVal(out, &v.index)?; write!(out, " * {}", v.scale) }
        CopyToOffset(v) => { write!(out, "{}[offset = {}] = ", v.dst, v.offset)?; ppTackyVal(out, &v.src) }
        CopyFromOffset(v) => { ppTackyVal(out, &v.dst)?; write!(out, " = {}[offset = {}]", v.src, v.offset) }
    }
}

fn assignment(out: &mut impl Write, dst: &TackyVal, prefix: &str, src: &TackyVal) -> io::Result<()> { ppTackyVal(out, dst)?; write!(out, " = {prefix}")?; ppTackyVal(out, src) }
fn conditional(out: &mut impl Write, name: &str, value: &TackyVal, target: &str) -> io::Result<()> { write!(out, "{name}(")?; ppTackyVal(out, value)?; write!(out, ", {target})") }
fn conversion(out: &mut impl Write, name: &str, value: &TackySrcDst) -> io::Result<()> { ppTackyVal(out, &value.dst)?; write!(out, " = {name}(")?; ppTackyVal(out, &value.src)?; write!(out, ")") }

pub fn ppFunctionDefinition(escape_brackets: bool, global: bool, name: &str, params: &[String], out: &mut impl Write, body: &[TackyInstruction]) -> io::Result<()> {
    if global { write!(out, "global ")?; } write!(out, "{name}(")?;
    commaList(out, params, |o, s| o.write_all(s.as_bytes()))?; writeln!(out, "):")?; write!(out, "    ")?;
    for (i, instruction) in body.iter().enumerate() { if i != 0 { write!(out, "\n    ")?; } ppInstruction(escape_brackets, out, instruction)?; }
    Ok(())
}

pub fn ppTl(escape_brackets: bool, out: &mut impl Write, item: &TackyTopLevel) -> io::Result<()> {
    match item {
        TackyTopLevel::Function(f) => ppFunctionDefinition(escape_brackets, f.global, &f.name, &f.params, out, &f.body),
        TackyTopLevel::StaticVariable(v) => { if v.global { write!(out, "global ")?; } write!(out, "{} {} = {{", showType(&v.t), v.name)?; commaList(out, &v.init, |o, x| write!(o, "{}", showInit(x)))?; write!(out, "}}") }
        TackyTopLevel::StaticConstant(v) => write!(out, "const {} {} = {}", showType(&v.t), v.name, showInit(&v.init)),
    }
}

pub fn ppProgram(escape_brackets: bool, out: &mut impl Write, program: &TackyProgram) -> io::Result<()> {
    for (i, item) in program.0.iter().enumerate() { if i != 0 { write!(out, "\n\n")?; } ppTl(escape_brackets, out, item)?; }
    writeln!(out)?; out.flush()
}

pub fn debugPrintTacky(debug: bool, counter: Counter, src_filename: &Path, program: &TackyProgram) -> io::Result<Counter> {
    if !debug { return Ok(counter); }
    let stem = src_filename.file_stem().and_then(|s| s.to_str()).unwrap_or_default();
    let (counter, label) = UniqueIds::make_label(stem, counter);
    let mut file = BufWriter::new(File::create(format!("{label}.debug.tacky"))?);
    ppProgram(false, &mut file, program)?;
    Ok(counter)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn prints_program_in_original_debug_format() {
        let program = TackyProgram(vec![TackyTopLevel::Function(TackyFunctionDef { name: "main".into(), global: true, params: vec![], body: vec![TackyInstruction::Binary(TackyBinaryInfo { op: TackyBinaryOperator::LessThan, src1: TackyVal::Constant(ConstValue::Int(1)), src2: TackyVal::Constant(ConstValue::Int(2)), dst: TackyVal::Var("tmp.0".into()) }), TackyInstruction::Return(Some(TackyVal::Var("tmp.0".into())))] })]);
        let mut out = Vec::new(); ppProgram(true, &mut out, &program).unwrap();
        assert_eq!(String::from_utf8(out).unwrap(), "global main():\n    tmp.0 = 1 &lt; 2\n    Return(tmp.0)\n");
    }
}
