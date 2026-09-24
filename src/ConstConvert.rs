//! C scalar constant conversions.
use super::Const::Constant;
use super::Types::Type;

pub fn convert(target: &Type, value: Constant) -> Constant {
    match target {
        Type::Int => Constant::Int(value.as_i128() as i32),
        Type::Long => Constant::Long(value.as_i128() as i64),
        Type::UInt => Constant::UInt(value.as_i128() as u32),
        Type::ULong => Constant::ULong(value.as_i128() as u64),
        Type::Double => Constant::Double(value.as_f64()),
        Type::Char | Type::SChar => Constant::Char(value.as_i128() as i8),
        Type::UChar => Constant::UChar(value.as_i128() as u8),
        _ => value,
    }
}
