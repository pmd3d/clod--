//! Typed compile-time constants.

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Constant { Int(i32), Long(i64), UInt(u32), ULong(u64), Double(f64), Char(i8), UChar(u8) }

impl Constant {
    pub fn as_i128(self) -> i128 { match self { Self::Int(v)=>v as i128, Self::Long(v)=>v as i128, Self::UInt(v)=>v as i128, Self::ULong(v)=>v as i128, Self::Double(v)=>v as i128, Self::Char(v)=>v as i128, Self::UChar(v)=>v as i128 } }
    pub fn as_f64(self) -> f64 { match self { Self::Double(v)=>v, other=>other.as_i128() as f64 } }
}
