//! C type representation.

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Type {
    Int, Long, UInt, ULong, Double, Char, SChar, UChar, Void,
    Pointer(Box<Type>), Array(Box<Type>, usize), Function(Vec<Type>, Box<Type>),
    Struct(String),
}

impl Type {
    pub fn is_integer(&self) -> bool { matches!(self, Self::Int|Self::Long|Self::UInt|Self::ULong|Self::Char|Self::SChar|Self::UChar) }
    pub fn is_signed(&self) -> bool { matches!(self, Self::Int|Self::Long|Self::Char|Self::SChar) }
    pub fn is_scalar(&self) -> bool { self.is_integer() || matches!(self, Self::Double|Self::Pointer(_)) }
}
