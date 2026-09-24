#![allow(non_snake_case)]
use clod::ports::{Const::Constant, ConstConvert::convert, Types::Type};
#[test]
fn wrapping_conversions() {
    assert_eq!(
        convert(&Type::Int, Constant::UInt(4_294_967_200)),
        Constant::Int(-96)
    );
    assert_eq!(
        convert(&Type::UChar, Constant::Int(356)),
        Constant::UChar(100)
    );
}
#[test]
fn floating_conversions() {
    assert_eq!(
        convert(&Type::Long, Constant::Double(2_148_429_099.3)),
        Constant::Long(2_148_429_099)
    );
}
