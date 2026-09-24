//! Signed-byte values represented as `i32`, matching the F# implementation.

pub type Int8Value = i32;

pub fn equal(a: Int8Value, b: Int8Value) -> bool { a == b }
pub fn compare(a: Int8Value, b: Int8Value) -> std::cmp::Ordering { a.cmp(&b) }
pub fn show(value: Int8Value) -> String { value.to_string() }
pub const ZERO: Int8Value = 0;

pub fn reset_upper_bytes(value: Int8Value) -> Int8Value {
    if value & 128 == 0 { value & 0xff } else { value | !0xff }
}

pub fn of_int(value: i64) -> Int8Value { reset_upper_bytes(value as i32) }
pub fn to_int(value: Int8Value) -> i32 { value }
pub fn of_int64(value: i64) -> Int8Value { reset_upper_bytes(value as i32) }
pub fn to_int64(value: Int8Value) -> i64 { i64::from(value) }
pub fn to_string(value: Int8Value) -> String { value.to_string() }

/// Bit-preserving conversion useful when emitting a byte.
pub fn to_uint8(value: Int8Value) -> u8 { value as u8 }
