#![allow(non_snake_case)]
use clod::ports::Int8::{of_int, to_uint8};
#[test]
fn wraps_bytes() {
    assert_eq!(of_int(255), -1);
    assert_eq!(to_uint8(-1), 255);
}
