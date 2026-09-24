#![allow(non_snake_case)]

use clod::ports::Bytes::{cat, getInt32Le, getInt64Le, getInt8, length, make, ofString, sub};

#[test]
fn converts_strings_to_latin1_bytes() {
    assert_eq!(ofString("Aéÿ"), [0x41, 0xe9, 0xff]);
    assert_eq!(ofString("A€B"), b"A?B");
}

#[test]
fn creates_and_combines_byte_arrays() {
    let padding = make(3, '\0');
    assert_eq!(length(&padding), 3);
    assert_eq!(cat(b"ab", &padding), [b'a', b'b', 0, 0, 0]);
    assert_eq!(sub(b"abcdef", 2, 3), b"cde");
    assert!(sub(b"abc", 3, 0).is_empty());
}

#[test]
fn reads_little_endian_signed_integers() {
    assert_eq!(
        getInt64Le(&[0, 0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff], 1),
        -2
    );
    assert_eq!(getInt32Le(&[0, 0xfe, 0xff, 0xff, 0xff], 1), -2);
    assert_eq!(getInt8(&[0, 0xff], 1), -1);
}
