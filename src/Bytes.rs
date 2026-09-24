//! Byte-array helpers ported from the original `Bytes.fs` module.

/// Encodes a string using the ISO-8859-1 (Latin-1) code page.
///
/// Like .NET's `Encoding.Latin1`, characters that cannot be represented in a
/// single Latin-1 byte are replaced with `?`.
pub fn ofString(value: &str) -> Vec<u8> {
    value
        .chars()
        .map(|character| u8::try_from(character as u32).unwrap_or(b'?'))
        .collect()
}

/// Creates a byte array containing `count` copies of `character`.
pub fn make(count: usize, character: char) -> Vec<u8> {
    vec![character as u8; count]
}

/// Returns the number of bytes in an array.
pub fn length(bytes: &[u8]) -> usize {
    bytes.len()
}

/// Concatenates two byte arrays.
pub fn cat(left: &[u8], right: &[u8]) -> Vec<u8> {
    let mut result = Vec::with_capacity(left.len() + right.len());
    result.extend_from_slice(left);
    result.extend_from_slice(right);
    result
}

/// Copies `count` bytes beginning at `offset` into a new byte array.
pub fn sub(bytes: &[u8], offset: usize, count: usize) -> Vec<u8> {
    bytes[offset..offset + count].to_vec()
}

/// Reads a little-endian signed 64-bit integer beginning at `offset`.
pub fn getInt64Le(bytes: &[u8], offset: usize) -> i64 {
    i64::from_le_bytes(bytes[offset..offset + 8].try_into().unwrap())
}

/// Reads a little-endian signed 32-bit integer beginning at `offset`.
pub fn getInt32Le(bytes: &[u8], offset: usize) -> i32 {
    i32::from_le_bytes(bytes[offset..offset + 4].try_into().unwrap())
}

/// Reads a byte at `offset`, preserving its bit pattern as a signed byte.
pub fn getInt8(bytes: &[u8], offset: usize) -> i8 {
    bytes[offset] as i8
}
