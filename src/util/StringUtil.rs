//! String helpers. Character counts are Unicode scalar-value counts, so every
//! returned slice is valid UTF-8 (and agrees with the original for ASCII text).

pub fn drop(count: usize, value: &str) -> &str {
    value.char_indices().nth(count).map_or("", |(index, _)| &value[index..])
}
pub fn drop_chars(count: usize, value: &str) -> &str { drop(count, value) }

pub fn chop_suffix(value: &str, count: usize) -> Option<&str> {
    if count == 0 { return Some(value); }
    value.char_indices().rev().nth(count - 1).map(|(index, _)| &value[..index])
}

pub fn chop_suffix1(value: &str) -> Option<&str> { chop_suffix(value, 1) }
pub fn of_list(chars: &[char]) -> String { chars.iter().collect() }
pub fn is_alnum(character: char) -> bool { character.is_ascii_alphanumeric() }
