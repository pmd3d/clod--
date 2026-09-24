//! UTF-8-safe string helpers.
pub fn drop_chars(count: usize, value: &str) -> &str { value.char_indices().nth(count).map_or("", |(index, _)| &value[index..]) }
pub fn chop_suffix(value: &str, count: usize) -> Option<&str> { value.char_indices().rev().nth(count.saturating_sub(1)).map(|(i, _)| &value[..i]) }
