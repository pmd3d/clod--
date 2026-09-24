//! Integer alignment helpers.

/// Round `value` to the next multiple of positive `alignment`, away from zero.
pub fn round_away_from_zero(alignment: i64, value: i64) -> i64 {
    assert!(alignment > 0, "alignment must be positive");
    let remainder = value % alignment;
    if remainder == 0 { value }
    else if value < 0 { value - alignment - remainder }
    else { value + alignment - remainder }
}

pub fn round_up(value: usize, alignment: usize) -> usize {
    assert!(alignment > 0, "alignment must be positive");
    value.div_ceil(alignment) * alignment
}
