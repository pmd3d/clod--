//! Alignment helpers.
pub fn round_away_from_zero(dividend: i64, divisor: i64) -> i64 { let q=dividend/divisor; let r=dividend%divisor; if r==0 { q } else if (dividend>0)==(divisor>0) { q+1 } else { q-1 } }
pub fn round_up(value: usize, alignment: usize) -> usize { value.div_ceil(alignment)*alignment }
