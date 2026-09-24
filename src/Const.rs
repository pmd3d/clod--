//! Typed compile-time constants.
//!
//! This is the Rust counterpart of the original `Const.fs` module.  In
//! particular, constants of different C types remain distinct even when their
//! numeric values are equal.

use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::io::{self, Write};

use super::Types::Type;

#[derive(Clone, Copy, Debug)]
pub enum Constant {
    Char(i8),
    UChar(u8),
    Int(i32),
    Long(i64),
    UInt(u32),
    ULong(u64),
    Double(f64),
}

/// The name used by the F# discriminated union.
pub type ConstValue = Constant;

impl Constant {
    pub fn as_i128(self) -> i128 {
        match self {
            Self::Char(value) => value as i128,
            Self::UChar(value) => value as i128,
            Self::Int(value) => value as i128,
            Self::Long(value) => value as i128,
            Self::UInt(value) => value as i128,
            Self::ULong(value) => value as i128,
            Self::Double(value) => value as i128,
        }
    }

    pub fn as_f64(self) -> f64 {
        match self {
            Self::Double(value) => value,
            other => other.as_i128() as f64,
        }
    }

    fn tag(self) -> u8 {
        match self {
            Self::Char(_) => 0,
            Self::UChar(_) => 1,
            Self::Int(_) => 2,
            Self::Long(_) => 3,
            Self::UInt(_) => 4,
            Self::ULong(_) => 5,
            Self::Double(_) => 6,
        }
    }
}

impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        match (*self, *other) {
            (Self::Char(left), Self::Char(right)) => left == right,
            (Self::UChar(left), Self::UChar(right)) => left == right,
            (Self::Int(left), Self::Int(right)) => left == right,
            (Self::Long(left), Self::Long(right)) => left == right,
            (Self::UInt(left), Self::UInt(right)) => left == right,
            (Self::ULong(left), Self::ULong(right)) => left == right,
            (Self::Double(left), Self::Double(right)) => left == right,
            _ => false,
        }
    }
}

impl Hash for Constant {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.tag().hash(state);
        match *self {
            Self::Char(value) => value.hash(state),
            Self::UChar(value) => value.hash(state),
            Self::Int(value) => value.hash(state),
            Self::Long(value) => value.hash(state),
            Self::UInt(value) => value.hash(state),
            Self::ULong(value) => value.hash(state),
            // Equality considers the two representations of zero equal, so
            // they must also have the same hash.
            Self::Double(value) => {
                let bits = if value == 0.0 { 0 } else { value.to_bits() };
                bits.hash(state);
            }
        }
    }
}

impl PartialOrd for Constant {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(match (*self, *other) {
            (Self::Char(left), Self::Char(right)) => left.cmp(&right),
            (Self::UChar(left), Self::UChar(right)) => left.cmp(&right),
            (Self::Int(left), Self::Int(right)) => left.cmp(&right),
            (Self::Long(left), Self::Long(right)) => left.cmp(&right),
            (Self::UInt(left), Self::UInt(right)) => left.cmp(&right),
            (Self::ULong(left), Self::ULong(right)) => left.cmp(&right),
            (Self::Double(left), Self::Double(right)) => compare_f64(left, right),
            _ => self.tag().cmp(&other.tag()),
        })
    }
}

fn compare_f64(left: f64, right: f64) -> Ordering {
    match (left.is_nan(), right.is_nan()) {
        (true, true) => Ordering::Equal,
        (true, false) => Ordering::Less,
        (false, true) => Ordering::Greater,
        (false, false) => left.partial_cmp(&right).expect("non-NaN values are comparable"),
    }
}

/// Render a constant in the debugging format used by the original module.
pub fn show(constant: Constant) -> String {
    match constant {
        Constant::Char(value) => value.to_string(),
        Constant::UChar(value) => value.to_string(),
        Constant::Int(value) => value.to_string(),
        Constant::Long(value) => format!("{value}L"),
        Constant::UInt(value) => format!("{value}U"),
        Constant::ULong(value) => format!("{value}UL"),
        Constant::Double(value) => format_double(value),
    }
}

/// Write [`show`]'s representation to a text writer.
pub fn pp(writer: &mut impl Write, constant: Constant) -> io::Result<()> {
    writer.write_all(show(constant).as_bytes())
}

pub const INT_ZERO: Constant = Constant::Int(0);
pub const INT_ONE: Constant = Constant::Int(1);

pub fn type_of_const(constant: Constant) -> Type {
    match constant {
        Constant::Char(_) => Type::SChar,
        Constant::UChar(_) => Type::UChar,
        Constant::Int(_) => Type::Int,
        Constant::Long(_) => Type::Long,
        Constant::UInt(_) => Type::UInt,
        Constant::ULong(_) => Type::ULong,
        Constant::Double(_) => Type::Double,
    }
}

fn format_double(value: f64) -> String {
    if value.is_nan() || value.is_infinite() || value == 0.0 {
        return value.to_string();
    }

    // F#'s `%.54g` requests 54 significant decimal digits and switches to
    // exponent notation for exponents below -4 or at least 54.
    let scientific = format!("{value:.53e}");
    let (mantissa, exponent) = scientific.split_once('e').expect("e format has an exponent");
    let exponent: i32 = exponent.parse().expect("formatted exponent is an integer");
    let mut digits = mantissa.replace('.', "");
    while digits.ends_with('0') {
        digits.pop();
    }

    let negative = digits.starts_with('-');
    let unsigned = digits.trim_start_matches('-');
    if !(-4..54).contains(&exponent) {
        let mut result = String::new();
        if negative {
            result.push('-');
        }
        result.push_str(&unsigned[..1]);
        if unsigned.len() > 1 {
            result.push('.');
            result.push_str(&unsigned[1..]);
        }
        result.push('e');
        result.push_str(&exponent.to_string());
        result
    } else {
        let decimal_position = exponent + 1;
        let body = if decimal_position <= 0 {
            format!("0.{}{}", "0".repeat((-decimal_position) as usize), unsigned)
        } else if decimal_position as usize >= unsigned.len() {
            format!("{}{}", unsigned, "0".repeat(decimal_position as usize - unsigned.len()))
        } else {
            let position = decimal_position as usize;
            format!("{}.{}", &unsigned[..position], &unsigned[position..])
        };
        if negative { format!("-{body}") } else { body }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn preserves_union_case_order() {
        let values = [
            Constant::Double(0.0), Constant::UInt(0), Constant::Char(0),
            Constant::ULong(0), Constant::Int(0), Constant::UChar(0),
            Constant::Long(0),
        ];
        let mut sorted = values;
        sorted.sort_by(|left, right| left.partial_cmp(right).unwrap());
        assert_eq!(sorted, [
            Constant::Char(0), Constant::UChar(0), Constant::Int(0),
            Constant::Long(0), Constant::UInt(0), Constant::ULong(0),
            Constant::Double(0.0),
        ]);
    }

    #[test]
    fn shows_values_like_fsharp_module() {
        assert_eq!(show(Constant::Long(-12)), "-12L");
        assert_eq!(show(Constant::UInt(12)), "12U");
        assert_eq!(show(Constant::ULong(12)), "12UL");
        assert_eq!(show(Constant::Double(1.5)), "1.5");
    }

    #[test]
    fn reports_the_constants_c_type() {
        assert_eq!(type_of_const(Constant::Char(-1)), Type::SChar);
        assert_eq!(type_of_const(Constant::Double(1.0)), Type::Double);
    }
}
