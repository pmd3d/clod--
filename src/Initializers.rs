//! Rust port of the original `Initializers.fs` initializers module.

/// Identifies this compiler subsystem in diagnostics and debug output.
pub const COMPONENT: &str = "initializers";

/// A value used to initialize an object with static storage duration.
///
/// This type is shared by the assembly IR even while the rest of the
/// initializer module is ported independently.
#[derive(Clone, Debug, PartialEq)]
pub enum StaticInit {
    CharInit(i8),
    UCharInit(u8),
    IntInit(i32),
    LongInit(i64),
    UIntInit(u32),
    ULongInit(u64),
    DoubleInit(f64),
    ZeroInit(i32),
    StringInit(String, bool),
    PointerInit(String),
}
