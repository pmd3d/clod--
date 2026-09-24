//! Rust port of the original `Settings.fs` settings module.

/// Identifies this compiler subsystem in diagnostics and debug output.
pub const COMPONENT: &str = "settings";

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Target {
    OS_X,
    Linux,
}

/// Individual optimization passes selected by the command-line driver.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct Optimizations {
    pub constant_folding: bool,
    pub dead_store_elimination: bool,
    pub unreachable_code_elimination: bool,
    pub copy_propagation: bool,
}
