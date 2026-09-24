//! Rust port of the original `Settings.fs` settings module.

/// Identifies this compiler subsystem in diagnostics and debug output.
pub const COMPONENT: &str = "settings";

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Target {
    OS_X,
    Linux,
}
