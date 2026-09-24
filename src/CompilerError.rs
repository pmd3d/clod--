//! Errors reported by the compiler pipeline.

use std::fmt;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CompilerError {
    LexError(String),
    ParseError(String),
    ResolveError(String),
    LoopLabelError(String),
    TypeError(String),
    DriverError(String),
}

/// Render an error using the wording of the original compiler.
pub fn show(error: &CompilerError) -> String {
    let (prefix, message) = match error {
        CompilerError::LexError(message) => ("Lex error: ", message),
        CompilerError::ParseError(message) => ("Parse error: ", message),
        CompilerError::ResolveError(message) => ("Resolve error: ", message),
        CompilerError::LoopLabelError(message) => ("Loop label error: ", message),
        CompilerError::TypeError(message) => ("Type error: ", message),
        CompilerError::DriverError(message) => ("Driver error: ", message),
    };
    format!("{prefix}{message}")
}

impl fmt::Display for CompilerError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&show(self))
    }
}

impl std::error::Error for CompilerError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn renders_every_error_category() {
        assert_eq!(show(&CompilerError::LexError("bad token".into())), "Lex error: bad token");
        assert_eq!(show(&CompilerError::ParseError("bad syntax".into())), "Parse error: bad syntax");
        assert_eq!(show(&CompilerError::ResolveError("unknown name".into())), "Resolve error: unknown name");
        assert_eq!(show(&CompilerError::LoopLabelError("bad break".into())), "Loop label error: bad break");
        assert_eq!(show(&CompilerError::TypeError("bad type".into())), "Type error: bad type");
        assert_eq!(show(&CompilerError::DriverError("bad file".into())), "Driver error: bad file");
    }
}
