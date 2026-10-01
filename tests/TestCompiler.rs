#![allow(non_snake_case)]

use clod::ports::Compile::compile;
use clod::ports::CompilerError::CompilerError;
use clod::ports::Settings::{CompilerConfig, Optimizations, Stage, Target};

fn config() -> CompilerConfig {
    CompilerConfig {
        Debug: false,
        Platform: Target::Linux,
    }
}

#[test]
fn classifies_downstream_validation_failures_as_type_errors() {
    let options = Optimizations::default();
    assert_eq!(
        compile(
            &config(),
            Stage::Validate,
            &options,
            "bad.c",
            "int main(void) { return missing; }",
        ),
        Err(CompilerError::TypeError(
            "Undeclared variable missing".into(),
        )),
    );
    assert_eq!(
        compile(
            &config(),
            Stage::Validate,
            &options,
            "bad.c",
            "int main(void) { return; }",
        ),
        Err(CompilerError::TypeError(
            "non-void function must return a value".into(),
        )),
    );
}
