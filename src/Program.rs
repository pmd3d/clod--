//! Executable boundary for the compiler driver.

use std::process::ExitCode;

pub fn main_entry() -> ExitCode {
    let options = match crate::parse_args(std::env::args_os().skip(1)) {
        Ok(options) => options,
        Err(crate::Error::Usage(message)) if message == crate::USAGE => {
            println!("{message}");
            return ExitCode::SUCCESS;
        }
        Err(error) => {
            eprintln!("Error: {error}\n\n{}", crate::USAGE);
            return ExitCode::FAILURE;
        }
    };

    match crate::run(&options) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("Error: {error}");
            ExitCode::FAILURE
        }
    }
}
