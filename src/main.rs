use std::process::ExitCode;

/// Run the command-line compiler.
///
/// Keep argument collection and diagnostic output in the binary crate so the
/// library remains usable without depending on process-global state.
fn main() -> ExitCode {
    let options = match clod::parse_args(std::env::args_os().skip(1)) {
        Ok(options) => options,
        Err(clod::Error::Usage(message)) if message == clod::USAGE => {
            println!("{message}");
            return ExitCode::SUCCESS;
        }
        Err(error) => {
            eprintln!("Error: {error}\n\n{}", clod::USAGE);
            return ExitCode::FAILURE;
        }
    };

    match clod::run(&options) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("Error: {error}");
            ExitCode::FAILURE
        }
    }
}
