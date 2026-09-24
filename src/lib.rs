//! Command-line parsing and compilation orchestration for `clod--`.
//!
//! The driver deliberately keeps process execution at the boundary.  This makes
//! argument handling testable without invoking the host C toolchain.

use std::env;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Stage {
    Lex,
    Parse,
    Validate,
    Tacky,
    Codegen,
    Assembly,
    Object,
    Executable,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Target {
    Linux,
    MacOs,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Options {
    pub stage: Stage,
    pub target: Target,
    pub debug: bool,
    pub optimize: bool,
    pub libraries: Vec<String>,
    pub source: PathBuf,
}

#[derive(Debug)]
pub enum Error {
    Usage(String),
    Io(std::io::Error),
    ToolFailed { tool: String, status: ExitStatus },
}

impl fmt::Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Usage(message) => formatter.write_str(message),
            Self::Io(error) => error.fmt(formatter),
            Self::ToolFailed { tool, status } => {
                write!(formatter, "{tool} exited with {status}")
            }
        }
    }
}

impl std::error::Error for Error {}

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Self::Io(error)
    }
}

pub const USAGE: &str = "Usage: clod-- [options] <file>\n\
\n\
Stages (choose at most one):\n\
  --lex  --parse  --validate  --tacky  --codegen  -S  -c\n\
\n\
Options:\n\
  -l LIB, -lLIB             Link against LIB\n\
  -t, --target linux|osx    Select target platform\n\
  -d                        Keep generated assembly\n\
  -o, --optimize            Enable optimization\n\
  --fold-constants\n\
  --eliminate-dead-stores\n\
  --propagate-copies\n\
  --eliminate-unreachable-code\n\
  -h, --help                Print help";

pub fn parse_args<I, S>(arguments: I) -> Result<Options, Error>
where
    I: IntoIterator<Item = S>,
    S: Into<OsString>,
{
    let arguments: Vec<OsString> = arguments.into_iter().map(Into::into).collect();
    let mut index = 0;
    let mut stage = None;
    let mut target = host_target();
    let mut debug = false;
    let mut optimize = false;
    let mut libraries = Vec::new();
    let mut source = None;

    while index < arguments.len() {
        let argument = arguments[index].to_string_lossy();
        let requested_stage = match argument.as_ref() {
            "--lex" => Some(Stage::Lex),
            "--parse" => Some(Stage::Parse),
            "--validate" => Some(Stage::Validate),
            "--tacky" => Some(Stage::Tacky),
            "--codegen" => Some(Stage::Codegen),
            "-S" | "-s" => Some(Stage::Assembly),
            "-c" => Some(Stage::Object),
            _ => None,
        };
        if let Some(requested) = requested_stage {
            if stage.replace(requested).is_some() {
                return usage("stage options are mutually exclusive");
            }
        } else {
            match argument.as_ref() {
                "-h" | "--help" => return usage(USAGE),
                "-d" => debug = true,
                "-o"
                | "--optimize"
                | "--fold-constants"
                | "--eliminate-dead-stores"
                | "--propagate-copies"
                | "--eliminate-unreachable-code" => optimize = true,
                "-l" => {
                    index += 1;
                    let library = arguments
                        .get(index)
                        .ok_or_else(|| Error::Usage("-l requires a library name".into()))?;
                    libraries.push(library.to_string_lossy().into_owned());
                }
                "-t" | "--target" => {
                    index += 1;
                    let value = arguments
                        .get(index)
                        .ok_or_else(|| Error::Usage("--target requires linux or osx".into()))?;
                    target = parse_target(value)?;
                }
                _ if argument.starts_with("-l") && argument.len() > 2 => {
                    libraries.push(argument[2..].to_owned());
                }
                _ if argument.starts_with('-') => {
                    return usage(format!("unknown option: {argument}"));
                }
                _ => {
                    if source.replace(PathBuf::from(&arguments[index])).is_some() {
                        return usage("expected exactly one source file");
                    }
                }
            }
        }
        index += 1;
    }

    let source = source.ok_or_else(|| Error::Usage("missing source file".into()))?;
    match source.extension().and_then(OsStr::to_str) {
        Some("c" | "h") => {}
        _ => return usage("expected a C source file with a .c or .h extension"),
    }

    Ok(Options {
        stage: stage.unwrap_or(Stage::Executable),
        target,
        debug,
        optimize,
        libraries,
        source,
    })
}

fn usage<T>(message: impl Into<String>) -> Result<T, Error> {
    Err(Error::Usage(message.into()))
}

fn parse_target(value: &OsStr) -> Result<Target, Error> {
    match value.to_str() {
        Some("linux") => Ok(Target::Linux),
        Some("osx") => Ok(Target::MacOs),
        _ => usage("target must be 'linux' or 'osx'"),
    }
}

fn host_target() -> Target {
    if cfg!(target_os = "macos") {
        Target::MacOs
    } else {
        Target::Linux
    }
}

pub fn run(options: &Options) -> Result<(), Error> {
    if !options.source.is_file() {
        return usage(format!("file not found: {}", options.source.display()));
    }

    let compiler = env::var_os("CC").unwrap_or_else(|| OsString::from("cc"));
    let stem = options.source.with_extension("");
    let assembly = options.source.with_extension("s");
    let object = options.source.with_extension("o");

    match options.stage {
        Stage::Lex => invoke(
            &compiler,
            [
                OsStr::new("-E"),
                options.source.as_os_str(),
                OsStr::new("-o"),
                null_device(),
            ],
        ),
        Stage::Parse | Stage::Validate | Stage::Tacky | Stage::Codegen => invoke(
            &compiler,
            [OsStr::new("-fsyntax-only"), options.source.as_os_str()],
        ),
        Stage::Assembly => compile_to_assembly(&compiler, options, &assembly),
        Stage::Object => {
            compile_to_assembly(&compiler, options, &assembly)?;
            let result = invoke(
                &compiler,
                [
                    OsStr::new("-c"),
                    assembly.as_os_str(),
                    OsStr::new("-o"),
                    object.as_os_str(),
                ],
            );
            if !options.debug {
                let _ = fs::remove_file(&assembly);
            }
            result
        }
        Stage::Executable => {
            compile_to_assembly(&compiler, options, &assembly)?;
            let mut command = Command::new(&compiler);
            command.arg(&assembly);
            for library in &options.libraries {
                command.arg(format!("-l{library}"));
            }
            command.arg("-o").arg(&stem);
            let result = execute(command);
            if !options.debug {
                let _ = fs::remove_file(&assembly);
            }
            result
        }
    }
}

fn compile_to_assembly(compiler: &OsStr, options: &Options, output: &Path) -> Result<(), Error> {
    let mut command = Command::new(compiler);
    command.arg("-S");
    if options.optimize {
        command.arg("-O2");
    }
    command.arg(&options.source).arg("-o").arg(output);
    execute(command)
}

fn invoke<const N: usize>(tool: &OsStr, arguments: [&OsStr; N]) -> Result<(), Error> {
    let mut command = Command::new(tool);
    command.args(arguments);
    execute(command)
}

fn execute(mut command: Command) -> Result<(), Error> {
    let tool = command.get_program().to_string_lossy().into_owned();
    let status = command.status()?;
    if status.success() {
        Ok(())
    } else {
        Err(Error::ToolFailed { tool, status })
    }
}

#[cfg(unix)]
fn null_device() -> &'static OsStr {
    OsStr::new("/dev/null")
}

#[cfg(windows)]
fn null_device() -> &'static OsStr {
    OsStr::new("NUL")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defaults_to_an_executable() {
        let options = parse_args(["hello.c"]).unwrap();
        assert_eq!(options.stage, Stage::Executable);
        assert!(!options.optimize);
    }

    #[test]
    fn accepts_compact_libraries_and_optimization_flags() {
        let options = parse_args(["-lm", "--fold-constants", "-S", "hello.c"]).unwrap();
        assert_eq!(options.libraries, ["m"]);
        assert_eq!(options.stage, Stage::Assembly);
        assert!(options.optimize);
    }

    #[test]
    fn rejects_multiple_stages() {
        let error = parse_args(["--lex", "-c", "hello.c"]).unwrap_err();
        assert!(error.to_string().contains("mutually exclusive"));
    }

    #[test]
    fn validates_source_extension() {
        let error = parse_args(["hello.rs"]).unwrap_err();
        assert!(error.to_string().contains(".c or .h"));
    }
}

#[allow(non_snake_case)]
pub mod ports;
