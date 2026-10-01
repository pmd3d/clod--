//! Command-line parsing and compilation orchestration for `clod--`.
//!
//! The driver deliberately keeps process execution at the boundary.  This makes
//! argument handling testable without invoking the host C toolchain.

use std::env;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::fs;
use std::path::PathBuf;
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
    pub optimizations: ports::Settings::Optimizations,
    pub libraries: Vec<String>,
    pub source: PathBuf,
}

#[derive(Debug)]
pub enum Error {
    Usage(String),
    Io(std::io::Error),
    ToolLaunch {
        tool: String,
        source: std::io::Error,
    },
    ToolFailed {
        tool: String,
        status: ExitStatus,
    },
    Compiler(ports::CompilerError::CompilerError),
}

impl fmt::Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Usage(message) => formatter.write_str(message),
            Self::Io(error) => error.fmt(formatter),
            Self::ToolLaunch { tool, source } if source.kind() == std::io::ErrorKind::NotFound => {
                write!(
                    formatter,
                    "could not find C toolchain command '{tool}'. Install GCC (use WSL on Windows) or set CC to the command or full path of a compatible C compiler"
                )
            }
            Self::ToolLaunch { tool, source } => {
                write!(
                    formatter,
                    "could not run C toolchain command '{tool}': {source}"
                )
            }
            Self::ToolFailed { tool, status } => {
                write!(formatter, "{tool} exited with {status}")
            }
            Self::Compiler(error) => error.fmt(formatter),
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
    let mut optimizations = ports::Settings::Optimizations::default();
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
                "-o" | "--optimize" => optimize = true,
                "--fold-constants" => optimizations.constant_folding = true,
                "--eliminate-dead-stores" => optimizations.dead_store_elimination = true,
                "--propagate-copies" => optimizations.copy_propagation = true,
                "--eliminate-unreachable-code" => optimizations.unreachable_code_elimination = true,
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
        optimize: optimize
            || optimizations.constant_folding
            || optimizations.dead_store_elimination
            || optimizations.copy_propagation
            || optimizations.unreachable_code_elimination,
        optimizations: if optimize {
            ports::Settings::Optimizations {
                constant_folding: true,
                dead_store_elimination: true,
                unreachable_code_elimination: true,
                copy_propagation: true,
            }
        } else {
            optimizations
        },
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

    let compiler = env::var_os("CC").unwrap_or_else(|| OsString::from("gcc"));
    let stem = options.source.with_extension("");
    let preprocessed = options.source.with_extension("i");
    let assembly = options.source.with_extension("s");
    let object = options.source.with_extension("o");

    invoke(
        &compiler,
        [
            OsStr::new("-E"),
            OsStr::new("-P"),
            options.source.as_os_str(),
            OsStr::new("-o"),
            preprocessed.as_os_str(),
        ],
    )?;
    let compile_result = fs::read_to_string(&preprocessed)
        .map_err(Error::Io)
        .and_then(|source| {
            let config = ports::Settings::CompilerConfig {
                Debug: options.debug,
                Platform: match options.target {
                    Target::Linux => ports::Settings::Target::Linux,
                    Target::MacOs => ports::Settings::Target::OS_X,
                },
            };
            ports::Compile::compile(
                &config,
                compiler_stage(options.stage),
                &options.optimizations,
                &preprocessed,
                &source,
            )
            .map_err(Error::Compiler)
        });
    let _ = fs::remove_file(&preprocessed);
    compile_result?;

    match options.stage {
        Stage::Lex
        | Stage::Parse
        | Stage::Validate
        | Stage::Tacky
        | Stage::Codegen
        | Stage::Assembly => Ok(()),
        Stage::Object => {
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

fn compiler_stage(stage: Stage) -> ports::Settings::Stage {
    match stage {
        Stage::Lex => ports::Settings::Stage::Lex,
        Stage::Parse => ports::Settings::Stage::Parse,
        Stage::Validate => ports::Settings::Stage::Validate,
        Stage::Tacky => ports::Settings::Stage::Tacky,
        Stage::Codegen => ports::Settings::Stage::Codegen,
        Stage::Assembly | Stage::Object | Stage::Executable => ports::Settings::Stage::Assembly,
    }
}

fn invoke<const N: usize>(tool: &OsStr, arguments: [&OsStr; N]) -> Result<(), Error> {
    let mut command = Command::new(tool);
    command.args(arguments);
    execute(command)
}

fn execute(mut command: Command) -> Result<(), Error> {
    let tool = command.get_program().to_string_lossy().into_owned();
    let status = command.status().map_err(|source| Error::ToolLaunch {
        tool: tool.clone(),
        source,
    })?;
    if status.success() {
        Ok(())
    } else {
        Err(Error::ToolFailed { tool, status })
    }
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

    #[test]
    fn missing_tool_error_identifies_the_toolchain_instead_of_the_source() {
        let command = Command::new("clod--tool-that-does-not-exist");
        let error = execute(command).unwrap_err();
        let message = error.to_string();

        assert!(message.contains("C toolchain command 'clod--tool-that-does-not-exist'"));
        assert!(message.contains("set CC"));
    }
}

#[allow(non_snake_case)]
pub mod ports;
