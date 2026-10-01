//! The compiler's source-to-assembly pipeline.
#![allow(non_snake_case)]

use std::any::Any;
use std::cell::Cell;
use std::path::Path;
use std::panic::{catch_unwind, resume_unwind, AssertUnwindSafe};
use std::sync::Once;

use super::{AddressTaken, Cfg, Codegen, CompilerError::CompilerError, Emit,
    InstructionFixup, LabelLoops, Lex, Optimize, Parse, Regalloc, ReplacePseudos,
    Resolve, Settings::{CompilerConfig, Optimizations, Stage}, TackyGen, TackyPrint,
    Typecheck, UniqueIds};

#[derive(Debug)]
enum CompileFailure {
    Lex(String),
    Parse(String),
    Driver(String),
}

fn compileInner(config: &CompilerConfig, stage: Stage, optimizations: &Optimizations,
                src_file: &Path, source: &str) -> Result<(), CompileFailure> {
    let counter = UniqueIds::INITIAL_COUNTER;
    let tokens = Lex::lex(source).map_err(CompileFailure::Lex)?;
    if stage == Stage::Lex { return Ok(()); }

    let ast = Parse::parse(tokens).map_err(CompileFailure::Parse)?;
    if stage == Stage::Parse {
        print!("{ast:?}");
        return Ok(());
    }

    let (counter, resolved_ast) = Resolve::resolve(counter, ast);
    let (counter, annotated_ast) = LabelLoops::labelLoops(counter, resolved_ast);
    UniqueIds::set_shared_counter(counter);
    let typed_ast = Typecheck::typecheck(annotated_ast);
    let counter = UniqueIds::shared_counter();
    if stage == Stage::Validate { return Ok(()); }

    let (counter, tacky) = TackyGen::gen(counter, typed_ast);
    let counter = TackyPrint::debugPrintTacky(config.Debug, counter, src_file, &tacky)
        .map_err(|error| CompileFailure::Driver(error.to_string()))?;
    let optimized_tacky = Optimize::optimize(optimizations, src_file, tacky);
    if stage == Stage::Tacky { return Ok(()); }

    let aliased_vars = AddressTaken::analyzeProgram(&optimized_tacky);
    let (counter, asm_ast) = Codegen::gen(counter, optimized_tacky);
    UniqueIds::set_shared_counter(counter);
    Cfg::setCounter(counter);
    if config.Debug {
        Emit::emit(config.Platform, src_file.with_extension("prealloc.debug.s"), &asm_ast)
            .map_err(|error| CompileFailure::Driver(error.to_string()))?;
    }
    let asm_ast1 = Regalloc::allocateRegisters(config.Debug, &aliased_vars, asm_ast.clone());
    if config.Debug {
        // This deliberately matches the original, which emits the pre-allocation
        // tree to both debug files.
        Emit::emit(config.Platform, src_file.with_extension("postalloc.debug.s"), &asm_ast)
            .map_err(|error| CompileFailure::Driver(error.to_string()))?;
    }
    let asm_ast2 = ReplacePseudos::replacePseudos(asm_ast1);
    let asm_ast3 = InstructionFixup::fixupProgram(asm_ast2);
    if stage != Stage::Codegen {
        Emit::emit(config.Platform, src_file.with_extension("s"), &asm_ast3)
            .map_err(|error| CompileFailure::Driver(error.to_string()))?;
    }
    Ok(())
}

fn panic_message(payload: &(dyn Any + Send)) -> Option<&str> {
    payload.downcast_ref::<String>().map(String::as_str)
        .or_else(|| payload.downcast_ref::<&'static str>().copied())
}

thread_local! {
    static COMPILER_HANDLES_PANIC: Cell<bool> = const { Cell::new(false) };
}

fn install_compiler_panic_hook() {
    static INSTALL_HOOK: Once = Once::new();

    INSTALL_HOOK.call_once(|| {
        let default_hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            let handled = COMPILER_HANDLES_PANIC.get()
                && panic_message(info.payload())
                    .is_some_and(|message| !message.starts_with("Internal error"));
            if !handled {
                default_hook(info);
            }
        }));
    });
}

/// Compile one already-loaded translation unit through the requested stage.
/// Internal compiler panics remain panics; user-facing validation failures are
/// converted to `TypeError`, matching the F# `Failure` handler.
pub fn compile(config: &CompilerConfig, stage: Stage, optimizations: &Optimizations,
               src_file: impl AsRef<Path>, source: &str) -> Result<(), CompilerError> {
    install_compiler_panic_hook();
    let previous = COMPILER_HANDLES_PANIC.replace(true);
    let result = catch_unwind(AssertUnwindSafe(|| {
        compileInner(config, stage, optimizations, src_file.as_ref(), source)
    }));
    COMPILER_HANDLES_PANIC.set(previous);

    match result {
        Ok(Ok(())) => Ok(()),
        Ok(Err(CompileFailure::Lex(message))) => Err(CompilerError::LexError(message)),
        Ok(Err(CompileFailure::Parse(message))) => Err(CompilerError::ParseError(message)),
        Ok(Err(CompileFailure::Driver(message))) => Err(CompilerError::DriverError(message)),
        Err(payload) => match panic_message(payload.as_ref()) {
            Some(message) if !message.starts_with("Internal error") =>
                Err(CompilerError::TypeError(message.to_owned())),
            _ => resume_unwind(payload),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::Settings::Target;

    fn config() -> CompilerConfig { CompilerConfig { Debug: false, Platform: Target::Linux } }

    #[test]
    fn classifies_frontend_failures_by_pipeline_stage() {
        let options = Optimizations::default();
        assert_eq!(
            compile(&config(), Stage::Lex, &options, "bad.c", "@"),
            Err(CompilerError::LexError("@".into())),
        );
        assert_eq!(
            compile(&config(), Stage::Parse, &options, "bad.c", "int ;"),
            Err(CompilerError::ParseError(
                "Expected a simple declarator but found Semicolon".into(),
            )),
        );
    }

    #[test]
    fn honors_early_pipeline_stages() {
        let options = Optimizations::default();
        assert_eq!(compile(&config(), Stage::Lex, &options, "ok.c", "int main(void) { return 0; }"), Ok(()));
    }
}
