//! Module map retaining the source layout of the original compiler.

macro_rules! port {
    ($name:ident, $path:literal) => {
        #[path = $path]
        pub mod $name;
    };
}

port!(AsmCfg, "AsmCfg.rs");
port!(Assembly, "Assembly.rs");
port!(Ast, "Ast.rs");
port!(BackwardDataflow, "BackwardDataflow.rs");
port!(Bytes, "Bytes.rs");
port!(Cfg, "Cfg.rs");
port!(Compile, "Compile.rs");
port!(CompilerError, "CompilerError.rs");
port!(Const, "Const.rs");
port!(ConstConvert, "ConstConvert.rs");
port!(Emit, "Emit.rs");
port!(Initializers, "Initializers.rs");
port!(Lex, "Lex.rs");
port!(Parse, "Parse.rs");
port!(Program, "Program.rs");
port!(RegSet, "RegSet.rs");
port!(Settings, "Settings.rs");
port!(Stream, "Stream.rs");
port!(Symbols, "Symbols.rs");
port!(Tacky, "Tacky.rs");
port!(TackyCfg, "TackyCfg.rs");
port!(TackyGen, "TackyGen.rs");
port!(TackyPrint, "TackyPrint.rs");
port!(TokStream, "TokStream.rs");
port!(Tokens, "Tokens.rs");
port!(TypeTable, "TypeTable.rs");
port!(Types, "Types.rs");

port!(AssemblySymbols, "backend/AssemblySymbols.rs");
port!(Codegen, "backend/Codegen.rs");
port!(InstructionFixup, "backend/InstructionFixup.rs");
port!(Regalloc, "backend/Regalloc.rs");
port!(ReplacePseudos, "backend/ReplacePseudos.rs");
port!(AddressTaken, "optimizations/AddressTaken.rs");
port!(Optimize, "optimizations/Optimize.rs");
port!(LabelLoops, "semantic_analysis/LabelLoops.rs");
port!(Resolve, "semantic_analysis/Resolve.rs");
port!(Typecheck, "semantic_analysis/Typecheck.rs");
port!(DisjointSets, "util/DisjointSets.rs");
port!(Int8, "util/Int8.rs");
port!(ListUtil, "util/ListUtil.rs");
port!(ResultCE, "util/ResultCE.rs");
port!(Rounding, "util/Rounding.rs");
port!(StringUtil, "util/StringUtil.rs");
port!(TypeUtils, "util/TypeUtils.rs");
port!(UniqueIds, "util/UniqueIds.rs");
