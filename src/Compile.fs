module Compile

exception private LexError of string
exception private ParseError of string
exception private ResolveError of string
exception private LoopLabelError of string

let private compileInner (config: Settings.CompilerConfig) (stage: Settings.Stage) (optimizations: Settings.Optimizations) (src_file: string) (source: string) =
    let counter = UniqueIds.initialCounter
    // Lex it
    let tokens =
        match Lexer.lex source with
        | Ok toks -> toks
        | Error msg -> raise (LexError msg)
    if stage = Settings.Lex then ()
    else
        let ast =
            match Parse.parse tokens with
            | Ok a -> a
            | Error msg -> raise (ParseError msg)
        if stage = Settings.Parse then
            printf "%A" ast
        else
            // Semantic analysis has three steps:
            // 1. resolve identifiers
            let counter', resolved_ast =
                match Resolve.resolve counter ast with
                | Ok r -> r
                | Error msg -> raise (ResolveError msg)
            // 2. annotate loops and break/continue statements
            let counter'', annotated_ast =
                match Label_loops.labelLoops counter' resolved_ast with
                | Ok r -> r
                | Error msg -> raise (LoopLabelError msg)
            // 3. typecheck definitions and uses of functions and variables
            // Sync shared counter before typecheck (it uses Symbols.addString which uses the shared counter)
            UniqueIds.sharedCounter <- counter''
            let typed_ast = Typecheck.typecheck annotated_ast
            let counter_after_typecheck = UniqueIds.sharedCounter
            if stage = Settings.Validate then ()
            else
                // Convert the AST to TACKY
                let counter''', tacky = TackyGen.gen counter_after_typecheck typed_ast
                // print to file (src filename with .debug.tacky extension) if debug is
                // enabled
                let counter'''' = Tacky_print.debugPrintTacky config.Debug counter''' src_file tacky
                // optimize it!
                let optimized_tacky = Optimize.optimize optimizations src_file tacky
                if stage = Settings.Tacky then ()
                else
                    // start by getting all aliased vars, we'll need them for register
                    // allocation
                    let aliased_vars = Address_taken.analyzeProgram optimized_tacky
                    // Assembly generation has three steps:
                    // 1. convert TACKY to assembly
                    let _counter5, asm_ast = Codegen.gen counter'''' optimized_tacky
                    // Sync shared counters for modules that still use them (e.g. Cfg.printGraphviz)
                    UniqueIds.sharedCounter <- _counter5
                    Cfg.setCounter _counter5
                    // print pre-pseudoreg-allocation assembly if debug enabled
                    if config.Debug then
                        let prealloc_filename =
                            System.IO.Path.ChangeExtension(src_file, null) + ".prealloc.debug.s"
                        Emit.emit config.Platform prealloc_filename asm_ast
                    // replace remaining pseudoregisters with Data/Stack operands
                    let asm_ast1 = Regalloc.allocateRegisters config.Debug aliased_vars asm_ast
                    if config.Debug then
                        let postalloc_filename =
                            System.IO.Path.ChangeExtension(src_file, null) + ".postalloc.debug.s"
                        Emit.emit config.Platform postalloc_filename asm_ast
                    let asm_ast2 = ReplacePseudos.replacePseudos asm_ast1
                    // fix up instructions
                    let asm_ast3 = InstructionFixup.fixupProgram asm_ast2
                    if stage = Settings.Codegen then ()
                    else
                        let asm_filename = System.IO.Path.ChangeExtension(src_file, ".s")
                        Emit.emit config.Platform asm_filename asm_ast3

let compile (config: Settings.CompilerConfig) (stage: Settings.Stage) (optimizations: Settings.Optimizations) (src_file: string) (source: string) : Result<unit, CompilerError.CompilerError> =
    try
        Ok (compileInner config stage optimizations src_file source)
    with
    | LexError msg -> Error (CompilerError.LexError msg)
    | ParseError msg -> Error (CompilerError.ParseError msg)
    | ResolveError msg -> Error (CompilerError.ResolveError msg)
    | LoopLabelError msg -> Error (CompilerError.LoopLabelError msg)
    | Failure msg when not (msg.StartsWith("Internal error")) ->
        Error (CompilerError.TypeError msg)
