module Compile

let compile (stage: Settings.Stage) (optimizations: Settings.Optimizations) (src_file: string) =
    // read in the file - TODO use streams?
    let source = System.IO.File.ReadAllText(src_file)
    // Lex it
    let tokens = Lexer.lex source
    if stage = Settings.Lex then ()
    else
        let ast = Parse.parse tokens
        if stage = Settings.Parse then
            printf "%A" ast
        else
            // Semantic analysis has three steps:
            // 1. resolve identifiers
            let resolved_ast = Resolve.resolve ast
            // 2. annotate loops and break/continue statements
            let annotated_ast = Label_loops.labelLoops resolved_ast
            // 3. typecheck definitions and uses of functions and variables
            let typed_ast = Typecheck.typecheck annotated_ast
            if stage = Settings.Validate then ()
            else
                // Convert the AST to TACKY
                let tacky = TackyGen.gen typed_ast
                // print to file (src filename with .debug.tacky extension) if debug is
                // enabled
                Tacky_print.debugPrintTacky src_file tacky
                // optimize it!
                let optimized_tacky = Optimize.optimize optimizations src_file tacky
                if stage = Settings.Tacky then ()
                else
                    // start by getting all aliased vars, we'll need them for register
                    // allocation
                    let aliased_vars = Address_taken.analyzeProgram optimized_tacky
                    // Assembly generation has three steps:
                    // 1. convert TACKY to assembly
                    let asm_ast = Codegen.gen optimized_tacky
                    // print pre-pseudoreg-allocation assembly if debug enabled
                    if Settings.Debug.Value then
                        let prealloc_filename =
                            System.IO.Path.ChangeExtension(src_file, null) + ".prealloc.debug.s"
                        Emit.emit prealloc_filename asm_ast
                    // replace remaining pseudoregisters with Data/Stack operands
                    let asm_ast1 = Regalloc.allocateRegisters aliased_vars asm_ast
                    if Settings.Debug.Value then
                        let postalloc_filename =
                            System.IO.Path.ChangeExtension(src_file, null) + ".postalloc.debug.s"
                        Emit.emit postalloc_filename asm_ast
                    let asm_ast2 = ReplacePseudos.replacePseudos asm_ast1
                    // fix up instructions
                    let asm_ast3 = InstructionFixup.fixupProgram asm_ast2
                    if stage = Settings.Codegen then ()
                    else
                        let asm_filename = System.IO.Path.ChangeExtension(src_file, ".s")
                        Emit.emit asm_filename asm_ast3