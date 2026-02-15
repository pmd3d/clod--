open System
open System.CommandLine
open System.CommandLine.Invocation
open System.Diagnostics
open System.IO

(* what platform are we on? *)
let currentPlatform =
    let unameOutput =
        let psi = ProcessStartInfo("uname", RedirectStandardOutput = true, UseShellExecute = false)
        let p = Process.Start(psi)
        let result = p.StandardOutput.ReadToEnd()
        p.WaitForExit()
        result.ToLowerInvariant().Trim()
    if unameOutput.StartsWith("darwin") then Settings.OS_X
    else Settings.Linux

(* Utilities *)
let validateExtension (filename: string) =
    let ext = Path.GetExtension(filename)
    if ext = ".c" || ext = ".h" then ()
    else failwith "Expected C source file with .c or .h extension"

let replaceExtension (filename: string) (newExtension: string) =
    Path.ChangeExtension(filename, newExtension)

let runCommand (cmd: string) (args: string list) =
    let argString = args |> List.map (fun a -> "\"" + a + "\"") |> String.concat " "
    let psi = ProcessStartInfo(cmd, argString, UseShellExecute = false)
    let p = Process.Start(psi)
    p.WaitForExit()
    if p.ExitCode <> 0 then
        failwith ("Command failed: " + cmd)

(* main driver *)
let preprocess (src: string) =
    let _ = validateExtension src
    let output = replaceExtension src ".i"
    let _ = runCommand "gcc" [ "-E"; "-P"; src; "-o"; output ]
    output

let compile (stage: Settings.Stage) (optimizations: Settings.Optimizations) (preprocessedSrc: string) =
    let _ = Compile.compile stage optimizations preprocessedSrc
    (* remove preprocessed src *)
    runCommand "rm" [ preprocessedSrc ]
    replaceExtension preprocessedSrc ".s"

let assembleAndLink (link: bool) (cleanup: bool) (libs: string list) (src: string) =
    let linkOption = if link then [] else [ "-c" ]
    let libOptions = List.map (fun l -> "-l" + l) libs
    let assemblyFile = replaceExtension src ".s"
    let outputFile =
        if link then Path.ChangeExtension(src, null) else replaceExtension src ".o"
    let _ =
        runCommand "gcc"
            (linkOption @ [ assemblyFile ] @ libOptions @ [ "-o"; outputFile ])
    (* cleanup .s files *)
    if cleanup then runCommand "rm" [ assemblyFile ]

let driver (target: Settings.Target) (debug: bool) (libs: string list) (stage: Settings.Stage) (optimizations: Settings.Optimizations) (src: string) =
    Settings.Platform := target
    Settings.Debug := debug
    let preprocessedName = preprocess src
    let assemblyName = compile stage optimizations preprocessedName
    match stage with
    | Settings.Executable ->
        assembleAndLink true (not debug) libs assemblyName
    | Settings.Obj ->
        assembleAndLink false (not debug) [] assemblyName
    | _ -> ()

(* Command-line options / optimization options *)
let setOptions optimize constantFolding deadStoreElimination
    copyPropagation unreachableCodeElimination =
    if optimize then
        (* TODO maybe warn if both --optimize and any of the other options are
           set, since those options will be redundant? *)
        { Settings.constant_folding = true
          Settings.dead_store_elimination = true
          Settings.unreachable_code_elimination = true
          Settings.copy_propagation = true }
    else
        { Settings.constant_folding = constantFolding
          Settings.dead_store_elimination = deadStoreElimination
          Settings.copy_propagation = copyPropagation
          Settings.unreachable_code_elimination = unreachableCodeElimination }

(* Command-line parsing via System.CommandLine *)

(* Stage options — mutually exclusive flags, default is Executable *)
let lexOption = Option<bool>("--lex", Description = "Run the lexer")
let parseOption = Option<bool>("--parse", Description = "Run the lexer and parser")
let validateOption = Option<bool>("--validate", Description = "Run the lexer, parser, and semantic analysis")
let tackyOption = Option<bool>("--tacky", Description = "Run the lexer, parser, semantic analysis, and tacky generator")
let codegenOption = Option<bool>("--codegen", Description = "Run through code generation but stop before emitting assembly")
let assemblyOption = Option<bool>("-S", "-s", Description = "Stop before assembling (keep .s file)")
let objOption = Option<bool>("-c", Description = "Stop before invoking linker (keep .o file)")

(* Other options *)
let libsOption = Option<string array>("-l", Description = "Link against library (passed through to assemble/link command)")
do libsOption.AllowMultipleArgumentsPerToken <- true

let targetOption =
    let opt = Option<string>("-t", "--target", Description = "Choose target platform", DefaultValueFactory = fun _ -> if currentPlatform = Settings.OS_X then "osx" else "linux")
    opt.AcceptOnlyFromAmong("linux", "osx") |> ignore
    opt

let debugOption = Option<bool>("-d", Description = "Write out pre- and post-register-allocation assembly and DOT files of interference graphs.")

(* Optimization options *)
let foldConstantsOption = Option<bool>("--fold-constants", Description = "Enable constant folding")
let eliminateDeadStoresOption = Option<bool>("--eliminate-dead-stores", Description = "Enable dead store elimination")
let propagateCopiesOption = Option<bool>("--propagate-copies", Description = "Enable copy-propagation")
let eliminateUnreachableCodeOption = Option<bool>("--eliminate-unreachable-code", Description = "Enable unreachable code elimination")
let optimizeOption = Option<bool>("-o", "--optimize", Description = "Enable optimizations")

(* Positional argument *)
let srcFileArgument = Argument<string>("files", Description = "Source file to compile")

let parseStage (parseResult: ParseResult) =
    if parseResult.GetValue(lexOption) then Settings.Lex
    elif parseResult.GetValue(parseOption) then Settings.Parse
    elif parseResult.GetValue(validateOption) then Settings.Validate
    elif parseResult.GetValue(tackyOption) then Settings.Tacky
    elif parseResult.GetValue(codegenOption) then Settings.Codegen
    elif parseResult.GetValue(assemblyOption) then Settings.Assembly
    elif parseResult.GetValue(objOption) then Settings.Obj
    else Settings.Executable

let parseTarget (parseResult : ParseResult) =
    match parseResult.GetValue(targetOption) with
    | "osx" -> Settings.OS_X
    | "linux" -> Settings.Linux
    | _ -> currentPlatform

(* Expand compact option forms like -lm into -l m for System.CommandLine compatibility *)
let expandCompactOptions (argv: string array) : string array =
    argv
    |> Array.collect (fun arg ->
        // Match pattern like -lXXX where XXX is the library name
        if arg.StartsWith("-l") && arg.Length > 2 then
            // Split "-lm" into [|"-l"; "m"|]
            [| "-l"; arg.Substring(2) |]
        else
            [| arg |]
    )

[<EntryPoint>]
let main argv =
    let rootCommand = RootCommand("A clod-- compiler")
    rootCommand.Options.Add(lexOption)
    rootCommand.Options.Add(parseOption)
    rootCommand.Options.Add(validateOption)
    rootCommand.Options.Add(tackyOption)
    rootCommand.Options.Add(codegenOption)
    rootCommand.Options.Add(assemblyOption)
    rootCommand.Options.Add(objOption)
    rootCommand.Options.Add(libsOption)
    rootCommand.Options.Add(targetOption)
    rootCommand.Options.Add(debugOption)
    rootCommand.Options.Add(foldConstantsOption)
    rootCommand.Options.Add(eliminateDeadStoresOption)
    rootCommand.Options.Add(propagateCopiesOption)
    rootCommand.Options.Add(eliminateUnreachableCodeOption)
    rootCommand.Options.Add(optimizeOption)
    rootCommand.Arguments.Add(srcFileArgument)

    let expandedArgv = expandCompactOptions argv
    let parseResult = rootCommand.Parse(expandedArgv :> System.Collections.Generic.IReadOnlyList<string>)
    if parseResult.Errors.Count > 0 then
        for error in parseResult.Errors do
            eprintfn "Error: %s" error.Message
        eprintfn ""
        eprintfn "Usage: clod-- <file> [options]"
        exit 1
    let stage = parseStage parseResult
    let target = parseTarget parseResult
    let debug = parseResult.GetValue(debugOption)
    let libs = match parseResult.GetValue(libsOption) with
                | null -> []
                | arr -> arr |> Array.toList
    let src = parseResult.GetValue(srcFileArgument)
    if not (File.Exists(src)) then
        eprintfn "Error: file not found: %s" src
        exit 1    
    let optimizations =
        setOptions
            (parseResult.GetValue(optimizeOption))
            (parseResult.GetValue(foldConstantsOption))
            (parseResult.GetValue(eliminateDeadStoresOption))
            (parseResult.GetValue(propagateCopiesOption))
            (parseResult.GetValue(eliminateUnreachableCodeOption))
    driver target debug libs stage optimizations src
    0