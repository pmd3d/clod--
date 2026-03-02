module Tacky_print

open Tacky

// -- Helpers to replace OCaml's Format module --

let inline write (out: System.IO.TextWriter) (s: string) = out.Write(s)

let pp_print_list (pp_sep: System.IO.TextWriter -> unit)
                  (pp_item: System.IO.TextWriter -> 'a -> unit)
                  (out: System.IO.TextWriter)
                  (items: 'a list) =
    items |> List.iteri (fun i item ->
        if i > 0 then pp_sep out
        pp_item out item)

let comma_sep (out: System.IO.TextWriter) = write out ", "

let pp_init_list (out: System.IO.TextWriter) (init_list: Initializers.static_init list) =
    write out "{"
    pp_print_list comma_sep Initializers.pp_static_init out init_list
    write out "}"

let pp_unary_operator (out: System.IO.TextWriter) = function
    | Complement -> write out "~"
    | Negate -> write out "-"
    | Not -> write out "!"

// optional escape_brackets argument lets us escape < and > when instructions
// appear in HTML-style tables in graphviz
let pp_binary_operator (escape_brackets: bool) (out: System.IO.TextWriter) = function
    | Add -> write out "+"
    | Subtract -> write out "-"
    | Multiply -> write out "*"
    | Divide -> write out "/"
    | Mod -> write out "%"
    | Equal -> write out "=="
    | NotEqual -> write out "!="
    | LessThan ->
        let s = if escape_brackets then "&lt;" else "<"
        write out s
    | LessOrEqual ->
        let s = if escape_brackets then "&lt;=" else "<="
        write out s
    | GreaterThan ->
        let s = if escape_brackets then "&gt;" else ">"
        write out s
    | GreaterOrEqual ->
        let s = if escape_brackets then "&gt;=" else ">="
        write out s

let const_to_string = function
    | Const.ConstInt i -> sprintf "%d" i
    | Const.ConstLong l -> sprintf "%d" l + "l"
    | Const.ConstUInt ui -> sprintf "%u" ui + "u"
    | Const.ConstULong ul -> sprintf "%u" ul + "ul"
    | Const.ConstDouble d -> sprintf "%g" d
    | Const.ConstChar c -> sprintf "%d" c
    | Const.ConstUChar uc -> sprintf "%u" uc

let pp_tacky_val (out: System.IO.TextWriter) = function
    | Constant i -> write out (const_to_string i)
    | Var s -> write out s

let pp_tacky_val_list (out: System.IO.TextWriter) (vals: TackyVal list) =
    pp_print_list comma_sep pp_tacky_val out vals

let pp_string_list (out: System.IO.TextWriter) (strs: string list) =
    pp_print_list comma_sep (fun o s -> write o s) out strs

let pp_instruction (escape_brackets: bool) (out: System.IO.TextWriter) = function
    | Return None -> write out "Return"
    | Return (Some v) ->
        write out "Return("
        pp_tacky_val out v
        write out ")"
    | Unary { op = op; src = src; dst = dst } ->
        pp_tacky_val out dst
        write out " = "
        pp_unary_operator out op
        pp_tacky_val out src
    | Binary { op = op; src1 = src1; src2 = src2; dst = dst } ->
        pp_tacky_val out dst
        write out " = "
        pp_tacky_val out src1
        write out " "
        pp_binary_operator escape_brackets out op
        write out " "
        pp_tacky_val out src2
    | Copy { src = src; dst = dst } ->
        pp_tacky_val out dst
        write out " = "
        pp_tacky_val out src
    | Jump s ->
        write out (sprintf "Jump(%s)" s)
    | JumpIfZero (cond, target) ->
        write out "JumpIfZero("
        pp_tacky_val out cond
        write out (sprintf ", %s)" target)
    | JumpIfNotZero (cond, target) ->
        write out "JumpIfNotZero("
        pp_tacky_val out cond
        write out (sprintf ", %s)" target)
    | Label s ->
        out.WriteLine()
        write out (sprintf "%s:" s)
    | FunCall { f = f; args = args; dst = None } ->
        write out (sprintf "%s(" f)
        pp_tacky_val_list out args
        write out ")"
    | FunCall { f = f; args = args; dst = Some dst } ->
        pp_tacky_val out dst
        write out (sprintf " = %s(" f)
        pp_tacky_val_list out args
        write out ")"
    | SignExtend { src = src; dst = dst } ->
        pp_tacky_val out dst
        write out " = SignExtend("
        pp_tacky_val out src
        write out ")"
    | ZeroExtend { src = src; dst = dst } ->
        pp_tacky_val out dst
        write out " = ZeroExtend("
        pp_tacky_val out src
        write out ")"
    | Truncate { src = src; dst = dst } ->
        pp_tacky_val out dst
        write out " = Truncate("
        pp_tacky_val out src
        write out ")"
    | DoubleToInt { src = src; dst = dst } ->
        pp_tacky_val out dst
        write out " = DoubleToInt("
        pp_tacky_val out src
        write out ")"
    | DoubleToUInt { src = src; dst = dst } ->
        pp_tacky_val out dst
        write out " = DoubleToUInt("
        pp_tacky_val out src
        write out ")"
    | IntToDouble { src = src; dst = dst } ->
        pp_tacky_val out dst
        write out " = IntToDouble("
        pp_tacky_val out src
        write out ")"
    | UIntToDouble { src = src; dst = dst } ->
        pp_tacky_val out dst
        write out " = UIntToDouble("
        pp_tacky_val out src
        write out ")"
    | GetAddress { src = src; dst = dst } ->
        pp_tacky_val out dst
        write out " = GetAddress("
        pp_tacky_val out src
        write out ")"
    | Load v ->
        pp_tacky_val out v.dst
        write out " = Load("
        pp_tacky_val out v.src_ptr
        write out ")"
    | Store v ->
        write out "*("
        pp_tacky_val out v.dst_ptr
        write out ") = "
        pp_tacky_val out v.src
    | AddPtr { ptr = ptr; index = index; scale = scale; dst = dst } ->
        pp_tacky_val out dst
        write out " = "
        pp_tacky_val out ptr
        write out " + "
        pp_tacky_val out index
        write out (sprintf " * %d" scale)
    | CopyToOffset { src = src; dst = dst; offset = offset } ->
        write out (sprintf "%s[offset = %d] = " dst offset)
        pp_tacky_val out src
    | CopyFromOffset { src = src; offset = offset; dst = dst } ->
        pp_tacky_val out dst
        write out (sprintf " = %s[offset = %d]" src offset)

let pp_function_definition (escape_brackets: bool) (``global``: bool) (name: string)
                           (``params``: string list) (out: System.IO.TextWriter)
                           (body: TackyInstruction list) =
    if ``global`` then write out "global "
    write out (sprintf "%s(" name)
    pp_string_list out ``params``
    write out "):"
    out.WriteLine()
    write out "    "
    pp_print_list
        (fun o -> o.WriteLine(); write o "    ")
        (pp_instruction escape_brackets)
        out body

let pp_tl (escape_brackets: bool) (out: System.IO.TextWriter) = function
    | Function { name = name; ``global`` = ``global``; ``params`` = ``params``; body = body } ->
        pp_function_definition escape_brackets ``global`` name ``params`` out body
    | StaticVariable { ``global`` = ``global``; name = name; init = init; t = t } ->
        if ``global`` then write out "global "
        Types.pp out t
        write out (sprintf " %s = " name)
        pp_init_list out init
    | StaticConstant { name = name; init = init; t = t } ->
        write out "const "
        Types.pp out t
        write out (sprintf " %s = " name)
        Initializers.pp_static_init out init

let pp_program (escape_brackets: bool) (out: System.IO.TextWriter) (Program tls) =
    pp_print_list
        (fun o -> o.WriteLine(); o.WriteLine())
        (pp_tl escape_brackets)
        out tls
    out.WriteLine()
    out.Flush()

let debug_print_tacky (src_filename: string) tacky_prog =
    if Settings.Debug.Value then
        let tacky_file =
            UniqueIds.makeLabel (System.IO.Path.GetFileNameWithoutExtension(src_filename))
            + ".debug.tacky"
        let chan = new System.IO.StreamWriter(tacky_file)
        pp_program false (chan :> System.IO.TextWriter) tacky_prog
        chan.Close()