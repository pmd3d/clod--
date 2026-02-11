module Tacky

type unary_operator = Complement | Negate | Not

type binary_operator =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Mod
    | Equal
    | NotEqual
    | LessThan
    | LessOrEqual
    | GreaterThan
    | GreaterOrEqual

// we need a custom comparison function for constants to make sure that 0.0 and
// -0.0 don't compare equal
let const_compare a b =
    match (a, b) with
    | Const.ConstDouble d1, Const.ConstDouble d2 when d1 = d2 ->
        compare (System.Math.CopySign(1.0, d1)) (System.Math.CopySign(1.0, d2))
    | _ -> Const.compare a b

type tacky_val =
    | Constant of Const.t
    | Var of string

let show_tacky_val = function Constant c -> Const.show c | Var v -> v
let pp_tacky_val (fmt: System.IO.TextWriter) v = fmt.Write(show_tacky_val v)

// TODO maybe this should be in a separate module?
let type_of_val = function
    // note: this reports the type of ConstChar as SChar instead of Char, doesn't
    // matter in this context
    | Constant c -> Const.type_of_const c
    | Var v -> (Symbols.get v).t

type src_dst = { src: tacky_val; dst: tacky_val }

type unary_info = { op: unary_operator; src: tacky_val; dst: tacky_val }

type binary_info = {
    op: binary_operator
    src1: tacky_val
    src2: tacky_val
    dst: tacky_val
}

type add_ptr_info = {
    ptr: tacky_val
    index: tacky_val
    scale: int
    dst: tacky_val
}

type copy_to_offset_info = { src: tacky_val; dst: string; offset: int }

type copy_from_offset_info = { src: string; offset: int; dst: tacky_val }

type fun_call_info = { f: string; args: tacky_val list; dst: tacky_val option }

type instruction =
    | Return of tacky_val option
    | SignExtend of src_dst
    | ZeroExtend of src_dst
    | DoubleToInt of src_dst
    | IntToDouble of src_dst
    | DoubleToUInt of src_dst
    | UIntToDouble of src_dst
    | Truncate of src_dst
    | Unary of unary_info
    | Binary of binary_info
    | Copy of src_dst
    | GetAddress of src_dst
    | Load of {| src_ptr: tacky_val; dst: tacky_val |}
    | Store of {| src: tacky_val; dst_ptr: tacky_val |}
    | AddPtr of add_ptr_info
    | CopyToOffset of copy_to_offset_info
    | CopyFromOffset of copy_from_offset_info
    | Jump of string
    | JumpIfZero of tacky_val * string
    | JumpIfNotZero of tacky_val * string
    | Label of string
    | FunCall of fun_call_info

type function_def = {
    name: string
    ``global``: bool
    ``params``: string list
    body: instruction list
}

type static_variable_def = {
    name: string
    t: Types.t
    ``global``: bool
    init: Initializers.static_init list
}

type static_constant_def = {
    name: string
    t: Types.t
    init: Initializers.static_init
}

type top_level =
    | Function of function_def
    | StaticVariable of static_variable_def
    | StaticConstant of static_constant_def

type t = Program of top_level list