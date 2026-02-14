module AssemblySymbols

open System.Collections.Generic

// Assuming these types are defined in external modules "Assembly" and "Reg_set"
// based on the input code.
// open Assembly
// open Reg_set

type FunEntry = {
    defined: bool
    bytes_required: int
    return_on_stack: bool
    param_regs: Assembly.reg list
    return_regs: Assembly.reg list
    callee_saved_regs_used: Reg_set.t
}

type ObjEntry = {
    t: Assembly.asm_type
    is_static: bool
    constant: bool
}

type Entry =
    | Fun of FunEntry
    | Obj of ObjEntry

let symbol_table = Dictionary<string, Entry>()

let add_fun fun_name defined return_on_stack param_regs return_regs =
    symbol_table.[fun_name] <- Fun {
        defined = defined
        bytes_required = 0
        callee_saved_regs_used = Reg_set.empty
        return_on_stack = return_on_stack
        param_regs = param_regs
        return_regs = return_regs
    }

let add_var var_name t is_static =
    symbol_table.[var_name] <- Obj { t = t; is_static = is_static; constant = false }

let add_constant const_name t =
    symbol_table.[const_name] <- Obj { t = t; is_static = true; constant = true }

let set_bytes_required fun_name bytes_required =
    let entry' =
        match symbol_table.[fun_name] with
        | Fun f -> Fun { f with bytes_required = bytes_required }
        | Obj _ -> failwith "Internal error: not a function"
    symbol_table.[fun_name] <- entry'

let get_bytes_required fun_name =
    match symbol_table.[fun_name] with
    | Fun f -> f.bytes_required
    | Obj _ -> failwith "Internal error: not a function"

let add_callee_saved_regs_used fun_name regs =
    let entry' =
        match symbol_table.[fun_name] with
        | Fun f ->
            Fun { f with callee_saved_regs_used = Reg_set.union f.callee_saved_regs_used regs }
        | Obj _ -> failwith "Internal error: not a function"
    symbol_table.[fun_name] <- entry'

let get_callee_saved_regs_used fun_name =
    match symbol_table.[fun_name] with
    | Fun f -> f.callee_saved_regs_used
    | Obj _ -> failwith "Internal error: not a function"

let get_size var_name =
    match symbol_table.[var_name] with
    | Obj { t = Assembly.Byte } -> 1
    | Obj { t = Assembly.Longword } -> 4
    | Obj { t = Assembly.Quadword }
    | Obj { t = Assembly.Double } -> 8
    | Obj { t = Assembly.ByteArray { size = size } } -> size
    | Fun _ -> failwith "Internal error: this is a function, not an object"

let get_type var_name =
    match symbol_table.[var_name] with
    | Obj { t = t } -> t
    | Fun _ -> failwith "Internal error: this is a function, not an object"

let get_alignment var_name =
    match symbol_table.[var_name] with
    | Obj { t = Assembly.Byte } -> 1
    | Obj { t = Assembly.Longword } -> 4
    | Obj { t = Assembly.Quadword }
    | Obj { t = Assembly.Double } -> 8
    | Obj { t = Assembly.ByteArray { alignment = alignment } } -> alignment
    | Fun _ -> failwith "Internal error: this is a function, not an object"

let is_defined fun_name =
    match symbol_table.[fun_name] with
    | Fun { defined = defined } -> defined
    | _ -> failwith "Internal error: not a function"

let is_static var_name =
    match symbol_table.[var_name] with
    | Obj o -> o.is_static
    | Fun _ -> failwith "Internal error: functions don't have storage duration"

let is_constant name =
    match symbol_table.[name] with
    | Obj { constant = true } -> true
    | Obj _ -> false
    | Fun _ -> failwith "Internal error: is_constant doesn't make sense for functions"

let returns_on_stack fun_name =
    match symbol_table.[fun_name] with
    | Fun f -> f.return_on_stack
    | Obj _ -> failwith "Internal error: this is an object, not a function"

let param_regs_used fun_name =
    match symbol_table.[fun_name] with
    | Fun f -> f.param_regs
    | Obj _ -> failwith "Internal error: not a function"

let return_regs_used fun_name =
    match symbol_table.[fun_name] with
    | Fun f -> f.return_regs
    | Obj _ -> failwith "Internal error: not a function"
