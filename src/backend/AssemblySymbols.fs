module AssemblySymbols

type AsmFunEntry = {
    defined: bool
    bytes_required: int
    return_on_stack: bool
    param_regs: Assembly.AsmReg list
    return_regs: Assembly.AsmReg list
    callee_saved_regs_used: Reg_set.RegSet
}

type AsmObjEntry = {
    t: Assembly.AsmType
    is_static: bool
    constant: bool
}

type AsmSymbolEntry =
    | Fun of AsmFunEntry
    | Obj of AsmObjEntry

let mutable private _symbolTable: Map<string, AsmSymbolEntry> = Map.empty

let addFun fun_name defined return_on_stack param_regs return_regs =
    _symbolTable <- Map.add fun_name (Fun {
        defined = defined
        bytes_required = 0
        callee_saved_regs_used = Reg_set.empty
        return_on_stack = return_on_stack
        param_regs = param_regs
        return_regs = return_regs
    }) _symbolTable

let addVar var_name t is_static =
    _symbolTable <- Map.add var_name (Obj { t = t; is_static = is_static; constant = false }) _symbolTable

let addConstant const_name t =
    _symbolTable <- Map.add const_name (Obj { t = t; is_static = true; constant = true }) _symbolTable

let private find name = Map.find name _symbolTable

let setBytesRequired fun_name bytes_required =
    let entry' =
        match find fun_name with
        | Fun f -> Fun { f with bytes_required = bytes_required }
        | Obj _ -> failwith "Internal error: not a function"
    _symbolTable <- Map.add fun_name entry' _symbolTable

let getBytesRequired fun_name =
    match find fun_name with
    | Fun f -> f.bytes_required
    | Obj _ -> failwith "Internal error: not a function"

let addCalleeSavedRegsUsed fun_name regs =
    let entry' =
        match find fun_name with
        | Fun f ->
            Fun { f with callee_saved_regs_used = Reg_set.union f.callee_saved_regs_used regs }
        | Obj _ -> failwith "Internal error: not a function"
    _symbolTable <- Map.add fun_name entry' _symbolTable

let getCalleeSavedRegsUsed fun_name =
    match find fun_name with
    | Fun f -> f.callee_saved_regs_used
    | Obj _ -> failwith "Internal error: not a function"

let getSize var_name =
    match find var_name with
    | Obj { t = Assembly.Byte } -> 1
    | Obj { t = Assembly.Longword } -> 4
    | Obj { t = Assembly.Quadword }
    | Obj { t = Assembly.Double } -> 8
    | Obj { t = Assembly.ByteArray { size = size } } -> size
    | Fun _ -> failwith "Internal error: this is a function, not an object"

let getType var_name =
    match find var_name with
    | Obj { t = t } -> t
    | Fun _ -> failwith "Internal error: this is a function, not an object"

let getAlignment var_name =
    match find var_name with
    | Obj { t = Assembly.Byte } -> 1
    | Obj { t = Assembly.Longword } -> 4
    | Obj { t = Assembly.Quadword }
    | Obj { t = Assembly.Double } -> 8
    | Obj { t = Assembly.ByteArray { alignment = alignment } } -> alignment
    | Fun _ -> failwith "Internal error: this is a function, not an object"

let isDefined fun_name =
    match find fun_name with
    | Fun { defined = defined } -> defined
    | _ -> failwith "Internal error: not a function"

let isStatic var_name =
    match find var_name with
    | Obj o -> o.is_static
    | Fun _ -> failwith "Internal error: functions don't have storage duration"

let isConstant name =
    match find name with
    | Obj { constant = true } -> true
    | Obj _ -> false
    | Fun _ -> failwith "Internal error: is_constant doesn't make sense for functions"

let returnsOnStack fun_name =
    match find fun_name with
    | Fun f -> f.return_on_stack
    | Obj _ -> failwith "Internal error: this is an object, not a function"

let paramRegsUsed fun_name =
    match find fun_name with
    | Fun f -> f.param_regs
    | Obj _ -> failwith "Internal error: not a function"

let returnRegsUsed fun_name =
    match find fun_name with
    | Fun f -> f.return_regs
    | Obj _ -> failwith "Internal error: not a function"

// Snapshot/restore for pipeline threading
let getTable () = _symbolTable
let setTable m = _symbolTable <- m
