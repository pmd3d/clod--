module TackyGen

open Ast.Ops
module Ast = Ast.Typed
module T = Tacky

let break_label id = "break." + id
let continue_label id = "continue." + id

(* use this as the "result" of void expressions that don't return a result *)
let dummy_operand = T.Constant Const.int_zero

let create_tmp t =
    let name = UniqueIds.makeTemporary ()
    Symbols.add_automatic_var name t
    name

let get_ptr_scale = function
    | Types.Pointer referenced -> int (TypeUtils.getSize referenced)
    | t ->
        failwith
            ("Internal error: tried to get scale of non-pointer type: "
             + Types.show t)

let get_member_offset ``member`` = function
    | Types.Structure tag ->
        try
            (Map.find ``member`` (TypeTable.find tag).members).offset
        with :? System.Collections.Generic.KeyNotFoundException ->
            failwith
                ("Internal error: failed to find member "
                 + ``member``
                 + " in structure "
                 + tag)
    | t ->
        failwith
            ("Internal error: tried to get offset of member "
             + ``member``
             + " within non-structure type "
             + Types.show t)

let get_member_pointer_offset ``member`` = function
    | Types.Pointer t -> get_member_offset ``member`` t
    | t ->
        failwith
            ("Internal error: trying to get member through pointer but "
             + Types.show t
             + " is not a pointer type")

let convert_op = function
    | Complement -> T.Complement
    | Negate -> T.Negate
    | Not -> T.Not

let convert_binop = function
    | Add -> T.Add
    | Subtract -> T.Subtract
    | Multiply -> T.Multiply
    | Divide -> T.Divide
    | Mod -> T.Mod
    | Equal -> T.Equal
    | NotEqual -> T.NotEqual
    | LessThan -> T.LessThan
    | LessOrEqual -> T.LessOrEqual
    | GreaterThan -> T.GreaterThan
    | GreaterOrEqual -> T.GreaterOrEqual
    | And | Or ->
        failwith
            "Internal error, cannot convert these directly to TACKY binops"

let eval_size t =
    let size = TypeUtils.getSize t
    T.Constant(Const.ConstULong(uint64 size))

(* an expression result that may or may not be lvalue converted *)
type exp_result =
    | PlainOperand of T.TackyVal
    | DereferencedPointer of T.TackyVal
    | SubObject of string * int

(* return list of instructions to evaluate expression and resulting exp_result
   value as a pair *)
let rec emit_tacky_for_exp (exp: Ast.Exp) =
    let e = exp.e
    let t = exp.t
    match e with
    (* don't need any instructions to calculate a constant or variable *)
    | Ast.InnerExp.Constant c -> ([], PlainOperand(T.Constant c))
    | Ast.InnerExp.Var v -> ([], PlainOperand(T.Var v))
    | Ast.InnerExp.String s ->
        let str_id = Symbols.add_string s
        ([], PlainOperand(T.Var str_id))
    | Ast.InnerExp.Cast(target_type, e) ->
        emit_cast_expression target_type e
    | Ast.InnerExp.Unary(op, inner) -> emit_unary_expression t op inner
    | Ast.InnerExp.Binary(And, e1, e2) -> emit_and_expression e1 e2
    | Ast.InnerExp.Binary(Or, e1, e2) -> emit_or_expression e1 e2
    | Ast.InnerExp.Binary(Add, e1, e2) when TypeUtils.isPointer t ->
        emit_pointer_addition t e1 e2
    | Ast.InnerExp.Binary(Subtract, ptr, index) when TypeUtils.isPointer t ->
        emit_subtraction_from_pointer t ptr index
    | Ast.InnerExp.Binary(Subtract, e1, e2) when TypeUtils.isPointer e1.t ->
        (* at least one operand is pointer but result isn't, must be subtracting
           one pointer from another *)
        emit_pointer_diff t e1 e2
    | Ast.InnerExp.Binary(op, e1, e2) -> emit_binary_expression t op e1 e2
    | Ast.InnerExp.Assignment(lhs, rhs) -> emit_assignment lhs rhs
    | Ast.InnerExp.Conditional(condition, then_result, else_result) ->
        emit_conditional_expression t condition then_result else_result
    | Ast.InnerExp.FunCall(f, args) -> emit_fun_call t f args
    | Ast.InnerExp.Dereference inner -> emit_dereference inner
    | Ast.InnerExp.AddrOf inner -> emit_addr_of t inner
    | Ast.InnerExp.Subscript(ptr, index) ->
        emit_subscript t ptr index
    | Ast.InnerExp.SizeOfT st -> ([], PlainOperand(eval_size st))
    | Ast.InnerExp.SizeOf inner -> ([], PlainOperand(eval_size inner.t))
    | Ast.InnerExp.Dot(strct, mbr) ->
        emit_dot_operator t strct mbr
    | Ast.InnerExp.Arrow(strct, mbr) ->
        emit_arrow_operator t strct mbr

(* helper functions for individual expression *)
and emit_unary_expression t op inner =
    let eval_inner, v = emit_tacky_and_convert inner
    (* define a temporary variable to hold result of this expression *)
    let dst_name = create_tmp t
    let dst = T.Var dst_name
    let tacky_op = convert_op op
    let instructions =
        eval_inner @ [ T.Unary { op = tacky_op; src = v; dst = dst } ]
    (instructions, PlainOperand dst)

and emit_cast_expression target_type inner =
    let eval_inner, result = emit_tacky_and_convert inner
    let inner_type = TypeUtils.getType inner
    if inner_type = target_type || target_type = Types.Void then
        (eval_inner, PlainOperand result)
    else
        let dst_name = create_tmp target_type
        let dst = T.Var dst_name
        let cast_instruction =
            match (target_type, inner_type) with
            | Types.Double, _ ->
                if TypeUtils.isSigned inner_type then
                    T.IntToDouble { src = result; dst = dst }
                else T.UIntToDouble { src = result; dst = dst }
            | _, Types.Double ->
                if TypeUtils.isSigned target_type then
                    T.DoubleToInt { src = result; dst = dst }
                else T.DoubleToUInt { src = result; dst = dst }
            | _ ->
                (* cast b/t int types *)
                if TypeUtils.getSize target_type = TypeUtils.getSize inner_type
                then T.Copy { src = result; dst = dst }
                else if
                    TypeUtils.getSize target_type < TypeUtils.getSize inner_type
                then T.Truncate { src = result; dst = dst }
                else if TypeUtils.isSigned inner_type then
                    T.SignExtend { src = result; dst = dst }
                else T.ZeroExtend { src = result; dst = dst }
        let instructions = eval_inner @ [ cast_instruction ]
        (instructions, PlainOperand dst)

and emit_pointer_addition t e1 e2 =
    let eval_v1, v1 = emit_tacky_and_convert e1
    let eval_v2, v2 = emit_tacky_and_convert e2
    let dst_name = create_tmp t
    let dst = T.Var dst_name
    let ptr, index = if t = e1.t then (v1, v2) else (v2, v1)
    let scale = get_ptr_scale t
    let instructions =
        eval_v1 @ eval_v2 @ [ T.AddPtr { ptr = ptr; index = index;
                                          scale = scale; dst = dst } ]
    (instructions, PlainOperand dst)

and emit_subscript t e1 e2 =
    let instructions, result =
        emit_pointer_addition (Types.Pointer t) e1 e2
    match result with
    | PlainOperand dst -> (instructions, DereferencedPointer dst)
    | _ ->
        failwith
            "Internal error: expected result of pointer addition to be lvalue \
             converted"

and emit_subtraction_from_pointer t ptr_e idx_e =
    let eval_v1, ptr = emit_tacky_and_convert ptr_e
    let eval_v2, index = emit_tacky_and_convert idx_e
    let dst_name = create_tmp t
    let dst = T.Var dst_name
    let negated_index = T.Var(create_tmp Types.Long)
    let scale = get_ptr_scale t
    (eval_v1
     @ eval_v2
     @ [ T.Unary { op = T.Negate; src = index; dst = negated_index }
         T.AddPtr { ptr = ptr; index = negated_index; scale = scale;
                    dst = dst } ],
     PlainOperand dst)

and emit_pointer_diff t e1 e2 =
    let eval_v1, v1 = emit_tacky_and_convert e1
    let eval_v2, v2 = emit_tacky_and_convert e2
    let ptr_diff = T.Var(create_tmp Types.Long)
    let dst_name = create_tmp t
    let dst = T.Var dst_name
    let scale =
        T.Constant(Const.ConstLong(int64 (get_ptr_scale e1.t)))
    (eval_v1
     @ eval_v2
     @ [ T.Binary { op = T.Subtract; src1 = v1; src2 = v2; dst = ptr_diff }
         T.Binary { op = T.Divide; src1 = ptr_diff; src2 = scale;
                    dst = dst } ],
     PlainOperand dst)

and emit_binary_expression t op e1 e2 =
    let eval_v1, v1 = emit_tacky_and_convert e1
    let eval_v2, v2 = emit_tacky_and_convert e2
    let dst_name = create_tmp t
    let dst = T.Var dst_name
    let tacky_op = convert_binop op
    let instructions =
        eval_v1
        @ eval_v2
        @ [ T.Binary { op = tacky_op; src1 = v1; src2 = v2; dst = dst } ]
    (instructions, PlainOperand dst)

and emit_and_expression e1 e2 =
    let eval_v1, v1 = emit_tacky_and_convert e1
    let eval_v2, v2 = emit_tacky_and_convert e2
    let false_label = UniqueIds.makeLabel "and_false"
    let end_label = UniqueIds.makeLabel "and_end"
    let dst_name = create_tmp Types.Int
    let dst = T.Var dst_name
    let instructions =
        eval_v1
        @ [ T.JumpIfZero(v1, false_label) ]
        @ eval_v2
        @ [ T.JumpIfZero(v2, false_label)
            T.Copy { src = T.Constant Const.int_one; dst = dst }
            T.Jump end_label
            T.Label false_label
            T.Copy { src = T.Constant Const.int_zero; dst = dst }
            T.Label end_label ]
    (instructions, PlainOperand dst)

and emit_or_expression e1 e2 =
    let eval_v1, v1 = emit_tacky_and_convert e1
    let eval_v2, v2 = emit_tacky_and_convert e2
    let true_label = UniqueIds.makeLabel "or_true"
    let end_label = UniqueIds.makeLabel "or_end"
    let dst_name = create_tmp Types.Int
    let dst = T.Var dst_name
    let instructions =
        eval_v1
        @ (T.JumpIfNotZero(v1, true_label) :: eval_v2)
        @ T.JumpIfNotZero(v2, true_label)
          :: T.Copy { src = T.Constant Const.int_zero; dst = dst }
          :: T.Jump end_label
          :: T.Label true_label
          :: T.Copy { src = T.Constant Const.int_one; dst = dst }
          :: [ T.Label end_label ]
    (instructions, PlainOperand dst)

and emit_assignment lhs rhs =
    let lhs_instructions, lval = emit_tacky_for_exp lhs
    let rhs_instructions, rval = emit_tacky_and_convert rhs
    let instructions = lhs_instructions @ rhs_instructions
    match lval with
    | PlainOperand o ->
        (instructions @ [ T.Copy { src = rval; dst = o } ], lval)
    | DereferencedPointer ptr ->
        (instructions @ [ T.Store {| src = rval; dst_ptr = ptr |} ],
         PlainOperand rval)
    | SubObject(``base``, offset) ->
        (instructions @ [ T.CopyToOffset { src = rval; offset = offset;
                                           dst = ``base`` } ],
         PlainOperand rval)

and emit_conditional_expression t condition e1 e2 =
    let eval_cond, c = emit_tacky_and_convert condition
    let eval_v1, v1 = emit_tacky_and_convert e1
    let eval_v2, v2 = emit_tacky_and_convert e2
    let e2_label = UniqueIds.makeLabel "conditional_else"
    let end_label = UniqueIds.makeLabel "conditional_end"
    let dst =
        if t = Types.Void then dummy_operand
        else
            let dst_name = create_tmp t
            T.Var dst_name
    let common_instructions =
        eval_cond @ (T.JumpIfZero(c, e2_label) :: eval_v1)
    let remaining_instructions =
        if t = Types.Void then
            (T.Jump end_label :: T.Label e2_label :: eval_v2)
            @ [ T.Label end_label ]
        else
            T.Copy { src = v1; dst = dst }
            :: T.Jump end_label
            :: T.Label e2_label
            :: eval_v2
            @ (T.Copy { src = v2; dst = dst } :: [ T.Label end_label ])
    (common_instructions @ remaining_instructions, PlainOperand dst)

and emit_fun_call t f args =
    let dst =
        if t = Types.Void then None
        else
            let dst_name = create_tmp t
            Some(T.Var dst_name)
    let arg_instructions, arg_vals =
        List.unzip (List.map emit_tacky_and_convert args)
    let instructions =
        List.concat arg_instructions
        @ [ T.FunCall { f = f; args = arg_vals; dst = dst } ]
    let dst_val = Option.defaultValue dummy_operand dst
    (instructions, PlainOperand dst_val)

and emit_dereference inner =
    let instructions, result = emit_tacky_and_convert inner
    (instructions, DereferencedPointer result)

and emit_dot_operator t (strct: Ast.Exp) mbr =
    let member_offset = get_member_offset mbr strct.t
    let instructions, inner_object = emit_tacky_for_exp strct
    match inner_object with
    | PlainOperand(T.Var v) ->
        (instructions, SubObject(v, member_offset))
    | SubObject(``base``, offset) ->
        (instructions, SubObject(``base``, offset + member_offset))
    | DereferencedPointer ptr ->
        if member_offset = 0 then (instructions, DereferencedPointer ptr)
        else
            let dst = T.Var(create_tmp (Types.Pointer t))
            let index = T.Constant(Const.ConstLong(int64 member_offset))
            let add_ptr_instr =
                T.AddPtr { ptr = ptr; index = index; scale = 1; dst = dst }
            (instructions @ [ add_ptr_instr ], DereferencedPointer dst)
    | PlainOperand(T.Constant _) ->
        failwith
            "Internal error: found dot operator applied to constant"

and emit_arrow_operator t (strct: Ast.Exp) mbr =
    let member_offset = get_member_pointer_offset mbr strct.t
    let instructions, ptr = emit_tacky_and_convert strct
    if member_offset = 0 then (instructions, DereferencedPointer ptr)
    else
        let dst = T.Var(create_tmp (Types.Pointer t))
        let index = T.Constant(Const.ConstLong(int64 member_offset))
        let add_ptr_instr =
            T.AddPtr { ptr = ptr; index = index; scale = 1; dst = dst }
        (instructions @ [ add_ptr_instr ], DereferencedPointer dst)

and emit_addr_of t inner =
    let instructions, result = emit_tacky_for_exp inner
    match result with
    | PlainOperand o ->
        let dst = T.Var(create_tmp t)
        (instructions @ [ T.GetAddress { src = o; dst = dst } ],
         PlainOperand dst)
    | DereferencedPointer ptr -> (instructions, PlainOperand ptr)
    | SubObject(``base``, offset) ->
        let dst = T.Var(create_tmp t)
        let get_addr = T.GetAddress { src = T.Var ``base``; dst = dst }
        if offset = 0 then
            (* skip AddPtr if offset is 0 *)
            (instructions @ [ get_addr ], PlainOperand dst)
        else
            let index = T.Constant(Const.ConstLong(int64 offset))
            (instructions
             @ [ get_addr
                 T.AddPtr { ptr = dst; index = index; scale = 1;
                            dst = dst } ],
             PlainOperand dst)

and emit_tacky_and_convert e =
    let instructions, result = emit_tacky_for_exp e
    match result with
    | PlainOperand o -> (instructions, o)
    | DereferencedPointer ptr ->
        let dst = T.Var(create_tmp e.t)
        (instructions @ [ T.Load {| src_ptr = ptr; dst = dst |} ], dst)
    | SubObject(``base``, offset) ->
        let dst = T.Var(create_tmp e.t)
        (instructions
         @ [ T.CopyFromOffset { src = ``base``; offset = offset; dst = dst } ],
         dst)

let rec emit_string_init dst offset (s: byte[]) =
    let len = Bytes.length s
    if len = 0 then []
    else if len >= 8 then
        let l = Bytes.get_int64_le s 0
        let instr =
            T.CopyToOffset { src = T.Constant(Const.ConstLong l);
                             dst = dst; offset = offset }
        let rest = Bytes.sub s 8 (len - 8)
        instr :: emit_string_init dst (offset + 8) rest
    else if len >= 4 then
        let i = Bytes.get_int32_le s 0
        let instr =
            T.CopyToOffset { src = T.Constant(Const.ConstInt i);
                             dst = dst; offset = offset }
        let rest = Bytes.sub s 4 (len - 4)
        instr :: emit_string_init dst (offset + 4) rest
    else
        let c = Bytes.get_int8 s 0
        let instr =
            T.CopyToOffset { src = T.Constant(Const.ConstChar c);
                             dst = dst; offset = offset }
        let rest = Bytes.sub s 1 (len - 1)
        instr :: emit_string_init dst (offset + 1) rest

let rec emit_compound_init name offset = function
    | Ast.Initializr.SingleInit { e = Ast.InnerExp.String s; t = Types.Array(_, size) } ->
        let str_bytes = Bytes.ofString s
        let padding_bytes = Bytes.make (int size - String.length s) (char 0)
        emit_string_init name offset (Bytes.cat str_bytes padding_bytes)
    | Ast.Initializr.SingleInit e ->
        let eval_init, v = emit_tacky_and_convert e
        eval_init
        @ [ T.CopyToOffset { src = v; dst = name; offset = offset } ]
    | Ast.Initializr.CompoundInit(Types.Array(elem_type, _), inits) ->
        let handle_init idx elem_init =
            let new_offset =
                offset + (idx * int (TypeUtils.getSize elem_type))
            emit_compound_init name new_offset elem_init
        List.concat (List.mapi handle_init inits)
    | Ast.Initializr.CompoundInit(Types.Structure tag, inits) ->
        let members = TypeTable.get_members tag
        let process_init (memb: TypeTable.member_entry) init =
            let mem_offset = offset + memb.offset
            emit_compound_init name mem_offset init
        List.concat (List.map2 process_init members inits)
    | Ast.Initializr.CompoundInit(_, _) ->
        failwith "Internal error: compound init has non-array type!"

let rec emit_tacky_for_statement = function
    | Ast.Return e ->
        let eval_exp, v =
            match Option.map emit_tacky_and_convert e with
            | Some(instrs, result) -> (instrs, Some result)
            | None -> ([], None)
        eval_exp @ [ T.Return v ]
    | Ast.Expression e ->
        (* evaluate expression but don't use result *)
        let eval_exp, _ = emit_tacky_for_exp e
        eval_exp
    | Ast.If(condition, then_clause, else_clause) ->
        emit_tacky_for_if_statement condition then_clause else_clause
    | Ast.Compound(Ast.Block items) ->
        List.collect emit_tacky_for_block_item items
    | Ast.Break id -> [ T.Jump(break_label id) ]
    | Ast.Continue id -> [ T.Jump(continue_label id) ]
    | Ast.DoWhile(body, condition, id) ->
        emit_tacky_for_do_loop body condition id
    | Ast.While(condition, body, id) ->
        emit_tacky_for_while_loop condition body id
    | Ast.For(init, condition, post, body, id) ->
        emit_tacky_for_for_loop init condition post body id
    | Ast.Null -> []

and emit_tacky_for_block_item = function
    | Ast.S s -> emit_tacky_for_statement s
    | Ast.D d -> emit_local_declaration d

and emit_local_declaration = function
    | Ast.VarDecl { storageClass = Some _ } -> []
    | Ast.VarDecl vd -> emit_var_declaration vd
    | Ast.FunDecl _ -> []
    | Ast.StructDecl _ -> []

and emit_var_declaration = function
    | { name = name; init = Some(Ast.Initializr.SingleInit({ e = Ast.InnerExp.String _; t = Types.Array _ }) as string_init) } ->
        emit_compound_init name 0 string_init
    | { name = name; init = Some(Ast.Initializr.SingleInit e); varType = varType } ->
        (* treat declaration with initializer like an assignment expression *)
        let eval_assignment, _assign_result =
            emit_assignment { e = Ast.InnerExp.Var name; t = varType } e
        eval_assignment
    | { name = name; init = Some compound_init } ->
        emit_compound_init name 0 compound_init
    | { init = None } ->
        (* don't generate instructions for declaration without initializer *)
        []

and emit_tacky_for_if_statement condition then_clause = function
    | None ->
        (* no else clause *)
        let end_label = UniqueIds.makeLabel "if_end"
        let eval_condition, c = emit_tacky_and_convert condition
        eval_condition
        @ (T.JumpIfZero(c, end_label)
           :: emit_tacky_for_statement then_clause)
        @ [ T.Label end_label ]
    | Some else_clause ->
        let else_label = UniqueIds.makeLabel "else"
        let end_label = UniqueIds.makeLabel ""
        let eval_condition, c = emit_tacky_and_convert condition
        eval_condition
        @ (T.JumpIfZero(c, else_label)
           :: emit_tacky_for_statement then_clause)
        @ T.Jump end_label
          :: T.Label else_label
          :: emit_tacky_for_statement else_clause
        @ [ T.Label end_label ]

and emit_tacky_for_do_loop body condition id =
    let start_label = UniqueIds.makeLabel "do_loop_start"
    let cont_label = continue_label id
    let br_label = break_label id
    let eval_condition, c = emit_tacky_and_convert condition
    (T.Label start_label :: emit_tacky_for_statement body)
    @ (T.Label cont_label :: eval_condition)
    @ [ T.JumpIfNotZero(c, start_label); T.Label br_label ]

and emit_tacky_for_while_loop condition body id =
    let cont_label = continue_label id
    let br_label = break_label id
    let eval_condition, c = emit_tacky_and_convert condition
    (T.Label cont_label :: eval_condition)
    @ (T.JumpIfZero(c, br_label) :: emit_tacky_for_statement body)
    @ [ T.Jump cont_label; T.Label br_label ]

and emit_tacky_for_for_loop init condition post body id =
    (* generate some labels *)
    let start_label = UniqueIds.makeLabel "for_start"
    let cont_label = continue_label id
    let br_label = break_label id
    let for_init_instructions =
        match init with
        | Ast.InitDecl d -> emit_var_declaration d
        | Ast.InitExp e ->
            match Option.map emit_tacky_for_exp e with
            | Some(instrs, _) -> instrs
            | None -> []
    let test_condition =
        match Option.map emit_tacky_and_convert condition with
        | Some(instrs, v) -> instrs @ [ T.JumpIfZero(v, br_label) ]
        | None -> []
    let post_instructions =
        match Option.map emit_tacky_for_exp post with
        | Some(instrs, _post_result) -> instrs
        | None -> []
    for_init_instructions
    @ (T.Label start_label :: test_condition)
    @ emit_tacky_for_statement body
    @ (T.Label cont_label :: post_instructions)
    @ [ T.Jump start_label; T.Label br_label ]

let emit_fun_declaration = function
    | Ast.FunDecl { name = name; ``params`` = ``params``;
                    body = Some(Ast.Block block_items) } ->
        let ``global`` = Symbols.is_global name
        let body_instructions =
            List.collect emit_tacky_for_block_item block_items
        let extra_return =
            T.Return(Some(T.Constant Const.int_zero))
        Some(
            T.Function { name = name; ``global`` = ``global``; ``params`` = ``params``;
                         body = body_instructions @ [ extra_return ] })
    | _ -> None

let convert_symbols_to_tacky all_symbols =
    let to_var (name, entry: Symbols.entry) =
        match entry.attrs with
        | Symbols.StaticAttr { init = init; ``global`` = ``global`` } ->
            match init with
            | Symbols.Initial i ->
                Some(T.StaticVariable { name = name; t = entry.t;
                                        ``global`` = ``global``; init = i })
            | Symbols.Tentative ->
                Some(
                    T.StaticVariable { name = name; t = entry.t;
                                       ``global`` = ``global``;
                                       init = Initializers.zero entry.t })
            | Symbols.NoInitializer -> None
        | Symbols.ConstAttr init ->
            Some(T.StaticConstant { name = name; t = entry.t; init = init })
        | _ -> None
    List.choose to_var all_symbols

let gen (Ast.Program decls) =
    let tacky_fn_defs = List.choose emit_fun_declaration decls
    let tacky_var_defs =
        convert_symbols_to_tacky (Symbols.bindings ())
    Tacky.Program(tacky_var_defs @ tacky_fn_defs)