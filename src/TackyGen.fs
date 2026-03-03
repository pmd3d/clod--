module TackyGen

open Ast.Ops
module Ast = Ast.Typed
module T = Tacky

let breakLabel id = "break." + id
let continueLabel id = "continue." + id

(* use this as the "result" of void expressions that don't return a result *)
let dummyOperand = T.Constant Const.intZero

let createTmp counter t =
    let counter', name = UniqueIds.makeTemporary counter
    Symbols.addAutomaticVar name t
    (counter', name)

let getPtrScale = function
    | Types.Pointer referenced -> int (TypeUtils.getSize referenced)
    | t ->
        failwith
            ("Internal error: tried to get scale of non-pointer type: "
             + Types.show t)

let getMemberOffset ``member`` = function
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

let getMemberPointerOffset ``member`` = function
    | Types.Pointer t -> getMemberOffset ``member`` t
    | t ->
        failwith
            ("Internal error: trying to get member through pointer but "
             + Types.show t
             + " is not a pointer type")

let convertOp = function
    | Complement -> T.Complement
    | Negate -> T.Negate
    | Not -> T.Not

let convertBinop = function
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

let evalSize t =
    let size = TypeUtils.getSize t
    T.Constant(Const.ConstULong(uint64 size))

(* an expression result that may or may not be lvalue converted *)
type ExpResult =
    | PlainOperand of T.TackyVal
    | DereferencedPointer of T.TackyVal
    | SubObject of string * int

(* return (counter, instructions, ExpResult) *)
let rec emitTackyForExp counter (exp: Ast.Exp) =
    let e = exp.e
    let t = exp.t
    match e with
    | Ast.InnerExp.Constant c -> (counter, [], PlainOperand(T.Constant c))
    | Ast.InnerExp.Var v -> (counter, [], PlainOperand(T.Var v))
    | Ast.InnerExp.String s ->
        let counter', str_id = Symbols.addStringWithCounter counter s
        (counter', [], PlainOperand(T.Var str_id))
    | Ast.InnerExp.Cast(target_type, e) ->
        emitCastExpression counter target_type e
    | Ast.InnerExp.Unary(op, inner) -> emitUnaryExpression counter t op inner
    | Ast.InnerExp.Binary(And, e1, e2) -> emitAndExpression counter e1 e2
    | Ast.InnerExp.Binary(Or, e1, e2) -> emitOrExpression counter e1 e2
    | Ast.InnerExp.Binary(Add, e1, e2) when TypeUtils.isPointer t ->
        emitPointerAddition counter t e1 e2
    | Ast.InnerExp.Binary(Subtract, ptr, index) when TypeUtils.isPointer t ->
        emitSubtractionFromPointer counter t ptr index
    | Ast.InnerExp.Binary(Subtract, e1, e2) when TypeUtils.isPointer e1.t ->
        emitPointerDiff counter t e1 e2
    | Ast.InnerExp.Binary(op, e1, e2) -> emitBinaryExpression counter t op e1 e2
    | Ast.InnerExp.Assignment(lhs, rhs) -> emitAssignment counter lhs rhs
    | Ast.InnerExp.Conditional(condition, then_result, else_result) ->
        emitConditionalExpression counter t condition then_result else_result
    | Ast.InnerExp.FunCall(f, args) -> emitFunCall counter t f args
    | Ast.InnerExp.Dereference inner -> emitDereference counter inner
    | Ast.InnerExp.AddrOf inner -> emitAddrOf counter t inner
    | Ast.InnerExp.Subscript(ptr, index) ->
        emitSubscript counter t ptr index
    | Ast.InnerExp.SizeOfT st -> (counter, [], PlainOperand(evalSize st))
    | Ast.InnerExp.SizeOf inner -> (counter, [], PlainOperand(evalSize inner.t))
    | Ast.InnerExp.Dot(strct, mbr) ->
        emitDotOperator counter t strct mbr
    | Ast.InnerExp.Arrow(strct, mbr) ->
        emitArrowOperator counter t strct mbr

and emitUnaryExpression counter t op inner =
    let counter', eval_inner, v = emitTackyAndConvert counter inner
    let counter'', dst_name = createTmp counter' t
    let dst = T.Var dst_name
    let tacky_op = convertOp op
    let instructions =
        eval_inner @ [ T.Unary { op = tacky_op; src = v; dst = dst } ]
    (counter'', instructions, PlainOperand dst)

and emitCastExpression counter target_type inner =
    let counter', eval_inner, result = emitTackyAndConvert counter inner
    let inner_type = TypeUtils.getType inner
    if inner_type = target_type || target_type = Types.Void then
        (counter', eval_inner, PlainOperand result)
    else
        let counter'', dst_name = createTmp counter' target_type
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
                if TypeUtils.getSize target_type = TypeUtils.getSize inner_type
                then T.Copy { src = result; dst = dst }
                else if
                    TypeUtils.getSize target_type < TypeUtils.getSize inner_type
                then T.Truncate { src = result; dst = dst }
                else if TypeUtils.isSigned inner_type then
                    T.SignExtend { src = result; dst = dst }
                else T.ZeroExtend { src = result; dst = dst }
        let instructions = eval_inner @ [ cast_instruction ]
        (counter'', instructions, PlainOperand dst)

and emitPointerAddition counter t e1 e2 =
    let counter', eval_v1, v1 = emitTackyAndConvert counter e1
    let counter'', eval_v2, v2 = emitTackyAndConvert counter' e2
    let counter''', dst_name = createTmp counter'' t
    let dst = T.Var dst_name
    let ptr, index = if t = e1.t then (v1, v2) else (v2, v1)
    let scale = getPtrScale t
    let instructions =
        eval_v1 @ eval_v2 @ [ T.AddPtr { ptr = ptr; index = index;
                                          scale = scale; dst = dst } ]
    (counter''', instructions, PlainOperand dst)

and emitSubscript counter t e1 e2 =
    let counter', instructions, result =
        emitPointerAddition counter (Types.Pointer t) e1 e2
    match result with
    | PlainOperand dst -> (counter', instructions, DereferencedPointer dst)
    | _ ->
        failwith
            "Internal error: expected result of pointer addition to be lvalue \
             converted"

and emitSubtractionFromPointer counter t ptr_e idx_e =
    let counter', eval_v1, ptr = emitTackyAndConvert counter ptr_e
    let counter'', eval_v2, index = emitTackyAndConvert counter' idx_e
    let counter''', dst_name = createTmp counter'' t
    let dst = T.Var dst_name
    let counter'''', neg_name = createTmp counter''' Types.Long
    let negated_index = T.Var neg_name
    let scale = getPtrScale t
    (counter'''',
     eval_v1
     @ eval_v2
     @ [ T.Unary { op = T.Negate; src = index; dst = negated_index }
         T.AddPtr { ptr = ptr; index = negated_index; scale = scale;
                    dst = dst } ],
     PlainOperand dst)

and emitPointerDiff counter t e1 e2 =
    let counter', eval_v1, v1 = emitTackyAndConvert counter e1
    let counter'', eval_v2, v2 = emitTackyAndConvert counter' e2
    let counter''', diff_name = createTmp counter'' Types.Long
    let ptr_diff = T.Var diff_name
    let counter'''', dst_name = createTmp counter''' t
    let dst = T.Var dst_name
    let scale =
        T.Constant(Const.ConstLong(int64 (getPtrScale e1.t)))
    (counter'''',
     eval_v1
     @ eval_v2
     @ [ T.Binary { op = T.Subtract; src1 = v1; src2 = v2; dst = ptr_diff }
         T.Binary { op = T.Divide; src1 = ptr_diff; src2 = scale;
                    dst = dst } ],
     PlainOperand dst)

and emitBinaryExpression counter t op e1 e2 =
    let counter', eval_v1, v1 = emitTackyAndConvert counter e1
    let counter'', eval_v2, v2 = emitTackyAndConvert counter' e2
    let counter''', dst_name = createTmp counter'' t
    let dst = T.Var dst_name
    let tacky_op = convertBinop op
    let instructions =
        eval_v1
        @ eval_v2
        @ [ T.Binary { op = tacky_op; src1 = v1; src2 = v2; dst = dst } ]
    (counter''', instructions, PlainOperand dst)

and emitAndExpression counter e1 e2 =
    let counter', eval_v1, v1 = emitTackyAndConvert counter e1
    let counter'', eval_v2, v2 = emitTackyAndConvert counter' e2
    let counter''', false_label = UniqueIds.makeLabel "and_false" counter''
    let counter'''', end_label = UniqueIds.makeLabel "and_end" counter'''
    let c5, dst_name = createTmp counter'''' Types.Int
    let dst = T.Var dst_name
    let instructions =
        eval_v1
        @ [ T.JumpIfZero(v1, false_label) ]
        @ eval_v2
        @ [ T.JumpIfZero(v2, false_label)
            T.Copy { src = T.Constant Const.intOne; dst = dst }
            T.Jump end_label
            T.Label false_label
            T.Copy { src = T.Constant Const.intZero; dst = dst }
            T.Label end_label ]
    (c5, instructions, PlainOperand dst)

and emitOrExpression counter e1 e2 =
    let counter', eval_v1, v1 = emitTackyAndConvert counter e1
    let counter'', eval_v2, v2 = emitTackyAndConvert counter' e2
    let counter''', true_label = UniqueIds.makeLabel "or_true" counter''
    let counter'''', end_label = UniqueIds.makeLabel "or_end" counter'''
    let c5, dst_name = createTmp counter'''' Types.Int
    let dst = T.Var dst_name
    let instructions =
        eval_v1
        @ (T.JumpIfNotZero(v1, true_label) :: eval_v2)
        @ T.JumpIfNotZero(v2, true_label)
          :: T.Copy { src = T.Constant Const.intZero; dst = dst }
          :: T.Jump end_label
          :: T.Label true_label
          :: T.Copy { src = T.Constant Const.intOne; dst = dst }
          :: [ T.Label end_label ]
    (c5, instructions, PlainOperand dst)

and emitAssignment counter lhs rhs =
    let counter', lhs_instructions, lval = emitTackyForExp counter lhs
    let counter'', rhs_instructions, rval = emitTackyAndConvert counter' rhs
    let instructions = lhs_instructions @ rhs_instructions
    match lval with
    | PlainOperand o ->
        (counter'', instructions @ [ T.Copy { src = rval; dst = o } ], lval)
    | DereferencedPointer ptr ->
        (counter'', instructions @ [ T.Store {| src = rval; dst_ptr = ptr |} ],
         PlainOperand rval)
    | SubObject(``base``, offset) ->
        (counter'', instructions @ [ T.CopyToOffset { src = rval; offset = offset;
                                           dst = ``base`` } ],
         PlainOperand rval)

and emitConditionalExpression counter t condition e1 e2 =
    let counter', eval_cond, c = emitTackyAndConvert counter condition
    let counter'', eval_v1, v1 = emitTackyAndConvert counter' e1
    let counter''', eval_v2, v2 = emitTackyAndConvert counter'' e2
    let counter'''', e2_label = UniqueIds.makeLabel "conditional_else" counter'''
    let c5, end_label = UniqueIds.makeLabel "conditional_end" counter''''
    let c6, dst =
        if t = Types.Void then (c5, dummyOperand)
        else
            let c6', dst_name = createTmp c5 t
            (c6', T.Var dst_name)
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
    (c6, common_instructions @ remaining_instructions, PlainOperand dst)

and emitFunCall counter t f args =
    let counter', dst =
        if t = Types.Void then (counter, None)
        else
            let c', dst_name = createTmp counter t
            (c', Some(T.Var dst_name))
    let counter'', arg_results =
        List.fold (fun (c, acc) arg ->
            let c', instrs, v = emitTackyAndConvert c arg
            (c', acc @ [(instrs, v)])) (counter', []) args
    let arg_instructions = List.collect fst arg_results
    let arg_vals = List.map snd arg_results
    let instructions =
        arg_instructions
        @ [ T.FunCall { f = f; args = arg_vals; dst = dst } ]
    let dst_val = Option.defaultValue dummyOperand dst
    (counter'', instructions, PlainOperand dst_val)

and emitDereference counter inner =
    let counter', instructions, result = emitTackyAndConvert counter inner
    (counter', instructions, DereferencedPointer result)

and emitDotOperator counter t (strct: Ast.Exp) mbr =
    let member_offset = getMemberOffset mbr strct.t
    let counter', instructions, inner_object = emitTackyForExp counter strct
    match inner_object with
    | PlainOperand(T.Var v) ->
        (counter', instructions, SubObject(v, member_offset))
    | SubObject(``base``, offset) ->
        (counter', instructions, SubObject(``base``, offset + member_offset))
    | DereferencedPointer ptr ->
        if member_offset = 0 then (counter', instructions, DereferencedPointer ptr)
        else
            let counter'', dst_name = createTmp counter' (Types.Pointer t)
            let dst = T.Var dst_name
            let index = T.Constant(Const.ConstLong(int64 member_offset))
            let add_ptr_instr =
                T.AddPtr { ptr = ptr; index = index; scale = 1; dst = dst }
            (counter'', instructions @ [ add_ptr_instr ], DereferencedPointer dst)
    | PlainOperand(T.Constant _) ->
        failwith
            "Internal error: found dot operator applied to constant"

and emitArrowOperator counter t (strct: Ast.Exp) mbr =
    let member_offset = getMemberPointerOffset mbr strct.t
    let counter', instructions, ptr = emitTackyAndConvert counter strct
    if member_offset = 0 then (counter', instructions, DereferencedPointer ptr)
    else
        let counter'', dst_name = createTmp counter' (Types.Pointer t)
        let dst = T.Var dst_name
        let index = T.Constant(Const.ConstLong(int64 member_offset))
        let add_ptr_instr =
            T.AddPtr { ptr = ptr; index = index; scale = 1; dst = dst }
        (counter'', instructions @ [ add_ptr_instr ], DereferencedPointer dst)

and emitAddrOf counter t inner =
    let counter', instructions, result = emitTackyForExp counter inner
    match result with
    | PlainOperand o ->
        let counter'', dst_name = createTmp counter' t
        let dst = T.Var dst_name
        (counter'', instructions @ [ T.GetAddress { src = o; dst = dst } ],
         PlainOperand dst)
    | DereferencedPointer ptr -> (counter', instructions, PlainOperand ptr)
    | SubObject(``base``, offset) ->
        let counter'', dst_name = createTmp counter' t
        let dst = T.Var dst_name
        let get_addr = T.GetAddress { src = T.Var ``base``; dst = dst }
        if offset = 0 then
            (counter'', instructions @ [ get_addr ], PlainOperand dst)
        else
            let index = T.Constant(Const.ConstLong(int64 offset))
            (counter'', instructions
             @ [ get_addr
                 T.AddPtr { ptr = dst; index = index; scale = 1;
                            dst = dst } ],
             PlainOperand dst)

and emitTackyAndConvert counter e =
    let counter', instructions, result = emitTackyForExp counter e
    match result with
    | PlainOperand o -> (counter', instructions, o)
    | DereferencedPointer ptr ->
        let counter'', dst_name = createTmp counter' e.t
        let dst = T.Var dst_name
        (counter'', instructions @ [ T.Load {| src_ptr = ptr; dst = dst |} ], dst)
    | SubObject(``base``, offset) ->
        let counter'', dst_name = createTmp counter' e.t
        let dst = T.Var dst_name
        (counter'',
         instructions
         @ [ T.CopyFromOffset { src = ``base``; offset = offset; dst = dst } ],
         dst)

let rec emitStringInit dst offset (s: byte[]) =
    let len = Bytes.length s
    if len = 0 then []
    else if len >= 8 then
        let l = Bytes.getInt64Le s 0
        let instr =
            T.CopyToOffset { src = T.Constant(Const.ConstLong l);
                             dst = dst; offset = offset }
        let rest = Bytes.sub s 8 (len - 8)
        instr :: emitStringInit dst (offset + 8) rest
    else if len >= 4 then
        let i = Bytes.getInt32Le s 0
        let instr =
            T.CopyToOffset { src = T.Constant(Const.ConstInt i);
                             dst = dst; offset = offset }
        let rest = Bytes.sub s 4 (len - 4)
        instr :: emitStringInit dst (offset + 4) rest
    else
        let c = Bytes.getInt8 s 0
        let instr =
            T.CopyToOffset { src = T.Constant(Const.ConstChar c);
                             dst = dst; offset = offset }
        let rest = Bytes.sub s 1 (len - 1)
        instr :: emitStringInit dst (offset + 1) rest

let rec emitCompoundInit counter name offset = function
    | Ast.Initializer.SingleInit { e = Ast.InnerExp.String s; t = Types.Array(_, size) } ->
        let str_bytes = Bytes.ofString s
        let padding_bytes = Bytes.make (int size - String.length s) (char 0)
        (counter, emitStringInit name offset (Bytes.cat str_bytes padding_bytes))
    | Ast.Initializer.SingleInit e ->
        let counter', eval_init, v = emitTackyAndConvert counter e
        (counter',
         eval_init
         @ [ T.CopyToOffset { src = v; dst = name; offset = offset } ])
    | Ast.Initializer.CompoundInit(Types.Array(elem_type, _), inits) ->
        let counter', instrs =
            List.fold (fun (c, acc_instrs) (idx, elem_init) ->
                let new_offset =
                    offset + (idx * int (TypeUtils.getSize elem_type))
                let c', instrs = emitCompoundInit c name new_offset elem_init
                (c', acc_instrs @ instrs))
                (counter, [])
                (List.mapi (fun i init -> (i, init)) inits)
        (counter', instrs)
    | Ast.Initializer.CompoundInit(Types.Structure tag, inits) ->
        let members = TypeTable.getMembers tag
        let counter', instrs =
            List.fold2 (fun (c, acc_instrs) (memb: TypeTable.MemberDef) init ->
                let mem_offset = offset + memb.offset
                let c', instrs = emitCompoundInit c name mem_offset init
                (c', acc_instrs @ instrs))
                (counter, []) members inits
        (counter', instrs)
    | Ast.Initializer.CompoundInit(_, _) ->
        failwith "Internal error: compound init has non-array type!"

let rec emitTackyForStatement counter = function
    | Ast.Return e ->
        let counter', eval_exp, v =
            match e with
            | Some expr ->
                let c, instrs, result = emitTackyAndConvert counter expr
                (c, instrs, Some result)
            | None -> (counter, [], None)
        (counter', eval_exp @ [ T.Return v ])
    | Ast.Expression e ->
        let counter', eval_exp, _ = emitTackyForExp counter e
        (counter', eval_exp)
    | Ast.If(condition, then_clause, else_clause) ->
        emitTackyForIfStatement counter condition then_clause else_clause
    | Ast.Compound(Ast.Block items) ->
        let counter', instrs =
            List.fold (fun (c, acc) item ->
                let c', instrs = emitTackyForBlockItem c item
                (c', acc @ instrs)) (counter, []) items
        (counter', instrs)
    | Ast.Break id -> (counter, [ T.Jump(breakLabel id) ])
    | Ast.Continue id -> (counter, [ T.Jump(continueLabel id) ])
    | Ast.DoWhile(body, condition, id) ->
        emitTackyForDoLoop counter body condition id
    | Ast.While(condition, body, id) ->
        emitTackyForWhileLoop counter condition body id
    | Ast.For(init, condition, post, body, id) ->
        emitTackyForForLoop counter init condition post body id
    | Ast.Null -> (counter, [])

and emitTackyForBlockItem counter = function
    | Ast.Stmt s -> emitTackyForStatement counter s
    | Ast.Decl d -> emitLocalDeclaration counter d

and emitLocalDeclaration counter = function
    | Ast.VarDecl { storageClass = Some _ } -> (counter, [])
    | Ast.VarDecl vd -> emitVarDeclaration counter vd
    | Ast.FunDecl _ -> (counter, [])
    | Ast.StructDecl _ -> (counter, [])

and emitVarDeclaration counter = function
    | { name = name; init = Some(Ast.Initializer.SingleInit({ e = Ast.InnerExp.String _; t = Types.Array _ }) as string_init) } ->
        emitCompoundInit counter name 0 string_init
    | { name = name; init = Some(Ast.Initializer.SingleInit e); varType = varType } ->
        let counter', eval_assignment, _assign_result =
            emitAssignment counter { e = Ast.InnerExp.Var name; t = varType } e
        (counter', eval_assignment)
    | { name = name; init = Some compound_init } ->
        emitCompoundInit counter name 0 compound_init
    | { init = None } ->
        (counter, [])

and emitTackyForIfStatement counter condition then_clause = function
    | None ->
        let counter', end_label = UniqueIds.makeLabel "if_end" counter
        let counter'', eval_condition, c = emitTackyAndConvert counter' condition
        let counter''', then_instrs = emitTackyForStatement counter'' then_clause
        (counter''',
         eval_condition
         @ (T.JumpIfZero(c, end_label)
            :: then_instrs)
         @ [ T.Label end_label ])
    | Some else_clause ->
        let counter', else_label = UniqueIds.makeLabel "else" counter
        let counter'', end_label = UniqueIds.makeLabel "" counter'
        let counter''', eval_condition, c = emitTackyAndConvert counter'' condition
        let counter'''', then_instrs = emitTackyForStatement counter''' then_clause
        let c5, else_instrs = emitTackyForStatement counter'''' else_clause
        (c5,
         eval_condition
         @ (T.JumpIfZero(c, else_label)
            :: then_instrs)
         @ T.Jump end_label
           :: T.Label else_label
           :: else_instrs
         @ [ T.Label end_label ])

and emitTackyForDoLoop counter body condition id =
    let counter', start_label = UniqueIds.makeLabel "do_loop_start" counter
    let cont_label = continueLabel id
    let br_label = breakLabel id
    let counter'', body_instrs = emitTackyForStatement counter' body
    let counter''', eval_condition, c = emitTackyAndConvert counter'' condition
    (counter''',
     (T.Label start_label :: body_instrs)
     @ (T.Label cont_label :: eval_condition)
     @ [ T.JumpIfNotZero(c, start_label); T.Label br_label ])

and emitTackyForWhileLoop counter condition body id =
    let cont_label = continueLabel id
    let br_label = breakLabel id
    let counter', eval_condition, c = emitTackyAndConvert counter condition
    let counter'', body_instrs = emitTackyForStatement counter' body
    (counter'',
     (T.Label cont_label :: eval_condition)
     @ (T.JumpIfZero(c, br_label) :: body_instrs)
     @ [ T.Jump cont_label; T.Label br_label ])

and emitTackyForForLoop counter init condition post body id =
    let counter', start_label = UniqueIds.makeLabel "for_start" counter
    let cont_label = continueLabel id
    let br_label = breakLabel id
    let counter'', for_init_instructions =
        match init with
        | Ast.InitDecl d -> emitVarDeclaration counter' d
        | Ast.InitExp e ->
            match e with
            | Some expr ->
                let c, instrs, _ = emitTackyForExp counter' expr
                (c, instrs)
            | None -> (counter', [])
    let counter''', test_condition =
        match condition with
        | Some cond ->
            let c, instrs, v = emitTackyAndConvert counter'' cond
            (c, instrs @ [ T.JumpIfZero(v, br_label) ])
        | None -> (counter'', [])
    let counter'''', body_instrs = emitTackyForStatement counter''' body
    let c5, post_instructions =
        match post with
        | Some p ->
            let c, instrs, _ = emitTackyForExp counter'''' p
            (c, instrs)
        | None -> (counter'''', [])
    (c5,
     for_init_instructions
     @ (T.Label start_label :: test_condition)
     @ body_instrs
     @ (T.Label cont_label :: post_instructions)
     @ [ T.Jump start_label; T.Label br_label ])

let emitFunDeclaration counter = function
    | Ast.FunDecl { name = name; ``params`` = ``params``;
                    body = Some(Ast.Block block_items) } ->
        let ``global`` = Symbols.isGlobal name
        let counter', body_instructions =
            List.fold (fun (c, acc) item ->
                let c', instrs = emitTackyForBlockItem c item
                (c', acc @ instrs)) (counter, []) block_items
        let extra_return =
            T.Return(Some(T.Constant Const.intZero))
        (counter',
         Some(
            T.Function { name = name; ``global`` = ``global``; ``params`` = ``params``;
                         body = body_instructions @ [ extra_return ] }))
    | _ -> (counter, None)

let convertSymbolsToTacky all_symbols =
    let to_var (name, entry: Symbols.SymbolEntry) =
        match entry.attrs with
        | Symbols.StaticAttr { init = init; ``global`` = ``global`` } ->
            match init with
            | Symbols.Initial i ->
                Some(T.StaticVariable { name = name; t = entry.symType;
                                        ``global`` = ``global``; init = i })
            | Symbols.Tentative ->
                Some(
                    T.StaticVariable { name = name; t = entry.symType;
                                       ``global`` = ``global``;
                                       init = Initializers.zero entry.symType })
            | Symbols.NoInitializer -> None
        | Symbols.ConstAttr init ->
            Some(T.StaticConstant { name = name; t = entry.symType; init = init })
        | _ -> None
    List.choose to_var all_symbols

let gen counter (Ast.Program decls) =
    let counter', tacky_fn_defs =
        List.fold (fun (c, acc) decl ->
            let c', result = emitFunDeclaration c decl
            match result with
            | Some fn -> (c', acc @ [fn])
            | None -> (c', acc)) (counter, []) decls
    let tacky_var_defs =
        convertSymbolsToTacky (Symbols.bindings ())
    (counter', Tacky.Program(tacky_var_defs @ tacky_fn_defs))
