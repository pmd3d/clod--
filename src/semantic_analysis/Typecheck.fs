module Typecheck

open Types
open TypeUtils
open StringUtil
open ListUtil
open Ast.TypedExp

module U = Ast.Untyped
module T = Ast.TypedExp
module UE = Ast.UntypedExp

let rec isLvalue { T.e = e } =
    match e with
    | T.Dereference _ | T.Subscript _ | T.Var _ | T.String _ | T.Arrow _ -> true
    | T.Dot(strct, _) -> isLvalue strct
    | _ -> false

let rec validateType = function
    | Types.Array(elem_type, _) ->
        if isComplete elem_type then validateType elem_type
        else failwith "Array of incomplete type"
    | Types.Pointer t -> validateType t
    | FunType(param_types, ret_type) ->
        List.iter validateType param_types
        validateType ret_type
    | Char | SChar | UChar | Int | Long | UInt | ULong | Double | Void
    | Structure _ ->
        ()

let validateStructDefinition { U.tag = tag; U.members = members } =
    if TypeTable.mem tag then failwith "Structure was already declared"
    else
        let validateMember member_names { U.memberName = member_name; U.memberType = member_type } =
            if Set.contains member_name member_names then
                failwith
                    ("Duplicate declaration of member "
                     + member_name
                     + " in structure "
                     + tag)
            validateType member_type
            match member_type with
            | Types.FunType _ ->
                failwith "Can't declare structure member with function type"
            | _ ->
                if isComplete member_type then ()
                else failwith "Cannot declare structure member with incomplete type"
            Set.add member_name member_names
        List.fold validateMember Set.empty members |> ignore

let typecheckStructDecl ({ U.tag = tag; U.members = members } as sd) =
    if members = [] then ()
    else
        (validateStructDefinition sd
         let build_member_def (current_size, current_alignment, current_members)
                 { U.memberName = member_name; U.memberType = member_type } =
             let member_alignment = getAlignment member_type
             let offset =
                 Rounding.roundAwayFromZero member_alignment current_size
             let member_entry = { TypeTable.member_type = member_type; TypeTable.offset = offset }
             let new_alignment = Operators.max current_alignment member_alignment
             let new_size = offset + int (getSize member_type)
             let new_members =
                 Map.add member_name member_entry current_members
             (new_size, new_alignment, new_members)
         let unpadded_size, alignment, member_defs =
             List.fold build_member_def (0, 1, Map.empty) members
         let size = Rounding.roundAwayFromZero alignment unpadded_size
         let struct_def = { TypeTable.alignment = alignment; TypeTable.size = size; TypeTable.members = member_defs }
         TypeTable.addStructDefinition tag struct_def)

    let cvt { U.memberName = member_name; U.memberType = member_type } =
        { Ast.Typed.memberName = member_name; Ast.Typed.memberType = member_type }
    { Ast.Typed.tag = tag; Ast.Typed.members = List.map cvt members }

let convertTo e target_type =
    let cast = T.Cast(target_type, e)
    setType cast target_type

let getCommonType t1 t2 =
    let t1 = if isCharacter t1 then Types.Int else t1
    let t2 = if isCharacter t2 then Types.Int else t2
    if t1 = t2 then t1
    else if t1 = Types.Double || t2 = Double then Double
    else if getSize t1 = getSize t2 then if isSigned t1 then t2 else t1
    else if getSize t1 > getSize t2 then t1
    else t2

let isZeroInt = function
    | Const.ConstInt i when i = 0 -> true
    | Const.ConstLong l when l = 0L -> true
    | Const.ConstUInt u when u = 0u -> true
    | Const.ConstULong ul when ul = 0UL -> true
    | _ -> false

let isNullPointerConstant = function
    | T.Constant c -> isZeroInt c
    | _ -> false

let getCommonPointerType e1 e2 =
    if e1.t = e2.t then e1.t
    else if isNullPointerConstant e1.e then e2.t
    else if isNullPointerConstant e2.e then e1.t
    else if
        (e1.t = Pointer Void && isPointer e2.t)
        || (e2.t = Pointer Void && isPointer e1.t)
    then Pointer Void
    else failwith "Expressions have incompatible types"

let convertByAssignment e target_type =
    if e.t = target_type then e
    else if isArithmetic e.t && isArithmetic target_type then
        convertTo e target_type
    else if isNullPointerConstant e.e && isPointer target_type then
        convertTo e target_type
    else if
        (target_type = Pointer Void && isPointer e.t)
        || (isPointer target_type && e.t = Pointer Void)
    then convertTo e target_type
    else failwith "Cannot convert type for asignment"

let typecheckVar v =
    let v_type = (Symbols.get v).symType
    let e = T.Var v
    match v_type with
    | FunType _ -> failwith "Tried to use function name as variable "
    | _ -> setType e v_type

let typecheckConst c =
    let e = T.Constant c
    setType e (Const.typeOfConst c)

let optTypecheck typecheck_fn = function
    | Some ast_node -> Some (typecheck_fn ast_node)
    | None -> None

let typecheckString s =
    let e = T.String s
    let t = Types.Array(Char, int64 (String.length s + 1))
    setType e t

let rec typecheckExp = function
    | UE.Var v -> typecheckVar v
    | UE.Constant c -> typecheckConst c
    | UE.String s -> typecheckString s
    | UE.Cast(target_type, inner) -> typecheckCast target_type inner
    | UE.Unary (Ast.Ops.Not, inner) -> typecheckNot inner
    | UE.Unary (Ast.Ops.Complement, inner) -> typecheckComplement inner
    | UE.Unary (Ast.Ops.Negate, inner) -> typecheckNegate inner
    | UE.Binary (op, e1, e2) ->
        (match op with
         | Ast.Ops.And | Ast.Ops.Or -> typecheckLogical op e1 e2
         | Ast.Ops.Add -> typecheckAddition e1 e2
         | Ast.Ops.Subtract -> typecheckSubtraction e1 e2
         | Ast.Ops.Multiply | Ast.Ops.Divide | Ast.Ops.Mod -> typecheckMultiplicative op e1 e2
         | Ast.Ops.Equal | Ast.Ops.NotEqual -> typecheckEquality op e1 e2
         | Ast.Ops.GreaterThan | Ast.Ops.GreaterOrEqual | Ast.Ops.LessThan | Ast.Ops.LessOrEqual ->
             typecheckComparison op e1 e2)
    | UE.Assignment (lhs, rhs) -> typecheckAssignment lhs rhs
    | UE.Conditional(condition, then_result, else_result) ->
        typecheckConditional condition then_result else_result
    | UE.FunCall(f, args) -> typecheckFunCall f args
    | UE.Dereference inner -> typecheckDereference inner
    | UE.AddrOf inner -> typecheckAddrOf inner
    | UE.Subscript(ptr, index) -> typecheckSubscript ptr index
    | UE.SizeOfT t -> typecheckSizeOfT t
    | UE.SizeOf e -> typecheckSizeOf e
    | UE.Dot(strct, ``member``) -> typecheckDotOperator strct ``member``
    | UE.Arrow(strct, ``member``) -> typecheckArrowOperator strct ``member``

and typecheckCast target_type inner =
    validateType target_type
    let typed_inner = typecheckAndConvert inner
    match (target_type, typed_inner.t) with
    | Types.Double, Types.Pointer _ | Pointer _, Double ->
        failwith "Cannot cast between pointer and double"
    | Void, _ ->
        let cast_exp = T.Cast(Void, typed_inner)
        setType cast_exp Void
    | _ ->
        if not (isScalar target_type) then
            failwith "Can only cast to scalar types or void"
        else if not (isScalar typed_inner.t) then
            failwith "Can only cast scalar expressions to non-void type"
        else
            let cast_exp = T.Cast(target_type, typed_inner)
            setType cast_exp target_type

and typecheckScalar e =
    let typed_e = typecheckAndConvert e
    if isScalar typed_e.t then typed_e
    else failwith "A scalar operand is required"

and typecheckNot inner =
    let typed_inner = typecheckScalar inner
    let not_exp = T.Unary (Ast.Ops.Not, typed_inner)
    setType not_exp Int

and typecheckComplement inner =
    let typed_inner = typecheckAndConvert inner
    if not (isInteger typed_inner.t) then
        failwith "Bitwise complement only valid for integer types"
    else
        let typed_inner =
            if isCharacter typed_inner.t then convertTo typed_inner Int
            else typed_inner
        let complement_exp = T.Unary (Ast.Ops.Complement, typed_inner)
        setType complement_exp typed_inner.t

and typecheckNegate inner =
    let typed_inner = typecheckAndConvert inner
    if isArithmetic typed_inner.t then
        let typed_inner =
            if isCharacter typed_inner.t then convertTo typed_inner Int
            else typed_inner
        let negate_exp = T.Unary (Ast.Ops.Negate, typed_inner)
        setType negate_exp typed_inner.t
    else failwith "Can only negate arithmetic types"

and typecheckLogical op e1 e2 =
    let typed_e1 = typecheckScalar e1
    let typed_e2 = typecheckScalar e2
    let typed_binexp = T.Binary (op, typed_e1, typed_e2)
    setType typed_binexp Int

and typecheckAddition e1 e2 =
    let typed_e1 = typecheckAndConvert e1
    let typed_e2 = typecheckAndConvert e2
    if isArithmetic typed_e1.t && isArithmetic typed_e2.t then
        let common_type = getCommonType typed_e1.t typed_e2.t
        let converted_e1 = convertTo typed_e1 common_type
        let converted_e2 = convertTo typed_e2 common_type
        let add_exp = T.Binary (Ast.Ops.Add, converted_e1, converted_e2)
        setType add_exp common_type
    else if isCompletePointer typed_e1.t && isInteger typed_e2.t then
        let converted_e2 = convertTo typed_e2 Types.Long
        let add_exp = T.Binary (Ast.Ops.Add, typed_e1, converted_e2)
        setType add_exp typed_e1.t
    else if isCompletePointer typed_e2.t && isInteger typed_e1.t then
        let converted_e1 = convertTo typed_e1 Types.Long
        let add_exp = T.Binary (Ast.Ops.Add, converted_e1, typed_e2)
        setType add_exp typed_e2.t
    else failwith "invalid operands for addition"

and typecheckSubtraction e1 e2 =
    let typed_e1 = typecheckAndConvert e1
    let typed_e2 = typecheckAndConvert e2
    if isArithmetic typed_e1.t && isArithmetic typed_e2.t then
        let common_type = getCommonType typed_e1.t typed_e2.t
        let converted_e1 = convertTo typed_e1 common_type
        let converted_e2 = convertTo typed_e2 common_type
        let sub_exp = T.Binary (Ast.Ops.Subtract, converted_e1, converted_e2)
        setType sub_exp common_type
    else if isCompletePointer typed_e1.t && isInteger typed_e2.t then
        let converted_e2 = convertTo typed_e2 Types.Long
        let sub_exp = T.Binary (Ast.Ops.Subtract, typed_e1, converted_e2)
        setType sub_exp typed_e1.t
    else if isCompletePointer typed_e1.t && typed_e1.t = typed_e2.t then
        let sub_exp = T.Binary (Ast.Ops.Subtract, typed_e1, typed_e2)
        setType sub_exp Types.Long
    else failwith "Invalid operands for subtraction"

and typecheckMultiplicative op e1 e2 =
    let typed_e1 = typecheckAndConvert e1
    let typed_e2 = typecheckAndConvert e2
    if isArithmetic typed_e1.t && isArithmetic typed_e2.t then
        let common_type = getCommonType typed_e1.t typed_e2.t
        let converted_e1 = convertTo typed_e1 common_type
        let converted_e2 = convertTo typed_e2 common_type
        let binary_exp = T.Binary (op, converted_e1, converted_e2)
        match op with
        | Ast.Ops.Mod when common_type = Double -> failwith "Can't apply % to double"
        | Ast.Ops.Multiply | Ast.Ops.Divide | Ast.Ops.Mod -> setType binary_exp common_type
        | _ ->
            failwith
                ("Internal error: "
                 + sprintf "%A" op
                 + " isn't a multiplicative operator")
    else failwith "Can only multiply arithmetic types"

and typecheckEquality op e1 e2 =
    let typed_e1 = typecheckAndConvert e1
    let typed_e2 = typecheckAndConvert e2
    let common_type =
        if isPointer typed_e1.t || isPointer typed_e2.t then
            getCommonPointerType typed_e1 typed_e2
        else if isArithmetic typed_e1.t && isArithmetic typed_e2.t then
            getCommonType typed_e1.t typed_e2.t
        else failwith "Invalid operands for equality"
    let converted_e1 = convertTo typed_e1 common_type
    let converted_e2 = convertTo typed_e2 common_type
    let binary_exp = T.Binary (op, converted_e1, converted_e2)
    setType binary_exp Int

and typecheckComparison op e1 e2 =
    let typed_e1 = typecheckAndConvert e1
    let typed_e2 = typecheckAndConvert e2
    let common_type =
        if isArithmetic typed_e1.t && isArithmetic typed_e2.t then
            getCommonType typed_e1.t typed_e2.t
        else if isPointer typed_e1.t && typed_e1.t = typed_e2.t then typed_e1.t
        else failwith "invalid types for comparions"
    let converted_e1 = convertTo typed_e1 common_type
    let converted_e2 = convertTo typed_e2 common_type
    let binary_exp = T.Binary (op, converted_e1, converted_e2)
    setType binary_exp Int

and typecheckAssignment lhs rhs =
    let typed_lhs = typecheckAndConvert lhs
    if isLvalue typed_lhs then
        let lhs_type = getType typed_lhs
        let typed_rhs = typecheckAndConvert rhs
        let converted_rhs = convertByAssignment typed_rhs lhs_type
        let assign_exp = T.Assignment (typed_lhs, converted_rhs)
        setType assign_exp lhs_type
    else failwith "left hand side of assignment is invalid lvalue"

and typecheckConditional condition then_exp else_exp =
    let typed_conditon = typecheckScalar condition
    let typed_then = typecheckAndConvert then_exp
    let typed_else = typecheckAndConvert else_exp
    let result_type =
        if typed_then.t = Void && typed_else.t = Void then Types.Void
        else if isPointer typed_then.t || isPointer typed_else.t then
            getCommonPointerType typed_then typed_else
        else if isArithmetic typed_then.t && isArithmetic typed_else.t then
            getCommonType typed_then.t typed_else.t
        else if typed_then.t = typed_else.t then typed_then.t
        else failwith "Invalid operands for conditional"
    let converted_then = convertTo typed_then result_type
    let converted_else = convertTo typed_else result_type
    let conditional_exp =
        T.Conditional(typed_conditon, converted_then, converted_else)
    setType conditional_exp result_type

and typecheckFunCall f args =
    let f_type = (Symbols.get f).symType
    match f_type with
    | FunType(param_types, ret_type) ->
        if List.length param_types <> List.length args then
            failwith "Function called with wrong number of arguments"
        else ()
        let process_arg arg param_t =
            convertByAssignment (typecheckAndConvert arg) param_t
        let converted_args = List.map2 process_arg args param_types
        let call_exp = T.FunCall(f, converted_args)
        setType call_exp ret_type
    | _ -> failwith "Tried to use variable as function name"

and typecheckDereference inner =
    let typed_inner = typecheckAndConvert inner
    match getType typed_inner with
    | Pointer Void -> failwith "Can't dereference pointer to void"
    | Pointer referenced_t ->
        let deref_exp = T.Dereference typed_inner
        setType deref_exp referenced_t
    | _ -> failwith "Tried to dereference non-pointer"

and typecheckAddrOf inner =
    let typed_inner = typecheckExp inner
    if isLvalue typed_inner then
        let inner_t = getType typed_inner
        let addr_exp = T.AddrOf typed_inner
        setType addr_exp (Pointer inner_t)
    else failwith "Cannot take address of non-value"

and typecheckSubscript e1 e2 =
    let typed_e1 = typecheckAndConvert e1
    let typed_e2 = typecheckAndConvert e2
    let ptr_type, converted_e1, converted_e2 =
        if isCompletePointer typed_e1.t && isInteger typed_e2.t then
            (typed_e1.t, typed_e1, convertTo typed_e2 Types.Long)
        else if isCompletePointer typed_e2.t && isInteger typed_e1.t then
            (typed_e2.t, convertTo typed_e1 Long, typed_e2)
        else failwith "Invalid types for subscript operation"
    let result_type =
        match ptr_type with
        | Pointer referenced -> referenced
        | _ -> failwith "Internal error typechecking subscript"
    let subscript_exp =
        T.Subscript(converted_e1, converted_e2)
    setType subscript_exp result_type

and typecheckSizeOfT t =
    validateType t
    if isComplete t then
        let sizeof_exp = T.SizeOfT t
        setType sizeof_exp ULong
    else failwith "Can't apply sizeof to incomplete type"

and typecheckSizeOf inner =
    let typed_inner = typecheckExp inner
    if isComplete typed_inner.t then
        let sizeof_exp = T.SizeOf typed_inner
        setType sizeof_exp ULong
    else failwith "Can't apply sizeof to incomplete type"

and typecheckAndConvert e =
    let typed_e = typecheckExp e
    match typed_e.t with
    | Types.Structure _ when not (isComplete typed_e.t) ->
        failwith "Incomplete structure type not permitted here"
    | Types.Array(elem_type, _) ->
        let addr_exp = T.AddrOf typed_e
        setType addr_exp (Pointer elem_type)
    | _ -> typed_e

and typecheckDotOperator strct ``member`` =
    let typed_strct = typecheckAndConvert strct
    match typed_strct.t with
    | Types.Structure tag ->
        let struct_def = TypeTable.find tag
        let member_typ =
            match Map.tryFind ``member`` struct_def.members with
            | Some entry -> entry.member_type
            | None ->
                failwith ("Struct type " + tag + " has no member " + ``member``)
        let dot_exp = T.Dot(typed_strct, ``member``)
        setType dot_exp member_typ
    | _ ->
        failwith
            "Dot operator can only be applied to expressions with structure type"

and typecheckArrowOperator strct_ptr ``member`` =
    let typed_strct_ptr = typecheckAndConvert strct_ptr
    match typed_strct_ptr.t with
    | Types.Pointer (Structure tag) ->
        let struct_def = TypeTable.find tag
        let member_typ =
            match Map.tryFind ``member`` struct_def.members with
            | Some entry -> entry.member_type
            | None ->
                failwith
                    ("Struct type " + tag + " is incomplete or has no member " + ``member``)
        let arrow_exp = T.Arrow(typed_strct_ptr, ``member``)
        setType arrow_exp member_typ
    | _ -> failwith "Arrow operator can only be applied to pointers to structure"

let rec staticInitHelper var_type init =
    match (var_type, init) with
    | Types.Array(elem_type, size), UE.SingleInit (UE.String s) ->
        if isCharacter elem_type then
            (match int size - String.length s with
             | 0 -> [ Initializers.StringInit (s, false) ]
             | 1 -> [ Initializers.StringInit (s, true) ]
             | n when n > 0 ->
                 [ Initializers.StringInit (s, true); Initializers.ZeroInit (n - 1) ]
             | _ -> failwith "string is too long for initializer")
        else
            failwith
                "Can't initialize array of non-character type with string literal"
    | Types.Array _, UE.SingleInit _ ->
        failwith "Can't initialize array from scalar value"
    | Types.Pointer Char, UE.SingleInit (UE.String s) ->
        let str_id = Symbols.addString s
        [ Initializers.PointerInit str_id ]
    | _, UE.SingleInit (UE.String _) ->
        failwith "String literal can only initialize char *"
    | Structure tag, UE.CompoundInit inits ->
        let struct_def = TypeTable.find tag
        let members = TypeTable.getMembers tag
        if List.length inits > List.length members then
            failwith "Too many elements in struct initializer"
        else
            let handle_member (current_offset, current_inits) (memb: TypeTable.MemberDef) init =
                let padding =
                    if current_offset < memb.offset then
                        [ Initializers.ZeroInit (memb.offset - current_offset) ]
                    else []
                let more_static_inits = staticInitHelper memb.member_type init
                let new_inits = current_inits @ padding @ more_static_inits
                let new_offset = memb.offset + int (getSize memb.member_type)
                (new_offset, new_inits)
            let initialized_members = ListUtil.take (List.length inits) members
            let initialized_size, explicit_initializers =
                List.fold2 handle_member (0, []) initialized_members inits
            let trailing_padding =
                if initialized_size < struct_def.size then
                    [ Initializers.ZeroInit (struct_def.size - initialized_size) ]
                else []
            explicit_initializers @ trailing_padding
    | Structure _, UE.SingleInit _ ->
        failwith " Can't initialize static structure with scalar value"
    | _, UE.SingleInit (UE.Constant c) when isZeroInt c ->
        Initializers.zero var_type
    | Types.Pointer _, _ -> failwith "invalid static initializer for pointer"
    | _, UE.SingleInit (UE.Constant c) ->
        if isArithmetic var_type then
            let init_val =
                match ConstConvert.constConvert var_type c with
                | Const.ConstChar c -> Initializers.CharInit c
                | Const.ConstInt i -> Initializers.IntInit i
                | Const.ConstLong l -> Initializers.LongInit l
                | Const.ConstUChar uc -> Initializers.UCharInit uc
                | Const.ConstUInt ui -> Initializers.UIntInit ui
                | Const.ConstULong ul -> Initializers.ULongInit ul
                | Const.ConstDouble d -> Initializers.DoubleInit d
            [ init_val ]
        else
            failwith
                ("Internal error: should have already rejected initializer with type "
                 + Types.show var_type)
    | _, UE.SingleInit _ -> failwith "non-constant initializer"
    | Array(elem_type, size), UE.CompoundInit inits ->
        let static_inits = List.collect (staticInitHelper elem_type) inits
        let padding =
            match int size - List.length inits with
            | 0 -> []
            | n when n > 0 ->
                let zero_bytes = int (getSize elem_type) * n
                [ Initializers.ZeroInit zero_bytes ]
            | _ -> failwith "Too many values in static initializer"
        static_inits @ padding
    | _, UE.CompoundInit _ ->
        failwith "Can't use compound initializer for object with scalar type"

let toStaticInit var_type init =
    let init_list = staticInitHelper var_type init
    Symbols.Initial init_list

let rec makeZeroInit t =
    let scalar c = T.SingleInit { e = Constant c; t = t }
    match t with
    | Types.Array(elem_type, size) ->
        T.CompoundInit (t, ListUtil.makeList (int size) (makeZeroInit elem_type))
    | Structure tag ->
        let members = TypeTable.getMembers tag
        T.CompoundInit
            (t, List.map (fun (m: TypeTable.MemberDef) -> makeZeroInit m.member_type) members)
    | Char | SChar -> scalar (Const.ConstChar 0y)
    | Int -> scalar (Const.ConstInt 0)
    | UChar -> scalar (Const.ConstUChar 0uy)
    | UInt -> scalar (Const.ConstUInt 0u)
    | Long -> scalar (Const.ConstLong 0L)
    | ULong | Pointer _ -> scalar (Const.ConstULong 0UL)
    | Double -> scalar (Const.ConstDouble 0.0)
    | (FunType _ | Void) as t ->
        failwith
            ("Internal error: can't create zero initializer with type"
             + Types.show t)

let rec typecheckInit target_type init =
    match (target_type, init) with
    | Types.Array(elem_type, size), UE.SingleInit (UE.String s) ->
        if not (isCharacter elem_type) then
            failwith "Can't initialize non-character type with string literal"
        else if String.length s > int size then
            failwith "Too many characters in string literal"
        else T.SingleInit (setType (T.String s) target_type)
    | Types.Structure tag, UE.CompoundInit init_list ->
        let members = TypeTable.getMembers tag
        if List.length init_list > List.length members then
            failwith "Too many elements in structure initializer"
        else
            let initialized_members, uninitialized_members =
                ListUtil.takeDrop (List.length init_list) members
            let typechecked_members =
                List.map2
                    (fun (memb: TypeTable.MemberDef) init -> typecheckInit memb.member_type init)
                    initialized_members init_list
            let padding =
                List.map
                    (fun (m: TypeTable.MemberDef) -> makeZeroInit m.member_type)
                    uninitialized_members
            T.CompoundInit (target_type, typechecked_members @ padding)
    | _, UE.SingleInit e ->
        let typechecked_e = typecheckAndConvert e
        let cast_exp = convertByAssignment typechecked_e target_type
        T.SingleInit cast_exp
    | Array(elem_type, size), UE.CompoundInit inits ->
        if List.length inits > int size then
            failwith "too many values in initializer "
        else
            let typechecked_inits = List.map (typecheckInit elem_type) inits
            let padding =
                ListUtil.makeList
                    (int size - List.length inits)
                    (makeZeroInit elem_type)
            T.CompoundInit (target_type, typechecked_inits @ padding)
    | _ -> failwith "Can't initializer scalar value from compound initializer"

let rec typecheckBlock ret_type (U.Block b) =
    Ast.Typed.Block (List.map (typecheckBlockItem ret_type) b)

and typecheckBlockItem ret_type = function
    | U.Stmt s -> Ast.Typed.Stmt (typecheckStatement ret_type s)
    | U.Decl d -> Ast.Typed.Decl (typecheckLocalDecl d)

and typecheckStatement ret_type = function
    | U.Return (Some e) ->
        if ret_type = Types.Void then
            failwith "function with void return type cannot return a value"
        else
            let typed_e =
                convertByAssignment (typecheckAndConvert e) ret_type
            Ast.Typed.Return (Some typed_e)
    | U.Return None ->
        if ret_type = Void then Ast.Typed.Return None
        else failwith "Function with non-void return type must return a value"
    | U.Expression e -> Ast.Typed.Expression (typecheckAndConvert e)
    | U.If(condition, thenClause, elseClause) ->
        Ast.Typed.If(
            typecheckScalar condition,
            typecheckStatement ret_type thenClause,
            Option.map (typecheckStatement ret_type) elseClause
        )
    | U.Compound block -> Ast.Typed.Compound (typecheckBlock ret_type block)
    | U.While(condition, body, id) ->
        Ast.Typed.While(
            typecheckScalar condition,
            typecheckStatement ret_type body,
            id
        )
    | U.DoWhile(body, condition, id) ->
        Ast.Typed.DoWhile(
            typecheckStatement ret_type body,
            typecheckScalar condition,
            id
        )
    | U.For(init, condition, post, body, id) ->
        let typechecked_for_init =
            match init with
            | U.InitDecl { U.storageClass = Some _ } ->
                failwith
                    "Storage class not permitted on declaration in for loop header"
            | U.InitDecl d -> Ast.Typed.InitDecl (typecheckLocalVarDecl d)
            | U.InitExp e -> Ast.Typed.InitExp (optTypecheck typecheckAndConvert e)
        Ast.Typed.For(
            typechecked_for_init,
            optTypecheck typecheckScalar condition,
            optTypecheck typecheckAndConvert post,
            typecheckStatement ret_type body,
            id
        )
    | U.Null -> Ast.Typed.Null
    | U.Break s -> Ast.Typed.Break s
    | U.Continue s -> Ast.Typed.Continue s

and typecheckLocalDecl = function
    | U.VarDecl vd -> Ast.Typed.VarDecl (typecheckLocalVarDecl vd)
    | U.FunDecl fd -> Ast.Typed.FunDecl (typecheckFnDecl fd)
    | U.StructDecl sd -> Ast.Typed.StructDecl (typecheckStructDecl sd)

and typecheckLocalVarDecl ({ U.name = name; U.varType = varType; U.init = init; U.storageClass = storageClass }: U.VariableDeclaration) : Ast.Typed.VariableDeclaration =
    if varType = Void then failwith "No void declarations"
    else validateType varType
    match storageClass with
    | Some Ast.StorageClass.Extern ->
        if Option.isSome init then
            failwith "initializer on local extern declaration"
        else ()
        // If an external local var is already in the symbol table, don't need
        // to add it
        (match Symbols.getOpt name with
         | Some { Symbols.symType = t } ->
             if t <> varType then
                 failwith "Variable redeclared with different type"
             else ()
         | None ->
             Symbols.addStaticVar name varType true Symbols.NoInitializer)
        { Ast.Typed.name = name
          Ast.Typed.init = None
          Ast.Typed.storageClass = storageClass
          Ast.Typed.varType = varType }
    | _ when not (isComplete varType) ->
        // can't define a variable with an incomplete type
        failwith "Cannot define a variable with an incomplete type"
    | Some Ast.StorageClass.Static ->
        let zero_init = Symbols.Initial (Initializers.zero varType)
        let static_init =
            match init with
            | Some i -> toStaticInit varType i
            | None -> zero_init
        Symbols.addStaticVar name varType false static_init
        // NOTE: we won't actually use init in subsequent passes so we can drop it
        { Ast.Typed.name = name
          Ast.Typed.init = None
          Ast.Typed.storageClass = storageClass
          Ast.Typed.varType = varType }
    | None ->
        Symbols.addAutomaticVar name varType
        { Ast.Typed.name = name
          Ast.Typed.varType = varType
          Ast.Typed.storageClass = storageClass
          Ast.Typed.init = optTypecheck (typecheckInit varType) init }
and typecheckFnDecl (fd: U.FunctionDeclaration) : Ast.Typed.FunctionDeclaration =
    let name = fd.name
    let funType = fd.funType
    let ``params`` = fd.``params``
    let body = fd.body
    let storageClass = fd.storageClass
    validateType funType
    // Note: we do this _before_ adjusting param types
    let adjustParamType = function
        | Types.Array(elem_type, _) -> Types.Pointer elem_type
        | Void -> failwith "No void params allowed"
        | t -> t
    let param_ts, return_t, funType =
        match funType with
        | Types.FunType(_, Types.Array(_, _)) ->
            failwith "A function cannot return an array"
        | Types.FunType(param_types, ret_type) ->
            let param_types = List.map adjustParamType param_types
            (param_types, ret_type, Types.FunType(param_types, ret_type))
        | _ ->
            failwith "Internal error, function has non-function type"
    let has_body = Option.isSome body
    // can't define a function with incomplete return or param type
    if
        has_body
        && not
             ((return_t = Void || isComplete return_t)
              && List.forall isComplete param_ts)
    then
        failwith
            "Can't define a function with incomplete return type or parameter type"
    else
        let ``global`` = storageClass <> Some Ast.StorageClass.Static
        // helper function to reconcile current and previous declarations
        let checkAgainstPrevious { Symbols.symType = prev_t; Symbols.attrs = attrs } =
            if prev_t <> funType then
                failwith ("Redeclared function " + name + " with a different type")
            else
                match attrs with
                | Symbols.FunAttr { ``global`` = prev_global; defined = prev_defined } ->
                    if prev_defined && has_body then
                        failwith ("Defined body of function " + name + "twice")
                    else if prev_global && storageClass = Some Ast.StorageClass.Static then
                        failwith "Static function declaration follows non-static"
                    else
                        let defined = has_body || prev_defined
                        (defined, prev_global)
                | _ ->
                    failwith
                        "Internal error: symbol has function type but not function attributes"
        let old_decl = Symbols.getOpt name
        let defined, ``global`` =
            match old_decl with
            | Some old_d -> checkAgainstPrevious old_d
            | None -> (has_body, ``global``)
        Symbols.addFun name funType ``global`` defined
        if has_body then
            List.iter2 (fun p t -> Symbols.addAutomaticVar p t) ``params`` param_ts
        else ()
        let body = Option.map (typecheckBlock return_t) body
        ({ funType = funType
           name = name
           ``params`` = ``params``
           body = body
           storageClass = storageClass } : Ast.Typed.FunctionDeclaration)

let typecheckFileScopeVarDecl
    ({ U.name = name; U.varType = varType; U.init = init; U.storageClass = storageClass }: U.VariableDeclaration)
    =
    if varType = Void then failwith "void variables not allowed"
    else validateType varType
    let default_init =
        if storageClass = Some Ast.StorageClass.Extern then Symbols.NoInitializer else Symbols.Tentative
    let static_init =
        match init with
        | Some i -> toStaticInit varType i
        | None -> default_init
    if not (isComplete varType || static_init = Symbols.NoInitializer) then
        // note: some compilers permit tentative definition with incomplete type, if
        // it's completed later in the file. we don't.
        failwith "Can't define a variable with an incomplete type "
    else
        let current_global = storageClass <> Some Ast.StorageClass.Static
        let old_decl = Symbols.getOpt name
        let checkAgainstPrevious { Symbols.symType = t; Symbols.attrs = attrs } =
            if t <> varType then failwith "Variable redeclared with different type"
            else
                match attrs with
                | Symbols.StaticAttr { ``global`` = prev_global; init = prev_init } ->
                    let ``global`` =
                        if storageClass = Some Ast.StorageClass.Extern then prev_global
                        else if current_global = prev_global then current_global
                        else failwith "Conflicting variable linkage"
                    let init =
                        match (prev_init, static_init) with
                        | Symbols.Initial _, Symbols.Initial _ ->
                            failwith "Conflicting global variable definition"
                        | Symbols.Initial _, _ -> prev_init
                        | Symbols.Tentative, (Symbols.Tentative | Symbols.NoInitializer) -> Symbols.Tentative
                        | _, Symbols.Initial _ | Symbols.NoInitializer, _ -> static_init
                    (``global``, init)
                | _ ->
                    failwith
                        "Internal error, file-scope variable previously declared as \
                         local variable or function"
        let ``global``, init =
            match old_decl with
            | Some old_d -> checkAgainstPrevious old_d
            | None -> (current_global, static_init)
        Symbols.addStaticVar name varType ``global`` init
        // Okay to drop initializer b/c it's never used after this pass
        { Ast.Typed.name = name
          Ast.Typed.varType = varType
          Ast.Typed.init = None
          Ast.Typed.storageClass = storageClass }

let typecheckGlobalDecl = function
    | U.FunDecl fd -> Ast.Typed.FunDecl (typecheckFnDecl fd)
    | U.VarDecl vd -> Ast.Typed.VarDecl (typecheckFileScopeVarDecl vd)
    | U.StructDecl sd -> Ast.Typed.StructDecl (typecheckStructDecl sd)

let typecheck (Ast.Untyped.Program decls) : Ast.Typed.TypedProgram =
    Ast.Typed.Program (List.map typecheckGlobalDecl decls)