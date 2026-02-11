module Resolve

open Ast.Untyped
open Types

type var_entry = {
    unique_name: string
    from_current_scope: bool
    has_linkage: bool
}

type struct_entry = { unique_tag: string; struct_from_current_scope: bool }

// F#'s Map.map takes (key -> value -> value), unlike OCaml's which takes (value -> value)
let copy_identifier_map (m: Map<string, var_entry>) =
    Map.map (fun _ entry -> { entry with from_current_scope = false }) m

let copy_struct_map m =
    Map.map
        (fun _ entry -> { entry with struct_from_current_scope = false })
        m

// F#'s List.mapFold has different tuple order than OCaml's List.fold_left_map
let foldLeftMap f acc lst =
    let results, finalAcc =
        List.mapFold (fun state x -> let s', r = f state x in (r, s')) acc lst
    (finalAcc, results)

(* replace structure tags in type specifiers *)
let rec resolve_type struct_map = function
    | Structure tag ->
        (match Map.tryFind tag struct_map with
         | Some entry -> Structure entry.unique_tag
         | None -> failwith "specified undeclared structure type")
    | Pointer referenced_t -> Pointer (resolve_type struct_map referenced_t)
    | Array ( elemType = elem_type; size = size ) ->
        let resolved_elem_type = resolve_type struct_map elem_type
        Array ( elemType = resolved_elem_type; size = size )
    | FunType ( paramTypes = param_types; retType = ret_type ) ->
        let resolved_param_types =
            List.map (resolve_type struct_map) param_types
        let resolved_ret_type = resolve_type struct_map ret_type
        FunType
            ( paramTypes = resolved_param_types, retType = resolved_ret_type )
    | t -> t

let rec resolve_exp struct_map id_map = function
    | Exp.Assignment (left, right) ->
        Exp.Assignment
            (resolve_exp struct_map id_map left, resolve_exp struct_map id_map right)
    | Exp.Var v ->
        (match Map.tryFind v id_map with
         | Some entry -> Exp.Var entry.unique_name
         | None -> failwith (sprintf "Undeclared variable %s" v))
    | Exp.Cast { target_type = target_type; e = e } ->
        let resolved_type = resolve_type struct_map target_type
        Exp.Cast { target_type = resolved_type; e = resolve_exp struct_map id_map e }
    | Exp.Unary (op, e) -> Unary (op, resolve_exp struct_map id_map e)
    | Exp.Binary (op, e1, e2) ->
        Exp.Binary
            (op, resolve_exp struct_map id_map e1, resolve_exp struct_map id_map e2)
    | Exp.Conditional { condition = condition; then_result = then_result; else_result = else_result } ->
        Exp.Conditional
            {
                condition = resolve_exp struct_map id_map condition
                then_result = resolve_exp struct_map id_map then_result
                else_result = resolve_exp struct_map id_map else_result
            }
    | Exp.FunCall { f = f; args = args } ->
        (match Map.tryFind f id_map with
         | Some entry ->
             FunCall
                 { f = entry.unique_name; args = List.map (resolve_exp struct_map id_map) args }
         | None -> failwith "Undeclared function!")
    | Exp.Dereference inner -> Dereference (resolve_exp struct_map id_map inner)
    | Exp.AddrOf inner -> AddrOf (resolve_exp struct_map id_map inner)
    | Exp.Subscript { ptr = ptr; index = index } ->
        Exp.Subscript
            {
                ptr = resolve_exp struct_map id_map ptr
                index = resolve_exp struct_map id_map index
            }
    | Exp.SizeOf e -> SizeOf (resolve_exp struct_map id_map e)
    | Exp.SizeOfT t -> SizeOfT (resolve_type struct_map t)
    | Exp.Dot { strct = strct; ``member`` = mbr } ->
        Exp.Dot { strct = resolve_exp struct_map id_map strct; ``member`` = mbr }
    | Arrow { strct = strct; ``member`` = mbr } ->
        Arrow { strct = resolve_exp struct_map id_map strct; ``member`` = mbr }
    | (Constant _ | String _) as c -> c

let resolve_optional_exp struct_map id_map =
    Option.map (resolve_exp struct_map id_map)

let resolve_local_var_helper id_map name storage_class =
    (match Map.tryFind name id_map with
     | Some { from_current_scope = true; has_linkage = has_linkage } ->
         if not (has_linkage && storage_class = Some Extern) then
             failwith "Duplicate variable declaration"
         else ()
     | _ -> ())
    let entry =
        if storage_class = Some Extern then
            { unique_name = name; from_current_scope = true; has_linkage = true }
        else
            let unique_name = UniqueIds.makeNamedTemporary name
            { unique_name = unique_name; from_current_scope = true; has_linkage = false }
    let new_map = Map.add name entry id_map
    (new_map, entry.unique_name)

let rec resolve_initializer struct_map id_map = function
    | Initializr.SingleInit e -> Initializr.SingleInit (resolve_exp struct_map id_map e)
    | Initializr.CompoundInit inits ->
        Initializr.CompoundInit (List.map (resolve_initializer struct_map id_map) inits)

let resolve_local_var_declaration struct_map id_map
        { name = name; varType = var_type; init = init; storageClass = storage_class } =
    let new_id_map, unique_name =
        resolve_local_var_helper id_map name storage_class
    let resolved_type = resolve_type struct_map var_type
    let resolved_init =
        Option.map (resolve_initializer struct_map new_id_map) init
    ( new_id_map,
      {
          name = unique_name
          varType = resolved_type
          init = resolved_init
          storageClass = storage_class
      } )

let resolve_for_init struct_map id_map = function
    | InitExp e -> (id_map, InitExp (resolve_optional_exp struct_map id_map e))
    | InitDecl d ->
        let new_id_map, resolved_decl =
            resolve_local_var_declaration struct_map id_map d
        (new_id_map, InitDecl resolved_decl)

let rec resolve_statement struct_map id_map = function
    | Return e ->
        let resolved_e = Option.map (resolve_exp struct_map id_map) e
        Return resolved_e
    | Expression e -> Expression (resolve_exp struct_map id_map e)
    | Statement.If ( condition = condition; thenClause = then_clause; elseClause = else_clause ) ->
        Statement.If
            (
                condition = resolve_exp struct_map id_map condition,
                thenClause = resolve_statement struct_map id_map then_clause,
                elseClause =
                    Option.map (resolve_statement struct_map id_map) else_clause
            )
    | While { condition = condition; body = body; id = id } ->
        While
            {
                condition = resolve_exp struct_map id_map condition
                body = resolve_statement struct_map id_map body
                id = id
            }
    | DoWhile { body = body; condition = condition; id = id } ->
        DoWhile
            {
                body = resolve_statement struct_map id_map body
                condition = resolve_exp struct_map id_map condition
                id = id
            }
    | For { init = init; condition = condition; post = post; body = body; id = id } ->
        let id_map1 = copy_identifier_map id_map
        let struct_map1 = copy_struct_map struct_map
        let id_map2, resolved_init = resolve_for_init struct_map1 id_map1 init
        For
            {
                init = resolved_init
                condition = resolve_optional_exp struct_map1 id_map2 condition
                post = resolve_optional_exp struct_map1 id_map2 post
                body = resolve_statement struct_map1 id_map2 body
                id = id
            }
    | Compound block ->
        let new_variable_map = copy_identifier_map id_map
        let new_struct_map = copy_struct_map struct_map
        Compound (resolve_block new_struct_map new_variable_map block)
    | (Null | Break _ | Continue _) as s -> s

and resolve_block_item (struct_map, id_map) = function
    | S s ->
        let resolved_s = resolve_statement struct_map id_map s
        ((struct_map, id_map), S resolved_s)
    | D d ->
        let new_maps, resolved_d =
            resolve_local_declaration struct_map id_map d
        (new_maps, D resolved_d)

and resolve_block struct_map id_map (Block items) =
    let _final_maps, resolved_items =
        foldLeftMap resolve_block_item (struct_map, id_map) items
    Block resolved_items

and resolve_local_declaration struct_map id_map = function
    | VarDecl vd ->
        let new_id_map, resolved_vd =
            resolve_local_var_declaration struct_map id_map vd
        ((struct_map, new_id_map), VarDecl resolved_vd)
    | FunDecl { body = Some _ } ->
        failwith "nested function definitions are not allowed"
    | FunDecl { storageClass = Some Static } ->
        failwith "static keyword not allowed on local function declarations"
    | FunDecl fd ->
        let new_id_map, resolved_fd =
            resolve_function_declaration struct_map id_map fd
        ((struct_map, new_id_map), FunDecl resolved_fd)
    | StructDecl sd ->
        let new_struct_map, resolved_sd =
            resolve_structure_declaration struct_map sd
        ((new_struct_map, id_map), StructDecl resolved_sd)

and resolve_params id_map =
    let fold_param new_map param_name =
        resolve_local_var_helper new_map param_name None
    foldLeftMap fold_param id_map

and resolve_function_declaration struct_map id_map fn =
    match Map.tryFind fn.name id_map with
    | Some { from_current_scope = true; has_linkage = false } ->
        failwith "Duplicate declaration"
    | _ ->
        let resolved_type = resolve_type struct_map fn.funType
        let new_entry =
            { unique_name = fn.name; from_current_scope = true; has_linkage = true }
        let new_id_map = Map.add fn.name new_entry id_map
        let inner_id_map = copy_identifier_map new_id_map
        let inner_id_map1, resolved_params =
            resolve_params inner_id_map fn.``params``
        let inner_struct_map = copy_struct_map struct_map
        let resolved_body =
            Option.map (resolve_block inner_struct_map inner_id_map1) fn.body
        ( new_id_map,
          {
              fn with
                  funType = resolved_type
                  ``params`` = resolved_params
                  body = resolved_body
          } )

and resolve_structure_declaration struct_map { tag = tag; members = members } =
    let prev_entry = Map.tryFind tag struct_map
    let new_map, resolved_tag =
        match prev_entry with
        | Some { unique_tag = unique_tag; struct_from_current_scope = true } ->
            (struct_map, unique_tag)
        | _ ->
            let unique_tag = UniqueIds.makeNamedTemporary tag
            let entry = { unique_tag = unique_tag; struct_from_current_scope = true }
            (Map.add tag entry struct_map, unique_tag)
    let resolve_member m =
        { m with memberType = resolve_type new_map m.memberType }
    let resolved_members = List.map resolve_member members
    (new_map, { tag = resolved_tag; members = resolved_members })

let resolve_file_scope_variable_declaration struct_map id_map
        ({ name = name; varType = var_type } as vd: VariableDeclaration) =
    let resolved_vd = { vd with varType = resolve_type struct_map var_type }
    let new_map =
        Map.add name
            { unique_name = name; from_current_scope = true; has_linkage = true }
            id_map
    (new_map, resolved_vd)

let resolve_global_declaration (struct_map, id_map) = function
    | FunDecl fd ->
        let id_map1, fd = resolve_function_declaration struct_map id_map fd
        ((struct_map, id_map1), FunDecl fd)
    | VarDecl vd ->
        let id_map1, resolved_vd =
            resolve_file_scope_variable_declaration struct_map id_map vd
        ((struct_map, id_map1), VarDecl resolved_vd)
    | StructDecl sd ->
        let struct_map1, resolved_sd =
            resolve_structure_declaration struct_map sd
        ((struct_map1, id_map), StructDecl resolved_sd)

let resolve (Program decls) =
    let _, resolved_decls =
        foldLeftMap resolve_global_declaration
            (Map.empty, Map.empty)
            decls
    Program resolved_decls
