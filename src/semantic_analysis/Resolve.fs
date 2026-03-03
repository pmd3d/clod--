module Resolve

open Ast.Untyped
open Types

type ResolvedVar = {
    unique_name: string
    from_current_scope: bool
    has_linkage: bool
}

type ResolvedStruct = { unique_tag: string; struct_from_current_scope: bool }

// F#'s Map.map takes (key -> value -> value), unlike OCaml's which takes (value -> value)
let copyIdentifierMap (m: Map<string, ResolvedVar>) =
    Map.map (fun _ entry -> { entry with from_current_scope = false }) m

let copyStructMap m =
    Map.map
        (fun _ entry -> { entry with struct_from_current_scope = false })
        m

(* replace structure tags in type specifiers *)
let rec resolveType struct_map = function
    | Structure tag ->
        (match Map.tryFind tag struct_map with
         | Some entry -> Structure entry.unique_tag
         | None -> failwith "specified undeclared structure type")
    | Pointer referenced_t -> Pointer (resolveType struct_map referenced_t)
    | Array (elem_type, size) ->
        let resolved_elem_type = resolveType struct_map elem_type
        Array (resolved_elem_type, size)
    | FunType (param_types, ret_type) ->
        let resolved_param_types =
            List.map (resolveType struct_map) param_types
        let resolved_ret_type = resolveType struct_map ret_type
        FunType (resolved_param_types, resolved_ret_type)
    | t -> t

let rec resolveExp struct_map id_map = function
    | Exp.Assignment (left, right) ->
        Exp.Assignment
            (resolveExp struct_map id_map left, resolveExp struct_map id_map right)
    | Exp.Var v ->
        (match Map.tryFind v id_map with
         | Some entry -> Exp.Var entry.unique_name
         | None -> failwith (sprintf "Undeclared variable %s" v))
    | Exp.Cast (target_type, e) ->
        let resolved_type = resolveType struct_map target_type
        Exp.Cast (resolved_type, resolveExp struct_map id_map e)
    | Exp.Unary (op, e) -> Exp.Unary (op, resolveExp struct_map id_map e)
    | Exp.Binary (op, e1, e2) ->
        Exp.Binary
            (op, resolveExp struct_map id_map e1, resolveExp struct_map id_map e2)
    | Exp.Conditional (condition, then_result, else_result) ->
        Exp.Conditional
            (resolveExp struct_map id_map condition,
             resolveExp struct_map id_map then_result,
             resolveExp struct_map id_map else_result)
    | Exp.FunCall (f, args) ->
        (match Map.tryFind f id_map with
         | Some entry ->
             Exp.FunCall
                 (entry.unique_name, List.map (resolveExp struct_map id_map) args)
         | None -> failwith "Undeclared function!")
    | Exp.Dereference inner -> Exp.Dereference (resolveExp struct_map id_map inner)
    | Exp.AddrOf inner -> Exp.AddrOf (resolveExp struct_map id_map inner)
    | Exp.Subscript (ptr, index) ->
        Exp.Subscript
            (resolveExp struct_map id_map ptr,
             resolveExp struct_map id_map index)
    | Exp.SizeOf e -> Exp.SizeOf (resolveExp struct_map id_map e)
    | Exp.SizeOfT t -> Exp.SizeOfT (resolveType struct_map t)
    | Exp.Dot (strct, mbr) ->
        Exp.Dot (resolveExp struct_map id_map strct, mbr)
    | Exp.Arrow (strct, mbr) ->
        Exp.Arrow (resolveExp struct_map id_map strct, mbr)
    | (Exp.Constant _ | Exp.String _) as c -> c

let resolveOptionalExp struct_map id_map =
    Option.map (resolveExp struct_map id_map)

let resolveLocalVarHelper counter id_map name storage_class =
    (match Map.tryFind name id_map with
     | Some { from_current_scope = true; has_linkage = has_linkage } ->
         if not (has_linkage && storage_class = Some Ast.StorageClass.Extern) then
             failwith "Duplicate variable declaration"
         else ()
     | _ -> ())
    let counter', entry =
        if storage_class = Some Ast.StorageClass.Extern then
            (counter, { unique_name = name; from_current_scope = true; has_linkage = true })
        else
            let counter', unique_name = UniqueIds.makeNamedTemporary name counter
            (counter', { unique_name = unique_name; from_current_scope = true; has_linkage = false })
    let new_map = Map.add name entry id_map
    (counter', new_map, entry.unique_name)

let rec resolveInitializer struct_map id_map = function
    | Initializer.SingleInit e -> Initializer.SingleInit (resolveExp struct_map id_map e)
    | Initializer.CompoundInit inits ->
        Initializer.CompoundInit (List.map (resolveInitializer struct_map id_map) inits)

let resolveLocalVarDeclaration counter struct_map id_map
        { name = name; varType = var_type; init = init; storageClass = storage_class } =
    let counter', new_id_map, unique_name =
        resolveLocalVarHelper counter id_map name storage_class
    let resolved_type = resolveType struct_map var_type
    let resolved_init =
        Option.map (resolveInitializer struct_map new_id_map) init
    ( counter', new_id_map,
      {
          name = unique_name
          varType = resolved_type
          init = resolved_init
          storageClass = storage_class
      } )

let resolveForInit counter struct_map id_map = function
    | InitExp e -> (counter, id_map, InitExp (resolveOptionalExp struct_map id_map e))
    | InitDecl d ->
        let counter', new_id_map, resolved_decl =
            resolveLocalVarDeclaration counter struct_map id_map d
        (counter', new_id_map, InitDecl resolved_decl)

let rec resolveStatement counter struct_map id_map = function
    | Return e ->
        let resolved_e = Option.map (resolveExp struct_map id_map) e
        (counter, Return resolved_e)
    | Expression e -> (counter, Expression (resolveExp struct_map id_map e))
    | Statement.If (condition, then_clause, else_clause) ->
        let counter', then' = resolveStatement counter struct_map id_map then_clause
        let counter'', else' =
            match else_clause with
            | Some e ->
                let c, e' = resolveStatement counter' struct_map id_map e
                (c, Some e')
            | None -> (counter', None)
        (counter'',
         Statement.If
            (resolveExp struct_map id_map condition,
             then',
             else'))
    | While (condition, body, id) ->
        let counter', body' = resolveStatement counter struct_map id_map body
        (counter',
         While
            (resolveExp struct_map id_map condition,
             body',
             id))
    | DoWhile (body, condition, id) ->
        let counter', body' = resolveStatement counter struct_map id_map body
        (counter',
         DoWhile
            (body',
             resolveExp struct_map id_map condition,
             id))
    | For (init, condition, post, body, id) ->
        let id_map1 = copyIdentifierMap id_map
        let struct_map1 = copyStructMap struct_map
        let counter', id_map2, resolved_init = resolveForInit counter struct_map1 id_map1 init
        let counter'', body' = resolveStatement counter' struct_map1 id_map2 body
        (counter'',
         For
            (resolved_init,
             resolveOptionalExp struct_map1 id_map2 condition,
             resolveOptionalExp struct_map1 id_map2 post,
             body',
             id))
    | Compound block ->
        let new_variable_map = copyIdentifierMap id_map
        let new_struct_map = copyStructMap struct_map
        let counter', block' = resolveBlock counter new_struct_map new_variable_map block
        (counter', Compound block')
    | (Null | Break _ | Continue _) as s -> (counter, s)

and resolveBlockItem counter (struct_map, id_map) = function
    | Stmt s ->
        let counter', resolved_s = resolveStatement counter struct_map id_map s
        (counter', (struct_map, id_map), Stmt resolved_s)
    | Decl d ->
        let counter', new_maps, resolved_d =
            resolveLocalDeclaration counter struct_map id_map d
        (counter', new_maps, Decl resolved_d)

and resolveBlock counter struct_map id_map (Block items) =
    let counter', _final_maps, resolved_items =
        List.fold (fun (c, maps, acc) item ->
            let c', maps', item' = resolveBlockItem c maps item
            (c', maps', acc @ [item'])) (counter, (struct_map, id_map), []) items
    (counter', Block resolved_items)

and resolveLocalDeclaration counter struct_map id_map = function
    | VarDecl vd ->
        let counter', new_id_map, resolved_vd =
            resolveLocalVarDeclaration counter struct_map id_map vd
        (counter', (struct_map, new_id_map), VarDecl resolved_vd)
    | FunDecl { body = Some _ } ->
        failwith "nested function definitions are not allowed"
    | FunDecl { storageClass = Some Ast.StorageClass.Static } ->
        failwith "static keyword not allowed on local function declarations"
    | FunDecl fd ->
        let counter', new_id_map, resolved_fd =
            resolveFunctionDeclaration counter struct_map id_map fd
        (counter', (struct_map, new_id_map), FunDecl resolved_fd)
    | StructDecl sd ->
        let counter', new_struct_map, resolved_sd =
            resolveStructureDeclaration counter struct_map sd
        (counter', (new_struct_map, id_map), StructDecl resolved_sd)

and resolveParams counter id_map param_names =
    List.fold (fun (c, m, acc) param_name ->
        let c', m', unique = resolveLocalVarHelper c m param_name None
        (c', m', acc @ [unique])) (counter, id_map, []) param_names

and resolveFunctionDeclaration counter struct_map id_map fn =
    match Map.tryFind fn.name id_map with
    | Some { from_current_scope = true; has_linkage = false } ->
        failwith "Duplicate declaration"
    | _ ->
        let resolved_type = resolveType struct_map fn.funType
        let new_entry =
            { unique_name = fn.name; from_current_scope = true; has_linkage = true }
        let new_id_map = Map.add fn.name new_entry id_map
        let inner_id_map = copyIdentifierMap new_id_map
        let counter', inner_id_map1, resolved_params =
            resolveParams counter inner_id_map fn.``params``
        let inner_struct_map = copyStructMap struct_map
        let counter'', resolved_body =
            match fn.body with
            | Some body ->
                let c, b = resolveBlock counter' inner_struct_map inner_id_map1 body
                (c, Some b)
            | None -> (counter', None)
        ( counter'', new_id_map,
          {
              fn with
                  funType = resolved_type
                  ``params`` = resolved_params
                  body = resolved_body
          } )

and resolveStructureDeclaration counter struct_map { tag = tag; members = members } =
    let prev_entry = Map.tryFind tag struct_map
    let counter', new_map, resolved_tag =
        match prev_entry with
        | Some { unique_tag = unique_tag; struct_from_current_scope = true } ->
            (counter, struct_map, unique_tag)
        | _ ->
            let counter', unique_tag = UniqueIds.makeNamedTemporary tag counter
            let entry = { unique_tag = unique_tag; struct_from_current_scope = true }
            (counter', Map.add tag entry struct_map, unique_tag)
    let resolveMember m =
        { m with memberType = resolveType new_map m.memberType }
    let resolved_members = List.map resolveMember members
    (counter', new_map, { tag = resolved_tag; members = resolved_members })

let resolveFileScopeVariableDeclaration struct_map id_map
        ({ name = name; varType = var_type } as vd: VariableDeclaration) =
    let resolved_vd = { vd with varType = resolveType struct_map var_type }
    let new_map =
        Map.add name
            { unique_name = name; from_current_scope = true; has_linkage = true }
            id_map
    (new_map, resolved_vd)

let resolveGlobalDeclaration counter (struct_map, id_map) = function
    | FunDecl fd ->
        let counter', id_map1, fd = resolveFunctionDeclaration counter struct_map id_map fd
        (counter', (struct_map, id_map1), FunDecl fd)
    | VarDecl vd ->
        let id_map1, resolved_vd =
            resolveFileScopeVariableDeclaration struct_map id_map vd
        (counter, (struct_map, id_map1), VarDecl resolved_vd)
    | StructDecl sd ->
        let counter', struct_map1, resolved_sd =
            resolveStructureDeclaration counter struct_map sd
        (counter', (struct_map1, id_map), StructDecl resolved_sd)

let resolve counter (Program decls) =
    let counter', _, resolved_decls =
        List.fold (fun (c, maps, acc) d ->
            let c', maps', d' = resolveGlobalDeclaration c maps d
            (c', maps', acc @ [d'])) (counter, (Map.empty, Map.empty), []) decls
    (counter', Program resolved_decls)
