module Label_loops

open Ast.Untyped

let rec labelStatement counter current_label = function
  | Break _ ->
      (match current_label with
       | Some l -> (counter, Break l)
       | None -> failwith "Break outside of loop")
  | Continue _ ->
      (match current_label with
       | Some l -> (counter, Continue l)
       | None -> failwith "Continue outside of loop")
  | While(condition, body, _id) ->
      let counter', new_id = UniqueIds.makeLabel "while" counter
      let counter'', body' = labelStatement counter' (Some new_id) body
      (counter'', Statement.While(condition = condition, body = body', id = new_id))
  | DoWhile(body, condition, _id) ->
      let counter', new_id = UniqueIds.makeLabel "do_while" counter
      let counter'', body' = labelStatement counter' (Some new_id) body
      (counter'', DoWhile(body = body', condition = condition, id = new_id))
  | For(init, condition, post, body, _id) ->
      let counter', new_id = UniqueIds.makeLabel "for" counter
      let counter'', body' = labelStatement counter' (Some new_id) body
      (counter'', Statement.For(init = init, condition = condition, post = post, body = body', id = new_id))
  | Compound blk ->
      let counter', blk' = labelBlock counter current_label blk
      (counter', Compound blk')
  | If(condition, thenClause, elseClause) ->
      let counter', thenClause' = labelStatement counter current_label thenClause
      let counter'', elseClause' =
          match elseClause with
          | Some e ->
              let c, e' = labelStatement counter' current_label e
              (c, Some e')
          | None -> (counter', None)
      (counter'', If(condition = condition, thenClause = thenClause', elseClause = elseClause'))
  | (Null | Return _ | Expression _) as s -> (counter, s)

and labelBlockItem counter current_label = function
  | Stmt s ->
      let counter', s' = labelStatement counter current_label s
      (counter', Stmt s')
  | decl -> (counter, decl)

and labelBlock counter current_label (Block b) =
  let counter', items =
      List.fold (fun (c, acc) item ->
          let c', item' = labelBlockItem c current_label item
          (c', acc @ [item'])) (counter, []) b
  (counter', Block items)

let labelDecl counter = function
  | FunDecl fn ->
      match fn.body with
      | Some body ->
          let counter', body' = labelBlock counter None body
          (counter', FunDecl { fn with body = Some body' })
      | None -> (counter, FunDecl fn)
  | var_decl -> (counter, var_decl)

let labelLoops counter (Program decls) =
    let counter', decls' =
        List.fold (fun (c, acc) d ->
            let c', d' = labelDecl c d
            (c', acc @ [d'])) (counter, []) decls
    (counter', Program decls')
