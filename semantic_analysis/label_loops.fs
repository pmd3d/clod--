module Label_loops

open Ast.Untyped

let rec label_statement current_label = function
  | Break _ ->
      (match current_label with
       | Some l -> Break l
       | None -> failwith "Break outside of loop")
  | Continue _ ->
      (match current_label with
       | Some l -> Continue l
       | None -> failwith "Continue outside of loop")
  | While(condition, body, id) ->
      let new_id = UniqueIds.makeLabel "while"
      Statement.While(condition = condition, body = label_statement (Some new_id) body, id = new_id)
  | DoWhile(body, condition, id) ->
      let new_id = UniqueIds.makeLabel "do_while"
      DoWhile(body = label_statement (Some new_id) body, condition = condition, id = new_id)
  | For(init, condition, post, body, id) ->
      let new_id = UniqueIds.makeLabel "for"
      Statement.For(init = init, condition = condition, post = post, body = label_statement (Some new_id) body, id = new_id)
  | Compound blk -> Compound (label_block current_label blk)
  | If(condition, thenClause, elseClause) ->
      If(condition = condition, thenClause = label_statement current_label thenClause, elseClause = Option.map (label_statement current_label) elseClause)
  | (Null | Return _ | Expression _) as s -> s

and label_block_item current_label = function
  | S s -> S (label_statement current_label s)
  | decl -> decl

and label_block current_label (Block b) =
  Block (List.map (label_block_item current_label) b)

let label_decl = function
  | FunDecl fn ->
      FunDecl { fn with body = Option.map (label_block None) fn.body }
  | var_decl -> var_decl

let label_loops (Program decls) = Program (List.map label_decl decls)
