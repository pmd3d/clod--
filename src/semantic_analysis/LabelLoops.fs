module Label_loops

open Ast.Untyped

let rec labelStatement current_label = function
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
      Statement.While(condition = condition, body = labelStatement (Some new_id) body, id = new_id)
  | DoWhile(body, condition, id) ->
      let new_id = UniqueIds.makeLabel "do_while"
      DoWhile(body = labelStatement (Some new_id) body, condition = condition, id = new_id)
  | For(init, condition, post, body, id) ->
      let new_id = UniqueIds.makeLabel "for"
      Statement.For(init = init, condition = condition, post = post, body = labelStatement (Some new_id) body, id = new_id)
  | Compound blk -> Compound (labelBlock current_label blk)
  | If(condition, thenClause, elseClause) ->
      If(condition = condition, thenClause = labelStatement current_label thenClause, elseClause = Option.map (labelStatement current_label) elseClause)
  | (Null | Return _ | Expression _) as s -> s

and labelBlockItem current_label = function
  | Stmt s -> Stmt (labelStatement current_label s)
  | decl -> decl

and labelBlock current_label (Block b) =
  Block (List.map (labelBlockItem current_label) b)

let labelDecl = function
  | FunDecl fn ->
      FunDecl { fn with body = Option.map (labelBlock None) fn.body }
  | var_decl -> var_decl

let labelLoops (Program decls) = Program (List.map labelDecl decls)
