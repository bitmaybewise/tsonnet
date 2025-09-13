open Ast
open Result
open Syntax_sugar

let interpret_arith_op (op: bin_op) (n1: number) (n2: number) =
  match op, n1, n2 with
  | Add, (Int a), (Int b) -> Int (a + b)
  | Add, (Float a), (Int b) -> Float (a +. (float_of_int b))
  | Add, (Int a), (Float b) -> Float ((float_of_int a) +. b)
  | Add, (Float a), (Float b) -> Float (a +. b)
  | Subtract, (Int a), (Int b) -> Int (a - b)
  | Subtract, (Float a), (Int b) -> Float (a -. (float_of_int b))
  | Subtract, (Int a), (Float b) -> Float ((float_of_int a) -. b)
  | Subtract, (Float a), (Float b) -> Float (a -. b)
  | Multiply, (Int a), (Int b) -> Int (a * b)
  | Multiply, (Float a), (Int b) -> Float (a *. (float_of_int b))
  | Multiply, (Int a), (Float b) -> Float ((float_of_int a) *. b)
  | Multiply, (Float a), (Float b) -> Float (a *. b)
  | Divide, (Int a), (Int b) -> Float ((float_of_int a) /. (float_of_int b))
  | Divide, (Float a), (Int b) -> Float (a /. (float_of_int b))
  | Divide, (Int a), (Float b) -> Float ((float_of_int a) /. b)
  | Divide, (Float a), (Float b) -> Float (a /. b)

let interpret_concat_op (e1 : expr) (e2 : expr) : (expr, string) result =
  match e1, e2 with
  | String (_, s1), String (_, s2) ->
    ok (String (dummy_pos, s1^s2))
  | String (_, s1), val2 ->
    let* s2 = Json.expr_to_string val2 in ok (String (dummy_pos, s1^s2))
  | val1, String (_, s2) ->
    let* s1 = Json.expr_to_string val1 in ok (String (dummy_pos, s1^s2))
  | _ ->
    error "Invalid string concatenation operation"

let interpret_unary_op (op: unary_op) (evaluated_expr: expr) =
  match op, evaluated_expr with
  | Plus, number -> ok number
  | Minus, Number (pos, Int i) -> ok (Number (pos, Int (-i)))
  | Minus, Number (pos, Float f) -> ok (Number (pos, Float (-. f)))
  | Not, (Bool (pos, b)) -> ok (Bool (pos, not b))
  | BitwiseNot, Number (pos, Int i) -> ok (Number (pos, Int (lnot i)))
  | _ -> error "Invalid unary operation"

(** [interpret expr] interprets and reduce the intermediate AST [expr] into a result AST. *)
let rec interpret env expr =
  match expr with
  | Null _ | Bool _ | String _ | Number _ -> ok (env, expr)
  | Array (pos, exprs) -> interpret_array env (pos, exprs)
  | Object (pos, entries) -> interpret_object env (pos, entries)
  | ObjectFieldAccess (pos, scope, field) -> interpret_object_field_access env (pos, scope, field)
  | Ident (pos, varname) ->
    Env.find_var varname env
      ~succ:(fun env' expr -> interpret env' expr)
      ~err:(Error.error_at pos)
  | BinOp (pos, op, e1, e2) ->
    (let* (env1, e1') = interpret env e1 in
    let* (env2, e2') = interpret env1 e2 in
    match op, e1', e2' with
    | Add, (String _ as v1), (_ as v2) | Add, (_ as v1), (String _ as v2) ->
      let* expr' = interpret_concat_op v1 v2 in
      ok (env, expr')
    | _, Number (pos, v1), Number (_, v2) ->
      ok (env2, Number (pos, interpret_arith_op op v1 v2))
    | _ ->
      Error.trace "Invalid binary operation" pos >>= error
    )
  | UnaryOp (pos, op, expr) ->
    let* (env', expr') = interpret env expr in
    Result.fold (interpret_unary_op op expr')
      ~ok:(fun expr' -> ok (env', expr'))
      ~error:(Error.error_at pos)
  | Local (_, vars) ->
    let acc_fun env (varname, expr) = Env.add_local varname expr env in
    let env' = List.fold_left acc_fun env vars
    in ok (env', Unit)
  | Unit -> ok (env, Unit)
  | Seq exprs ->
    (match exprs with
    | [] -> ok (env, Unit)
    | [expr] -> interpret env expr
    | (expr :: exprs) -> interpret env expr >>= fun (env', _) -> interpret env' (Seq exprs))
  | IndexedExpr (pos, varname, index_expr) ->
    let* (env', index_expr') = interpret env index_expr in
    Env.find_var varname env'
      ~succ:(fun env' expr ->
        Result.fold
          (Indexable.get index_expr' expr)
          ~ok:(fun e -> interpret env' e)
          ~error:(Error.error_at pos)
      )
      ~err:(Error.error_at pos)
    | expr -> error (Printf.sprintf "Expression %s cannot be interpreted" (string_of_type expr))

and interpret_array env (pos, exprs) =
  let* (env', evaluated_exprs) = List.fold_left
    (fun result expr ->
      let* (env', result') = result in
      let* (env'', expr') = interpret env' expr in
      ok (env'', result' @ [expr'])
    )
    (ok (env, []))
    exprs
  in ok (env', Array (pos, evaluated_exprs))

and interpret_object env (pos, entries) =
  let* obj_id = Env.Id.generate () in
  let obj = ObjectSelf obj_id in
  let env' = Env.add_local "self" obj env in
  let env' = Env.add_local_when_not_present "$" obj env' in
  (* First add locals and object fields to env *)
  let* env'' = List.fold_left
    (fun result entry ->
      let* env' = result in
      match entry with
      | ObjectExpr expr ->
        (* ObjectExpr holds a single local. Interpreting
          it will add the expr to the environment *)
        let* (env'', _) = interpret env' expr in (ok env'')
      | ObjectField (attr, expr) ->
        ok (Env.add_obj_field attr expr obj_id env')
    )
    (ok env')
    entries
  in
  (* Then interpret after env is populated. This allows locals
    and object fields to be accessed in a lazy evaluated manner. *)
  let* evaluated_entries = List.fold_left
    (fun result entry ->
      let* entries' = result in
      match entry with
      | ObjectField (attr, _) ->
        let* (_, entry) = Env.get_obj_field attr obj_id env''
          ~succ:(fun env''' expr -> interpret env''' expr)
          ~err:(Error.error_at pos)
        in ok (entries' @ [ObjectField (attr, entry)])
      | _ ->
        (* Ignore previously evaluated expressions *)
        result
    )
    (ok [])
    entries
  in
  ok (env, Object (pos, evaluated_entries))

and interpret_object_field_access env (pos, scope, field) =
  let* (_, evaluated_expr) = Env.find_var (string_of_object_scope scope) env
    ~succ:(fun env' expr ->
      match expr with
      | ObjectSelf obj_id ->
        Env.get_obj_field field obj_id env'
          ~succ:interpret
          ~err:(Error.error_at pos)
      | _ ->
        Error.error_at pos
          (match scope with
          | Self -> Scope.self_out_of_scope
          | TopLevel -> Scope.no_toplevel_object)
    )
    ~err:(Error.error_at pos)
  in ok (env, evaluated_expr)

let eval expr =
  let* (_env, evaluated_expr) = interpret Env.empty expr
  in ok evaluated_expr
