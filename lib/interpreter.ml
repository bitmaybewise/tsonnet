open Ast
open Result
open Syntax_sugar

let interpret_unary_op (op: unary_op) (evaluated_expr: expr) =
  match op, evaluated_expr with
  | Plus, number -> ok number
  | Minus, Number (pos, Int i) -> ok (Number (pos, Int (-i)))
  | Minus, Number (pos, Float f) -> ok (Number (pos, Float (-. f)))
  | Not, (Bool (pos, b)) -> ok (Bool (pos, not b))
  | BitwiseNot, Number (pos, Int i) -> ok (Number (pos, Int (lnot i)))
  | _ -> error Error.Msg.invalid_unary_op

(** [interpret expr] interprets and reduce the intermediate AST [expr] into a result AST. *)
let rec interpret env expr =
  match expr with
  | Null _ | Bool _ | String _ | Number _ | EvaluatedObject _ -> ok (env, expr)
  | Array (pos, exprs) -> interpret_array env (pos, exprs)
  | ParsedObject (pos, entries) -> interpret_object env (pos, entries)
  | RuntimeObject (pos, obj_env, fields) -> interpret_runtime_object env (pos, obj_env, fields)
  | ObjectPtr _ as obj_ptr -> ok (env, obj_ptr)
  | ObjectFieldAccess (pos, scope, chain) -> interpret_object_field_access env (pos, scope, chain)
  | Ident (pos, varname) ->
    Env.find_var varname env
      ~succ:(fun env' expr -> interpret env' expr)
      ~err:(Error.error_at pos)
  | BinOp (pos, op, e1, e2) -> interpret_bin_op env (pos, op, e1, e2)
  | UnaryOp (pos, op, expr) ->
    let* (env', expr') = interpret env expr in
    Result.fold (interpret_unary_op op expr')
      ~ok:(fun expr' -> ok (env', expr'))
      ~error:(Error.error_at pos)
  | Local (_, vars) -> interpret_local env vars
  | Unit -> ok (env, Unit)
  | Seq exprs -> interpret_seq env exprs
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

and interpret_string_concat_op env e1 e2 =
    match e1, e2 with
    | String (_, s1), String (_, s2) ->
      ok (env, String (dummy_pos, s1^s2))
    | String (_, s1), val2 ->
      let* (_, val2) = interpret env val2 in
      let* s2 = Json.expr_to_string val2 in
      ok (env, String (dummy_pos, s1^s2))
    | val1, String (_, s2) ->
      let* (_, val1) = interpret env val1 in
      let* s1 = Json.expr_to_string val1 in
      ok (env, String (dummy_pos, s1^s2))
    | _ ->
      error Error.Msg.interp_invalid_concat

and interpret_array_concat_op env e1 e2 =
    match e1, e2 with
    | Array (pos, exprs1), Array (_, exprs2) ->
      ok (env, Array (pos, List.append exprs1 exprs2))
    | _ ->
      error Error.Msg.interp_invalid_concat

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

and interpret_local env vars =
  let* env' =
    List.fold_left
      (fun acc (varname, expr) ->
        let* env = acc in
        match expr with
        | ObjectFieldAccess (_, (Self | TopLevel), []) ->
          (* Eagerly evaluate unchained self/$ references to capture the current object.
            AST example:
            (Ast.Local (3:3, [("drink", (Ast.ObjectFieldAccess (3:3, Ast.Self, [])))])));
          *)
          let* (env', evaluated_expr) = interpret env expr in
          ok (Env.add_local varname evaluated_expr env')
        | _ ->
          (* Other expressions remain lazy *)
          ok (Env.add_local varname expr env)
      )
      (ok env)
      vars
  in ok (env', Unit)

and interpret_seq env exprs =
  match exprs with
  | [] -> ok (env, Unit)
  | [expr] -> interpret env expr
  | (expr :: exprs') ->
    interpret env expr >>= fun (env', _) ->
    interpret env' (Seq exprs')

and interpret_object env (pos, entries) =
  let* obj_id = Env.Id.generate () in
  let obj_env = Env.add_local "self" (ObjectPtr (obj_id, Self)) env in
  let obj_env, _ = Env.add_local_when_not_present "$" (ObjectPtr (obj_id, TopLevel)) obj_env in

  (* First add locals and object fields to env *)
  let* (obj_env, fields) = List.fold_left
    (fun result entry ->
      let* (env', fields) = result in
      match entry with
      | ObjectExpr expr ->
        (* ObjectExpr holds a single local. Interpreting
          it will add the expr to the environment *)
        let* (env', _) = interpret env' expr in ok (env', fields)
      | ObjectField (name, expr) ->
        (* Object fields are kept lazy -- they will be evaluated only when accessed.
           This prevents infinite loops from circular references. *)
        let env' = Env.add_obj_field name expr obj_id env' in
        ok (env', ObjectFields.add name fields)
    )
    (ok (obj_env, ObjectFields.empty))
    entries
  in
  (* We return env unchanged. RuntimeObject holds its own scoped env. *)
  ok (env, RuntimeObject (pos, obj_env, fields))

and interpret_object_field_access env (pos, scope, chain_exprs) =
  let* (env', obj) =
    (* Special case: if this is just `self` or `$` with no field chain,
     return the ObjectPtr directly. This ensures that when stored in variables,
     they capture the concrete object ID, not a dynamic scope reference. *)
    match scope with
    | Self | TopLevel ->
      (* For self and $, look them up as scopes in the environment *)
      (match Env.find_opt (string_of_object_scope scope) env with
      | Some (ObjectPtr _ as obj) -> ok (env, obj)
      | Some (RuntimeObject _ as obj) -> ok (env, obj)
      | _ ->
        Error.error_at pos
          (match scope with
          | Self -> Error.Msg.self_out_of_scope
          | TopLevel -> Error.Msg.no_toplevel_object
          | ObjVarRef _ -> Error.Msg.var_not_found "" (* unreachable -- should never happen, TODO: make this unrepresentable *)
          )
      )
    | ObjVarRef varname ->
      (* For variable references, look up and evaluate the variable *)
      let* (env', expr) =
        Env.find_var varname env ~succ:(interpret) ~err:(Error.error_at pos)
      in
      match expr with
      | ObjectPtr (obj_id, (Self | TopLevel)) ->
        (* If the variable holds a Self/TopLevel reference, the obj_id
           already captures which object it refers to. We don't need to
           re-resolve Self/TopLevel in the current environment. *)
        ok (env', ObjectPtr (obj_id, ObjVarRef varname))
      | ObjectPtr _ as obj -> ok (env', obj)
      | RuntimeObject _ as obj -> ok (env', obj)
      | _ -> Error.error_at pos Error.Msg.must_be_object
  in

  List.fold_left
    (fun acc field_expr ->
      let* (env', prev_expr) = acc in
      let get_obj_id =
        match prev_expr with
        | ObjectPtr (obj_id, _) ->
          (* Temporarily add self and $ to env for lazy field evaluation *)
          let field_env = Env.add_local "self" (ObjectPtr (obj_id, Self)) env' in
          let field_env = Env.add_local_when_not_present "$" (ObjectPtr (obj_id, TopLevel)) field_env |> fst in
          ok (obj_id, field_env)
        | RuntimeObject (_, obj_env, _) ->
          (match Env.find_opt "self" obj_env with
          | Some (ObjectPtr (obj_id, _)) -> ok (obj_id, obj_env)
          | _ -> error Error.Msg.must_be_object
          )
        | _ -> Error.error_at pos Error.Msg.must_be_object
      in

      match field_expr with
      | String (pos, field) | Ident (pos, field) ->
        let* (obj_id, field_env) = get_obj_id in
        Env.get_obj_field field obj_id field_env
          ~succ:(interpret)
          ~err:(Error.error_at pos)
      | Number _ as index_expr ->
        (* Handle array/string indexing: prev_expr[number] *)
        Result.fold
          (Indexable.get index_expr prev_expr)
          ~ok:(fun e -> ok (env', e))
          ~error:(Error.error_at pos)
      | _e ->
        Error.error_at pos Error.Msg.interp_invalid_lookup
    )
    (ok (env', obj))
    chain_exprs

and interpret_runtime_object env (pos, obj_env, fields) =
  let* evaluated_fields = interpret_runtime_object_fields obj_env fields in
  ok (env, EvaluatedObject (pos, evaluated_fields))

and interpret_runtime_object_fields obj_env fields =
  match Env.Map.find_opt "self" obj_env with
  | Some (ObjectPtr (obj_id, _)) ->
    let* field_list =
      ObjectFields.fold
        (fun field acc ->
          let* evaluated_fields = acc in
          let key = Env.uniq_field_ident obj_id field in
          match Env.Map.find_opt key obj_env with
          | Some expr ->
            let* (_, evaluated) = interpret obj_env expr in
            ok ((field, evaluated) :: evaluated_fields)
          | None -> acc
        )
        fields
        (ok [])
    in ok (List.rev field_list)
  | _ -> ok []

and interpret_bin_op env (pos, op, e1, e2) =
   let* (env1, e1') = interpret env e1 in
   let* (env2, e2') = interpret env1 e2 in
   match op, e1', e2' with
   | Add, (String _ as v1), (_ as v2) | Add, (_ as v1), (String _ as v2) ->
     interpret_string_concat_op env2 v1 v2
   | Add, (Array _ as v1), (Array _ as v2)  ->
     interpret_array_concat_op env2 v1 v2
   | In, (String _ | Ident _ as field), (EvaluatedObject _ | RuntimeObject (_, _, _) as obj) ->
     interpret_in_op env2 pos field obj
   | _, v1, v2 ->
     interpret_arith_op env2 (pos, op, v1, v2)

and interpret_arith_op env (pos, bin_op, n1, n2) =
  match bin_op, n1, n2 with
  | Add, Number (_, Int a), Number (_, Int b) ->
    ok (env, Number (pos, Int (a + b)))
  | Add, Number (_, Float a), Number (_, Int b) ->
    ok (env, Number (pos, Float (a +. (float_of_int b))))
  | Add, Number (_, Int a), Number (_, Float b) ->
    ok (env, Number (pos, Float ((float_of_int a) +. b)))
  | Add, Number (_, Float a), Number (_, Float b) ->
    ok (env, Number (pos, Float (a +. b)))
  | Subtract, Number (_, Int a), Number (_, Int b) ->
    ok (env, Number (pos, Int (a - b)))
  | Subtract, Number (_, Float a), Number (_, Int b) ->
    ok (env, Number (pos, Float (a -. (float_of_int b))))
  | Subtract, Number (_, Int a), Number (_, Float b) ->
    ok (env, Number (pos, Float ((float_of_int a) -. b)))
  | Subtract, Number (_, Float a), Number (_, Float b) ->
    ok (env, Number (pos, Float (a -. b)))
  | Multiply, Number (_, Int a), Number (_, Int b) ->
    ok (env, Number (pos, Int (a * b)))
  | Multiply, Number (_, Float a), Number (_, Int b) ->
    ok (env, Number (pos, Float (a *. (float_of_int b))))
  | Multiply, Number (_, Int a), Number (_, Float b) ->
    ok (env, Number (pos, Float ((float_of_int a) *. b)))
  | Multiply, Number (_, Float a), Number (_, Float b) ->
    ok (env, Number (pos, Float (a *. b)))
  | Divide, Number (_, Int a), Number (_, Int b) ->
    ok (env, Number (pos, Float ((float_of_int a) /. (float_of_int b))))
  | Divide, Number (_, Float a), Number (_, Int b) ->
    ok (env, Number (pos, Float (a /. (float_of_int b))))
  | Divide, Number (_, Int a), Number (_, Float b) ->
    ok (env, Number (pos, Float ((float_of_int a) /. b)))
  | Divide, Number (_, Float a), Number (_, Float b) ->
    ok (env, Number (pos, Float (a /. b)))
  | Modulo, Number (_, Int a), Number (_, Int b) ->
    ok (env, Number (pos, Int (a mod b)))
  | Modulo, Number (_, Float a), Number (_, Int b) ->
    ok (env, Number (pos, Float (Float.rem a (float_of_int b))))
  | Modulo, Number (_, Int a), Number (_, Float b) ->
    ok (env, Number (pos, Float (Float.rem (float_of_int a) b)))
  | Modulo, Number (_, Float a), Number (_, Float b) ->
    ok (env, Number (pos, Float (Float.rem a b)))
  | BitwiseOr, Number (_, Int a), Number (_, Int b) ->
    ok (env, Number (pos, Int (a lor b)))
  | BitwiseOr, Number (_, Float a), Number (_, Int b) ->
    ok (env, Number (pos, Int (int_of_float a lor b)))
  | BitwiseOr, Number (_, Int a), Number (_, Float b) ->
    ok (env, Number (pos, Int (a lor int_of_float b)))
  | BitwiseOr, Number (_, Float a), Number (_, Float b) ->
    ok (env, Number (pos, Int (int_of_float a lor int_of_float b)))
  | BitwiseAnd, Number (_, Int a), Number (_, Int b) ->
    ok (env, Number (pos, Int (a land b)))
  | BitwiseAnd, Number (_, Float a), Number (_, Int b) ->
    ok (env, Number (pos, Int (int_of_float a land b)))
  | BitwiseAnd, Number (_, Int a), Number (_, Float b) ->
    ok (env, Number (pos, Int (a land int_of_float b)))
  | BitwiseAnd, Number (_, Float a), Number (_, Float b) ->
    ok (env, Number (pos, Int (int_of_float a land int_of_float b)))
  | BitwiseXor, Number (_, Int a), Number (_, Int b) ->
    ok (env, Number (pos, Int (a lxor b)))
  | BitwiseXor, Number (_, Float a), Number (_, Int b) ->
    ok (env, Number (pos, Int (int_of_float a lxor b)))
  | BitwiseXor, Number (_, Int a), Number (_, Float b) ->
    ok (env, Number (pos, Int (a lxor int_of_float b)))
  | BitwiseXor, Number (_, Float a), Number (_, Float b) ->
    ok (env, Number (pos, Int (int_of_float a lxor int_of_float b)))
  | LogicalAnd, Bool (_, a), Bool (_, b) ->
    ok (env, Bool (pos, a && b))
  | LogicalOr, Bool (_, a), Bool (_, b) ->
    ok (env, Bool (pos, a || b))
  | Equality, Array (_, items1), Array (_, items2) ->
    (* Early exit: skip evaluation if lengths differ for efficiency *)
    if List.length items1 <> List.length items2 then
      ok (env, Bool (pos, false))
    else
      let* (_, evaluated1) = interpret_array env (pos, items1) in
      let* (_, evaluated2) = interpret_array env (pos, items2) in
      ok (env, Bool (pos, evaluated1 =~ evaluated2))
  | Equality, v1, v2 ->
    let* (_, eval_expr1) = interpret env v1 in
    let* (_, eval_expr2) = interpret env v2 in
    ok (env, Bool (pos, eval_expr1 =~ eval_expr2))
  | Inequality, v1, v2 ->
    (* Inequality is, simply put, negation of equality *)
    let* (_, expr) = interpret_arith_op env (pos, Equality, v1, v2) in
    (match expr with
    | Bool (_, value) -> ok (env, Bool (pos, not value))
    | _ -> Error.trace Error.Msg.invalid_binary_op pos >>= error
    )
  | _ ->
    Error.trace Error.Msg.invalid_binary_op pos >>= error

and interpret_in_op env pos field obj =
  match field, obj with
  | String (_, field_str), EvaluatedObject (_, fields)
  | Ident (_, field_str), EvaluatedObject (_, fields) ->
    let field_exists = List.exists (fun (name, _) -> name = field_str) fields in
    ok (env, Bool (pos, field_exists))
  | String (_, field_str), RuntimeObject (_, _, fields)
  | Ident (_, field_str), RuntimeObject (_, _, fields) ->
    let field_exists = ObjectFields.exists (fun name -> name = field_str) fields in
    ok (env, Bool (pos, field_exists))
  | _ ->
    Error.trace Error.Msg.invalid_binary_op pos >>= error

let rec deep_eval expr =
  match expr with
  | Null _ | Bool _ | String _ | Number _ -> ok expr
  | Array (pos, items) ->
    let* evaluated_items = List.fold_left
      (fun acc item ->
        let* list = acc in
        let* evaluated = deep_eval item in
        ok (evaluated :: list)
      )
      (ok [])
      items
    in
    ok (Array (pos, List.rev evaluated_items))
  | EvaluatedObject (pos, fields) ->
    let* evaluated_fields = List.fold_left
      (fun acc (name, expr) ->
        let* list = acc in
        let* evaluated = deep_eval expr in
        ok ((name, evaluated) :: list)
      )
      (ok [])
      fields
    in
    ok (EvaluatedObject (pos, List.rev evaluated_fields))
  | RuntimeObject _ ->
    let* (_, evaluated) = interpret Env.empty expr in
    deep_eval evaluated
  | expr ->
    let* (_, evaluated) = interpret Env.empty expr in
    deep_eval evaluated

let eval expr =
  let* (_, expr) = interpret Env.empty expr in
  let* expr = deep_eval expr in
  ok expr
