open Ast
open Result
open Syntax_sugar

let translating_fields = ref ObjectFields.empty

type tsonnet_type =
  | Tunit
  | Tnull
  | Tbool
  | Tnumber
  | Tstring of string
  | Tany
  | Tarray of tsonnet_type
  | Tobject of t_object_entry list
  | TruntimeObject of Env.env_id * t_object_entry list
  | TobjectPtr of Env.env_id * t_object_scope
  | Lazy of expr
  | Tunresolved
  | TfunctionDef of t_function_def
  | TfunctionCall of t_function_call
  | Tclosure of t_closure

and t_object_entry =
  | TobjectField of string * tsonnet_type
  | TobjectExpr of tsonnet_type
and t_object_scope =
  | TobjectSelf
  | TobjectTopLevel

and t_function_def = {
  params: (string * tsonnet_type) list;
  body: expr;
  return: tsonnet_type;
}
and t_function_call = {
  params: tsonnet_type list;
  return: tsonnet_type;
}
and t_closure = {
  params: (string * tsonnet_type) list;
  body: expr;
}

(* Semantic equality for types. *)
let semantic_type_equal ty_a ty_b =
  match ty_a, ty_b with
  (* Ignores representation details that do not affect type identity,
     such as the concrete value carried by Tstring.  *)
  | Tstring _, Tstring _ -> true
  (* Falls back to structural equality otherwise. *)
  | _ -> ty_a = ty_b

let rec to_string = function
  | Tunit -> "()"
  | Tnull -> "Null"
  | Tbool -> "Bool"
  | Tnumber -> "Number"
  | Tstring _ -> "String"
  | Tany -> "Any"
  | Tarray ty -> "Array of " ^ to_string ty
  | Tobject fields ->
    let field_to_string = function
      | TobjectField (field, ty) -> field ^ " : " ^ to_string ty
      | TobjectExpr ty -> to_string ty
    in
    "{" ^ (
      String.concat ", " (List.map field_to_string fields)
    ) ^ "}"
  | TruntimeObject (_, fields) ->
    let field_to_string = function
      | TobjectField (field, ty) -> field ^ " : " ^ to_string ty
      | TobjectExpr ty -> to_string ty
    in
    "{" ^ (
      String.concat ", " (List.map field_to_string fields)
    ) ^ "}"
  | TobjectPtr (Env.EnvId id, scope) ->
    let s =
      match scope with
      | TobjectSelf -> "self"
      | TobjectTopLevel -> "$"
    in Printf.sprintf "%s (%d)" s id
  | Lazy ty -> string_of_type ty
  | TfunctionDef {params; return; _} ->
    Printf.sprintf "function(%s) -> %s"
      (List.map (fun (name, ty) -> name ^ ": " ^ to_string ty) params
      |> String.concat ", "
      )
      (to_string return)
  | TfunctionCall {params=params_type; return} ->
    Printf.sprintf "function(%s) -> %s"
      (List.map to_string params_type |> String.concat ", ")
      (to_string return)
  | Tclosure {params; _} ->
    Printf.sprintf "function(%s)"
      (List.map (fun (name, ty) -> name ^ ": " ^ to_string ty) params
      |> String.concat ", "
      )
  | Tunresolved -> "<unresolved>"

let rec collect_free_idents = function
  | Unit | Null _ | Number _ | String _ | Bool _ -> []
  | Ident (_, name) -> [name]
  | Array (_, exprs) -> List.concat_map collect_free_idents exprs
  | BinOp (_, _, e1, e2) -> collect_free_idents e1 @ collect_free_idents e2
  | UnaryOp (_, _, e) -> collect_free_idents e
  | Seq exprs -> List.concat_map collect_free_idents exprs
  | ParsedObject (_, entries) ->
    List.concat_map (function
      | ObjectField (_, e) -> collect_free_idents e
      | ObjectConditionalField (field, e) -> List.append (collect_free_idents field) (collect_free_idents e)
      | ObjectExpr e -> collect_free_idents e
    ) entries
  | ObjectFieldAccess (_, scope, exprs) ->
    let scope_idents = match scope with
      | ObjVarRef name -> [name]
      | Self | TopLevel -> []
    in
    scope_idents @ List.concat_map collect_free_idents exprs
  | IndexedExpr (_, name, e) -> name :: collect_free_idents e
  | Local (_, vars) -> List.concat_map (fun (_, e) -> collect_free_idents e) vars
  | FunctionCall (_, call) ->
    collect_free_idents call.callee @ List.concat_map (function
      | Positional e -> collect_free_idents e
      | Named (_, e) -> collect_free_idents e
    ) call.args
  | Closure (_, closure) -> collect_free_idents closure.body
  | _ -> []

let reachable_bindings bindings initial_idents =
  let rec go visited = function
    | [] -> visited
    | name :: rest ->
      if List.mem name visited
      then go visited rest
      else
        let new_idents =
          match List.assoc_opt name bindings with
          | Some expr -> collect_free_idents expr
          | None -> []
        in
        go (name :: visited) (new_idents @ rest)
  in
  go [] initial_idents

let rec check_cyclic_refs venv varname seen pos =
  if List.mem varname seen
  then
    Error.error_at pos (Error.Msg.type_cyclic_reference varname)
  else
    match Env.find_opt varname venv with
    | Some (Lazy expr) -> check_expr_for_cycles venv expr (varname :: seen)
    | _ -> ok ()
and check_expr_for_cycles venv expr seen =
  match expr with
  | Unit | Null _ | Number _ | String _ | Bool _ -> ok ()
  | Array (_, exprs) -> iter_for_cycles venv seen exprs
  | ParsedObject (_, entries) -> check_object_for_cycles venv entries seen
  | ObjectFieldAccess (pos, scope, exprs) -> check_object_field_chain_for_cycles venv (pos, scope, exprs) seen
  | Ident (pos, varname) -> check_cyclic_refs venv varname seen pos
  | BinOp (_, _, e1, e2) -> iter_for_cycles venv seen [e1; e2]
  | UnaryOp (_, _, e) -> check_expr_for_cycles venv e seen
  | Seq exprs -> iter_for_cycles venv seen exprs
  | If (_, cond_expr, then_expr, else_expr_opt) -> check_conditional_for_cycles venv (cond_expr, then_expr, else_expr_opt) seen
  | _ -> ok ()
and iter_for_cycles venv seen exprs =
  List.fold_left
    (fun ok' expr -> ok' >>= fun _ -> (check_expr_for_cycles venv expr seen))
    (ok ())
    exprs
and check_object_for_cycles venv entries seen =
  List.fold_left
    (fun ok entry -> ok >>= fun _ ->
      match entry with
      | ObjectField (field, expr) -> check_expr_for_cycles venv expr (field :: seen)
      | ObjectConditionalField (field, expr) ->
        check_expr_for_cycles venv field seen >>= fun () ->
        check_expr_for_cycles venv expr seen
      | ObjectExpr expr -> check_expr_for_cycles venv expr seen
    )
    (ok ())
    entries
and check_object_field_for_cycles venv (pos, scope, field_expr) seen =
  (match Env.find_opt (string_of_object_scope scope) venv with
  | Some (TobjectPtr (obj_id, _)) ->
    (match field_expr with
    | String (_, field) | Ident (_, field) ->
      let obj_field = Env.uniq_field_ident obj_id field in
      check_cyclic_refs venv obj_field seen pos
    | IndexedExpr (_, field, index_expr) ->
      let obj_field = Env.uniq_field_ident obj_id field in
      let* () = check_cyclic_refs venv obj_field seen pos in
      check_expr_for_cycles venv index_expr seen
    | _ -> ok ()
    )
  | _ -> ok ()
  )

and check_object_field_chain_for_cycles venv (pos, scope, exprs) seen =
  List.fold_left
    (fun result expr ->
      let* () = result in
      check_object_field_for_cycles venv (pos, scope, expr) seen
    )
    (ok ())
    exprs

and check_conditional_for_cycles venv (cond_expr, then_expr, else_expr_opt) seen =
  let* () = check_expr_for_cycles venv cond_expr seen in
  let* () = check_expr_for_cycles venv then_expr seen in
  (match else_expr_opt with
  | Some else_expr -> check_expr_for_cycles venv else_expr seen
  | None -> ok ()
  )

let rec translate venv expr =
  match expr with
  | Unit -> ok (venv, Tunit)
  | Null _ -> ok (venv, Tnull)
  | Bool _ -> ok (venv, Tbool)
  | Number _ -> ok (venv, Tnumber)
  | String (_, s) -> ok (venv, Tstring s)
  | Ident (pos, varname) -> translate_ident venv pos varname
  | Array (_pos, elems) -> translate_array venv elems
  | ParsedObject (pos, entries) -> translate_object venv pos entries
  | ObjectFieldAccess (pos, scope, chain) -> translate_object_field_access venv pos scope chain
  | Local (_pos, vars) -> translate_local venv vars
  | Seq exprs -> translate_seq venv exprs
  | BinOp (pos, op, e1, e2) -> translate_bin_op venv pos op e1 e2
  | UnaryOp (pos, op, expr) -> translate_unary_op venv (pos, op, expr)
  | IndexedExpr (pos, varname, index_expr) -> translate_indexed_expr venv (pos, varname, index_expr)
  | FunctionDef (pos, def) -> translate_function_def venv (pos, def)
  | FunctionCall (pos, call) -> translate_function_call venv (pos, call)
  | Closure (pos, closure) -> translate_closure venv (pos, closure)
  | If (pos, cond_expr, then_expr, else_expr_opt) ->
    translate_conditional venv (pos, cond_expr, then_expr, else_expr_opt)
  | expr' ->
    error (Error.Msg.type_invalid_expr (string_of_type expr'))

and translate_indexed_expr venv (pos, varname, index_expr) =
  let* (venv', index_expr') = translate venv index_expr in
  match index_expr' with
  | Tnumber ->
    Env.find_var varname venv'
      ~succ:(fun venv' expr' ->
        match expr' with
        | (Tarray _) as ty -> ok (venv', ty)
        | (Tstring _) as ty -> ok (venv', ty)
        | Lazy expr -> translate venv expr
        | ty -> error (Error.Msg.type_non_indexable_value (to_string ty))
      )
      ~err:(Error.error_at pos)
  | ty -> Error.error_at pos (Error.Msg.type_expected_integer_index (to_string ty))

and translate_unary_op venv (pos, op, expr) =
  let* (venv', expr') = translate venv expr in
  match op, expr' with
  | Plus, Tnumber | Minus, Tnumber | BitwiseNot, Tnumber -> ok (venv', Tnumber)
  | Not, Tbool | BitwiseNot, Tbool -> ok (venv', Tbool)
  | _ -> Error.error_at pos Error.Msg.invalid_unary_op

and translate_local venv vars =
  let venv' = List.fold_left
    (fun venv (varname, var_expr) -> Env.add_local varname (Lazy var_expr) venv)
    venv
    vars
  in ok (venv', Tunit)

and translate_seq venv exprs =
  let rec collect_locals = function
    | Local (pos, vars) :: rest ->
      let (all_vars, body) = collect_locals rest in
      (List.map (fun v -> (pos, v)) vars @ all_vars, body)
    | rest -> ([], rest)
  in
  let rec go venv = function
    | [] -> ok (venv, Tunit)
    | [expr] -> translate venv expr
    | (Local _ :: _) as exprs ->
      let (all_pos_vars, body) = collect_locals exprs in
      let all_vars = List.map snd all_pos_vars in
      (* Add all local bindings to env *)
      let venv' = List.fold_left
        (fun venv (varname, var_expr) ->
          Env.add_local varname (Lazy var_expr) venv
        )
        venv
        all_vars
      in
      (* Determine which vars are reachable from the body *)
      let body_idents = List.concat_map collect_free_idents body in
      let reachable = reachable_bindings all_vars body_idents in
      (* Warn on unused variables *)
      List.iter (fun (pos, (varname, _)) ->
        if not (List.mem varname reachable)
        then Error.warn (Error.Msg.type_unused_variable varname) pos
      ) all_pos_vars;
      (* Check cycles: error for reachable, warn for unreachable *)
      let* () = List.fold_left
        (fun acc (pos, (varname, _)) -> acc >>= fun () ->
          match check_cyclic_refs venv' varname [] pos with
          | Ok () -> ok ()
          | Error msg ->
            if List.mem varname reachable
            then error msg
            else (Error.warn (Error.Msg.type_cyclic_reference varname) pos; ok ())
        )
        (ok ())
        all_pos_vars
      in
      go venv' body
    | expr :: rest ->
      let* (venv', _) = translate venv expr in
      go venv' rest
  in
  go venv exprs

and translate_ident venv pos varname =
  if ObjectFields.mem varname !translating_fields then
    Error.error_at pos (Error.Msg.type_cyclic_reference varname)
  else begin
    translating_fields := ObjectFields.add varname !translating_fields;
    let result = Env.find_var varname venv
      ~succ:(fun venv ty ->
        match ty with
        | Lazy expr -> translate venv expr
        | _ -> ok (venv, ty)
      )
      ~err:(Error.error_at pos)
    in
    translating_fields := ObjectFields.remove varname !translating_fields;
    result
  end

and translate_array venv elems =
  (* As of now, we compare each element and if all have the same type,
     it is an array of this type, otherwise it will be an array of any.
     Since JSON doesn't care about types, this is the simpler way of
     handling such case in compatibility with Jsonnet. However, when we
     actually use the items, given the flexibility of JSON and Jsonnet
     regarding types, we may need to support polymorphic arrays in a more
     flexible way, but I'm leaving this problem unsolved for now until I
     finally need to.
  *)
  (match elems with
  | [] -> ok (venv, Tany)
  | elem :: rest ->
    let ty = Lazy elem in
    let* (venv, ty) =
      List.fold_left
        (fun acc elem -> acc >>= fun (venv, ty) ->
          let elem_ty = Lazy elem in
          if ty = elem_ty
          then ok (venv, elem_ty)
          else ok (venv, Tany)
        )
        (ok (venv, ty))
        rest
    in ok (venv, Tarray ty)
  )

and translate_lazy venv = function
  | Lazy expr -> translate venv expr
  | ty -> error (Error.Msg.type_invalid_expr (to_string ty))

and translate_object venv pos entries =
  let* obj_id = Env.Id.generate () in
  let had_toplevel = Option.is_some (Env.find_opt "$" venv) in
  let venv = Env.add_local "self" (TobjectPtr (obj_id, TobjectSelf)) venv in
  let venv, _ =
    Env.add_local_when_not_present "$" (TobjectPtr (obj_id, TobjectTopLevel)) venv
  in

  (* Translate locals *)
  let* venv = List.fold_left
    (fun result entry ->
      let* venv = result in
      match entry with
      | ObjectExpr expr ->
        let* (venv', _) = translate venv expr in (ok venv')
      | ObjectField (attr, expr) ->
        ok (Env.add_obj_field attr (Lazy expr) obj_id venv)
      | ObjectConditionalField (attr_expr, expr) ->
        (match check_expr_for_cycles venv attr_expr [] with
        | Ok () -> ()
        | Error _ ->
          let attr_pos = match attr_expr with If (p, _, _, _) -> p | _ -> pos in
          Error.warn Error.Msg.type_cyclic_conditional_field_key attr_pos
        );
        (* It's past attr_expr, which means it has no cyclic refs.
           Now we need to add the expr to the attr name it resolves to. *)
        let* (_, ty) = translate venv attr_expr in
        (match ty with
        | Tstring attr -> ok (Env.add_obj_field attr (Lazy expr) obj_id venv)
        | Tnull -> ok venv
        | _ ->
          let attr_pos = match attr_expr with If (p, _, _, _) -> p | _ -> pos in
          Error.error_at attr_pos
            (Error.Msg.invalid_conditional_field_key (to_string ty))
        )
    )
    (ok venv)
    entries
  in

  (* Check for cyclical references among object fields
    (warn, don't error when the reference is not part of the evaluation tree) *)
  List.iter
    (fun entry ->
      match entry with
      | ObjectField (attr, _) ->
        (match check_cyclic_refs venv (Env.uniq_field_ident obj_id attr) [] pos with
        | Ok () -> ()
        | Error _ -> Error.warn (Error.Msg.type_cyclic_reference (Env.uniq_field_ident obj_id attr)) pos
        )
      | ObjectConditionalField (attr_expr, _) ->
        (* attr_expr must be translated first to discover its name, then we can check_cyclic_refs *)
        (match translate venv attr_expr with
        | Ok (_, Tstring attr) ->
          (match check_cyclic_refs venv (Env.uniq_field_ident obj_id attr) [] pos with
          | Ok () -> ()
          | Error _ -> Error.warn (Error.Msg.type_cyclic_reference (Env.uniq_field_ident obj_id attr)) pos
          )
        | _ -> ()
        )
      | _ -> ()
    )
    entries;

  (* Translate object fields lazily: warn on errors, skip invalid fields *)
  let entry_types = List.fold_left
    (fun entries' entry ->
      (* Resolve the field from the env to reuse memoized lazy translations
        instead of retyping the field expression. *)
      let get_field_type_from_env attr =
        let obj_field = Env.get_obj_field attr obj_id venv ~succ:translate_lazy ~err:(Error.error_at pos) in
        (match obj_field with
        | Ok (_, entry_ty) -> entries' @ [TobjectField (attr, entry_ty)]
        | Error _ -> entries'
        )
      in
      match entry with
      | ObjectField (attr, _) -> get_field_type_from_env attr
      | ObjectConditionalField (attr_expr, _) ->
        (match translate venv attr_expr with
        | Ok (_, Tstring attr) -> get_field_type_from_env attr
        | _ -> entries'
        )
      | _ -> entries'
    )
    []
    entries
  in
  (* Remove self and $ from the environment to prevent leaking *)
  let venv = Env.Map.remove "self" venv in
  let venv = if had_toplevel then venv else Env.Map.remove "$" venv in
  ok (venv, TruntimeObject (obj_id, entry_types))

and translate_object_field_access venv pos scope chain_exprs =
  let* (venv, obj) =
    match scope with
    | Self | TopLevel ->
      (* For self and $, look them up directly *)
      (match Env.find_opt (string_of_object_scope scope) venv with
      | Some (TobjectPtr _ as obj) -> ok (venv, obj)
      | _ ->
        Error.error_at pos
          (match scope with
          | Self -> Error.Msg.self_out_of_scope
          | TopLevel -> Error.Msg.no_toplevel_object
          | ObjVarRef _ -> "" (* unreachable *)
          )
      )
    | ObjVarRef varname ->
      (* For variable references, look up and translate the variable *)
      Env.find_var varname venv
        ~succ:(fun venv ty ->
          match ty with
          | TobjectPtr _ | TruntimeObject _ as obj -> ok (venv, obj)
          | Lazy expr -> translate venv expr
          | _ -> Error.error_at pos Error.Msg.must_be_object
        )
        ~err:(Error.error_at pos)
  in

  List.fold_left
    (fun acc field_expr ->
      let* (venv, prev_ty) = acc in

      let get_obj_id_and_env =
        match prev_ty with
        | TobjectPtr (obj_id, _) -> ok (obj_id, venv)
        | TruntimeObject (obj_id, _) ->
          (* TODO: we haven't included the environment in TruntimeObject yet.
             It must be done such as Ast.RuntimeObject *)
          let field_venv =
            Env.add_local "self" (TobjectPtr (obj_id, TobjectSelf)) venv
          in
          let field_venv =
            Env.add_local_when_not_present "$" (TobjectPtr (obj_id, TobjectTopLevel)) field_venv |> fst
          in
          ok (obj_id, field_venv)
        | _ -> Error.error_at pos Error.Msg.must_be_object
      in

      match field_expr with
      | String (_, field) | Ident (_, field) ->
        let* (obj_id, field_venv) = get_obj_id_and_env in
        let key = Env.uniq_field_ident obj_id field in
        if ObjectFields.mem key !translating_fields then
          Error.error_at pos (Error.Msg.type_cyclic_reference key)
        else begin
          translating_fields := ObjectFields.add key !translating_fields;
          let result = Env.get_obj_field field obj_id field_venv
            ~succ:translate_lazy
            ~err:(Error.error_at pos)
          in
          translating_fields := ObjectFields.remove key !translating_fields;
          result
        end
      | Number (pos, _) ->
        (* Handle numeric indexing of strings and arrays *)
        (match prev_ty with
        | Tstring _ as ty -> ok (venv, ty)
        | Tarray elem_ty -> ok (venv, elem_ty)
        | _ -> Error.error_at pos (Error.Msg.type_non_indexable_type (to_string prev_ty))
        )
      | _ ->
        Error.error_at pos (Error.Msg.type_invalid_lookup_key (string_of_type field_expr))
    )
    (ok (venv, obj))
    chain_exprs

and translate_bin_op venv pos op e1 e2 =
  let* (venv', e1') = translate venv e1 in
  let* (venv'', e2') = translate venv' e2 in
  match op, e1', e2' with
  | Add, _, Tstring s | Add, Tstring s, _ -> ok (venv'', Tstring s)
  | Add, Tnumber, Tnumber -> ok (venv'', Tnumber)
  | Add, (Tarray _), (Tarray _) -> ok (venv'', Tarray Tany)
  | Add, (Tobject _ | TruntimeObject _ | TobjectPtr _), (Tobject _ | TruntimeObject _ | TobjectPtr _) ->
    ok (venv'', Tany)
  | Subtract, Tnumber, Tnumber -> ok (venv'', Tnumber)
  | Multiply, Tnumber, Tnumber -> ok (venv'', Tnumber)
  | Divide, Tnumber, Tnumber -> ok (venv'', Tnumber)
  | Modulo, Tnumber, Tnumber -> ok (venv'', Tnumber)
  | BitwiseOr, Tnumber, Tnumber -> ok (venv'', Tnumber)
  | BitwiseAnd, Tnumber, Tnumber -> ok (venv'', Tnumber)
  | BitwiseXor, Tnumber, Tnumber -> ok (venv'', Tnumber)
  | ShiftLeft, Tnumber, Tnumber -> ok (venv'', Tnumber)
  | ShiftRight, Tnumber, Tnumber -> ok (venv'', Tnumber)
  | LogicalAnd, Tbool, Tbool -> ok (venv'', Tbool)
  | LogicalOr, Tbool, Tbool -> ok (venv'', Tbool)
  | Equality, _, _ -> ok (venv'', Tbool)
  | Inequality, _, _ -> ok (venv'', Tbool)
  | GreaterThan, Tnumber, Tnumber -> ok (venv'', Tbool)
  | GreaterThanOrEqual, Tnumber, Tnumber -> ok (venv'', Tbool)
  | LessThan, Tnumber, Tnumber -> ok (venv'', Tbool)
  | LessThanOrEqual, Tnumber, Tnumber -> ok (venv'', Tbool)
  | GreaterThan, Tstring _, Tstring _ -> ok (venv'', Tbool)
  | GreaterThanOrEqual, Tstring _, Tstring _ -> ok (venv'', Tbool)
  | LessThan, Tstring _, Tstring _ -> ok (venv'', Tbool)
  | LessThanOrEqual, Tstring _, Tstring _ -> ok (venv'', Tbool)
  | In, Tstring _, (Tobject _ | Tany | TruntimeObject _ | TobjectPtr _) -> ok (venv'', Tbool)
  | _, Tany, _ | _, _, Tany -> ok (venv'', Tany)
  | _ -> Error.error_at pos Error.Msg.invalid_binary_op

and translate_function_def venv (pos, def) =
  (* For params with defaults, we can infer the type from the default expression;
     params without defaults remain Tunresolved until the first call *)
  let* params_typed = List.fold_left
    (fun acc (name, default) ->
      let* params' = acc in
      match default with
      | Some default_expr ->
        let* (_, default_ty) = translate venv default_expr in
        ok (params' @ [(name, default_ty)])
      | None ->
        ok (params' @ [(name, Tunresolved)])
    )
    (ok [])
    def.params
  in
  let fun_def = TfunctionDef
    { params = params_typed;
      body = def.body;
      return = Tunresolved;
    }
  in
  (* So, function declaration will have an unresolved type definition,
     that only later it will be translated: before function call translation!
     After first function call, concrete types are set and subsequent calls will
     type check against the initial type assignment(s). *)
  let venv' = Env.add_local def.name fun_def venv in
  ok (venv', fun_def)

and translate_function_call venv (pos, call) =
  match call.callee with
  | Ident (_, name) ->
    translate_named_function_call venv (pos, name, call.args)
  | Closure (_, closure) ->
    translate_closure_call venv (pos, closure.params, closure.body, call.args)
  | _ ->
    let* (venv', callee_ty) = translate venv call.callee in
    (match callee_ty with
    | Tclosure { params = closure_params; body = body_expr } ->
      let def_params = List.map (fun (name, _ty) -> (name, None)) closure_params in
      translate_closure_call venv' (pos, def_params, body_expr, call.args)
    | _ ->
      Error.error_at pos (Error.Msg.type_invalid_expr (string_of_type call.callee))
    )

and translate_named_function_call venv (pos, name, args) =
  let positional_args =
    List.filter_map (function Positional e -> Some e | Named _ -> None) args
  in
  let named_args =
    List.filter_map (function Named (n, e) -> Some (n, e) | Positional _ -> None) args
  in
  let num_positional = List.length positional_args in
  match Env.find_opt name venv with
  | Some (TfunctionDef { params = def_params;
                          body = body_expr;
                          return = return_type;
                        }) ->
    let num_def = List.length def_params in
    let num_provided = num_positional + List.length named_args in
    if num_provided > num_def
    then
      Error.error_at pos
        (Error.Msg.wrong_number_of_params num_def num_provided)
    else
      let* (venv', resolved_params) =
        List.fold_left
          (fun acc (index, (param_name, def_param_type)) ->
            let* (venv', params') = acc in
            let call_expr_opt =
              if index < num_positional
              then Some (List.nth positional_args index)
              else Option.map snd (List.find_opt (fun (n, _) -> n = param_name) named_args)
            in
            match call_expr_opt with
            | Some call_param ->
              let* (venv'', call_param_type) = translate venv' call_param in
              (match def_param_type with
              | Tunresolved ->
                ok (venv'', params' @ [(param_name, call_param_type)])
              | expected ->
                if call_param_type = expected
                then ok (venv'', params' @ [(param_name, expected)])
                else Error.error_at pos
                  (Error.Msg.type_mismatch
                    ~expected:(to_string expected)
                    ~got:(to_string call_param_type))
              )
            | None ->
              ok (venv', params' @ [(param_name, def_param_type)])
          )
          (ok (venv, []))
          (List.mapi (fun i p -> (i, p)) def_params)
      in
      let body_venv = List.fold_left
        (fun env (name, ty) -> Env.add_local name ty env)
        venv'
        resolved_params
      in
      let* (_, body_type) = translate body_venv body_expr in
      let* resolved_return =
        match return_type with
        | Tunresolved ->
          ok body_type
        | expected ->
          if body_type = expected
          then ok expected
          else Error.error_at pos
            (Error.Msg.type_mismatch
              ~expected:(to_string expected)
              ~got:(to_string body_type))
      in
      let resolved_fun =
        TfunctionDef {
          params = resolved_params;
          body = body_expr;
          return = resolved_return;
        }
      in
      let venv_with_resolved_fun = Env.add_local name resolved_fun venv' in
      ok (venv_with_resolved_fun, resolved_return)
  | Some (Lazy expr) ->
    let* (venv', resolved) = translate venv expr in
    let venv'' = Env.add_local name resolved venv' in
    translate_named_function_call venv'' (pos, name, args)
  | Some (Tclosure { params = closure_params; body = body_expr }) ->
    let def_params = List.map (fun (name, _ty) -> (name, None)) closure_params in
    translate_closure_call venv (pos, def_params, body_expr, args)
  | _ ->
    Error.error_at pos (Error.Msg.var_not_found name)

and translate_closure venv (_pos, closure) =
  let* params_typed = List.fold_left
    (fun acc (name, default) ->
      let* params' = acc in
      match default with
      | Some default_expr ->
        let* (_, default_ty) = translate venv default_expr in
        ok (params' @ [(name, default_ty)])
      | None ->
        ok (params' @ [(name, Tunresolved)])
    )
    (ok [])
    closure.params
  in
  ok (venv, Tclosure { params = params_typed; body = closure.body })

and translate_closure_call venv (pos, def_params, body, call_args) =
  let positional_args =
    List.filter_map (function Positional e -> Some e | Named _ -> None) call_args
  in
  let named_args =
    List.filter_map (function Named (n, e) -> Some (n, e) | Positional _ -> None) call_args
  in
  let num_positional = List.length positional_args in
  let num_def = List.length def_params in
  let num_required =
    List.length (List.filter (fun (_, default) -> Option.is_none default) def_params)
  in
  let num_provided = num_positional + List.length named_args in
  if num_provided < num_required || num_provided > num_def
  then Error.error_at pos (Error.Msg.wrong_number_of_params num_def num_provided)
  else
    let* (venv', resolved_params) =
      List.fold_left
        (fun acc (index, (param_name, default)) ->
          let* (venv', params') = acc in
          let call_expr_opt =
            if index < num_positional
            then Some (List.nth positional_args index)
            else Option.map snd (List.find_opt (fun (n, _) -> n = param_name) named_args)
          in
          match call_expr_opt with
          | Some call_param ->
            let* (venv'', call_param_type) = translate venv' call_param in
            ok (venv'', params' @ [(param_name, call_param_type)])
          | None ->
            (match default with
            | Some default_expr ->
              let* (venv'', default_type) = translate venv' default_expr in
              ok (venv'', params' @ [(param_name, default_type)])
            | None ->
              Error.error_at pos (Error.Msg.wrong_number_of_params num_def num_provided)
            )
        )
        (ok (venv, []))
        (List.mapi (fun i p -> (i, p)) def_params)
    in
    let body_venv = List.fold_left
      (fun env (name, ty) -> Env.add_local name ty env)
      venv'
      resolved_params
    in
    let* (_, body_type) = translate body_venv body in
    ok (venv, body_type)

and translate_conditional venv (pos, cond_expr, then_expr, else_expr_opt) =
  let* (_, cond_ty) = translate venv cond_expr in
  match cond_ty with
  | Tbool ->
    let* (_, then_type) = translate venv then_expr in
    (match else_expr_opt with
    | Some else_expr ->
      let* (_, else_type) = translate venv else_expr in
      if semantic_type_equal then_type else_type
      then ok (venv, then_type)
        else Error.error_at pos
          (Error.Msg.type_conditional_branches_mismatch
            ~then_type:(to_string then_type)
            ~else_type:(to_string else_type)
          )
    | None ->
      ok (venv, then_type)
    )
  | _ -> Error.error_at pos
    (Error.Msg.type_mismatch
      ~expected:(string_of_type (Bool (pos, true)))
      ~got:(string_of_type cond_expr)
    )

let check (config : Config.t) expr  =
  let* _ = Scope.validate expr in
  if config.skip_typecheck then
    (prerr_endline Error.Msg.warn_skip_typecheck;
    ok expr)
  else
    let* _ = translate Env.empty expr in
    Env.Id.reset ();
    translating_fields := ObjectFields.empty;
    ok expr
