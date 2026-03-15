open Ast
open Result
open Syntax_sugar

let translating_fields = ref ObjectFields.empty

type tsonnet_type =
  | Tunit
  | Tnull
  | Tbool
  | Tnumber
  | Tstring
  | Tany
  | Tarray of tsonnet_type
  | Tobject of t_object_entry list
  | TruntimeObject of Env.env_id * t_object_entry list
  | TobjectPtr of Env.env_id * t_object_scope
  | Lazy of expr
and t_object_entry =
  | TobjectField of string * tsonnet_type
  | TobjectExpr of tsonnet_type
and t_object_scope =
  | TobjectSelf
  | TobjectTopLevel

let rec to_string = function
  | Tunit -> "()"
  | Tnull -> "Null"
  | Tbool -> "Bool"
  | Tnumber -> "Number"
  | Tstring -> "String"
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
      | ObjectExpr e -> collect_free_idents e
    ) entries
  | ObjectFieldAccess (_, _, exprs) -> List.concat_map collect_free_idents exprs
  | IndexedExpr (_, name, e) -> name :: collect_free_idents e
  | Local (_, vars) -> List.concat_map (fun (_, e) -> collect_free_idents e) vars
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

let rec translate venv expr =
  match expr with
  | Unit -> ok (venv, Tunit)
  | Null _ -> ok (venv, Tnull)
  | Bool _ -> ok (venv, Tbool)
  | Number _ -> ok (venv, Tnumber)
  | String _ -> ok (venv, Tstring)
  | Ident (pos, varname) -> translate_ident venv pos varname
  | Array (_pos, elems) ->
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
  | ParsedObject (pos, entries) -> translate_object venv pos entries
  | ObjectFieldAccess (pos, scope, chain) -> translate_object_field_access venv pos scope chain
  | Local (_pos, vars) ->
    let venv' = List.fold_left
      (* Adds an expr to the env to be evaluated at a later point in time (when required) *)
      (fun venv (varname, var_expr) -> Env.add_local varname (Lazy var_expr) venv)
      venv
      vars
    in ok (venv', Tunit)
  | Seq exprs ->
    translate_seq venv exprs
  | BinOp (pos, op, e1, e2) ->
    translate_bin_op venv pos op e1 e2
  | UnaryOp (pos, op, expr) ->
    (let* (venv', expr') = translate venv expr in
    match op, expr' with
    | Plus, Tnumber | Minus, Tnumber | BitwiseNot, Tnumber -> ok (venv', Tnumber)
    | Not, Tbool | BitwiseNot, Tbool -> ok (venv', Tbool)
    | _ -> Error.error_at pos Error.Msg.invalid_unary_op
    )
  | IndexedExpr (pos, varname, index_expr) ->
    (let* (venv', index_expr') = translate venv index_expr in
    match index_expr' with
    | Tnumber ->
      Env.find_var varname venv'
        ~succ:(fun venv' expr' ->
          match expr' with
          | (Tarray _) as ty -> ok (venv', ty)
          | Tstring as ty -> ok (venv', ty)
          | Lazy expr -> translate venv expr
          | ty -> error (Error.Msg.type_non_indexable_value (to_string ty))
        )
        ~err:(Error.error_at pos)
    | ty -> Error.error_at pos (Error.Msg.type_expected_integer_index (to_string ty))
    )
  | expr' ->
    error (Error.Msg.type_invalid_expr (string_of_type expr'))

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
    )
    (ok venv)
    entries
  in

  (* Check for cyclical references among object fields
    (warn, don't error when the reference is not part of
    the evaluation tree)
  *)
  List.iter
    (fun entry ->
      match entry with
      | ObjectField (attr, _) ->
        (match check_cyclic_refs venv (Env.uniq_field_ident obj_id attr) [] pos with
        | Ok () -> ()
        | Error _ -> Error.warn (Error.Msg.type_cyclic_reference (Env.uniq_field_ident obj_id attr)) pos)
      | _ -> ()
    )
    entries;

  (* Translate object fields lazily: warn on errors, skip invalid fields *)
  let entry_types = List.fold_left
    (fun entries' entry ->
      match entry with
      | ObjectField (attr, _) ->
        (match Env.get_obj_field attr obj_id venv
          ~succ:translate_lazy
          ~err:(Error.error_at pos)
        with
        | Ok (_, entry_ty) -> entries' @ [TobjectField (attr, entry_ty)]
        | Error _ -> entries')
      | _ ->
        entries'
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
        | Tstring -> ok (venv, Tstring)
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
  | Add, _, Tstring | Add, Tstring, _ -> ok (venv'', Tstring)
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
  | GreaterThan, Tstring, Tstring -> ok (venv'', Tbool)
  | GreaterThanOrEqual, Tstring, Tstring -> ok (venv'', Tbool)
  | LessThan, Tstring, Tstring -> ok (venv'', Tbool)
  | LessThanOrEqual, Tstring, Tstring -> ok (venv'', Tbool)
  | In, Tstring, (Tobject _ | Tany | TruntimeObject _ | TobjectPtr _) -> ok (venv'', Tbool)
  | _ -> Error.error_at pos Error.Msg.invalid_binary_op

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
