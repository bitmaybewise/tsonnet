open Ast
open Result
open Syntax_sugar

type translation_key =
  | TranslatingVar of string
  | TranslatingObjField of Env.env_id * string
  | TranslatingFunction of string
  | TranslatingDefaultParam of string

module TranslationKeys = Set.Make(struct
  type t = translation_key
  let compare = Stdlib.compare
end)

let translating_bindings = ref TranslationKeys.empty

let string_of_translation_key = function
  | TranslatingVar varname -> varname
  | TranslatingObjField (obj_id, field) -> Env.uniq_field_ident obj_id field
  | TranslatingFunction name -> name
  | TranslatingDefaultParam name -> name

let with_translating key pos fn =
  if TranslationKeys.mem key !translating_bindings then
    Error.error_at pos (Error.Msg.type_cyclic_reference (string_of_translation_key key))
  else begin
    translating_bindings := TranslationKeys.add key !translating_bindings;
    let result = fn () in
    translating_bindings := TranslationKeys.remove key !translating_bindings;
    result
  end

let with_shadowed_translating_vars names fn =
  let saved_translating_bindings = !translating_bindings in
  List.iter
    (fun name ->
      translating_bindings := TranslationKeys.remove (TranslatingVar name) !translating_bindings
    )
    names;
  let result = fn () in
  translating_bindings := saved_translating_bindings;
  result

type tsonnet_type =
  | Tunit
  | Tnull
  | Tbool
  | Tnumber
  | Tstring of string
  | Tany
  | Tarray of tsonnet_type list
  | Tobject of t_object_entry list
  | TruntimeObject of Env.env_id * tsonnet_type Env.Map.t * t_object_entry list
  | TobjectPtr of Env.env_id * t_object_scope * tsonnet_type Env.Map.t option
  | Lazy of expr
  | LazyIn of tsonnet_type Env.Map.t * expr
  | LazyDefault of string * tsonnet_type Env.Map.t * (t_function_param * expr option) list * expr
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

and t_function_param = {
  param_name: string;
  param_type: tsonnet_type;
  param_default: expr option;
}

and t_function_def = {
  params: t_function_param list;
  body: expr;
  return: tsonnet_type;
}
and t_function_call = {
  params: tsonnet_type list;
  return: tsonnet_type;
}
and t_closure = {
  params: t_function_param list;
  body: expr;
}

(* Semantic equality for types. *)
let rec semantic_type_equal ty_a ty_b =
  match ty_a, ty_b with
  (* Ignores representation details that do not affect type identity,
     such as the concrete value carried by Tstring.  *)
  | Tstring _, Tstring _ -> true
  | Tarray [], Tarray _ | Tarray _, Tarray [] -> true
  | Tarray elems_a, Tarray elems_b ->
    List.length elems_a = List.length elems_b
    && List.for_all2 semantic_type_equal elems_a elems_b
  (* Falls back to structural equality otherwise. *)
  | _ -> ty_a = ty_b

let rec to_string = function
  | Tunit -> "()"
  | Tnull -> "Null"
  | Tbool -> "Bool"
  | Tnumber -> "Number"
  | Tstring _ -> "String"
  | Tany -> "Any"
  | Tarray tys ->
    Printf.sprintf "Array of %s"
      (match tys with
      | [] -> "Any"
      | ty :: rest ->
        if List.for_all (semantic_type_equal ty) rest
        then to_string ty
        else "Any"
      )
  | Tobject fields ->
    let field_to_string = function
      | TobjectField (field, ty) -> field ^ " : " ^ to_string ty
      | TobjectExpr ty -> to_string ty
    in
    "{" ^ (
      String.concat ", " (List.map field_to_string fields)
    ) ^ "}"
  | TruntimeObject (_, _, fields) ->
    let field_to_string = function
      | TobjectField (field, ty) -> field ^ " : " ^ to_string ty
      | TobjectExpr ty -> to_string ty
    in
    "{" ^ (
      String.concat ", " (List.map field_to_string fields)
    ) ^ "}"
  | TobjectPtr (Env.EnvId id, scope, _) ->
    let s =
      match scope with
      | TobjectSelf -> "self"
      | TobjectTopLevel -> "$"
    in Printf.sprintf "%s (%d)" s id
  | Lazy ty | LazyIn (_, ty) | LazyDefault (_, _, _, ty) -> string_of_type ty
  | TfunctionDef {params; return; _} ->
    Printf.sprintf "function(%s) -> %s"
      (List.map
        (fun param -> param.param_name ^ ": " ^ to_string param.param_type)
        params
      |> String.concat ", "
      )
      (to_string return)
  | TfunctionCall {params=params_type; return} ->
    Printf.sprintf "function(%s) -> %s"
      (List.map to_string params_type |> String.concat ", ")
      (to_string return)
  | Tclosure {params; _} ->
    Printf.sprintf "function(%s)"
      (List.map
        (fun param -> param.param_name ^ ": " ^ to_string param.param_type)
        params
      |> String.concat ", "
      )
  | Tunresolved -> "<unresolved>"

(* These helpers are only used for unused-local warnings. Cycle detection is
   handled during translation via translating_bindings/with_translating. *)
let exclude_bound_idents bound_names idents =
  List.filter (fun ident -> not (List.mem ident bound_names)) idents

let rec collect_free_idents = function
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
  | Closure (_, closure) ->
    let param_names = List.map fst closure.params in
    collect_param_defaults closure.params
    @ exclude_bound_idents param_names (collect_free_idents closure.body)
  | FunctionDef (_, def) ->
    let bound_names = def.name :: List.map fst def.params in
    collect_param_defaults def.params
    @ exclude_bound_idents bound_names (collect_free_idents def.body)
  | If (_, cond_expr, then_expr, else_expr_opt) ->
    collect_free_idents cond_expr
    @ collect_free_idents then_expr
    @ (match else_expr_opt with
      | Some else_expr -> collect_free_idents else_expr
      | None -> [])
  (* Terminal/runtime variants do not contain source-level free identifiers. *)
  | Unit | Null _ | Number _ | String _ | Bool _ | EvaluatedObject _
  | RuntimeObject _ | ObjectPtr _ | LazyDefault _ -> []

and collect_param_defaults params =
  List.concat_map
    (function
    | _, Some expr -> collect_free_idents expr
    | _, None -> []
    )
    params

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

let expr_pos fallback = function
  | Null pos | Number (pos, _) | Bool (pos, _) | String (pos, _)
  | EvaluatedObject (pos, _) | Ident (pos, _) | Array (pos, _)
  | ParsedObject (pos, _) | ObjectFieldAccess (pos, _, _)
  | BinOp (pos, _, _, _) | UnaryOp (pos, _, _) | IndexedExpr (pos, _, _)
  | Local (pos, _) | FunctionDef (pos, _) | FunctionCall (pos, _)
  | Closure (pos, _) | If (pos, _, _, _) -> pos
  | Unit | RuntimeObject _ | ObjectPtr _ | LazyDefault _ | Seq _ -> fallback

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
  | EvaluatedObject _ | RuntimeObject _ | ObjectPtr _ | LazyDefault _ as expr' ->
    error (Error.Msg.type_invalid_expr (string_of_type expr'))

and translate_indexed_expr venv (pos, varname, index_expr) =
  let* (venv', index_expr') = translate venv index_expr in
  match index_expr' with
  | Tnumber ->
    let index_opt =
      match index_expr with
      | Number (_, Int i) -> Some i
      | _ -> None
    in
    let translate_indexable venv' expr' =
      match expr' with
      | Tarray elems as ty ->
        (match index_opt with
        | Some index when index >= 0 && index < List.length elems ->
          deep_translate_type pos venv' (List.nth elems index) >>= fun elem_ty -> ok (venv', elem_ty)
        | _ -> ok (venv', ty)
        )
      | (Tstring _) as ty -> ok (venv', ty)
      | ty -> Error.error_at pos (Error.Msg.type_non_indexable_value (to_string ty))
    in
    Env.find_var varname venv'
      ~succ:(fun venv' expr' ->
        match expr' with
        | Lazy expr ->
          with_translating (TranslatingVar varname) pos (fun () ->
            let* (venv'', ty) = translate venv expr in
            translate_indexable venv'' ty
          )
        | ty -> translate_indexable venv' ty
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
  let* venv' = List.fold_left
    (fun acc (varname, var_expr) ->
      let* venv = acc in
      match var_expr with
      | ObjectFieldAccess (_, (Self | TopLevel), []) ->
        let* (venv', ty) = translate venv var_expr in
        ok (Env.add_local varname ty venv')
      | _ -> ok (Env.add_local varname (Lazy var_expr) venv)
    )
    (ok venv)
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
      let local_names = List.map fst all_vars in
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
      (* Locals introduce a lexical scope. If a local shadows a binding that is
         currently being translated, the body should resolve to the local binding
         instead of reporting a cycle against the outer one. *)
      with_shadowed_translating_vars local_names (fun () -> go venv' body)
    | expr :: rest ->
      let* (venv', _) = translate venv expr in
      go venv' rest
  in
  go venv exprs

and translate_ident venv pos varname =
  Env.find_var varname venv
    ~succ:(fun venv ty ->
      match ty with
      | LazyDefault _ as ty ->
        let* ty' = deep_translate_type pos venv ty in
        ok (venv, ty')
      | Lazy expr ->
        with_translating (TranslatingVar varname) pos (fun () ->
          let* (venv', ty) = translate venv expr in
          let* ty' = deep_translate_type pos venv' ty in
          ok (venv', ty')
        )
      | _ -> ok (venv, ty)
    )
    ~err:(Error.error_at pos)

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
  ok (venv, Tarray (List.map (fun elem -> LazyIn (venv, elem)) elems))

and translate_lazy venv = function
  | Lazy expr -> translate venv expr
  | LazyIn (lazy_venv, expr) -> translate lazy_venv expr
  | LazyDefault (name, outer_venv, params, expr) ->
    let default_env = add_default_params_to_env outer_venv name params in
    translate default_env expr
  | ty -> error (Error.Msg.type_invalid_expr (to_string ty))

and translate_object ?alias venv pos entries =
  let* obj_id = Env.Id.generate () in
  let had_toplevel = Option.is_some (Env.find_opt "$" venv) in
  let obj_venv = Env.add_local "self" (TobjectPtr (obj_id, TobjectSelf, None)) venv in
  let obj_venv, _ =
    Env.add_local_when_not_present "$" (TobjectPtr (obj_id, TobjectTopLevel, None)) obj_venv
  in
  let obj_venv =
    match alias with
    | Some varname -> Env.add_local varname (TobjectPtr (obj_id, TobjectSelf, None)) obj_venv
    | None -> obj_venv
  in

  (* Translate locals *)
  let* (obj_venv, fields) = List.fold_left
    (fun result entry ->
      let* (obj_venv, fields) = result in
      match entry with
      | ObjectExpr expr ->
        let* (obj_venv', _) = translate obj_venv expr in ok (obj_venv', fields)
      | ObjectField (attr, expr) ->
        ok (
          Env.add_obj_field attr (Lazy expr) obj_id obj_venv,
          fields @ [TobjectField (attr, Lazy expr)]
        )
      | ObjectConditionalField (attr_expr, expr) ->
        let* (_, ty) = translate obj_venv attr_expr in
        (match ty with
        | Tstring attr ->
          ok (
            Env.add_obj_field attr (Lazy expr) obj_id obj_venv,
            fields @ [TobjectField (attr, Lazy expr)]
          )
        | Tnull -> ok (obj_venv, fields)
        | _ ->
          let attr_pos = match attr_expr with If (p, _, _, _) -> p | _ -> pos in
          Error.error_at attr_pos
            (Error.Msg.invalid_conditional_field_key (to_string ty))
        )
    )
    (ok (obj_venv, []))
    entries
  in
  (* Remove self and $ from the environment to prevent leaking *)
  let venv = Env.Map.remove "self" venv in
  let venv = if had_toplevel then venv else Env.Map.remove "$" venv in
  let captured_obj_venv = obj_venv in
  let obj_venv = Env.Map.map
    (function
    | TobjectPtr (ptr_id, scope, _) when ptr_id = obj_id ->
      TobjectPtr (ptr_id, scope, Some captured_obj_venv)
    | ty -> ty
    )
    obj_venv
  in
  ok (venv, TruntimeObject (obj_id, obj_venv, fields))

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
      if TranslationKeys.mem (TranslatingVar varname) !translating_bindings then
        match Env.find_opt "self" venv with
        | Some (TobjectPtr _ as obj) -> ok (venv, obj)
        | _ -> Error.error_at pos (Error.Msg.type_cyclic_reference varname)
      else
        Env.find_var varname venv
          ~succ:(fun venv ty ->
            match ty with
            | TobjectPtr _ as obj -> ok (venv, obj)
            | TruntimeObject _ as obj -> ok (venv, obj)
            | Lazy expr ->
              with_translating (TranslatingVar varname) pos (fun () ->
                match expr with
                | ParsedObject (obj_pos, entries) -> translate_object ~alias:varname venv obj_pos entries
                | _ -> translate venv expr
              )
            | _ -> Error.error_at pos Error.Msg.must_be_object
          )
          ~err:(Error.error_at pos)
  in

  let rec go acc = function
    | [] -> acc
    | field_expr :: rest ->
      let* (venv, prev_ty) = acc in

      let get_obj_id_and_env =
        match prev_ty with
        | TobjectPtr (obj_id, _, Some obj_venv) -> ok (obj_id, obj_venv)
        | TobjectPtr (obj_id, _, None) -> ok (obj_id, venv)
        | TruntimeObject (obj_id, obj_venv, _) -> ok (obj_id, obj_venv)
        | _ -> Error.error_at pos Error.Msg.must_be_object
      in

      let* next = match field_expr with
        | String (_, field) | Ident (_, field) ->
          let* (obj_id, field_venv) = get_obj_id_and_env in
          let lookup_field () = Env.get_obj_field field obj_id field_venv
            ~succ:translate_lazy
            ~err:(Error.error_at pos)
          in
          (match rest with
          | Number _ :: _ -> lookup_field ()
          | _ -> with_translating (TranslatingObjField (obj_id, field)) pos lookup_field
          )
        | Number (pos, index) ->
          (* Handle numeric indexing of strings and arrays *)
          (match prev_ty with
          | Tstring _ as ty -> ok (venv, ty)
          | Tarray elems ->
            (match index with
            | Int i when i >= 0 && i < List.length elems ->
              let* elem_ty = deep_translate_type pos venv (List.nth elems i) in
              ok (venv, elem_ty)
            | _ -> ok (venv, Tany)
            )
          | _ -> Error.error_at pos (Error.Msg.type_non_indexable_type (to_string prev_ty))
          )
        | _ ->
          Error.error_at pos (Error.Msg.type_invalid_lookup_key (string_of_type field_expr))
      in
      go (ok next) rest
  in
  go (ok (venv, obj)) chain_exprs

and translate_bin_op venv pos op e1 e2 =
  let* (venv', e1') = translate venv e1 in
  let* (venv'', e2') = translate venv' e2 in
  match op, e1', e2' with
  | Add, _, Tstring s | Add, Tstring s, _ -> ok (venv'', Tstring s)
  | Add, Tnumber, Tnumber -> ok (venv'', Tnumber)
  | Add, Tarray items1, Tarray items2 -> ok (venv'', Tarray (items1 @ items2))
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

and make_function_params params =
  List.map
    (fun (name, default) -> {
      param_name = name;
      param_type = Tunresolved;
      param_default = default;
    })
    params

and param_names params = List.map (fun param -> param.param_name) params

and add_params_to_env venv params =
  List.fold_left
    (fun env param -> Env.add_local param.param_name param.param_type env)
    venv
    params

and add_default_params_to_env outer_venv current_name params =
  List.fold_left
    (fun env (param, default_opt) ->
      if param.param_name = current_name
      then env
      else
        let param_type =
          match default_opt with
          | Some default_expr -> LazyDefault (param.param_name, outer_venv, params, default_expr)
          | None -> param.param_type
        in
        Env.add_local param.param_name param_type env
    )
    outer_venv
    params

and translate_function_def venv (_pos, def) =
  let fun_def = TfunctionDef
    { params = make_function_params def.params;
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
    let closure_ty = Tclosure {
      params = make_function_params closure.params;
      body = closure.body;
    } in
    translate_closure_call venv (pos, closure_ty, call.args)
  | _ ->
    let* (venv', callee_ty) = translate venv call.callee in
    (match callee_ty with
    | Tclosure _ as closure_ty -> translate_closure_call venv' (pos, closure_ty, call.args)
    | _ ->
      Error.error_at pos (Error.Msg.type_invalid_expr (string_of_type call.callee))
    )

and resolve_call_params venv pos def_params call_args =
  let positional_args =
    List.filter_map (function Positional e -> Some e | Named _ -> None) call_args
  in
  let named_args =
    List.filter_map (function Named (n, e) -> Some (n, e) | Positional _ -> None) call_args
  in
  let num_positional = List.length positional_args in
  let num_def = List.length def_params in
  let num_provided = num_positional + List.length named_args in
  if num_provided > num_def
  then Error.error_at pos (Error.Msg.wrong_number_of_params num_def num_provided)
  else
    let* (venv', resolved_with_defaults) =
      List.fold_left
        (fun acc (index, param) ->
          let* (venv_acc, params') = acc in
          let call_expr_opt =
            if index < num_positional
            then Some (List.nth positional_args index)
            else Option.map snd (List.find_opt (fun (n, _) -> n = param.param_name) named_args)
          in
          match call_expr_opt with
          | Some call_param ->
            let* (venv_next, call_param_type) = translate venv_acc call_param in
            let* param_type =
              match param.param_type with
              | Tunresolved -> ok call_param_type
              | expected ->
                if semantic_type_equal call_param_type expected
                then ok expected
                else Error.error_at pos
                  (Error.Msg.type_mismatch
                    ~expected:(to_string expected)
                    ~got:(to_string call_param_type))
            in
            ok (venv_next, params' @ [({ param with param_type }, None)])
          | None ->
            (match param.param_default with
            | Some default_expr ->
              ok (venv_acc, params' @ [(param, Some default_expr)])
            | None ->
              Error.error_at pos (Error.Msg.wrong_number_of_params num_def num_provided)
            )
        )
        (ok (venv, []))
        (List.mapi (fun i param -> (i, param)) def_params)
    in
    let resolved_params = List.map fst resolved_with_defaults in
    let resolved_params = List.map
      (fun param ->
        match List.assoc_opt param.param_name
          (List.filter_map
            (fun (p, default) ->
              Option.map
                (fun expr -> (p.param_name, expr)) default)
                resolved_with_defaults
            )
        with
        | Some default_expr ->
          { param with param_type = LazyDefault (param.param_name, venv', resolved_with_defaults, default_expr) }
        | None -> param
      )
      resolved_params
    in
    ok (venv', resolved_params)

and translate_named_function_call venv (pos, name, args) =
  match Env.find_opt name venv with
  | Some (TfunctionDef { params = def_params;
                         body = body_expr;
                         return = return_type;
                        }) ->
      let* (venv', resolved_params) = resolve_call_params venv pos def_params args in
      let body_venv = add_params_to_env venv' resolved_params in
      let* (_, body_type) =
        with_translating (TranslatingFunction name) pos (fun () ->
          with_shadowed_translating_vars (param_names resolved_params)
            (fun () -> translate body_venv body_expr)
        )
      in
      let* body_type = deep_translate_type pos body_venv body_type in
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
    let* (venv', resolved) =
      with_translating (TranslatingVar name) pos (fun () -> translate venv expr)
    in
    let venv'' = Env.add_local name resolved venv' in
    translate_named_function_call venv'' (pos, name, args)
  | Some (Tclosure _ as closure_ty) ->
    with_translating (TranslatingFunction name) pos
      (fun () -> translate_closure_call venv (pos, closure_ty, args))
  | _ ->
    Error.error_at pos (Error.Msg.var_not_found name)

and translate_closure venv (_pos, closure) =
  ok (venv, Tclosure { params = make_function_params closure.params; body = closure.body })

and translate_closure_call venv (pos, closure_ty, call_args) =
  match closure_ty with
  | Tclosure { params = def_params; body } ->
    let* (venv', resolved_params) = resolve_call_params venv pos def_params call_args in
    let body_venv = add_params_to_env venv' resolved_params in
    let* (_, body_type) =
      with_shadowed_translating_vars (param_names resolved_params)
        (fun () -> translate body_venv body)
    in
    let* body_type = deep_translate_type pos body_venv body_type in
    ok (venv, body_type)
  | _ -> Error.error_at pos (Error.Msg.type_invalid_expr (to_string closure_ty))

and deep_translate_type pos venv = function
  | Lazy expr ->
    let* (venv', ty) = translate venv expr in
    deep_translate_type (expr_pos pos expr) venv' ty
  | LazyIn (lazy_venv, expr) ->
    let* (venv', ty) = translate lazy_venv expr in
    deep_translate_type (expr_pos pos expr) venv' ty
  | LazyDefault (name, outer_venv, params, expr) ->
    with_translating (TranslatingDefaultParam name) pos (fun () ->
      with_shadowed_translating_vars [name] (fun () ->
        let default_env = add_default_params_to_env outer_venv name params in
        let* (venv', ty) = translate default_env expr in
        deep_translate_type (expr_pos pos expr) venv' ty
      )
    )
  | Tarray tys ->
    let* tys' = List.fold_left
      (fun acc ty ->
        let* tys = acc in
        let* ty' = deep_translate_type pos venv ty in
        ok (tys @ [ty'])
      )
      (ok [])
      tys
    in
    ok (Tarray tys')
  | TruntimeObject (obj_id, obj_venv, fields) ->
    let* fields' = List.fold_left
      (fun acc field ->
        let* fields = acc in
        match field with
        | TobjectField (name, ty) ->
          let* ty' = with_translating (TranslatingObjField (obj_id, name)) pos (fun () ->
            deep_translate_type pos obj_venv ty
          ) in
          ok (fields @ [TobjectField (name, ty')])
        | TobjectExpr ty ->
          let* ty' = deep_translate_type pos obj_venv ty in
          ok (fields @ [TobjectExpr ty'])
      )
      (ok [])
      fields
    in
    ok (TruntimeObject (obj_id, obj_venv, fields'))
  | TobjectPtr (obj_id, _, _) ->
    Error.error_at pos (Error.Msg.type_cyclic_reference (Env.uniq_field_ident obj_id "self"))
  | ty -> ok ty

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
    let* (venv, ty) = translate Env.empty expr in
    let* _ = deep_translate_type dummy_pos venv ty in
    Env.Id.reset ();
    translating_bindings := TranslationKeys.empty;
    ok expr
