open Ast
open Result
open Syntax_sugar

type tsonnet_type =
  | Tunit
  | Tnull
  | Tbool
  | Tnumber
  | Tstring
  | Tany
  | Tarray of tsonnet_type
  | Tobject of t_object_entry list
  | TobjectSelf of Env.env_id
  | Lazy of expr
and t_object_entry =
  | TobjectField of string * tsonnet_type
  | TobjectExpr of tsonnet_type

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
  | TobjectSelf (Env.EnvId id) -> Printf.sprintf "self (%d)" id
  | Lazy ty -> string_of_type ty

let rec check_cyclic_refs venv varname seen pos =
  if List.mem varname seen
  then
    Error.trace ("Cyclic reference found for " ^ varname) pos >>= error
  else
    match Env.find_opt varname venv with
    | Some (Lazy expr) -> check_expr_for_cycles venv expr (varname :: seen)
    | _ -> ok ()
and check_expr_for_cycles venv expr seen =
  match expr with
  | Unit | Null _ | Number _ | String _ | Bool _ -> ok ()
  | Array (_, exprs) -> iter_for_cycles venv seen exprs
  | Object (_, entries) -> check_object_for_cycles venv entries seen
  | ObjectFieldAccess (pos, field) -> check_object_field_for_cycles venv field pos seen
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
and check_object_field_for_cycles venv field pos seen =
  (match Env.find_opt "self" venv with
  | Some (TobjectSelf obj_id) ->
    let obj_field = Env.uniq_field_ident obj_id field in
    check_cyclic_refs venv obj_field seen pos
  | _ -> ok ()
  )

let rec translate expr venv =
  match expr with
  | Unit -> ok (venv, Tunit)
  | Null _ -> ok (venv, Tnull)
  | Bool _ -> ok (venv, Tbool)
  | Number _ -> ok (venv, Tnumber)
  | String _ -> ok (venv, Tstring)
  | Ident (pos, varname) ->
    Env.find_var varname venv
      ~succ:(fun venv ty ->
        match ty with
        | Lazy expr -> translate expr venv
        | _ -> ok (venv, ty)
      )
      ~err:(Error.error_at pos)
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
  | Object (pos, entries) -> translate_object venv pos entries
  | ObjectFieldAccess (pos, field) -> translate_object_field_access venv pos field
  | Local (pos, vars) ->
    let venv' = List.fold_left
      (* Adds an expr to the env to be evaluated at a later point in time (when required) *)
      (fun venv (varname, var_expr) -> Env.add_local varname (Lazy var_expr) venv)
      venv
      vars
    in
    let* _ = List.fold_left
      (fun ok' (varname, _) -> ok' >>= fun _ -> check_cyclic_refs venv' varname [] pos)
      (ok ())
      vars
    in ok (venv', Tunit)
  | Seq exprs ->
    List.fold_left
      (fun acc expr -> acc >>= fun (venv, _) -> translate expr venv)
      (ok (venv, Tunit))
      exprs
  | BinOp (pos, op, e1, e2) ->
    (let* (venv', e1') = translate e1 venv in
    let* (venv'', e2') = translate e2 venv' in
    match op, e1', e2' with
    | _, Tnumber, Tnumber -> ok (venv'', Tnumber)
    | Add, _, Tstring | Add, Tstring, _ -> ok (venv'', Tstring)
    | _ -> Error.trace "Invalid binary operation" pos >>= error
    )
  | UnaryOp (pos, op, expr) ->
    (let* (venv', expr') = translate expr venv in
    match op, expr' with
    | Plus, Tnumber | Minus, Tnumber | BitwiseNot, Tnumber -> ok (venv', Tnumber)
    | Not, Tbool | BitwiseNot, Tbool -> ok (venv', Tbool)
    | _ -> Error.trace "Invalid unary operation" pos >>= error
    )
  | IndexedExpr (pos, varname, index_expr) ->
    (let* (venv', index_expr') = translate index_expr venv in
    match index_expr' with
    | Tnumber ->
      Env.find_var varname venv'
        ~succ:(fun venv' expr' ->
          match expr' with
          | (Tarray _) as ty -> ok (venv', ty)
          | Tstring as ty -> ok (venv', ty)
          | Lazy expr -> translate expr venv
          | ty -> error (to_string ty ^ " is a non indexable value")
        )
        ~err:(Error.error_at pos)
    | ty -> Error.trace ("Expected Integer index, got " ^ to_string ty) pos >>= error
    )
  | expr' -> error ("Invalid type " ^ string_of_type expr')

and translate_lazy venv = function
  | Lazy expr -> translate expr venv
  | ty -> error ("Invalid type " ^ to_string ty)

and translate_object venv pos entries =
  let* obj_id = Env.Id.generate () in
  let venv' = Env.add_local "self" (TobjectSelf obj_id) venv in
  (* Translate locals *)
  let* venv'' = List.fold_left
    (fun result entry ->
      let* venv = result in
      match entry with
      | ObjectExpr expr ->
        let* (venv', _) = translate expr venv in (ok venv')
      | ObjectField (attr, expr) ->
        ok (Env.add_obj_field attr (Lazy expr) obj_id venv)
    )
    (ok venv')
    entries
  in
  (* Check for cyclical references among object fields *)
  let* () = List.fold_left
      (fun ok' entry -> ok' >>= fun _ ->
        match entry with
        | ObjectField (attr, _) ->
          check_cyclic_refs venv'' (Env.uniq_field_ident obj_id attr) [] pos
        | _ -> ok'
      )
      (ok ())
      entries
  in
  (* Then translate object fields *)
  let* entry_types = List.fold_left
    (fun result entry ->
      let* entries' = result in
      match entry with
      | ObjectField (attr, _) ->
        let* (_, entry_ty) = Env.get_obj_field attr obj_id venv''
          ~succ:translate_lazy
          ~err:(Error.error_at pos)
        in ok (entries' @ [TobjectField (attr, entry_ty)])
      | _ ->
        result
    )
    (ok [])
    entries
  in
  ok (venv, Tobject entry_types)

and translate_object_field_access venv pos field =
  match Env.find_opt "self" venv with
  | Some (TobjectSelf obj_id) ->
    Env.get_obj_field field obj_id venv
      ~succ:translate_lazy
      ~err:(Error.error_at pos)
  | _ ->
    Error.error_at pos "Can't use self outside of an object"

let check expr =
  Scope.validate expr
  >>= fun _ -> translate expr Env.empty
  >>= fun _ ->
    Env.Id.reset ();
    ok expr
