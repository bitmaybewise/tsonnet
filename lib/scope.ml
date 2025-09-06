(* This module handles eager scope analysis to ensure that identifiers
   like 'self' are used in appropriate contexts before lazy evaluation begins.
*)

open Ast
open Result
open Syntax_sugar

type context = {
  in_object: bool;
  object_depth: int;
  current_locals: string list;
}

let empty_context = {
  in_object = false;
  object_depth = 0;
  current_locals = []
}

let enter_object_scope context = {
  context with
  in_object = true;
  object_depth = context.object_depth + 1
}

let add_locals_to_context locals context = {
  context with
  current_locals = locals @ context.current_locals
}

let rec _validate expr context =
  match expr with
  | Unit | Null _ | Number _ | String _ | Bool _ -> ok ()
  | Ident (pos, varname) ->
    (* Identifier validation - the heart of scope checking *)
    validate_ident pos varname context
  | Array (_, exprs) ->
    validate_expression_list exprs context
  | Object (_, entries) ->
    (* Object validation - this is where scope context changes *)
    validate_object entries context
  | ObjectFieldAccess (pos, _) ->
    validate_object_field_access pos context
  | Local (_, vars) ->
    validate_locals vars context
  | Seq exprs ->
    validate_expression_list exprs context
  | BinOp (_, _, e1, e2) ->
    validate_binop e1 e2 context
  | UnaryOp (_, _, expr) ->
    _validate expr context
  | IndexedExpr (_, _, index_expr) ->
    _validate index_expr context
  | _ ->
    (* For any other expression types, no special scope validation needed *)
    ok ()

and validate_ident pos varname context =
  if varname = "self" && not context.in_object
  then Error.trace ("Can't use self outside of an object") pos >>= error
  else ok ()

and validate_expression_list exprs context =
  List.fold_left
    (fun acc expr -> acc >>= fun _ -> _validate expr context)
    (ok ())
    exprs

and validate_object entries context =
  let object_context = enter_object_scope context in
  (* First pass: collect all local variable names from ObjectExpr entries
    This handles cases like: { local x = 1; field: x } *)
  let local_vars = collect_local_variables entries in
  let context_with_locals = add_locals_to_context local_vars object_context in
  validate_object_entries entries context_with_locals

and validate_object_entries entries context =
  List.fold_left
    (fun acc entry ->
      acc >>= fun _ ->
      match entry with
      | ObjectField (_, expr) -> _validate expr context
      | ObjectExpr expr -> _validate expr context
    )
    (ok ())
    entries

and collect_local_variables entries =
  List.fold_left
    (fun acc entry ->
      match entry with
      | ObjectExpr (Local (_, vars)) ->
        acc @ (List.map (fun (name, _) -> name) vars)
      | _ -> acc
    )
    []
    entries

and validate_object_field_access pos context =
  (* This catches cases like: local x = self.field; outside of objects *)
  if not context.in_object
  then Error.trace ("Can't use self outside of an object") pos >>= error
  else ok ()

and validate_locals vars context =
  (* This is crucial - it catches: local x = self.field; outside objects *)
  List.fold_left
    (fun acc (_, expr) -> acc >>= fun _ -> _validate expr context)
    (ok ())
    vars

and validate_binop e1 e2 context =
  _validate e1 context >>= fun _ ->  _validate e2 context

(* This function performs a single eager pass through the AST to validate
   that all identifiers are used in appropriate scopes. It catches scope
   errors before lazy evaluation begins, while still preserving the lazy
   evaluation benefits in the main type checker. *)
let validate expr = _validate expr empty_context
