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
  | Tobject of (string * tsonnet_type) list
  | Lazy of expr

let translate_late_binding translate_fun = fun venv expr ->
  match expr with
  | Lazy expr -> translate_fun expr venv
  | _ -> error "Expected a lazy evaluated expression"

let rec translate expr venv =
  match expr with
  | Null _ -> ok (venv, Tnull)
  | Bool _ -> ok (venv, Tbool)
  | Number _ -> ok (venv, Tnumber)
  | String _ -> ok (venv, Tstring)
  | Ident (pos, varname) ->
    Env.find_var varname venv
      ~succ:(translate_late_binding translate)
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
      let* (venv, ty) = translate elem venv in
      let* (venv, ty) =
        List.fold_left
          (fun acc elem -> acc >>= fun (venv, ty) ->
            let* (venv', elem_ty) = translate elem venv in
            if ty = elem_ty
            then ok (venv', elem_ty)
            else ok (venv', Tany)
          )
          (ok (venv, ty))
          rest
      in ok (venv, Tarray ty)
    )
  | Object (_pos, elems) ->
    let* fields =
      List.fold_left
        (fun acc (attr, expr) ->
          let* attrs = acc in
          let* (_, ty) = translate expr venv in
          ok ((attr, ty) :: attrs)
        )
        (ok [])
        elems
    in ok (venv, Tobject fields)
  | Local (_, vars) ->
    let venv' =
      (List.fold_left
        (fun venv (varname, var_expr) ->
          (* Adds an expr to the env to be evaluated at a later point in time (when required) *)
          Env.Map.add varname (Lazy var_expr) venv
        )
        venv
        vars
      )
    in ok (venv', Tunit)
  | Seq exprs ->
    List.fold_left
      (fun acc expr -> acc >>= fun (venv, _) -> translate expr venv)
      (ok (venv, Tunit))
      exprs
  | _ -> error "Not yet implemented"

let check expr =
  translate expr Env.empty >>= fun _ -> ok expr
