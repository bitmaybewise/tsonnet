open Ast
open Result
open Syntax_sugar

type tsonnet_type =
  | Tunit
  | Tnull
  | Tbool
  | Tnumber
  | Tstring
  | Tobject of (string * tsonnet_type) list
  | Tarray of tsonnet_type
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
