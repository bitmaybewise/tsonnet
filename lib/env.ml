open Syntax_sugar

module Env =
  struct
    type t = string
    let compare = String.compare
  end

module Map = Map.Make(Env)

let empty = Map.empty

let keys env = Map.fold (fun k _ acc -> k :: acc) env []

let find_var varname env ~succ ~err =
  match Map.find_opt varname env with
  | Some expr ->
    let* (env', evaluated_expr) = succ env expr in
    (* Since `succ` has evaluated expr, we can now memoize it
       and subsequent look ups operating in this new environment
       will already have it evaluated *)
    let updated_env = Map.add varname evaluated_expr env' in
    Result.ok (updated_env, evaluated_expr)
  | None -> err ("Undefined variable: " ^ varname)
