open Syntax_sugar

module Env = struct
  type t = string
  let compare = String.compare
end

type env_id = EnvId of int

module Id = struct
  let counter = ref 0

  let generate () =
    if !counter < max_int
    then (incr counter; Result.ok (EnvId !counter))
    else Result.error "Too many uniquely identifiable expressions added to the environment!"

  let reset () = counter := 0
end

module Map = Map.Make(Env)

let empty = Map.empty

let keys env = Map.fold (fun k _ acc -> k :: acc) env []

let find_opt = Map.find_opt

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

let add_local = Map.add

let uniq_field_ident (EnvId id) name =
  Printf.sprintf "%d->%s" id name

let add_obj_field name expr obj_id env =
  add_local (uniq_field_ident obj_id name) expr env

let get_obj_field name obj_id env ~succ ~err =
  find_var (uniq_field_ident obj_id name) env ~succ ~err
