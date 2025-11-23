open Ast
open Result
open Syntax_sugar

(* The interpreter type allows us to break the circular dependency
   between Json and Interpreter modules.
   Ideally, the Json module does not need to know anything about the
   previous step.
   TODO: Json module fuctions should not receive unevaluated values.
   Guarantee Interpreter generates a new and evaluated AST. *)
type interpreter = expr Env.Map.t -> expr -> ((expr Env.Map.t * expr), string) result

let rec value_to_yojson ~(eval: interpreter) (env : expr Env.Map.t) (expr : Ast.expr) : (Yojson.t, string) result =
  match expr with
  | Number (_, n) ->
    ok (match n with
    | Int i -> `Int i
    | Float f -> `Float f)
  | Null _ -> ok `Null
  | Bool (_, b) -> ok (`Bool b)
  | String (_, s) -> ok (`String s)
  | Array (_, values) ->
    let expr_to_list expr' = to_list (value_to_yojson ~eval env expr') in
    let results = values |> List.map expr_to_list |> List.concat in
    ok (`List results)
  | RuntimeObject (pos, obj_env, fieldset) -> obj_to_yojson ~eval env (pos, obj_env, fieldset)
  | expr -> error (Error.Msg.value_not_represetable_as_json (string_of_type expr))

and obj_to_yojson ~(eval: interpreter) _env (pos, obj_env, fieldset) =
  let* fields =
    ObjectFields.fold
      (fun field acc ->
        let* obj_id = match Env.find_opt "self" obj_env with
        | Some (Ast.ObjectPtr (obj_id, _)) -> ok obj_id
        | _ -> error Error.Msg.must_be_object
        in
        let* (env', expr) =
          Env.get_obj_field field obj_id obj_env ~succ:eval ~err:(Error.error_at pos)
        in
        let* yo_value = value_to_yojson ~eval env' expr in
        let* fields = acc in
        ok ((field, yo_value) :: fields)
      )
      fieldset
      (ok [])
  in ok (`Assoc (List.rev fields))

let expr_to_string ~(eval: interpreter) (env, expr) =
  let yojson = value_to_yojson ~eval env expr
  in Result.map Yojson.pretty_to_string yojson
