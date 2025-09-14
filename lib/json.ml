open Ast
open Result
open Syntax_sugar

let rec value_to_yojson (env : expr Env.Map.t) (expr : Ast.expr) : (Yojson.t, string) result =
  match expr with
  | Number (_, n) ->
    ok (match n with
    | Int i -> `Int i
    | Float f -> `Float f)
  | Null _ -> ok `Null
  | Bool (_, b) -> ok (`Bool b)
  | String (_, s) -> ok (`String s)
  | Array (_, values) ->
    let expr_to_list expr' = to_list (value_to_yojson env expr') in
    let results = values |> List.map expr_to_list |> List.concat in
    ok (`List results)
  | RuntimeObject (pos, context, fieldset) -> obj_to_yojson env (pos, context, fieldset)
  | expr -> error ("value type not representable as JSON: " ^ string_of_type expr)

and obj_to_yojson env (pos, obj_id, fieldset) =
  let* fields =
    ObjectFields.fold
      (fun field acc ->
        let* (_, expr) =
          Env.get_obj_field field obj_id env
            ~succ:(fun _ expr -> ok (env, expr))
            ~err:(Error.error_at pos)
        in
        let* yo_value = value_to_yojson env expr in
        let* fields = acc in
        ok ((field, yo_value) :: fields)
      )
      fieldset
      (ok [])
  in ok (`Assoc (List.rev fields))

let expr_to_string (env, expr) =
  let yojson = value_to_yojson env expr
  in Result.map Yojson.pretty_to_string yojson
