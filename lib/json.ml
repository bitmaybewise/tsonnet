open Ast
open Result
open Syntax_sugar

let rec value_to_yojson (expr : Ast.expr) : (Yojson.t, string) result =
  match expr with
  | Number (_, n) ->
    ok (match n with
    | Int i -> `Int i
    | Float f -> `Float f)
  | Null _ -> ok `Null
  | Bool (_, b) -> ok (`Bool b)
  | String (_, s) -> ok (`String s)
  | Array (_, values) ->
    let* results = List.fold_left
      (fun acc v ->
        let* list = acc in
        let* json = value_to_yojson v in
        ok (json :: list)
      )
      (ok [])
      values
    in
    ok (`List (List.rev results))
  | EvaluatedObject (_, fields) ->
    let* json_fields = List.fold_left
      (fun acc (name, expr) ->
        let* list = acc in
        let* json = value_to_yojson expr in
        ok ((name, json) :: list)
      )
      (ok [])
      fields
    in
    ok (`Assoc (List.rev json_fields))
  | expr -> error (Error.Msg.value_not_represetable_as_json (string_of_type expr))

let expr_to_string expr =
  let yojson = value_to_yojson expr
  in Result.map Yojson.pretty_to_string yojson
