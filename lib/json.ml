open Ast
open Result

let rec value_to_yojson : Ast.expr -> (Yojson.t, string) result = function
  | Number (_, n) ->
    ok (match n with
    | Int i -> `Int i
    | Float f -> `Float f)
  | Null _ -> ok `Null
  | Bool (_, b) -> ok (`Bool b)
  | String (_, s) -> ok (`String s)
  | Array (_, values) ->
    let expr_to_list expr' = to_list (value_to_yojson expr') in
    let results = values |> List.map expr_to_list |> List.concat in
    ok (`List results)
  | Object (_, entries) ->
    let eval' = fun entry ->
      match entry with
      | ObjectField (k, v) ->
        let result = value_to_yojson v
        in Result.map (fun val' -> (k, val')) result
      | _ ->
        error "Object expression(s) not representable as JSON"
    in
    let results = entries |> List.map eval' |> List.map to_list |> List.concat
    in ok (`Assoc results)
  | _ -> error "value type not representable as JSON"

let expr_to_string expr =
  let yojson = value_to_yojson expr
  in Result.map Yojson.pretty_to_string yojson
