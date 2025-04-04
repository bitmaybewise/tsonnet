open Ast
open Result

let rec value_to_yojson : Ast.value -> (Yojson.t, string) result = function
  | Number n ->
    ok (match n with
    | Int i -> `Int i
    | Float f -> `Float f)
  | Null -> ok `Null
  | Bool b -> ok (`Bool b)
  | String s -> ok (`String s)
  | Ident id -> ok (`String id)
  | Array values ->
    let expr_to_list expr' = to_list (value_to_yojson expr') in
    let results = values |> List.map expr_to_list |> List.concat in
    ok (`List results)
  | Object attrs ->
    let eval' = fun (k, v) ->
      let result = value_to_yojson v
      in Result.map (fun val' -> (k, val')) result
    in
    let results = attrs |> List.map eval' |> List.map to_list |> List.concat
    in ok (`Assoc results)
  | _ -> error "value type not representable as JSON"

let expr_to_string expr =
  let yojson = value_to_yojson expr.value
  in Result.map Yojson.pretty_to_string yojson
