open Ast

let rec expr_to_yojson : (expr -> Yojson.t) = function
  | Number n ->
    (match n with
    | Int i -> `Int i
    | Float f -> `Float f)
  | Null -> `Null
  | Bool b -> `Bool b
  | String s -> `String s
  | Array values -> `List (List.map expr_to_yojson values)
  | Object attrs ->
    let eval' = fun (k, v) -> (k, expr_to_yojson v)
    in `Assoc (List.map eval' attrs)
  | _ -> failwith "value type not representable as JSON"

let expr_to_string expr = Yojson.pretty_to_string (expr_to_yojson expr)
