open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s: string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec print = function
  | Int i -> Printf.sprintf "%d" i
  | Float f -> Printf.sprintf "%f" f
  | Null -> Printf.sprintf "null"
  | Bool b -> Printf.sprintf "%b" b
  | String s -> Printf.sprintf "\"%s\"" s
  | Array values -> Printf.sprintf "[%s]" (String.concat ", " (List.map print values))
  | Object attrs ->
    let print_key_val = function
      | (k, v) -> Printf.sprintf "\"%s\": %s" k (print v)
    in
    Printf.sprintf "{%s}" (
      String.concat ", " (List.map print_key_val attrs)
    )
  | _ -> failwith "not implemented"

let interpret_bin_op op n1 n2 =
  let float_op =
    match op with
    | Add -> (+.)
    | Subtract -> (-.)
    | Multiply -> ( *. )
    | Divide -> (/.)
  in match (n1, n2) with
  | Int i1, Int i2 -> Float (float_op (Float.of_int i1) (Float.of_int i2))
  | Float f1, Float f2 -> Float (float_op f1 f2)
  | Float f1, Int e2 -> Float (float_op f1 (Float.of_int e2))
  | Int e1, Float f2 -> Float (float_op (Float.of_int e1) f2)
  | _ -> failwith "invalid operation"

(** [interpret expr] interprets the intermediate AST [expr] into an AST. *)
let rec interpret (e: expr) : expr =
  match e with
  | Null | Bool _ | String _ | Int _ | Float _ | Array _ | Object _ -> e
  | BinOp (op, e1, e2) ->
    let n1, n2 = interpret e1, interpret e2
    in interpret_bin_op op n1 n2

let run (s: string) : expr =
  let ast = parse s in
  let evaluated_ast = interpret ast in
  evaluated_ast
