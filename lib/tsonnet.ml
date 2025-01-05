open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s: string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec print = function
  | Number n ->
    (match n with
    | Int i -> Printf.sprintf "%d" i
    | Float f -> Printf.sprintf "%f" f)
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

let interpret_bin_op (op: bin_op) (n1: number) (n2: number) : expr =
  match op, n1, n2 with
  | Add, (Int a), (Int b) -> Number (Int (a + b))
  | Add, (Float a), (Int b) -> Number (Float (a +. (float_of_int b)))
  | Add, (Int a), (Float b) -> Number (Float ((float_of_int a) +. b))
  | Add, (Float a), (Float b) -> Number (Float (a +. b))
  | Subtract, (Int a), (Int b) -> Number (Int (a - b))
  | Subtract, (Float a), (Int b) -> Number (Float (a -. (float_of_int b)))
  | Subtract, (Int a), (Float b) -> Number (Float ((float_of_int a) -. b))
  | Subtract, (Float a), (Float b) -> Number (Float (a -. b))
  | Multiply, (Int a), (Int b) -> Number (Int (a * b))
  | Multiply, (Float a), (Int b) -> Number (Float (a *. (float_of_int b)))
  | Multiply, (Int a), (Float b) -> Number (Float ((float_of_int a) *. b))
  | Multiply, (Float a), (Float b) -> Number (Float (a *. b))
  | Divide, (Int a), (Int b) -> Number (Float ((float_of_int a) /. (float_of_int b)))
  | Divide, (Float a), (Int b) -> Number (Float (a /. (float_of_int b)))
  | Divide, (Int a), (Float b) -> Number (Float ((float_of_int a) /. b))
  | Divide, (Float a), (Float b) -> Number (Float (a /. b))

(** [interpret expr] interprets and reduce the intermediate AST [expr] into a result AST. *)
let rec interpret (e: expr) : expr =
  match e with
  | Null | Bool _ | String _ | Number _ | Array _ | Object _ -> e
  | BinOp (op, e1, e2) ->
    match (interpret e1, interpret e2) with
    | (Number v1), (Number v2) -> interpret_bin_op op v1 v2
    | _ -> failwith "invalid binary operation"

let run (s: string) : expr =
  let ast = parse s in
  let evaluated_ast = interpret ast in
  evaluated_ast
