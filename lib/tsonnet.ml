open Ast
open Result

let (let*) = Result.bind
let (>>=) = Result.bind

(** [parse s] parses [s] into an AST. *)
let parse (s: string)  =
  let lexbuf = Lexing.from_string s in
  try ok (Parser.prog Lexer.read lexbuf)
  with | Lexer.SyntaxError err_msg -> error err_msg

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
let rec interpret (e: expr) : (expr, string) result =
  match e with
  | Null | Bool _ | String _ | Number _ | Array _ | Object _ -> ok e
  | BinOp (Add, String a, String b) -> ok (String (a^b))
  | BinOp (op, e1, e2) ->
    let* e1' = interpret e1 in
    let* e2' = interpret e2 in
    match e1', e2' with
    | String a, String b -> ok (String (a^b))
    | Number v1, Number v2 -> ok (interpret_bin_op op v1 v2)
    | _ -> error "invalid binary operation"

let run (s: string) : (string, string) result =
  parse s >>= interpret >>= Json.expr_to_string
