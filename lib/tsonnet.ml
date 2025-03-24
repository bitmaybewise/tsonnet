open Ast
open Result

let (let*) = Result.bind
let (>>=) = Result.bind

let format_error err (lexbuf: Lexing.lexbuf) =
  Printf.sprintf "%s:%d:%d %s"
    lexbuf.lex_curr_p.pos_fname
    lexbuf.lex_curr_p.pos_lnum
    (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
    err

(** [parse s] parses [s] into an AST. *)
let parse (filename: string) : (expr, string) result  =
  let input = open_in filename in
  let lexbuf = Lexing.from_channel input in
  Lexing.set_filename lexbuf filename;
  let result =
    try ok (Parser.prog Lexer.read lexbuf)
    with | Lexer.SyntaxError err -> error (format_error err lexbuf)
  in
  close_in input;
  result

let interpret_arith_op (op: bin_op) (n1: number) (n2: number) : expr =
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

let interpret_concat_op (e1 : expr) (e2 : expr) : (expr, string) result =
  match e1, e2 with
  | String s1, String s2 -> ok (String (s1^s2))
  | String s1, expr2 ->
    let* s2 = Json.expr_to_string expr2 in
    ok (String (s1^s2))
  | expr1, String s2 ->
    let* s1 = Json.expr_to_string expr1 in
    ok (String (s1^s2))
  | _ -> error "invalid string concatenation operation"

let interpret_unary_op (op: unary_op) (evaluated_expr: expr)  =
    match op, evaluated_expr with
    | Plus, number -> ok number
    | Minus, Number (Int i) -> ok (Number (Int (-i)))
    | Minus, Number (Float f) -> ok (Number (Float (-. f)))
    | Not, (Bool b) -> ok (Bool (not b))
    | BitwiseNot, Number (Int i) -> ok (Number (Int (lnot i)))
    | _ -> error "invalid unary operation"

(** [interpret expr] interprets and reduce the intermediate AST [expr] into a result AST. *)
let rec interpret (e: expr) : (expr, string) result =
  match e with
  | Null | Bool _ | String _ | Number _ | Array _ | Object _ | Ident _ -> ok e
  | BinOp (op, e1, e2) ->
    (let* e1' = interpret e1 in
    let* e2' = interpret e2 in
    match op, e1', e2' with
    | Add, (String _ as expr1), (_ as expr2) | Add, (_ as expr1), (String _ as expr2) ->
      interpret_concat_op expr1 expr2
    | _, Number v1, Number v2 -> ok (interpret_arith_op op v1 v2)
    | _ -> error "invalid binary operation")
  | UnaryOp (op, expr) -> interpret expr >>= interpret_unary_op op

let run (filename: string) : (string, string) result =
  parse filename >>= interpret >>= Json.expr_to_string
