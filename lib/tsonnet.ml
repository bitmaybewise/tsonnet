open Ast
open Result

let (let*) = Result.bind
let (>>=) = Result.bind

(** [parse s] parses [s] into an AST. *)
let parse (filename: string) : ((expr * Lexing.lexbuf), string) result  =
  let input = open_in filename in
  let lexbuf = Lexing.from_channel input in
  Lexing.set_filename lexbuf filename;
  let result =
    try ok (Parser.prog Lexer.read lexbuf, lexbuf)
    with | Lexer.SyntaxError err -> (Error.trace err (Ast.pos_from_lexbuf lexbuf)) >>= error
  in
  close_in input;
  result

let interpret_arith_op (op: bin_op) (n1: number) (n2: number) : Ast.value =
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
  let value =
    match e1.value, e2.value with
    | String s1, String s2 -> ok (String (s1^s2))
    | String s1, val2 ->
      let* s2 = Json.expr_to_string {e2 with value = val2} in
      ok (String (s1^s2))
    | val1, String s2 ->
      let* s1 = Json.expr_to_string {e1 with value = val1} in
      ok (String (s1^s2))
    | _ -> error "invalid string concatenation operation"
    in Result.map (fun v -> { e1 with value = v }) value

let interpret_unary_op (op: unary_op) (evaluated_expr: expr)  =
    let* new_value = (match op, evaluated_expr.value with
                    | Plus, number -> ok number
                    | Minus, Number (Int i) -> ok (Number (Int (-i)))
                    | Minus, Number (Float f) -> ok (Number (Float (-. f)))
                    | Not, (Bool b) -> ok (Bool (not b))
                    | BitwiseNot, Number (Int i) -> ok (Number (Int (lnot i)))
                    | _ -> error "invalid unary operation")
    in ok { evaluated_expr with value = new_value }

(** [interpret expr] interprets and reduce the intermediate AST [expr] into a result AST. *)
let rec interpret (expr, lexbuf: expr * Lexing.lexbuf) : (expr, string) result =
  match expr.value with
  | Null | Bool _ | String _ | Number _ | Array _ | Object _ | Ident _ -> ok expr
  | BinOp (op, e1, e2) ->
    (let* e1' = interpret ({expr with value = e1}, lexbuf) in
    let* e2' = interpret ({expr with value = e2}, lexbuf) in
    match op, e1'.value, e2'.value with
    | Add, (String _ as v1), (_ as v2) | Add, (_ as v1), (String _ as v2) ->
      let expr1 = { expr with value = v1 }
      in let expr2 = { expr with value = v2 }
      in interpret_concat_op expr1 expr2
    | _, Number v1, Number v2 ->
      let value = interpret_arith_op op v1 v2
      in ok ({expr with value = value})
    | _ ->
      Error.trace "invalid binary operation" expr.position >>= error
    )
  | UnaryOp (op, value) ->
    interpret ({expr with value = value}, lexbuf) >>= interpret_unary_op op

let run (filename: string) : (string, string) result =
  parse filename >>= interpret >>= Json.expr_to_string
