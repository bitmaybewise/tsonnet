open Ast
open Result

let (let*) = Result.bind
let (>>=) = Result.bind

let enumerate_file_content filename =
  let channel = open_in filename in
  try
    let rec read_lines acc line_num =
      try
        let line = input_line channel in
        let numbered_line = Printf.sprintf "%d: %s" line_num line in
        read_lines (numbered_line :: acc) (line_num + 1)
      with End_of_file -> (List.rev acc, line_num)
    in
    let numbered_lines, line_num = read_lines [] 1 in
    close_in channel;
    ok (String.concat "\n" numbered_lines, line_num)
  with e ->
    close_in_noerr channel;
    error (Printexc.to_string e)

let plot_caret column_size =
  if column_size <= 0 then
    ""
  else
    let buffer = Buffer.create column_size in
    (* Fill with spaces except the last position *)
    for _ = 0 to column_size - 1 do
      Buffer.add_char buffer ' '
    done;
    (* Add caret at the end *)
    Buffer.add_char buffer '^';
    Buffer.contents buffer

let format_error (err: string) (lexbuf: Lexing.lexbuf) : (string, string) result =
  let* content, n = enumerate_file_content lexbuf.lex_curr_p.pos_fname in
  let pos_cnum = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol + 1 in
  let carot_padding = String.length (string_of_int n) + 1
  in ok (Printf.sprintf "%s:%d:%d %s\n\n%s\n  %*s"
    lexbuf.lex_curr_p.pos_fname
    lexbuf.lex_curr_p.pos_lnum
    pos_cnum
    err
    content
    carot_padding (plot_caret pos_cnum)
  )

(** [parse s] parses [s] into an AST. *)
let parse (filename: string) : ((expr * Lexing.lexbuf), string) result  =
  let input = open_in filename in
  let lexbuf = Lexing.from_channel input in
  Lexing.set_filename lexbuf filename;
  let result =
    try ok (Parser.prog Lexer.read lexbuf, lexbuf)
    with | Lexer.SyntaxError err -> (format_error err lexbuf) >>= error
  in
  close_in input;
  result

let interpret_arith_op (op: bin_op) (n1: number) (n2: number) start_pos : expr =
  let number_with_pos = fun (num: number) -> Number (start_pos, num) in
  match op, n1, n2 with
  | Add, (Int a), (Int b) -> number_with_pos (Int (a + b))
  | Add, (Float a), (Int b) -> number_with_pos (Float (a +. (float_of_int b)))
  | Add, (Int a), (Float b) -> number_with_pos (Float ((float_of_int a) +. b))
  | Add, (Float a), (Float b) -> number_with_pos (Float (a +. b))
  | Subtract, (Int a), (Int b) -> number_with_pos (Int (a - b))
  | Subtract, (Float a), (Int b) -> number_with_pos (Float (a -. (float_of_int b)))
  | Subtract, (Int a), (Float b) -> number_with_pos (Float ((float_of_int a) -. b))
  | Subtract, (Float a), (Float b) -> number_with_pos (Float (a -. b))
  | Multiply, (Int a), (Int b) -> number_with_pos (Int (a * b))
  | Multiply, (Float a), (Int b) -> number_with_pos (Float (a *. (float_of_int b)))
  | Multiply, (Int a), (Float b) -> number_with_pos (Float ((float_of_int a) *. b))
  | Multiply, (Float a), (Float b) -> number_with_pos (Float (a *. b))
  | Divide, (Int a), (Int b) -> number_with_pos (Float ((float_of_int a) /. (float_of_int b)))
  | Divide, (Float a), (Int b) -> number_with_pos (Float (a /. (float_of_int b)))
  | Divide, (Int a), (Float b) -> number_with_pos (Float ((float_of_int a) /. b))
  | Divide, (Float a), (Float b) -> number_with_pos (Float (a /. b))

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
    | Minus, Number (pos, Int i) -> ok (Number (pos, Int (-i)))
    | Minus, Number (pos, Float f) -> ok (Number (pos, Float (-. f)))
    | Not, (Bool (pos, b)) -> ok (Bool (pos, not b))
    | BitwiseNot, Number (pos, Int i) -> ok (Number (pos, Int (lnot i)))
    | _ -> error "invalid unary operation"

(** [interpret expr] interprets and reduce the intermediate AST [expr] into a result AST. *)
let rec interpret (e, lexbuf: expr * Lexing.lexbuf) : (expr, string) result =
  match e with
  | Null | Bool _ | String _ | Number _ | Array _ | Object _ | Ident _ -> ok e
  | BinOp (op, e1, e2) ->
    (let* e1' = interpret (e1, lexbuf) in
    let* e2' = interpret (e2, lexbuf) in
    match op, e1', e2' with
    | Add, (String _ as expr1), (_ as expr2) | Add, (_ as expr1), (String _ as expr2) ->
      interpret_concat_op expr1 expr2
    | _, Number (p1, v1), Number (_p2, v2) -> ok (interpret_arith_op op v1 v2 p1)
    | _, Number (_pos, _v1), Bool (pos, _) ->
      let buf = { lexbuf with lex_curr_p = pos }
      in format_error "invalid binary operation" buf >>= error
    | _ -> error "invalid binary operation")
  | UnaryOp (op, expr) -> interpret (expr, lexbuf) >>= interpret_unary_op op

let run (filename: string) : (string, string) result =
  parse filename >>= interpret >>= Json.expr_to_string
