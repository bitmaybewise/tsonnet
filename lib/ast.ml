type bin_op =
  | Add
  | Subtract
  | Multiply
  | Divide

type unary_op =
  | Plus
  | Minus
  | Not
  | BitwiseNot

type number =
  | Int of int
  | Float of float

type value =
  | Number of number
  | Null
  | Bool of bool
  | String of string
  | Ident of string
  | Array of value list
  | Object of (string * value) list
  | BinOp of bin_op * value * value
  | UnaryOp of unary_op * value
  | Local of string * value
  | Unit

type position = {
  startpos: Lexing.position;
  endpos: Lexing.position;
}

let dummy_pos = {
  startpos = Lexing.dummy_pos;
  endpos = Lexing.dummy_pos;
}

type expr = {
  position: position;
  value: value;
}

let dummy_expr = {
  position=dummy_pos;
  value=Unit;
}

type prog =
  | Expr of expr
  | Sequence of expr list

let pos_from_lexbuf (lexbuf : Lexing.lexbuf) : position =
  { startpos = lexbuf.lex_curr_p;
    endpos = lexbuf.lex_curr_p;
  };
