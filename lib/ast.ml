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

type expr = {
  startpos: Lexing.position;
  value: value;
}
