type bin_op =
  | Add
  | Subtract
  | Multiply
  | Divide

type unary_op =
  | Plus
  | Minus
  | Not

type number =
  | Int of int
  | Float of float

type expr =
  | Number of number
  | Null
  | Bool of bool
  | String of string
  | Ident of string
  | Array of expr list
  | Object of (string * expr) list
  | BinOp of bin_op * expr * expr
  | UnaryOp of unary_op * expr
