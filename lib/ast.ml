type bin_op =
  | Add
  | Subtract
  | Multiply
  | Divide

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
