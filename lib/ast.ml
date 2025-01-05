type bin_op =
  | Add
  | Subtract
  | Multiply
  | Divide

type expr =
  | Int of int
  | Float of float
  | Null
  | Bool of bool
  | String of string
  | Array of expr list
  | Object of (string * expr) list
  | BinOp of bin_op * expr * expr
