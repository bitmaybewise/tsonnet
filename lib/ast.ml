type expr =
  | Int of int
  | Float of float
  | Null
  | Bool of bool
  | String of string
  | Array of expr list
  | Object of (string * expr) list
