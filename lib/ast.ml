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

type position = {
  startpos: Lexing.position;
  endpos: Lexing.position;
}

type expr =
  | Null of position
  | Number of position * number
  | Bool of position * bool
  | String of position * string
  | Ident of position * string
  | Array of position * expr list
  | Object of position * (string * expr) list
  | BinOp of position * bin_op * expr * expr
  | UnaryOp of position * unary_op * expr
  | Local of position * (string * expr) list
  | Unit
  | Seq of expr list
  | IndexedExpr of position * string * expr

let dummy_pos = {
  startpos = Lexing.dummy_pos;
  endpos = Lexing.dummy_pos;
}

let dummy_expr = Unit

let pos_from_lexbuf (lexbuf : Lexing.lexbuf) : position =
  { startpos = lexbuf.lex_curr_p;
    endpos = lexbuf.lex_curr_p;
  }

let string_of_type = function
  | Null _ -> "Null"
  | Number (_, number) ->
    (match number with
    | Int _ -> "Int"
    | Float _ -> "Float")
  | Bool _ -> "Bool"
  | String _ -> "String"
  | Ident _ -> "Identity"
  | Array _ -> "Array"
  | Object _ -> "Object"
  | BinOp (_, bin_op, _, _) ->
    let prefix = "Binary Operation" in
    let bin_op = match bin_op with
    | Add -> "+"
    | Subtract -> "-"
    | Multiply -> "*"
    | Divide -> "/"
    in prefix ^ " " ^ bin_op
  | UnaryOp (_, unary_op, _) ->
    let prefix = "Unary Operation" in
    let unary_op = match unary_op with
    | Plus -> "+"
    | Minus -> "-"
    | Not -> "!"
    | BitwiseNot -> "~"
    in prefix ^ " " ^ unary_op
  | Local _ -> "Local"
  | Unit -> "()"
  | Seq _ -> "Sequence"
  | IndexedExpr _ -> "Indexed Expression"
