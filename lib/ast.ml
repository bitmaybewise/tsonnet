open Syntax_sugar

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

module Indexable = struct
  let length (e : expr) =
    match e with
    | Array (_, exprs) -> Result.ok (List.length exprs)
    | String (_, s) -> Result.ok (String.length s)
    | evaluated_expr -> Result.error (string_of_type evaluated_expr ^ " is a non indexable value")

  let nth (expr : expr) (index : int) =
    match expr with
    | Array (_, exprs) -> Result.ok (List.nth exprs index)
    | String (_, s) -> Result.ok (String (dummy_pos, String.make 1 (String.get s index)))
    | evaluated_expr -> Result.error (string_of_type evaluated_expr ^ " is a non indexable value")

  let get (index : expr) (expr : expr) : (expr, string) result =
    match index with
    | Number (_, Int i) ->
      let* len = length expr in
      if i >= 0 && i < len
        then nth expr i
        else
          Result.error ("Index out of bounds. Trying to access index " ^ string_of_int i ^ " but length is " ^ string_of_int len)
    | expr' ->
      Result.error ("Expected Integer index, got " ^ (string_of_type expr'))
end
