open Syntax_sugar

type bin_op =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Equality
  | BitwiseOr
  | BitwiseAnd
  | BitwiseXor
  [@@deriving qcheck, show]

type unary_op =
  | Plus
  | Minus
  | Not
  | BitwiseNot
  [@@deriving qcheck, show]

type number =
  | Int of int
  | Float of float
  [@@deriving qcheck, show]

type position = {
  startpos: Lexing.position;
  endpos: Lexing.position;
}

let pp_position fmt pos =
  Format.fprintf fmt "%d:%d"
    pos.startpos.pos_lnum
    pos.endpos.pos_lnum

let show_position pos =
  Format.asprintf "%a" pp_position pos

let dummy_pos = {
  startpos = Lexing.dummy_pos;
  endpos = Lexing.dummy_pos;
}

module StringSet = struct
  type t = string
  let compare = String.compare
end

module ObjectFields = struct
  include Set.Make(StringSet)

  let pp fmt s =
    Format.fprintf fmt "{%s}"
      (String.concat ", " (to_list s))

  let show s =
    Format.asprintf "%a" pp s
end

type expr =
  (* terminal variants can appear in any stage, representable in the final JSON *)
  | Unit
  | Null of position
  | Number of position * number
  | Bool of position * bool
  | String of position * string
  | EvaluatedObject of position * (string * expr) list

  (* interpreted variants were parsed, interpreted and transformed,
    but are not terminal, and are waiting to be transformed into terminal variants *)
  | Ident of position * string
  | Array of position * expr list
  | ParsedObject of position * object_entry list
  | RuntimeObject of position * (expr Env.Map.t [@opaque]) * ObjectFields.t
  | ObjectPtr of (Env.env_id [@opaque]) * object_scope
  | ObjectFieldAccess of position * object_scope * expr list
  | BinOp of position * bin_op * expr * expr
  | UnaryOp of position * unary_op * expr
  | Local of position * (string * expr) list
  | Seq of expr list
  | IndexedExpr of position * string * expr

and object_entry =
  | ObjectField of string * expr
  | ObjectExpr of expr
and object_scope =
  | Self
  | TopLevel
  | ObjVarRef of string
[@@deriving show]

let dummy_expr = Unit

(* Semantic equality for expressions.

  Standard structural equality (=) includes position information, which means
  two expressions that are semantically identical but parsed from different
  locations would be considered unequal. This function compares expressions
  based solely on their semantic content.

  Usage: expr1 =~ expr2  or  semantic_equal expr1 expr2 *)
let rec semantic_equal evaluated_expr1 evaluated_expr2 =
  match (evaluated_expr1, evaluated_expr2) with
  | Null _, Null _ -> true
  | Number (_, n1), Number (_, n2) -> n1 = n2
  | Bool (_, b1), Bool (_, b2) -> b1 = b2
  | String (_, s1), String (_, s2) -> s1 = s2
  | Ident (_, id1), Ident (_, id2) -> id1 = id2
  | Array (_, items1), Array (_, items2) ->
    List.length items1 = List.length items2
    && List.for_all2 semantic_equal items1 items2
  | EvaluatedObject (_, fields1), EvaluatedObject (_, fields2) ->
    List.length fields1 = List.length fields2
    && List.for_all2
      (fun (k1, v1) (k2, v2) -> k1 = k2 && semantic_equal v1 v2)
      fields1
      fields2
  | ObjectPtr (id1, scope1), ObjectPtr (id2, scope2) ->
    id1 = id2 && scope1 = scope2
  | _, _ ->
    (* different types can't be compared, as well as operations,
      just representable values such as number, boolean, string, object, array *)
    false

let ( =~ ) = semantic_equal

let debug (config : Config.t) (ast : expr) : (expr, string) result =
  if config.debug_ast then
    prerr_endline (show_expr ast);
  Result.ok ast

let pos_from_lexbuf (lexbuf : Lexing.lexbuf) : position =
  { startpos = lexbuf.lex_curr_p;
    endpos = lexbuf.lex_curr_p;
  }

let string_of_object_scope = function
  | Self -> "self"
  | TopLevel -> "$"
  | ObjVarRef id -> id

let rec string_of_type = function
  | Null _ -> "Null"
  | Number (_, number) ->
    (match number with
    | Int _ -> "Int"
    | Float _ -> "Float")
  | Bool _ -> "Bool"
  | String (_, s) -> "\"" ^ s ^ "\""
  | Ident (_, id) -> Printf.sprintf "Ident(%s)" id
  | Array (_, items) ->
    Printf.sprintf "[%s]"
      (String.concat ", " (List.map string_of_type items))
  | ParsedObject (_, fields) ->
    Printf.sprintf "PlainObject{%s}"
      (String.concat ", " (List.map string_of_object_entry fields))
  | RuntimeObject (_, _env, fields) ->
    Printf.sprintf "obj{%s}" (String.concat ", " (ObjectFields.to_list fields))
  | BinOp (_, bin_op, _, _) ->
    let prefix = "Binary Operation" in
    let bin_op = match bin_op with
    | Add -> "+"
    | Subtract -> "-"
    | Multiply -> "*"
    | Divide -> "/"
    | Modulo -> "%"
    | Equality -> "=="
    | BitwiseOr -> "|"
    | BitwiseAnd -> "&"
    | BitwiseXor -> "^"
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
  | IndexedExpr (_, field, expr) ->
    Printf.sprintf "Indexed expr %s[%s]" field (string_of_type expr)
  | ObjectPtr (EnvId id, scope) ->
    Printf.sprintf "%s <%d>" (string_of_object_scope scope) id
  | ObjectFieldAccess (_, scope, field_chain) ->
    Printf.sprintf "%s.%s"
      (string_of_object_scope scope)
      (String.concat "." (List.map string_of_type field_chain))
  | expr ->
    (* using show_expr as a fallback *)
    show_expr expr

and string_of_object_entry = function
  | ObjectField (field, expr) -> field ^ ": " ^ string_of_type expr
  | ObjectExpr expr -> string_of_type expr


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
