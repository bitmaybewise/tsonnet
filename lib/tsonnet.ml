open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s: string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec print = function
  | Int i -> Printf.sprintf "%d" i
  | Float f -> Printf.sprintf "%f" f
  | Null -> Printf.sprintf "null"
  | Bool b -> Printf.sprintf "%b" b
  | String s -> Printf.sprintf "\"%s\"" s
  | Array values -> Printf.sprintf "[%s]" (String.concat ", " (List.map print values))
