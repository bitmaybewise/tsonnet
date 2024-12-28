open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s: string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let print ast = match ast with
  | Int i -> Printf.printf "Int %d\n" i
  | Null -> print_endline "null"
