open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s: string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let print = function
  | Int i -> Printf.printf "Int %d\n" i
  | Float f -> Printf.printf "Float %f\n" f
  | Null -> print_endline "Null"
  | Bool b -> Printf.printf "Bool %b\n" b
  | String s -> Printf.printf "\"%s\"\n" s
