open Result
open Syntax_sugar

(** [parse s] parses [s] into an AST. *)
let parse (filename: string) =
  let input = open_in filename in
  let lexbuf = Lexing.from_channel input in
  Lexing.set_filename lexbuf filename;
  let result =
    try ok (Parser.prog Lexer.read lexbuf)
    with
    | Lexer.SyntaxError err -> (Error.trace err (Ast.pos_from_lexbuf lexbuf)) >>= error
    | Parser.Error -> (Error.trace "Invalid syntax" (Ast.pos_from_lexbuf lexbuf)) >>= error
    | Failure err -> Error.trace ("Invalid token error: " ^ err) (Ast.pos_from_lexbuf lexbuf) >>= error
  in
  close_in input;
  result

let run ?(skip_typecheck = false) (filename: string) : (string, string) result =
  if skip_typecheck then
    prerr_endline "Warning: Type checking is skipped. This is not recommended as it may lead to runtime errors.\n";
  parse filename
    >>= (if skip_typecheck then ok else Type.check)
    >>= Interpreter.eval
    >>= Json.expr_to_string
