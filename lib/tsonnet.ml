open Result
open Syntax_sugar

module Config = Config

(** [parse s] parses [s] into an AST. *)
let parse (filename: string) =
  let input = open_in filename in
  let lexbuf = Lexing.from_channel input in
  Lexing.set_filename lexbuf filename;
  let result =
    try ok (Parser.prog Lexer.read lexbuf)
    with
    | Lexer.SyntaxError err -> (Error.trace err (Ast.pos_from_lexbuf lexbuf)) >>= error
    | Parser.Error -> (Error.trace Error.Msg.parse_error (Ast.pos_from_lexbuf lexbuf)) >>= error
    | Failure err -> Error.trace (Error.Msg.parse_invalid_token err) (Ast.pos_from_lexbuf lexbuf) >>= error
  in
  close_in input;
  result

let run (config : Config.t) (filename: string) : (string, string) result =
  parse filename
    >>= Ast.debug config
    >>= Type.check config
    >>= Interpreter.eval
    >>= Json.expr_to_string
