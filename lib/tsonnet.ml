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
    | Lexer.SyntaxError err -> Error.error_at (Ast.pos_from_lexbuf lexbuf) err
    | Parser.Error -> Error.error_at (Ast.pos_from_lexbuf lexbuf) Error.Msg.parse_error
    | Failure err -> Error.error_at (Ast.pos_from_lexbuf lexbuf) (Error.Msg.parse_invalid_token err)
  in
  close_in input;
  result

let run (config : Config.t) (filename: string) : (string, string) result =
  parse filename
    >>= Ast.debug config
    >>= Type.check config
    >>= Interpreter.eval
    >>= Json.expr_to_string
