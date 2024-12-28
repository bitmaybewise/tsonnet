{
  open Lexing
  open Parser
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let int = '-'? digit+

rule read =
  parse
  | white { read lexbuf }
  | newline { new_line lexbuf; read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
