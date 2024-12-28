{
  open Lexing
  open Parser
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let int = '-'? digit+
let frac = '.' digit*
let exp = ['e' 'E']['-' '+']? digit+
let float = digit* frac? exp?
let null = "null"

rule read =
  parse
  | white { read lexbuf }
  | newline { new_line lexbuf; read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | null { NULL }
  | eof { EOF }
