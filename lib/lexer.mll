{
  [@@@coverage exclude_file]
  open Lexing
  open Parser
  exception SyntaxError of string

  let string_not_terminated = SyntaxError ("String is not terminated")

  let illegal_string_char invalid = SyntaxError ("Illegal string character: " ^ invalid)

  let verbatim_string s =
    (String.split_on_char '\n' s)
    |> List.drop_while (fun line -> line = "")
    |> List.map String.trim
    |> String.concat "\n"
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let int = digit+
let frac = '.' digit*
let exp = ['e' 'E']['-' '+']? digit+
let float = digit+ '.' digit* exp? (* 123.456, 123.456e10 *)
          | digit* '.' digit+ exp? (* .456, .456e10 *)
          | digit+ exp             (* 123e10 *)
let null = "null"
let bool = "true" | "false"
let letter = ['a'-'z' 'A'-'Z']
let id = (letter | '_') (letter | digit | '_')*
let inline_comment = ("//" | "#") [^ '\n']* newline

rule read =
  parse
  | white { read lexbuf }
  | newline { new_line lexbuf; read lexbuf }
  | inline_comment { read lexbuf }
  | "/*" { block_comment lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | null { NULL }
  | bool { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
  | '"' { read_double_quoted_string (Buffer.create 16) lexbuf }
  | '\'' { read_single_quoted_string (Buffer.create 16) lexbuf }
  | "|||" { read_verbatim_string (Buffer.create 16) lexbuf }
  | "@\"" { read_single_line_verbatim_double_quoted_string (Buffer.create 16) lexbuf }
  | "@'" { read_single_line_verbatim_single_quoted_string (Buffer.create 16) lexbuf }
  | '[' { LEFT_SQR_BRACKET }
  | ']' { RIGHT_SQR_BRACKET }
  | '{' { LEFT_CURLY_BRACKET }
  | '}' { RIGHT_CURLY_BRACKET }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | ',' { COMMA }
  | ':' { COLON }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULTIPLY }
  | '/' { DIVIDE }
  | '!' { NOT }
  | '~' { BITWISE_NOT }
  | '=' { ASSIGN }
  | ';' { SEMICOLON }
  | "local" { LOCAL }
  | id { ID (Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
and read_double_quoted_string buf =
  parse
  | '\\' '"'  { Buffer.add_char buf '"'; read_double_quoted_string buf lexbuf }
  | '\\' '\''  { Buffer.add_char buf '\''; read_double_quoted_string buf lexbuf }
  | '\\' '/'  { Buffer.add_char buf '/'; read_double_quoted_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_double_quoted_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_double_quoted_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_double_quoted_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_double_quoted_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_double_quoted_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_double_quoted_string buf lexbuf }
  | '"' { STRING (Buffer.contents buf) }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_double_quoted_string buf lexbuf
    }
  | _ { raise (illegal_string_char (Lexing.lexeme lexbuf)) }
  | eof { raise string_not_terminated }
and read_single_quoted_string buf =
  parse
  | '\\' '"'  { Buffer.add_char buf '"'; read_single_quoted_string buf lexbuf }
  | '\\' '\''  { Buffer.add_char buf '\''; read_single_quoted_string buf lexbuf }
  | '\\' '/'  { Buffer.add_char buf '/'; read_single_quoted_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_single_quoted_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_single_quoted_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_single_quoted_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_single_quoted_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_single_quoted_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_single_quoted_string buf lexbuf }
  | '\'' { STRING (Buffer.contents buf) }
  | [^ '\'' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_single_quoted_string buf lexbuf
    }
  | _ { raise (illegal_string_char (Lexing.lexeme lexbuf)) }
  | eof { raise string_not_terminated }
and read_single_line_verbatim_single_quoted_string buf =
  parse
  | '\'' { STRING (verbatim_string (Buffer.contents buf)) }
  | _ as c  { Buffer.add_char buf c; read_single_line_verbatim_single_quoted_string buf lexbuf }
  | _ { raise (illegal_string_char (Lexing.lexeme lexbuf)) }
  | eof { raise string_not_terminated }
and read_single_line_verbatim_double_quoted_string buf =
  parse
  | '"' { STRING (verbatim_string (Buffer.contents buf)) }
  | _ as c  { Buffer.add_char buf c; read_single_line_verbatim_double_quoted_string buf lexbuf }
  | _ { raise (illegal_string_char (Lexing.lexeme lexbuf)) }
  | eof { raise string_not_terminated }
and read_verbatim_string buf =
  parse
  | "|||" { STRING (verbatim_string (Buffer.contents buf)) }
  | _ as c  { Buffer.add_char buf c; read_verbatim_string buf lexbuf }
  | _ { raise (illegal_string_char (Lexing.lexeme lexbuf)) }
  | eof { raise string_not_terminated }
and block_comment =
  parse
  | "*/" { read lexbuf }
  | newline { new_line lexbuf; block_comment lexbuf }
  | _ { block_comment lexbuf }
  | eof { raise (SyntaxError ("Unterminated block comment")) }
