open Lexing
open Parser

exception SyntaxError of string

val read : lexbuf -> token
val read_string : Buffer.t -> lexbuf -> token
