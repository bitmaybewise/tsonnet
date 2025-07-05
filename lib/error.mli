val trace : string -> Ast.position -> (string, string) result
val error_at : Ast.position -> string -> ('a, string) result
