type t = {
  skip_typecheck : bool;
  debug_ast : bool;
}

let default = {
  skip_typecheck = false;
  debug_ast = false;
}

let make ?(skip_typecheck = false) ?(debug_ast = false) () = {
  skip_typecheck;
  debug_ast;
}
