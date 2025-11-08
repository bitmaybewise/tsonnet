type t = {
  skip_typecheck : bool;
  debug_ast : bool;
}

val default : t
val make : ?skip_typecheck:bool -> ?debug_ast:bool -> unit -> t
