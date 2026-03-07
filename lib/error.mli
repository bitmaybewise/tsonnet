module Msg : sig
  (* Parameters configuration *)
  val warn_skip_typecheck : string

  (* Scope-related messages *)
  val self_out_of_scope : string
  val no_toplevel_object : string
  val var_not_found : string -> string

  (* Shared operation messages *)
  val invalid_binary_op : string
  val invalid_unary_op : string
  val must_be_object : string

  (* Parser messages *)
  val parse_error : string
  val parse_invalid_token : string -> string

  (* Type checker messages *)
  val type_cyclic_reference : string -> string
  val type_non_indexable_value : string -> string
  val type_expected_integer_index : string -> string
  val type_invalid_expr : string -> string
  val type_non_indexable_type : string -> string
  val type_non_indexable_field : string -> string
  val type_invalid_lookup_key : string -> string

  (* Interpreter messages *)
  val interp_division_by_zero : string
  val interp_invalid_concat : string
  val interp_invalid_lookup : string
  val interp_cannot_interpret : string -> string

  (* Other messages *)
  val value_not_represetable_as_json : string -> string
end

val trace : string -> Ast.position -> (string, string) result
val error_at : Ast.position -> string -> ('a, string) result
