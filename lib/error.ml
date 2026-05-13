open Ast
open Result
open Syntax_sugar

module Msg = struct
  (* Parameters configuration *)
  let warn_skip_typecheck = "Warning: Type checking is skipped. This is not recommended as it may lead to runtime errors.\n"


  (* Shared operation messages *)
  let self_out_of_scope = "Can't use self outside of an object"
  let no_toplevel_object = "No top-level object found"
  let var_not_found varname = varname ^ " not found"
  let invalid_binary_op = "Invalid binary operation"
  let invalid_unary_op = "Invalid unary operation"
  let must_be_object = "Must be an object"

  (* Parser messages *)
  let parse_error = "Parsing error. Invalid syntax:"
  let parse_invalid_token err = "Invalid token error: " ^ err

  (* Type checker messages *)
  let type_cyclic_reference varname = "Cyclic reference found for " ^ varname
  let type_unused_variable varname = "Unused variable " ^ varname
  let type_non_indexable_value ty = ty ^ " is a non indexable value"
  let type_expected_integer_index ty = "Expected Integer index, got " ^ ty
  let type_invalid_expr expr = "Invalid type " ^ expr
  let type_non_indexable_type ty = ty ^ " is a non-indexable type"
  let type_non_indexable_field field = field ^ " is a non-indexable value"
  let type_invalid_lookup_key expr = "Invalid object lookup key: " ^ expr
  let type_mismatch ~expected ~got =
    Printf.sprintf "Expected type %s, got %s" expected got
  let type_conditional_branches_mismatch ~then_type ~else_type =
    Printf.sprintf
      "Conditional branches have different types: then branch returns %s, else branch returns %s"
      then_type else_type

  (* Interpreter messages *)
  let interp_division_by_zero = "Division by zero"
  let interp_invalid_concat = "Invalid concatenation operation"
  let interp_invalid_lookup = "Invalid object lookup"
  let interp_cannot_interpret expr = Printf.sprintf "Expression %s cannot be interpreted" expr

  (* Others messages *)
  let value_not_represetable_as_json value = "value type not representable as JSON: " ^ value
  let wrong_number_of_params expected got =
    Printf.sprintf "Expected %d argument(s), got %d" expected got
end

let enumerate_error_lines filename position ~highlight_error =
  let channel = open_in filename in
  try
    let eaten_chars = ref 0 in

    let rec read_lines acc line_num =
      try
        let line = input_line channel in
        let numbered_line = Printf.sprintf "%d: %s" line_num line in

        let is_error_line = line_num >= position.startpos.pos_lnum && line_num <= position.endpos.pos_lnum in
        let eat_more_chars = position.endpos.pos_cnum > !eaten_chars in

        if is_error_line && eat_more_chars then
          read_lines (highlight_error line_num line position eaten_chars :: numbered_line :: acc) (line_num + 1)
        else
          read_lines acc (line_num + 1)
      with End_of_file -> List.rev acc
    in
    let numbered_lines = read_lines [] 1 in
    close_in channel;
    ok (String.concat "\n" numbered_lines)
  with e ->
    close_in_noerr channel;
    error (Printexc.to_string e)

let plot_caret line_num line pos (eaten_chars: int ref) =
  let line_length = String.length line in
  let left_padding = String.length (string_of_int line_num) + 2 in (* width of line number + colon + space, e.g.: "%d: "... *)
  let buf_size = left_padding + line_length in
  let buffer = Buffer.create buf_size in

  let start' = min pos.startpos.pos_cnum !eaten_chars in
  let end' = min pos.endpos.pos_cnum (line_length-1) in

  (* Fill with spaces for the left padding (line number area) *)
  for _ = 1 to left_padding do
    Buffer.add_char buffer ' ';
    incr eaten_chars;
  done;
  (* Add spaces up to the starting position *)
  for _ = 1 to start' do
    Buffer.add_char buffer ' ';
    incr eaten_chars;
  done;
  (* Add caret to highlight expr between start and end columns *)
  for _ = start' to end' do
    Buffer.add_char buffer '^';
    incr eaten_chars;
  done;

  incr eaten_chars; (* +1 for newline *)

  let carets = Buffer.contents buffer in
  (Printf.sprintf "%*s" left_padding carets)

let trace_file_position err pos =
  let start_col = pos.startpos.pos_cnum - pos.startpos.pos_bol in
  Printf.sprintf "%s:%d:%d %s\n" pos.startpos.pos_fname pos.startpos.pos_lnum start_col err

let trace (err: string) (pos: position) : (string, string) result =
  bind
    (enumerate_error_lines pos.startpos.pos_fname pos ~highlight_error: plot_caret)
    (fun content -> ok (Printf.sprintf "%s\n%s" (trace_file_position err pos) content))

let error_at pos = fun msg ->
  let* error_msg = trace msg pos in
  error ("ERROR: " ^ error_msg)

let warn msg pos =
  let _warn msg = prerr_endline ("WARNING: " ^ msg ^ "\n---") in
  match trace msg pos with
  | Ok formatted -> _warn formatted
  | Error _ -> _warn msg
