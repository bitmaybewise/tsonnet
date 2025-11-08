let usage_msg = "tsonnet <file1> [<file2>] ..."
let input_files = ref []
let skip_typecheck = ref false
let debug_ast = ref false
let anonymous_fun filename = input_files := filename :: !input_files
let spec_list = [
  ("--skip-typecheck", Arg.Set skip_typecheck, "Skip type checking step");
  ("--debug-ast", Arg.Set debug_ast, "Print abstract syntax tree");
]

let run_parser filename =
  let open Tsonnet in
  let config = Config.make ~skip_typecheck:!skip_typecheck ~debug_ast:!debug_ast () in
  match run config filename with
  | Ok stringified_json -> print_endline stringified_json
  | Error err -> prerr_endline err; exit 1


let () =
  Arg.parse spec_list anonymous_fun usage_msg;
  List.iter run_parser !input_files
