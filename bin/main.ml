let usage_msg = "tsonnet <file1> [<file2>] ..."
let input_files = ref []
let anonymous_fun filename = input_files := filename :: !input_files
let spec_list = []

let run_parser filename =
  match Tsonnet.run filename with
  | Ok stringified_json -> print_endline stringified_json
  | Error err -> prerr_endline err; exit 1


let () =
  Arg.parse spec_list anonymous_fun usage_msg;
  List.iter run_parser !input_files
