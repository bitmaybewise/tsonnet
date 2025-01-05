let usage_msg = "tsonnet <file1> [<file2>] ..."
let input_files = ref []
let anonymous_fun filename = input_files := filename :: !input_files
let spec_list = []

let run_parser filename =
  let input_channel = open_in filename in
  let content = really_input_string input_channel (in_channel_length input_channel) in
  close_in input_channel;
  let expr = Tsonnet.run content in
  print_endline (Tsonnet.print expr)

let () =
  Arg.parse spec_list anonymous_fun usage_msg;
  List.iter run_parser !input_files
