let read_all_content filepath =
  let ic = open_in filepath in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content

let read_files_in_directory dir =
  let rec read_files_recursive dir =
    Sys.readdir dir
    |> Array.to_list
    |> List.fold_left (fun acc file ->
      let path = Filename.concat dir file in
      if Sys.is_directory path then
        acc @ (read_files_recursive path)
      else
        let content = read_all_content path in
        (path, content) :: acc
    ) []
  in
  read_files_recursive dir

let sample_files = read_files_in_directory "../samples"

let test_parse_sample_files () =
  List.iter (fun (filename, content) ->
    Alcotest.(check unit)
      (Printf.sprintf "Parsing %s" filename)
      ()
      (ignore (Tsonnet.parse content));
      ()
  ) sample_files

let () =
  Alcotest.run "Tsonnet Test Suite" [
    "Parsing sample files", [
      Alcotest.test_case "Parse all sample files successfully" `Quick test_parse_sample_files;
    ]
  ]
