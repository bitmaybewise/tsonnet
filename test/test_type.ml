open Alcotest
open Tsonnet__Ast
open Tsonnet__Type
open Tsonnet__Syntax_sugar

module Env = Tsonnet__Env

let test_translate_object_no_self () =
  let entries = [
    ObjectField ("x", Number (dummy_pos, Int 1));
    ObjectField ("y", Number (dummy_pos, Int 2));
  ] in
  match translate_object Env.empty dummy_pos entries with
  | Ok (env, _) ->
    let has_self = Env.find_opt "self" env in
    Alcotest.(check (option reject)) "Environment should not contain 'self'" None has_self
  | Error msg ->
    Alcotest.fail ("test_translate_object_no_self failed: " ^ msg)

let test_translate_object_no_dollar () =
  let entries = [
    ObjectField ("x", Number (dummy_pos, Int 1));
    ObjectField ("y", Number (dummy_pos, Int 2));
  ] in
  match translate_object Env.empty dummy_pos entries with
  | Ok (env, _) ->
    let has_dollar = Env.find_opt "$" env in
    Alcotest.(check (option reject)) "Environment should not contain '$'" None has_dollar
  | Error msg ->
    Alcotest.fail ("test_translate_object_no_dollar failed: " ^ msg)

let test_translate_object_empty_no_self () =
  match translate_object Env.empty dummy_pos [] with
  | Ok (env, _) ->
    let has_self = Env.find_opt "self" env in
    Alcotest.(check (option reject)) "Empty object environment should not contain 'self'" None has_self
  | Error msg ->
    Alcotest.fail ("test_translate_object_empty_no_self failed: " ^ msg)

let test_translate_object_empty_no_dollar () =
  match translate_object Env.empty dummy_pos [] with
  | Ok (env, _) ->
    let has_dollar = Env.find_opt "$" env in
    Alcotest.(check (option reject)) "Empty object environment should not contain '$'" None has_dollar
  | Error msg ->
    Alcotest.fail ("test_translate_object_empty_no_dollar failed: " ^ msg)

let test_translate_object_with_local_no_self () =
  let obj_entries = [
    ObjectExpr (Local (dummy_pos, [("a", Number (dummy_pos, Int 10))]));
    ObjectField ("x", Ident (dummy_pos, "a"));
  ] in
  match translate_object Env.empty dummy_pos obj_entries with
  | Ok (env, _) ->
    let has_self = Env.find_opt "self" env in
    Alcotest.(check (option reject)) "Object with local should not have 'self' in env" None has_self
  | Error msg ->
    Alcotest.fail ("test_translate_object_with_local_no_self failed: " ^ msg)

let test_translate_object_with_local_no_dollar () =
  let obj_entries = [
    ObjectExpr (Local (dummy_pos, [("a", Number (dummy_pos, Int 10))]));
    ObjectField ("x", Ident (dummy_pos, "a"));
  ] in
  match translate_object Env.empty dummy_pos obj_entries with
  | Ok (env, _) ->
    let has_dollar = Env.find_opt "$" env in
    Alcotest.(check (option reject)) "Object with local should not have '$' in env" None has_dollar
  | Error msg ->
    Alcotest.fail ("test_translate_object_with_local_no_dollar failed: " ^ msg)

let test_translate_object_preserves_toplevel_when_present () =
  let open Result in
  let run_test () =
    (* Create an outer object and get its $ reference from the environment *)
    let entries = [ObjectField ("x", Number (dummy_pos, Int 1))] in
    let* (outer_env, _) = translate_object Env.empty dummy_pos entries in

    (* The outer object's $ should not be in the environment (since it started from empty) *)
    let* () = match Env.find_opt "$" outer_env with
      | Some _ -> error "outer object unexpectedly has $"
      | None -> ok ()
    in

    (* Now create an environment with a top-level reference *)
    let* outer_id = Env.Id.generate () in
    let toplevel_ptr = TobjectPtr (outer_id, TobjectTopLevel, None) in
    let env_with_toplevel = Env.add_local "$" toplevel_ptr outer_env in

    (* Interpret an object that contains a nested inner object - this is where $ could be overridden *)
    let nested_obj = ParsedObject (dummy_pos, [ObjectField ("z", Number (dummy_pos, Int 3))]) in
    let inner_entries = [ObjectField ("y", nested_obj)] in
    let* (inner_env, _) = translate_object env_with_toplevel dummy_pos inner_entries in

    (* The inner object should preserve the $ reference *)
    match Env.find_opt "$" inner_env with
    | Some preserved_ref ->
      Alcotest.(check bool) "Inner object should preserve outer '$' reference" true (preserved_ref = toplevel_ptr);
      ok ()
    | None ->
      error "$ was not preserved"
  in
  match run_test () with
  | Ok () -> ()
  | Error msg -> Alcotest.fail ("test_translate_object_preserves_toplevel_when_present: " ^ msg)

let () =
  run "Type" [
    "translate_object", [
      test_case "object with fields returns no 'self' in env" `Quick test_translate_object_no_self;
      test_case "object with fields returns no '$' in env" `Quick test_translate_object_no_dollar;
      test_case "empty object returns no 'self' in env" `Quick test_translate_object_empty_no_self;
      test_case "empty object returns no '$' in env" `Quick test_translate_object_empty_no_dollar;
      test_case "object with local returns no 'self' in env" `Quick test_translate_object_with_local_no_self;
      test_case "object with local returns no '$' in env" `Quick test_translate_object_with_local_no_dollar;
      test_case "nested object preserves outer '$' reference" `Quick test_translate_object_preserves_toplevel_when_present;
    ];
  ]
