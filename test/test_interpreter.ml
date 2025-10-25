open Alcotest
open Tsonnet__Ast
open Tsonnet__Interpreter

module Env = Tsonnet__Env

let test_interpret_object_no_self () =
  let entries = [
    ObjectField ("x", Number (dummy_pos, Int 1));
    ObjectField ("y", Number (dummy_pos, Int 2));
  ] in
  match interpret_object Env.empty (dummy_pos, entries) with
  | Ok (env, _) ->
    let has_self = Env.find_opt "self" env in
    check (option reject) "Environment should not contain 'self'" None has_self
  | Error msg ->
    fail ("test_interpret_object_no_self failed: " ^ msg)

let test_interpret_object_no_dollar () =
  let entries = [
    ObjectField ("x", Number (dummy_pos, Int 1));
    ObjectField ("y", Number (dummy_pos, Int 2));
  ] in
  match interpret_object Env.empty (dummy_pos, entries) with
  | Ok (env, _) ->
    let has_dollar = Env.find_opt "$" env in
    check (option reject) "Environment should not contain '$'" None has_dollar
  | Error msg ->
    fail ("test_interpret_object_no_dollar failed: " ^ msg)

let test_interpret_object_empty_no_self () =
  match interpret_object Env.empty (dummy_pos, []) with
  | Ok (env, _) ->
    let has_self = Env.find_opt "self" env in
    check (option reject) "Empty object environment should not contain 'self'" None has_self
  | Error msg ->
    fail ("test_interpret_object_empty_no_self failed: " ^ msg)

let test_interpret_object_empty_no_dollar () =
  match interpret_object Env.empty (dummy_pos, []) with
  | Ok (env, _) ->
    let has_dollar = Env.find_opt "$" env in
    check (option reject) "Empty object environment should not contain '$'" None has_dollar
  | Error msg ->
    fail ("test_interpret_object_empty_no_dollar failed: " ^ msg)

let test_interpret_object_with_local_no_self () =
  let obj_entries = [
    ObjectExpr (Local (dummy_pos, [("a", Number (dummy_pos, Int 10))]));
    ObjectField ("x", Ident (dummy_pos, "a"));
  ] in
  match interpret_object Env.empty (dummy_pos, obj_entries) with
  | Ok (env, _) ->
    let has_self = Env.find_opt "self" env in
    check (option reject) "Object with local should not have 'self' in env" None has_self
  | Error msg ->
    fail ("test_interpret_object_with_local_no_self failed: " ^ msg)

let test_interpret_object_with_local_no_dollar () =
  let obj_entries = [
    ObjectExpr (Local (dummy_pos, [("a", Number (dummy_pos, Int 10))]));
    ObjectField ("x", Ident (dummy_pos, "a"));
  ] in
  match interpret_object Env.empty (dummy_pos, obj_entries) with
  | Ok (env, _) ->
    let has_dollar = Env.find_opt "$" env in
    check (option reject) "Object with local should not have '$' in env" None has_dollar
  | Error msg ->
    fail ("test_interpret_object_with_local_no_dollar failed: " ^ msg)

let () =
  run "Interpreter" [
    "interpret_object", [
      test_case "object with fields returns no 'self' in env" `Quick test_interpret_object_no_self;
      test_case "object with fields returns no '$' in env" `Quick test_interpret_object_no_dollar;
      test_case "empty object returns no 'self' in env" `Quick test_interpret_object_empty_no_self;
      test_case "empty object returns no '$' in env" `Quick test_interpret_object_empty_no_dollar;
      test_case "object with local returns no 'self' in env" `Quick test_interpret_object_with_local_no_self;
      test_case "object with local returns no '$' in env" `Quick test_interpret_object_with_local_no_dollar;
    ];
  ]
