open Tsonnet__Ast

module Env = Tsonnet__Env

let succ env expr = Result.ok (env, expr)
let err = Result.error

let test_undefined_variable () =
  let result = Result.get_error (Env.find_var "foo" Env.empty ~succ ~err) in
  let expected = Result.get_error (Result.error "Undefined variable: foo") in
  Alcotest.(check string) "returns error" expected result

let test_add_local_when_not_present_adds_new_variable () =
  let new_expr = Number (dummy_pos, Int 42) in
  let (env, returned_expr) = Env.add_local_when_not_present "$" new_expr Env.empty in
  let found_expr = Env.find_opt "$" env in
  Alcotest.(check bool) "adds new variable to environment" true (found_expr = Some new_expr);
  Alcotest.(check bool) "returns the new expression" true (returned_expr = new_expr)

let test_add_local_when_not_present_preserves_existing_variable () =
  let existing_expr = Number (dummy_pos, Int 1) in
  let new_expr = Number (dummy_pos, Int 2) in
  let env_with_existing = Env.add_local "$" existing_expr Env.empty in
  let (env, returned_expr) = Env.add_local_when_not_present "$" new_expr env_with_existing in
  let found_expr = Env.find_opt "$" env in
  Alcotest.(check bool) "preserves existing variable in environment" true (found_expr = Some existing_expr);
  Alcotest.(check bool) "returns the existing expression, not the new one" true (returned_expr = existing_expr)

let test_lookup =
  let local_env = ref Env.empty in
  QCheck.Test.make
    ~count:1000
    ~name:"Variable lookup successfully retrieves it from the environment"
    QCheck.(pair string Ast_gen.arbitrary_expr)
    (fun (varname, expr) ->
      let new_env = Env.Map.add varname expr !local_env in
      local_env := new_env;
      Env.find_var varname new_env ~succ ~err |> Result.is_ok
    )

let () =
  let open Alcotest in
  run "Env" [
    "find_var", [
      test_case "Undefined variable" `Quick test_undefined_variable;
      QCheck_alcotest.to_alcotest test_lookup;
    ];
    "add_local_when_not_present", [
      test_case "adds new variable when not present" `Quick test_add_local_when_not_present_adds_new_variable;
      test_case "preserves existing variable when present" `Quick test_add_local_when_not_present_preserves_existing_variable;
    ];
  ]
