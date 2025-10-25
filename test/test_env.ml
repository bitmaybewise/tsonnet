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

let gen_position = QCheck.Gen.return dummy_pos

let rec gen_expr_sized n =
  if n <= 0
    then
      QCheck.Gen.oneofl [Unit]
    else
      let pos_gen = gen_position in
      QCheck.Gen.frequency [
        (1, QCheck.Gen.return Unit);
        (1, QCheck.Gen.map (fun pos -> Null pos) pos_gen);
        (1, QCheck.Gen.map2 (fun pos n -> Number (pos, n)) pos_gen (gen_number));
        (1, QCheck.Gen.map2 (fun pos b -> Bool (pos, b)) pos_gen QCheck.Gen.bool);
        (1, QCheck.Gen.map2 (fun pos s -> String (pos, s)) pos_gen QCheck.Gen.string);
        (1, QCheck.Gen.map2 (fun pos s -> Ident (pos, s)) pos_gen QCheck.Gen.string);
        (2, QCheck.Gen.map2
          (fun pos exprs -> Array (pos, exprs))
          pos_gen
          (QCheck.Gen.list_size (QCheck.Gen.int_range 0 3) (gen_expr_sized (n-1)))
        );
        (2, QCheck.Gen.map2
          (fun pos entries -> ParsedObject (pos, entries))
          pos_gen
          (QCheck.Gen.list_size
            (QCheck.Gen.int_range 0 3)
            (QCheck.Gen.oneof [
              (QCheck.Gen.map
                (fun expr -> ObjectExpr expr)
                (gen_expr_sized (n-1))
              );
               (QCheck.Gen.map2
                (fun field expr -> ObjectField (field, expr))
                 QCheck.Gen.string
                 (gen_expr_sized (n-1))
               )
            ]
            )
          )
        );
        (1, QCheck.Gen.map4
          (fun pos op e1 e2 -> BinOp (pos, op, e1, e2))
          pos_gen
          gen_bin_op
          (gen_expr_sized (n-1)) (gen_expr_sized (n-1))
        );
        (1, QCheck.Gen.map3
          (fun pos op e1 -> UnaryOp (pos, op, e1))
          pos_gen
          gen_unary_op
          (gen_expr_sized (n-1))
        );
        (2, QCheck.Gen.map2
          (fun pos exprs -> Local (pos, exprs))
          pos_gen
          (QCheck.Gen.list_size (QCheck.Gen.int_range 0 3)
            (QCheck.Gen.pair QCheck.Gen.string (gen_expr_sized (n-1)))
          )
        );
        (2, QCheck.Gen.map
          (fun exprs -> Seq exprs)
          (QCheck.Gen.list_size (QCheck.Gen.int_range 0 3) (gen_expr_sized (n-1)))
        );
        (1, QCheck.Gen.map3
          (fun pos varname e1 -> IndexedExpr (pos, varname, e1))
          pos_gen
          QCheck.Gen.string
          (gen_expr_sized (n-1))
        );
      ]

let gen_expr = gen_expr_sized 5
let arbitrary_expr = QCheck.make gen_expr

let test_lookup =
  let local_env = ref Env.empty in
  QCheck.Test.make
    ~count:1000
    ~name:"Variable lookup successfully retrieves it from the environment"
    QCheck.(pair string arbitrary_expr)
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
