open Ast
open Result
open Syntax_sugar

(** [parse s] parses [s] into an AST. *)
let parse (filename: string) =
  let input = open_in filename in
  let lexbuf = Lexing.from_channel input in
  Lexing.set_filename lexbuf filename;
  let result =
    try ok (Parser.prog Lexer.read lexbuf)
    with
    | Lexer.SyntaxError err -> (Error.trace err (Ast.pos_from_lexbuf lexbuf)) >>= error
    | Parser.Error -> (Error.trace "Invalid syntax" (Ast.pos_from_lexbuf lexbuf)) >>= error
  in
  close_in input;
  result

let interpret_arith_op (op: bin_op) (n1: number) (n2: number) =
  match op, n1, n2 with
  | Add, (Int a), (Int b) -> Int (a + b)
  | Add, (Float a), (Int b) -> Float (a +. (float_of_int b))
  | Add, (Int a), (Float b) -> Float ((float_of_int a) +. b)
  | Add, (Float a), (Float b) -> Float (a +. b)
  | Subtract, (Int a), (Int b) -> Int (a - b)
  | Subtract, (Float a), (Int b) -> Float (a -. (float_of_int b))
  | Subtract, (Int a), (Float b) -> Float ((float_of_int a) -. b)
  | Subtract, (Float a), (Float b) -> Float (a -. b)
  | Multiply, (Int a), (Int b) -> Int (a * b)
  | Multiply, (Float a), (Int b) -> Float (a *. (float_of_int b))
  | Multiply, (Int a), (Float b) -> Float ((float_of_int a) *. b)
  | Multiply, (Float a), (Float b) -> Float (a *. b)
  | Divide, (Int a), (Int b) -> Float ((float_of_int a) /. (float_of_int b))
  | Divide, (Float a), (Int b) -> Float (a /. (float_of_int b))
  | Divide, (Int a), (Float b) -> Float ((float_of_int a) /. b)
  | Divide, (Float a), (Float b) -> Float (a /. b)

let interpret_concat_op (e1 : expr) (e2 : expr) : (expr, string) result =
  match e1, e2 with
  | String (_, s1), String (_, s2) ->
    ok (String (dummy_pos, s1^s2))
  | String (_, s1), val2 ->
    let* s2 = Json.expr_to_string val2 in ok (String (dummy_pos, s1^s2))
  | val1, String (_, s2) ->
    let* s1 = Json.expr_to_string val1 in ok (String (dummy_pos, s1^s2))
  | _ ->
    error "Invalid string concatenation operation"

let interpret_unary_op (op: unary_op) (evaluated_expr: expr) =
  match op, evaluated_expr with
  | Plus, number -> ok number
  | Minus, Number (pos, Int i) -> ok (Number (pos, Int (-i)))
  | Minus, Number (pos, Float f) -> ok (Number (pos, Float (-. f)))
  | Not, (Bool (pos, b)) -> ok (Bool (pos, not b))
  | BitwiseNot, Number (pos, Int i) -> ok (Number (pos, Int (lnot i)))
  | _ -> error "Invalid unary operation"

(** [interpret expr] interprets and reduce the intermediate AST [expr] into a result AST. *)
let rec interpret env expr =
  match expr with
  | Null _ | Bool _ | String _ | Number _ | Object _ -> ok (env, expr)
  | Array (pos, exprs) ->
    (let rec eval' env' exprs' =
      match exprs' with
      | [] -> ok (env', [])
      | e :: exprs ->
        (let* (env1, expr') = interpret env' e in
        let* (env2, rest) = eval' env1 exprs in
        ok (env2, expr' :: rest))
    in eval' env exprs >>= fun (env3, exprs') -> ok (env3, Array (pos, exprs'))
    )
  | Ident (pos, varname) ->
    Env.find_var varname env
      ~succ:(fun env' expr -> interpret env' expr)
      ~err:(fun err_msg -> Error.trace err_msg pos >>= error)
  | BinOp (pos, op, e1, e2) ->
    (let* (env1, e1') = interpret env e1 in
    let* (env2, e2') = interpret env1 e2 in
    match op, e1', e2' with
    | Add, (String _ as v1), (_ as v2) | Add, (_ as v1), (String _ as v2) ->
      let* expr' = interpret_concat_op v1 v2 in
      ok (env, expr')
    | _, Number (pos, v1), Number (_, v2) ->
      ok (env2, Number (pos, interpret_arith_op op v1 v2))
    | _ ->
      Error.trace "Invalid binary operation" pos >>= error
    )
  | UnaryOp (pos, op, expr) ->
    (let* (env', expr') = interpret env expr in
    Result.fold (interpret_unary_op op expr')
      ~ok:(fun expr' -> ok (env', expr'))
      ~error:(fun errmsg ->  Error.trace errmsg pos >>= error)
    )
  | Local (_, vars) ->
    let acc_fun env (varname, expr) = Env.Map.add varname expr env in
    let env' = List.fold_left acc_fun env vars
    in ok (env', Unit)
  | Unit -> ok (env, Unit)
  | Seq exprs ->
    (match exprs with
    | [] -> ok (env, Unit)
    | [expr] -> interpret env expr
    | (expr :: exprs) -> interpret env expr >>= fun (env', _) -> interpret env' (Seq exprs))
  | IndexedExpr (pos, varname, index_expr) ->
    Env.find_var varname env
      ~succ:(fun env' expr ->
      match expr with
      | Array (_, exprs) ->
        let* (env', idx_expr') = interpret env' index_expr in
        (match idx_expr' with
        | Number (_, Int i)->
          (let len = List.length exprs in
          if i >= 0 && i < len
          then ok (env', List.nth exprs i)
          else Error.trace ("Index out of bounds. Trying to access index " ^ string_of_int i ^ " but \"" ^ varname ^ "\" length is " ^ string_of_int len) pos >>= error)
        | expr' -> Error.trace ("Expected Integer index, got " ^ Ast.string_of_type expr') pos >>= error
        )
      | evaluated_expr -> Error.trace ("Expected \"Array\", found \"" ^ string_of_type evaluated_expr ^ "\"") pos >>= error
      )
      ~err:(fun err_msg -> Error.trace err_msg pos >>= error)

let run (filename: string) : (string, string) result =
  let env = Env.Map.empty in
  parse filename >>= interpret env >>= fun (_env, expr) -> Json.expr_to_string expr
