Display help message with -help flag:

  $ tsonnet -help
  tsonnet <file1> [<file2>] ...
    --skip-typecheck Skip type checking step
    --debug-ast Print abstract syntax tree
    -help  Display this list of options
    --help  Display this list of options

Display help message with --help flag:

  $ tsonnet --help
  tsonnet <file1> [<file2>] ...
    --skip-typecheck Skip type checking step
    --debug-ast Print abstract syntax tree
    -help  Display this list of options
    --help  Display this list of options

Testing the --skip-typecheck flag:

Run with type checking enabled (default behavior):

  $ tsonnet ../samples/literals/int.jsonnet
  42

Run with type checking skipped using long flag:

  $ tsonnet --skip-typecheck ../samples/literals/int.jsonnet
  Warning: Type checking is skipped. This is not recommended as it may lead to runtime errors.
  
  42

Testing the --debug-ast flag:

Run with AST debugging enabled:

  $ tsonnet --debug-ast ../samples/literals/object.jsonnet
  (Ast.ParsedObject (1:8,
     [(Ast.ObjectField ("int_attr", (Ast.Number (2:2, (Ast.Int 1)))));
       (Ast.ObjectField ("float_attr", (Ast.Number (3:3, (Ast.Float 4.2)))));
       (Ast.ObjectField ("string_attr", (Ast.String (4:4, "Hello, world!"))));
       (Ast.ObjectField ("null_attr", (Ast.Null 5:5)));
       (Ast.ObjectField ("array_attr",
          (Ast.Array (6:6,
             [(Ast.Number (6:6, (Ast.Int 1))); (Ast.Bool (6:6, false));
               (Ast.ParsedObject (6:6, []))]
             ))
          ));
       (Ast.ObjectField ("obj_attr",
          (Ast.ParsedObject (7:7,
             [(Ast.ObjectField ("a", (Ast.Bool (7:7, true))));
               (Ast.ObjectField ("b", (Ast.Bool (7:7, false))));
               (Ast.ObjectField ("c",
                  (Ast.ParsedObject (7:7,
                     [(Ast.ObjectField ("d",
                         (Ast.Array (7:7, [(Ast.Number (7:7, (Ast.Int 42)))]))
                         ))
                       ]
                     ))
                  ))
               ]
             ))
          ))
       ]
     ))
  {
    "array_attr": [ 1, false, {} ],
    "float_attr": 4.2,
    "int_attr": 1,
    "null_attr": null,
    "obj_attr": { "a": true, "b": false, "c": { "d": [ 42 ] } },
    "string_attr": "Hello, world!"
  }
