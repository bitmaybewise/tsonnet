Display help message with -help flag:

  $ tsonnet -help
  tsonnet <file1> [<file2>] ...
    --skip-typecheck Skip type checking step
    -help  Display this list of options
    --help  Display this list of options

Display help message with --help flag:

  $ tsonnet --help
  tsonnet <file1> [<file2>] ...
    --skip-typecheck Skip type checking step
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
