  $ tsonnet ../../samples/functions/positional_params.jsonnet
  6

  $ tsonnet ../../samples/functions/multiline.jsonnet
  [ 6, 7 ]

  $ tsonnet ../../samples/functions/default_args.jsonnet
  12

  $ tsonnet ../../samples/functions/closure.jsonnet
  25

  $ tsonnet ../../samples/functions/named_params.jsonnet
  5

  $ tsonnet ../../samples/functions/method.jsonnet
  4

  $ tsonnet ../../samples/functions/no_args.jsonnet
  {
    "applied": "world",
    "greet": "hello",
    "inline": 42,
    "method_call": 7,
    "obj": { "x": 1, "y": 2 }
  }
