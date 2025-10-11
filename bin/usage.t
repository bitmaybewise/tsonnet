Using the Tsonnet program:

  $ tsonnet ../samples/literals/int.jsonnet
  42

  $ tsonnet ../samples/literals/string.jsonnet
  "Hello, world! Here's \"Tsonnet\"."

  $ tsonnet ../samples/literals/object.jsonnet
  {
    "array_attr": [ 1, false, {} ],
    "float_attr": 4.2,
    "int_attr": 1,
    "null_attr": null,
    "obj_attr": { "a": true, "b": false, "c": { "d": [ 42 ] } },
    "string_attr": "Hello, world!"
  }

  $ tsonnet ../samples/binary_operations.jsonnet
  42.3
