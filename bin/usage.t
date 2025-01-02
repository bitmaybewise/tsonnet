Using the Tsonnet program:

  $ tsonnet ../samples/literals/int.jsonnet
  42

  $ tsonnet ../samples/literals/string.jsonnet
  "Hello, world!"

  $ tsonnet ../samples/literals/object.jsonnet
  {"int_attr": 1, "float_attr": 4.200000, "string_attr": "Hello, world!", "null_attr": null, "array_attr": [1, false, {}], "obj_attr": {"a": true, "b": false, "c": {"d": [42]}}}
