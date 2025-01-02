  $ tsonnet ../../samples/literals/int.jsonnet
  42

  $ tsonnet ../../samples/literals/float.jsonnet
  4.222222222222222

  $ tsonnet ../../samples/literals/negative_int.jsonnet
  -42

  $ tsonnet ../../samples/literals/negative_float.jsonnet
  -4.222222222222222

  $ tsonnet ../../samples/literals/true.jsonnet
  true

  $ tsonnet ../../samples/literals/false.jsonnet
  false

  $ tsonnet ../../samples/literals/null.jsonnet
  null

  $ tsonnet ../../samples/literals/string.jsonnet
  "Hello, world!"

  $ tsonnet ../../samples/literals/array.jsonnet
  [ 1, 2.0, "hi", null ]

  $ tsonnet ../../samples/literals/object.jsonnet
  {
    "int_attr": 1,
    "float_attr": 4.2,
    "string_attr": "Hello, world!",
    "null_attr": null,
    "array_attr": [ 1, false, {} ],
    "obj_attr": { "a": true, "b": false, "c": { "d": [ 42 ] } }
  }
