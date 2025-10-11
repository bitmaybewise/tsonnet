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
  "Hello, world! Here's \"Tsonnet\"."

  $ tsonnet ../../samples/literals/string_single_quote.jsonnet
  "Hello, world! Here's \"Tsonnet\"."

  $ tsonnet ../../samples/literals/string_raw.jsonnet
  "Hi stranger, this is a\nmulti-line verbatim string,\nalso called raw-string or\nliteral string.\n"

  $ tsonnet ../../samples/literals/string_raw_single_line.jsonnet
  "Hello, stranger!"

  $ tsonnet ../../samples/literals/string_raw_single_line_single_quote.jsonnet
  "Hello, stranger!"

  $ tsonnet ../../samples/literals/array.jsonnet
  [ 1, 2.0, "hi", null ]

  $ tsonnet ../../samples/literals/object.jsonnet
  {
    "array_attr": [ 1, false, {} ],
    "float_attr": 4.2,
    "int_attr": 1,
    "null_attr": null,
    "obj_attr": { "a": true, "b": false, "c": { "d": [ 42 ] } },
    "string_attr": "Hello, world!"
  }
