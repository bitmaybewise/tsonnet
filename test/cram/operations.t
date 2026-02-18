  $ tsonnet ../../samples/operations/binary.jsonnet
  42.3

  $ tsonnet ../../samples/operations/unary.jsonnet
  -666

  $ tsonnet ../../samples/operations/precedence.jsonnet
  2.119565217391304

  $ tsonnet ../../samples/operations/not_true.jsonnet
  false

  $ tsonnet ../../samples/operations/not_false.jsonnet
  true

  $ tsonnet ../../samples/operations/bitwise_not.jsonnet
  -2

  $ tsonnet ../../samples/operations/modulo.jsonnet
  1

  $ tsonnet ../../samples/operations/equality.jsonnet
  {
    "dif_array_array": false,
    "dif_complex_obj_complex_obj": false,
    "dif_number_number": false,
    "dif_number_str": false,
    "dif_obj_obj": false,
    "dif_str_str": false,
    "eq_array_array": true,
    "eq_complex_obj_complex_obj": true,
    "eq_number_number": true,
    "eq_obj_obj": true,
    "eq_str_str": true
  }

  $ tsonnet ../../samples/operations/bitwise.jsonnet
  { "and": 1, "or": 3, "xor": 2 }

  $ tsonnet ../../samples/operations/logical.jsonnet
  { "and_false": false, "and_true": true, "or_false": false, "or_true": true }

  $ tsonnet ../../samples/operations/in.jsonnet
  {
    "has_field": true,
    "has_field_bar": true,
    "has_no_field": false,
    "has_no_field_bar": false
  }

  $ tsonnet ../../samples/operations/shift_left.jsonnet
  2

  $ tsonnet ../../samples/operations/shift_right.jsonnet
  2
