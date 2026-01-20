  $ tsonnet ../../samples/binary_operations.jsonnet
  42.3

  $ tsonnet ../../samples/unary_operations.jsonnet
  -666

  $ tsonnet ../../samples/operation_precedence.jsonnet
  2.119565217391304

  $ tsonnet ../../samples/not_true.jsonnet
  false

  $ tsonnet ../../samples/not_false.jsonnet
  true

  $ tsonnet ../../samples/bitwise.jsonnet
  -2

  $ tsonnet ../../samples/equality.jsonnet
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
