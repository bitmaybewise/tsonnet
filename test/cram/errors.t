  $ tsonnet ../../samples/errors/malformed_string.jsonnet
  ../../samples/errors/malformed_string.jsonnet:1:22 String is not terminated
  
  1: "oops... no end quote
                         ^
  [1]

  $ tsonnet ../../samples/errors/sum_int_to_boolean.jsonnet
  ../../samples/errors/sum_int_to_boolean.jsonnet:1:0 invalid binary operation
  
  1: 42 + true
     ^
  [1]
