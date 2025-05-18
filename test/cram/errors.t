  $ tsonnet ../../samples/errors/unterminated_block.jsonnet
  ../../samples/errors/unterminated_block.jsonnet:12:1 Unterminated block comment
  
  12: ?
      ^
  [1]

  $ tsonnet ../../samples/errors/malformed_string.jsonnet
  ../../samples/errors/malformed_string.jsonnet:1:22 String is not terminated
  
  1: "oops... no end quote
     ^^^^^^^^^^^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/errors/sum_int_to_boolean.jsonnet
  ../../samples/errors/sum_int_to_boolean.jsonnet:1:0 Invalid binary operation
  
  1: 42 + true
     ^^^^^^^^^
  [1]

  $ tsonnet ../../samples/errors/sum_int_to_boolean_multiline.jsonnet
  ../../samples/errors/sum_int_to_boolean_multiline.jsonnet:1:0 Invalid binary operation
  
  1: (1+1+1+1)+
     ^^^^^^^^^^
  2: 2+2+2+2+
     ^^^^^^^^
  3: 3+3+3+false+
     ^^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/errors/unscoped_local.jsonnet
  ../../samples/errors/unscoped_local.jsonnet:1:15 Invalid syntax
  
  1: local a = local b = 1;
     ^^^^^^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/errors/undefined_local.jsonnet
  ../../samples/errors/undefined_local.jsonnet:3:0 Undefined variable: c
  
  3: c
     ^
  [1]

  $ tsonnet ../../samples/errors/array_index_out_of_bounds.jsonnet
  ../../samples/errors/array_index_out_of_bounds.jsonnet:3:0 Index out of bounds. Trying to access index 4 but "list" length is 3
  
  3: list[index]
     ^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/errors/array_index_not_int.jsonnet
  ../../samples/errors/array_index_not_int.jsonnet:3:0 Expected Integer index, got String
  
  3: list[index]
     ^^^^^^^^^^^
  [1]

