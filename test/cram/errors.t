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

  $ tsonnet ../../samples/errors/malformed_verbatim_string.jsonnet
  ../../samples/errors/malformed_verbatim_string.jsonnet:1:18 String is not terminated
  
  1: local s = |||
     ^^^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/errors/malformed_verbatim_string_single_line.jsonnet
  ../../samples/errors/malformed_verbatim_string_single_line.jsonnet:1:24 String is not terminated
  
  1: local s = @'hello...;
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
  ../../samples/errors/array_index_out_of_bounds.jsonnet:3:0 Index out of bounds. Trying to access index 4 but length is 3
  
  3: list[index]
     ^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/errors/array_index_not_int.jsonnet
  ../../samples/errors/array_index_not_int.jsonnet:3:0 Expected Integer index, got String
  
  3: list[index]
     ^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/errors/value_non_indexable.jsonnet
  ../../samples/errors/value_non_indexable.jsonnet:2:0 Int is a non indexable value
  
  2: answer[0]
     ^^^^^^^^^
  [1]

  $ tsonnet ../../samples/errors/string_index_out_of_bounds.jsonnet
  ../../samples/errors/string_index_out_of_bounds.jsonnet:2:0 Index out of bounds. Trying to access index 1234 but length is 7
  
  2: name[1234]
     ^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/errors/object_self_out_of_scope.jsonnet
  ../../samples/errors/object_self_out_of_scope.jsonnet:2:13 Can't use self outside of an object
  
  2: local _two = self.one + 1;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]
