  $ tsonnet ../../samples/errors/unterminated_block.jsonnet
  ERROR: ../../samples/errors/unterminated_block.jsonnet:12:1 Unterminated block comment
  
  12: ?
      ^
  [1]


  $ tsonnet ../../samples/errors/malformed_string.jsonnet
  ERROR: ../../samples/errors/malformed_string.jsonnet:1:22 String is not terminated
  
  1: "oops... no end quote
     ^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/malformed_verbatim_string.jsonnet
  ERROR: ../../samples/errors/malformed_verbatim_string.jsonnet:1:18 String is not terminated
  
  1: local s = |||
     ^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/malformed_verbatim_string_single_line.jsonnet
  ERROR: ../../samples/errors/malformed_verbatim_string_single_line.jsonnet:1:24 String is not terminated
  
  1: local s = @'hello...;
     ^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/sum_int_to_boolean.jsonnet
  ERROR: ../../samples/errors/sum_int_to_boolean.jsonnet:1:0 Invalid binary operation
  
  1: 42 + true
     ^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/sum_int_to_boolean_multiline.jsonnet
  ERROR: ../../samples/errors/sum_int_to_boolean_multiline.jsonnet:1:0 Invalid binary operation
  
  1: (1+1+1+1)+
     ^^^^^^^^^^
  2: 2+2+2+2+
     ^^^^^^^^
  3: 3+3+3+false+
     ^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/unscoped_local.jsonnet
  ERROR: ../../samples/errors/unscoped_local.jsonnet:1:15 Parsing error. Invalid syntax:
  
  1: local a = local b = 1;
     ^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/undefined_local.jsonnet
  WARNING: ../../samples/errors/undefined_local.jsonnet:1:0 Unused variable a
  
  1: local a = 1;
     ^^^^^^^^^^^^
  ---
  WARNING: ../../samples/errors/undefined_local.jsonnet:2:0 Unused variable b
  
  2: local b = 2;
     ^^^^^^^^^^^^
  ---
  ERROR: ../../samples/errors/undefined_local.jsonnet:3:0 Undefined variable: c
  
  3: c
     ^
  [1]


  $ tsonnet ../../samples/errors/array_index_out_of_bounds.jsonnet
  ERROR: ../../samples/errors/array_index_out_of_bounds.jsonnet:3:0 Index out of bounds. Trying to access index 4 but length is 3
  
  3: list[index]
     ^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/array_index_not_int.jsonnet
  ERROR: ../../samples/errors/array_index_not_int.jsonnet:3:0 Expected Integer index, got String
  
  3: list[index]
     ^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/value_non_indexable.jsonnet
  ERROR: ../../samples/errors/value_non_indexable.jsonnet:2:0 Int is a non indexable value
  
  2: answer[0]
     ^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/string_index_out_of_bounds.jsonnet
  ERROR: ../../samples/errors/string_index_out_of_bounds.jsonnet:2:0 Index out of bounds. Trying to access index 1234 but length is 7
  
  2: name[1234]
     ^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/object_self_out_of_scope.jsonnet
  ERROR: ../../samples/errors/object_self_out_of_scope.jsonnet:2:13 Can't use self outside of an object
  
  2: local _two = self.one + 1;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/object_outer_most_ref_out_of_scope.jsonnet
  ERROR: ../../samples/errors/object_outer_most_ref_out_of_scope.jsonnet:2:13 No top-level object found
  
  2: local _two = $.one + 1;
     ^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/object_field_access_index_self_out_of_scope.jsonnet
  ERROR: ../../samples/errors/object_field_access_index_self_out_of_scope.jsonnet:2:4 Can't use self outside of an object
  
  2: obj[self.one].two
     ^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/object_field_access_computed_self_out_of_scope.jsonnet
  ERROR: ../../samples/errors/object_field_access_computed_self_out_of_scope.jsonnet:2:5 Can't use self outside of an object
  
  2: obj.[self.one].two
     ^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/object_field_access_index_toplevel_out_of_scope.jsonnet
  ERROR: ../../samples/errors/object_field_access_index_toplevel_out_of_scope.jsonnet:2:4 No top-level object found
  
  2: obj[$.one].two
     ^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/conditional_self_out_of_scope.jsonnet
  ERROR: ../../samples/errors/conditional_self_out_of_scope.jsonnet:1:27 Can't use self outside of an object
  
  1: local value = if true then self.one else 1;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/conditional_toplevel_out_of_scope.jsonnet
  ERROR: ../../samples/errors/conditional_toplevel_out_of_scope.jsonnet:1:35 No top-level object found
  
  1: local value = if false then 1 else $.one;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/function_def_self_out_of_scope.jsonnet
  ERROR: ../../samples/errors/function_def_self_out_of_scope.jsonnet:1:20 Can't use self outside of an object
  
  1: local get_value() = self.value;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/function_def_toplevel_out_of_scope.jsonnet
  ERROR: ../../samples/errors/function_def_toplevel_out_of_scope.jsonnet:1:20 No top-level object found
  
  1: local get_value() = $.value;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/function_def_default_self_out_of_scope.jsonnet
  ERROR: ../../samples/errors/function_def_default_self_out_of_scope.jsonnet:1:24 Can't use self outside of an object
  
  1: local get_value(value = self.value) = value;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/function_def_default_toplevel_out_of_scope.jsonnet
  ERROR: ../../samples/errors/function_def_default_toplevel_out_of_scope.jsonnet:1:24 No top-level object found
  
  1: local get_value(value = $.value) = value;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/function_call_callee_self_out_of_scope.jsonnet
  ERROR: ../../samples/errors/function_call_callee_self_out_of_scope.jsonnet:1:1 Can't use self outside of an object
  
  1: (self.get_value)()
     ^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/function_call_callee_toplevel_out_of_scope.jsonnet
  ERROR: ../../samples/errors/function_call_callee_toplevel_out_of_scope.jsonnet:1:1 No top-level object found
  
  1: ($.get_value)()
     ^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/function_call_arg_self_out_of_scope.jsonnet
  ERROR: ../../samples/errors/function_call_arg_self_out_of_scope.jsonnet:2:10 Can't use self outside of an object
  
  2: get_value(self.value)
     ^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/function_call_arg_toplevel_out_of_scope.jsonnet
  ERROR: ../../samples/errors/function_call_arg_toplevel_out_of_scope.jsonnet:2:10 No top-level object found
  
  2: get_value($.value)
     ^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/function_call_named_arg_self_out_of_scope.jsonnet
  ERROR: ../../samples/errors/function_call_named_arg_self_out_of_scope.jsonnet:2:18 Can't use self outside of an object
  
  2: get_value(value = self.value)
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/function_call_named_arg_toplevel_out_of_scope.jsonnet
  ERROR: ../../samples/errors/function_call_named_arg_toplevel_out_of_scope.jsonnet:2:18 No top-level object found
  
  2: get_value(value = $.value)
     ^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/divide_by_zero.jsonnet
  ERROR: ../../samples/errors/divide_by_zero.jsonnet:1:0 Division by zero
  
  1: 5 / 0
     ^^^^^
  [1]


  $ tsonnet ../../samples/errors/divide_by_zero_float.jsonnet
  ERROR: ../../samples/errors/divide_by_zero_float.jsonnet:1:0 Division by zero
  
  1: 5 / 0.0
     ^^^^^^^
  [1]


  $ tsonnet ../../samples/errors/modulo_by_zero.jsonnet
  ERROR: ../../samples/errors/modulo_by_zero.jsonnet:1:0 Division by zero
  
  1: 5 % 0
     ^^^^^
  [1]


  $ tsonnet ../../samples/errors/modulo_by_zero_float.jsonnet
  ERROR: ../../samples/errors/modulo_by_zero_float.jsonnet:1:0 Division by zero
  
  1: 5 % 0.0
     ^^^^^^^
  [1]
