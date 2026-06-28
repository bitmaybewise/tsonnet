  $ tsonnet ../../samples/semantics/valid_binding_cycle.jsonnet
  WARNING: ../../samples/semantics/valid_binding_cycle.jsonnet:3:0 Unused variable c
  
  3: local c = a;
     ^^^^^^^^^^^^
  ---
  1


  $ tsonnet ../../samples/semantics/invalid_binding_itself.jsonnet
  ERROR: ../../samples/semantics/invalid_binding_itself.jsonnet:1:10 Cyclic reference found for a
  
  1: local a = a;
     ^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_binding_cycle.jsonnet
  WARNING: ../../samples/semantics/invalid_binding_cycle.jsonnet:2:0 Unused variable b
  
  2: local b = d;
     ^^^^^^^^^^^^
  ---
  WARNING: ../../samples/semantics/invalid_binding_cycle.jsonnet:4:0 Unused variable d
  
  4: local d = 1;
     ^^^^^^^^^^^^
  ---
  ERROR: ../../samples/semantics/invalid_binding_cycle.jsonnet:3:10 Cyclic reference found for a
  
  3: local c = a;
     ^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_binding_cycle_array.jsonnet
  ERROR: ../../samples/semantics/invalid_binding_cycle_array.jsonnet:1:10 Cyclic reference found for b
  
  1: local a = b,
     ^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_binding_cycle_object.jsonnet
  ERROR: ../../samples/semantics/invalid_binding_cycle_object.jsonnet:1:12 Cyclic reference found for 1->c
  
  1: local obj = { a: 1, b: 2, c: obj };
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_binding_cycle_object_locals.jsonnet
  ERROR: ../../samples/semantics/invalid_binding_cycle_object_locals.jsonnet:4:14 Cyclic reference found for b
  
  4:     local c = b,
     ^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_binding_cycle_binop.jsonnet
  ERROR: ../../samples/semantics/invalid_binding_cycle_binop.jsonnet:2:10 Cyclic reference found for a
  
  2: local b = a + 1;
     ^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_binding_cycle_unaryop.jsonnet
  ERROR: ../../samples/semantics/invalid_binding_cycle_unaryop.jsonnet:2:11 Cyclic reference found for a
  
  2: local b = ~a;
     ^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_binding_cycle_local.jsonnet
  ERROR: ../../samples/semantics/invalid_binding_cycle_local.jsonnet:2:24 Cyclic reference found for a
  
  2: local b = (local b = 4; a + b);
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_binding_cycle_nested_local.jsonnet
  ERROR: ../../samples/semantics/invalid_binding_cycle_nested_local.jsonnet:1:21 Cyclic reference found for b
  
  1: local a = (local b = b; b);
     ^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/valid_binding_local_shadowing.jsonnet
  1


  $ tsonnet ../../samples/semantics/invalid_binding_cycle_object_fields.jsonnet
  ERROR: ../../samples/semantics/invalid_binding_cycle_object_fields.jsonnet:3:12 Cyclic reference found for 1->a
  
  3:     b: self.a,
     ^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_binding_cycle_object_nested_field.jsonnet
  ERROR: ../../samples/semantics/invalid_binding_cycle_object_nested_field.jsonnet:3:17 Cyclic reference found for 1->b
  
  3:         value: $.b
     ^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_binding_cycle_outer_object_fields.jsonnet
  ERROR: ../../samples/semantics/invalid_binding_cycle_outer_object_fields.jsonnet:3:9 Cyclic reference found for 1->a
  
  3:     b: $.a,
     ^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_binding_cycle_object_field_and_local.jsonnet
  ERROR: ../../samples/semantics/invalid_binding_cycle_object_field_and_local.jsonnet:2:19 Cyclic reference found for 1->b
  
  2:     local a = self.b,
     ^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_binding_cycle_index_expr.jsonnet
  ERROR: ../../samples/semantics/invalid_binding_cycle_index_expr.jsonnet:1:10 Cyclic reference found for i
  
  1: local i = i;
     ^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_binding_cycle_indexed_local.jsonnet
  ERROR: ../../samples/semantics/invalid_binding_cycle_indexed_local.jsonnet:1:10 Cyclic reference found for a
  
  1: local a = a[0];
     ^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_binding_cycle_indexed_field.jsonnet
  ERROR: ../../samples/semantics/invalid_binding_cycle_indexed_field.jsonnet:3:16 Cyclic reference found for 1->arr
  
  3:     first: self.arr[0]
     ^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_object_with_cyclic_field.jsonnet
  ERROR: ../../samples/semantics/invalid_object_with_cyclic_field.jsonnet:4:12 Cyclic reference found for 1->b
  
  4:     c: self.b,
     ^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/valid_object_access_non_cyclic_field.jsonnet
  1


  $ tsonnet ../../samples/semantics/invalid_object_var_access_self_cycle.jsonnet
  ERROR: ../../samples/semantics/invalid_object_var_access_self_cycle.jsonnet:1:12 Cyclic reference found for obj
  
  1: local obj = obj;
     ^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_object_var_field_cycle.jsonnet
  ERROR: ../../samples/semantics/invalid_object_var_field_cycle.jsonnet:2:7 Cyclic reference found for 1->a
  
  2:     a: obj.a,
     ^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_object_var_indirect_field_cycle.jsonnet
  ERROR: ../../samples/semantics/invalid_object_var_indirect_field_cycle.jsonnet:3:7 Cyclic reference found for 1->a
  
  3:     b: obj.a,
     ^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/valid_object_var_non_cyclic_field_access.jsonnet
  1


  $ tsonnet ../../samples/semantics/valid_object_var_shadowed_by_object_local.jsonnet
  1


  $ tsonnet ../../samples/semantics/invalid_function_default_cycle.jsonnet
  ERROR: ../../samples/semantics/invalid_function_default_cycle.jsonnet:3:0 Invalid binary operation
  
  3: f() + a[0]
     ^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/valid_unused_recursive_function_body.jsonnet
  1


  $ tsonnet ../../samples/semantics/valid_function_body_uses_outer_local.jsonnet
  1


  $ tsonnet ../../samples/semantics/invalid_closure_default_cycle.jsonnet
  ERROR: ../../samples/semantics/invalid_closure_default_cycle.jsonnet:3:0 Expected 1 argument(s), got 0
  
  3: f() + a[0]
     ^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/valid_unused_recursive_closure_body.jsonnet
  WARNING: ../../samples/semantics/valid_unused_recursive_closure_body.jsonnet:1:0 Unused variable f
  
  1: local f = function() f();
     ^^^^^^^^^^^^^^^^^^^^^^^^^
  ---
  1


  $ tsonnet ../../samples/semantics/valid_closure_param_shadowing_unused_outer.jsonnet
  WARNING: ../../samples/semantics/valid_closure_param_shadowing_unused_outer.jsonnet:1:0 Unused variable x
  
  1: local x = 1;
     ^^^^^^^^^^^^
  ---
  2


  $ tsonnet ../../samples/semantics/invalid_recursive_function_call.jsonnet
  ERROR: ../../samples/semantics/invalid_recursive_function_call.jsonnet:1:12 Cyclic reference found for f
  
  1: local f() = f();
     ^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_recursive_closure_call.jsonnet
  ERROR: ../../samples/semantics/invalid_recursive_closure_call.jsonnet:1:21 Cyclic reference found for f
  
  1: local f = function() f();
     ^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_mutual_recursive_function_call.jsonnet
  ERROR: ../../samples/semantics/invalid_mutual_recursive_function_call.jsonnet:2:12 Cyclic reference found for f
  
  2: local g() = f();
     ^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_mutual_recursive_closure_call.jsonnet
  ERROR: ../../samples/semantics/invalid_mutual_recursive_closure_call.jsonnet:2:21 Cyclic reference found for f
  
  2: local g = function() f();
     ^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_function_call_positional_arg_cycle.jsonnet
  ERROR: ../../samples/semantics/invalid_function_call_positional_arg_cycle.jsonnet:2:13 Cyclic reference found for a
  
  2: f((local a = a; a))
     ^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_function_call_named_arg_cycle.jsonnet
  ERROR: ../../samples/semantics/invalid_function_call_named_arg_cycle.jsonnet:2:15 Cyclic reference found for a
  
  2: f(x=(local a = a; a))
     ^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_function_call_callee_cycle.jsonnet
  ERROR: ../../samples/semantics/invalid_function_call_callee_cycle.jsonnet:1:11 Cyclic reference found for f
  
  1: (local f = f; f)(1)
     ^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_function_call_type.jsonnet
  ERROR: ../../samples/semantics/invalid_function_call_type.jsonnet:2:18 Expected type Number, got String
  
  2: my_function(3) && my_function("oops")
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_conditional_branches_type.jsonnet
  ERROR: ../../samples/semantics/invalid_conditional_branches_type.jsonnet:1:0 Conditional branches have different types: then branch returns Number, else branch returns String
  
  1: if true then 1 else "oops"
     ^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_conditional_field_cyclic_value.jsonnet
  ERROR: ../../samples/semantics/invalid_conditional_field_cyclic_value.jsonnet:4:12 Cyclic reference found for 1->b
  
  4:     c: self.b,
     ^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/valid_conditional_field_access_cyclic.jsonnet
  1


  $ tsonnet ../../samples/semantics/invalid_conditional_field_cyclic_key.jsonnet
  ERROR: ../../samples/semantics/invalid_conditional_field_cyclic_key.jsonnet:3:7 Cyclic reference found for 1->a
  
  3:     b: self.a,
     ^^^^^^^^^^^^^^
  [1]
