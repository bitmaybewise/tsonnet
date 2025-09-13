  $ tsonnet ../../samples/semantics/valid_binding_cycle.jsonnet
  1

  $ tsonnet ../../samples/semantics/invalid_binding_itself.jsonnet
  ../../samples/semantics/invalid_binding_itself.jsonnet:1:10 Cyclic reference found for a
  
  1: local a = a;
     ^^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/semantics/invalid_binding_cycle.jsonnet
  ../../samples/semantics/invalid_binding_cycle.jsonnet:1:10 Cyclic reference found for c
  
  1: local a = c;
     ^^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/semantics/invalid_binding_cycle_array.jsonnet
  ../../samples/semantics/invalid_binding_cycle_array.jsonnet:3:30 Cyclic reference found for a
  
  3:     c = (local d = 1, e = 2; [a, b, c, d, e]);
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/semantics/invalid_binding_cycle_object.jsonnet
  ../../samples/semantics/invalid_binding_cycle_object.jsonnet:1:29 Cyclic reference found for obj
  
  1: local obj = { a: 1, b: 2, c: obj };
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/semantics/invalid_binding_cycle_object_locals.jsonnet
  ../../samples/semantics/invalid_binding_cycle_object_locals.jsonnet:3:14 Cyclic reference found for c
  
  3:     local b = c,
     ^^^^^^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/semantics/invalid_binding_cycle_binop.jsonnet
  ../../samples/semantics/invalid_binding_cycle_binop.jsonnet:1:10 Cyclic reference found for b
  
  1: local a = b;
     ^^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/semantics/invalid_binding_cycle_unaryop.jsonnet
  ../../samples/semantics/invalid_binding_cycle_unaryop.jsonnet:1:10 Cyclic reference found for b
  
  1: local a = b;
     ^^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/semantics/invalid_binding_cycle_local.jsonnet
  ../../samples/semantics/invalid_binding_cycle_local.jsonnet:1:10 Cyclic reference found for b
  
  1: local a = b;
     ^^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/semantics/invalid_binding_cycle_object_fields.jsonnet
  ../../samples/semantics/invalid_binding_cycle_object_fields.jsonnet:3:7 Cyclic reference found for 1->a
  
  3:     b: self.a,
     ^^^^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/semantics/invalid_binding_cycle_outer_object_fields.jsonnet
  ../../samples/semantics/invalid_binding_cycle_outer_object_fields.jsonnet:3:7 Cyclic reference found for 1->a
  
  3:     b: $.a,
     ^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/semantics/invalid_binding_cycle_object_field_and_local.jsonnet
  ../../samples/semantics/invalid_binding_cycle_object_field_and_local.jsonnet:2:14 Cyclic reference found for 1->b
  
  2:     local a = self.b,
     ^^^^^^^^^^^^^^^^^^^^^
  [1]
