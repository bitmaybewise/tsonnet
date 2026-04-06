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
  ERROR: ../../samples/semantics/invalid_binding_cycle_array.jsonnet:3:30 Cyclic reference found for a
  
  3:     c = (local d = 1, e = 2; [a, b, c, d, e]);
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_binding_cycle_object.jsonnet
  ERROR: ../../samples/semantics/invalid_binding_cycle_object.jsonnet:1:29 Cyclic reference found for obj
  
  1: local obj = { a: 1, b: 2, c: obj };
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/semantics/invalid_binding_cycle_object_locals.jsonnet
  WARNING: ../../samples/semantics/invalid_binding_cycle_object_locals.jsonnet:1:0 Cyclic reference found for 1->c
  
  1: {
     ^
  2:     local a = b,
     ^^^^^^^^^^^^^^^^
  3:     local b = c,
     ^^^^^^^^^^^^^^^^
  4:     local c = b,
     ^^^^^^^^^^^^^^^^
  ---
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


  $ tsonnet ../../samples/semantics/invalid_binding_cycle_object_fields.jsonnet
  WARNING: ../../samples/semantics/invalid_binding_cycle_object_fields.jsonnet:1:0 Cyclic reference found for 1->a
  
  1: {
     ^
  2:     a: self.b,
     ^^^^^^^^^^^^^^
  3:     b: self.a,
     ^^^^^^^^^^^^^^
  ---
  WARNING: ../../samples/semantics/invalid_binding_cycle_object_fields.jsonnet:1:0 Cyclic reference found for 1->b
  
  1: {
     ^
  2:     a: self.b,
     ^^^^^^^^^^^^^^
  3:     b: self.a,
     ^^^^^^^^^^^^^^
  ---
  ERROR: ../../samples/semantics/invalid_binding_cycle_object_fields.jsonnet:2:12 Cyclic reference found for 1->b
  
  2:     a: self.b,
     ^^^^^^^^^^^^^^
  [1]




  $ tsonnet ../../samples/semantics/invalid_binding_cycle_object_nested_field.jsonnet
  WARNING: ../../samples/semantics/invalid_binding_cycle_object_nested_field.jsonnet:1:0 Cyclic reference found for 1->a
  
  1: {
     ^
  2:     a: {
     ^^^^^^^^
  3:         value: $.b
     ^^^^^^^^^^^^^^^^^^
  4:     },
     ^^^^^^
  5:     b: self.a.value,
     ^^^^^^^^^^^^^^^^^^^^
  ---
  WARNING: ../../samples/semantics/invalid_binding_cycle_object_nested_field.jsonnet:1:0 Cyclic reference found for 1->b
  
  1: {
     ^
  2:     a: {
     ^^^^^^^^
  3:         value: $.b
     ^^^^^^^^^^^^^^^^^^
  4:     },
     ^^^^^^
  5:     b: self.a.value,
     ^^^^^^^^^^^^^^^^^^^^
  ---
  WARNING: ../../samples/semantics/invalid_binding_cycle_object_nested_field.jsonnet:2:7 Cyclic reference found for 2->value
  
  2:     a: {
     ^^^^^^^^
  3:         value: $.b
              ^^^^^^^^^
  4:     },
              
  ---
  WARNING: ../../samples/semantics/invalid_binding_cycle_object_nested_field.jsonnet:2:7 Cyclic reference found for 3->value
  
  2:     a: {
     ^^^^^^^^
  3:         value: $.b
              ^^^^^^^^^
  4:     },
              
  ---
  ERROR: ../../samples/semantics/invalid_binding_cycle_object_nested_field.jsonnet:3:17 Cyclic reference found for 1->b
  
  3:         value: $.b
     ^^^^^^^^^^^^^^^^^^
  [1]








  $ tsonnet ../../samples/semantics/invalid_binding_cycle_outer_object_fields.jsonnet
  WARNING: ../../samples/semantics/invalid_binding_cycle_outer_object_fields.jsonnet:1:0 Cyclic reference found for 1->a
  
  1: {
     ^
  2:     a: $.b,
     ^^^^^^^^^^^
  3:     b: $.a,
     ^^^^^^^^^^^
  ---
  WARNING: ../../samples/semantics/invalid_binding_cycle_outer_object_fields.jsonnet:1:0 Cyclic reference found for 1->b
  
  1: {
     ^
  2:     a: $.b,
     ^^^^^^^^^^^
  3:     b: $.a,
     ^^^^^^^^^^^
  ---
  ERROR: ../../samples/semantics/invalid_binding_cycle_outer_object_fields.jsonnet:2:9 Cyclic reference found for 1->b
  
  2:     a: $.b,
     ^^^^^^^^^^^
  [1]




  $ tsonnet ../../samples/semantics/invalid_binding_cycle_object_field_and_local.jsonnet
  WARNING: ../../samples/semantics/invalid_binding_cycle_object_field_and_local.jsonnet:1:0 Cyclic reference found for 1->b
  
  1: {
     ^
  2:     local a = self.b,
     ^^^^^^^^^^^^^^^^^^^^^
  3:     b: a,
     ^^^^^^^^^
  ---
  ERROR: ../../samples/semantics/invalid_binding_cycle_object_field_and_local.jsonnet:3:7 Cyclic reference found for a
  
  3:     b: a,
     ^^^^^^^^^
  [1]



  $ tsonnet ../../samples/semantics/invalid_binding_cycle_indexed_field.jsonnet
  WARNING: ../../samples/semantics/invalid_binding_cycle_indexed_field.jsonnet:1:0 Cyclic reference found for 1->arr
  
  1: {
     ^
  2:     arr: [self.first],
     ^^^^^^^^^^^^^^^^^^^^^^
  3:     first: self.arr[0]
     ^^^^^^^^^^^^^^^^^^^^^^
  ---
  WARNING: ../../samples/semantics/invalid_binding_cycle_indexed_field.jsonnet:1:0 Cyclic reference found for 1->first
  
  1: {
     ^
  2:     arr: [self.first],
     ^^^^^^^^^^^^^^^^^^^^^^
  3:     first: self.arr[0]
     ^^^^^^^^^^^^^^^^^^^^^^
  ---
  ERROR: ../../samples/semantics/invalid_binding_cycle_indexed_field.jsonnet:2:15 Cyclic reference found for 1->first
  
  2:     arr: [self.first],
     ^^^^^^^^^^^^^^^^^^^^^^
  [1]




  $ tsonnet ../../samples/semantics/invalid_object_with_cyclic_field.jsonnet
  WARNING: ../../samples/semantics/invalid_object_with_cyclic_field.jsonnet:1:0 Cyclic reference found for 1->b
  
  1: {
     ^
  2:     a: 1,
     ^^^^^^^^^
  3:     b: self.c,
     ^^^^^^^^^^^^^^
  4:     c: self.b,
     ^^^^^^^^^^^^^^
  ---
  WARNING: ../../samples/semantics/invalid_object_with_cyclic_field.jsonnet:1:0 Cyclic reference found for 1->c
  
  1: {
     ^
  2:     a: 1,
     ^^^^^^^^^
  3:     b: self.c,
     ^^^^^^^^^^^^^^
  4:     c: self.b,
     ^^^^^^^^^^^^^^
  ---
  ERROR: ../../samples/semantics/invalid_object_with_cyclic_field.jsonnet:3:12 Cyclic reference found for 1->c
  
  3:     b: self.c,
     ^^^^^^^^^^^^^^
  [1]




  $ tsonnet ../../samples/semantics/valid_object_access_non_cyclic_field.jsonnet
  WARNING: ../../samples/semantics/valid_object_access_non_cyclic_field.jsonnet:1:12 Cyclic reference found for 1->b
  
  1: local obj = {
     ^^^^^^^^^^^^^
  2:     a: 1,
                 
  3:     b: self.c,
                 ^^
  4:     c: self.b,
                 ^^
  ---
  WARNING: ../../samples/semantics/valid_object_access_non_cyclic_field.jsonnet:1:12 Cyclic reference found for 1->c
  
  1: local obj = {
     ^^^^^^^^^^^^^
  2:     a: 1,
                 
  3:     b: self.c,
                 ^^
  4:     c: self.b,
                 ^^
  ---
  1






  $ tsonnet ../../samples/semantics/invalid_function_call_type.jsonnet
  ERROR: ../../samples/semantics/invalid_function_call_type.jsonnet:2:18 Expected type Number, got String
  
  2: my_function(3) && my_function("oops")
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]

