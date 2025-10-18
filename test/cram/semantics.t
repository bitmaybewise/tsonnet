  $ tsonnet ../../samples/semantics/valid_binding_cycle.jsonnet
  Warning: ../../samples/semantics/valid_binding_cycle.jsonnet:3:0 Unused variable c
  
  3: local c = a;
     ^^^^^^^^^^^^
  1

  $ tsonnet ../../samples/semantics/invalid_binding_itself.jsonnet
  ../../samples/semantics/invalid_binding_itself.jsonnet:1:10 Cyclic reference found for a
  
  1: local a = a;
     ^^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/semantics/invalid_binding_cycle.jsonnet
  Warning: ../../samples/semantics/invalid_binding_cycle.jsonnet:2:0 Unused variable b
  
  2: local b = d;
     ^^^^^^^^^^^^
  Warning: ../../samples/semantics/invalid_binding_cycle.jsonnet:4:0 Unused variable d
  
  4: local d = 1;
     ^^^^^^^^^^^^
  ../../samples/semantics/invalid_binding_cycle.jsonnet:3:10 Cyclic reference found for a
  
  3: local c = a;
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
  Warning: ../../samples/semantics/invalid_binding_cycle_object_locals.jsonnet:1:0 Cyclic reference found for 1->c
  
  1: {
     ^
  2:     local a = b,
     ^^^^^^^^^^^^^^^^
  3:     local b = c,
     ^^^^^^^^^^^^^^^^
  4:     local c = b,
     ^^^^^^^^^^^^^^^^
  {}

  $ tsonnet ../../samples/semantics/invalid_binding_cycle_binop.jsonnet
  ../../samples/semantics/invalid_binding_cycle_binop.jsonnet:2:10 Cyclic reference found for a
  
  2: local b = a + 1;
     ^^^^^^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/semantics/invalid_binding_cycle_unaryop.jsonnet
  ../../samples/semantics/invalid_binding_cycle_unaryop.jsonnet:2:11 Cyclic reference found for a
  
  2: local b = ~a;
     ^^^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/semantics/invalid_binding_cycle_local.jsonnet
  ../../samples/semantics/invalid_binding_cycle_local.jsonnet:2:24 Cyclic reference found for a
  
  2: local b = (local b = 4; a + b);
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  [1]

  $ tsonnet ../../samples/semantics/invalid_binding_cycle_object_fields.jsonnet
  Warning: ../../samples/semantics/invalid_binding_cycle_object_fields.jsonnet:1:0 Cyclic reference found for 1->a
  
  1: {
     ^
  2:     a: self.b,
     ^^^^^^^^^^^^^^
  3:     b: self.a,
     ^^^^^^^^^^^^^^
  Warning: ../../samples/semantics/invalid_binding_cycle_object_fields.jsonnet:1:0 Cyclic reference found for 1->b
  
  1: {
     ^
  2:     a: self.b,
     ^^^^^^^^^^^^^^
  3:     b: self.a,
     ^^^^^^^^^^^^^^
  {}

  $ tsonnet ../../samples/semantics/invalid_binding_cycle_object_nested_field.jsonnet
  Warning: ../../samples/semantics/invalid_binding_cycle_object_nested_field.jsonnet:1:0 Cyclic reference found for 1->a
  
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
  Warning: ../../samples/semantics/invalid_binding_cycle_object_nested_field.jsonnet:1:0 Cyclic reference found for 1->b
  
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
  Warning: ../../samples/semantics/invalid_binding_cycle_object_nested_field.jsonnet:2:7 Cyclic reference found for 2->value
  
  2:     a: {
     ^^^^^^^^
  3:         value: $.b
              ^^^^^^^^^
  4:     },
              
  Warning: ../../samples/semantics/invalid_binding_cycle_object_nested_field.jsonnet:2:7 Cyclic reference found for 3->value
  
  2:     a: {
     ^^^^^^^^
  3:         value: $.b
              ^^^^^^^^^
  4:     },
              
  { "a": {} }

  $ tsonnet ../../samples/semantics/invalid_binding_cycle_outer_object_fields.jsonnet
  Warning: ../../samples/semantics/invalid_binding_cycle_outer_object_fields.jsonnet:1:0 Cyclic reference found for 1->a
  
  1: {
     ^
  2:     a: $.b,
     ^^^^^^^^^^^
  3:     b: $.a,
     ^^^^^^^^^^^
  Warning: ../../samples/semantics/invalid_binding_cycle_outer_object_fields.jsonnet:1:0 Cyclic reference found for 1->b
  
  1: {
     ^
  2:     a: $.b,
     ^^^^^^^^^^^
  3:     b: $.a,
     ^^^^^^^^^^^
  {}

  $ tsonnet ../../samples/semantics/invalid_binding_cycle_object_field_and_local.jsonnet
  Warning: ../../samples/semantics/invalid_binding_cycle_object_field_and_local.jsonnet:1:0 Cyclic reference found for 1->b
  
  1: {
     ^
  2:     local a = self.b,
     ^^^^^^^^^^^^^^^^^^^^^
  3:     b: a,
     ^^^^^^^^^
  {}

  $ tsonnet ../../samples/semantics/invalid_binding_cycle_indexed_field.jsonnet
  Warning: ../../samples/semantics/invalid_binding_cycle_indexed_field.jsonnet:1:0 Cyclic reference found for 1->arr
  
  1: {
     ^
  2:     arr: [self.first],
     ^^^^^^^^^^^^^^^^^^^^^^
  3:     first: self.arr[0]
     ^^^^^^^^^^^^^^^^^^^^^^
  Warning: ../../samples/semantics/invalid_binding_cycle_indexed_field.jsonnet:1:0 Cyclic reference found for 1->first
  
  1: {
     ^
  2:     arr: [self.first],
     ^^^^^^^^^^^^^^^^^^^^^^
  3:     first: self.arr[0]
     ^^^^^^^^^^^^^^^^^^^^^^
  {}

  $ tsonnet ../../samples/semantics/valid_object_with_cyclic_field.jsonnet
  Warning: ../../samples/semantics/valid_object_with_cyclic_field.jsonnet:1:0 Cyclic reference found for 1->b
  
  1: {
     ^
  2:     a: 1,
     ^^^^^^^^^
  3:     b: self.c,
     ^^^^^^^^^^^^^^
  4:     c: self.b,
     ^^^^^^^^^^^^^^
  Warning: ../../samples/semantics/valid_object_with_cyclic_field.jsonnet:1:0 Cyclic reference found for 1->c
  
  1: {
     ^
  2:     a: 1,
     ^^^^^^^^^
  3:     b: self.c,
     ^^^^^^^^^^^^^^
  4:     c: self.b,
     ^^^^^^^^^^^^^^
  { "a": 1 }

  $ tsonnet ../../samples/semantics/valid_object_access_non_cyclic_field.jsonnet
  Warning: ../../samples/semantics/valid_object_access_non_cyclic_field.jsonnet:1:0 Unused variable obj
  
  1: local obj = {
     ^^^^^^^^^^^^^
  2:     a: 1,
     ^^^^^^^^^
  3:     b: self.c,
     ^^^^^^^^^^^^^^
  4:     c: self.b,
     ^^^^^^^^^^^^^^
  Warning: ../../samples/semantics/valid_object_access_non_cyclic_field.jsonnet:1:12 Cyclic reference found for 1->b
  
  1: local obj = {
     ^^^^^^^^^^^^^
  2:     a: 1,
                 
  3:     b: self.c,
                 ^^
  4:     c: self.b,
                 ^^
  Warning: ../../samples/semantics/valid_object_access_non_cyclic_field.jsonnet:1:12 Cyclic reference found for 1->c
  
  1: local obj = {
     ^^^^^^^^^^^^^
  2:     a: 1,
                 
  3:     b: self.c,
                 ^^
  4:     c: self.b,
                 ^^
  1
