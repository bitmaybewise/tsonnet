  $ tsonnet ../../samples/variables/single.jsonnet
  42

  $ tsonnet ../../samples/variables/inline_multiple.jsonnet
  3

  $ tsonnet ../../samples/variables/multiline_multiple.jsonnet
  3

  $ tsonnet ../../samples/variables/scoped.jsonnet
  42

  $ tsonnet ../../samples/variables/late_binding_simple.jsonnet
  3

  $ tsonnet ../../samples/variables/late_binding_array.jsonnet
  [ "apple", "banana" ]

  $ tsonnet ../../samples/variables/obj_variable.jsonnet
  {
    "Daiquiri": {
      "ingredients": [
        { "kind": "Banks Rum", "qty": 1.5 },
        { "kind": "Lime", "qty": 1 },
        { "kind": "Simple Syrup", "qty": 0.5 }
      ],
      "served": "Straight Up"
    }
  }

  $ tsonnet ../../samples/variables/obj_variable_late_binding_access.jsonnet
  {
    "Daiquiri": {
      "ingredients": [
        { "kind": "Banks Rum", "qty": 1.5 },
        { "kind": "Lime", "qty": 1 },
        { "kind": "Simple Syrup", "qty": 0.5 }
      ],
      "served": "Straight Up"
    }
  }

  $ tsonnet ../../samples/variables/untouched_variable.jsonnet
  WARNING: ../../samples/variables/untouched_variable.jsonnet:1:0 Unused variable a
  
  1: local a = 1, b = 42;
     ^^^^^^^^^^^^^^^^^^^^
  ---
  42

  $ tsonnet ../../samples/variables/untouched_invalid_variable.jsonnet
  WARNING: ../../samples/variables/untouched_invalid_variable.jsonnet:1:0 Unused variable c
  
  1: local a = 1, b = a, c = d, d = c;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ---
  WARNING: ../../samples/variables/untouched_invalid_variable.jsonnet:1:0 Unused variable d
  
  1: local a = 1, b = a, c = d, d = c;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ---
  WARNING: ../../samples/variables/untouched_invalid_variable.jsonnet:1:0 Cyclic reference found for c
  
  1: local a = 1, b = a, c = d, d = c;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ---
  WARNING: ../../samples/variables/untouched_invalid_variable.jsonnet:1:0 Cyclic reference found for d
  
  1: local a = 1, b = a, c = d, d = c;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ---
  1
