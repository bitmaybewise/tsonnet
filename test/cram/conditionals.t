  $ tsonnet ../../samples/conditionals/conditionals.jsonnet
  {
    "cond_else_expr": 15,
    "cond_else_false": "else branch",
    "cond_else_true": "then branch",
    "cond_nested_chain": 42,
    "cond_nested_object": { "result": "this one" },
    "cond_null": null,
    "cond_true": "if true works!",
    "conditional_attribute_else": false,
    "conditional_attribute_then": true
  }


  $ tsonnet ../../samples/conditionals/conditional_attr_not_string.jsonnet
  ERROR: ../../samples/conditionals/conditional_attr_not_string.jsonnet:2:3 Conditional field key must be String or Null, got Number
  
  2:   [if true then 42]: "value"
     ^^^^^^^^^^^^^^^^^^^^^
  [1]


  $ tsonnet ../../samples/conditionals/conditional_object_scope.jsonnet
  { "one": 1, "three": 3, "two": 2 }
