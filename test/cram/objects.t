  $ tsonnet ../../samples/objects/self_reference.jsonnet
  { "one": 1, "two": 2 }

  $ tsonnet ../../samples/objects/self_bracket_lookup.jsonnet
  { "answer": 42, "answer_to_the_ultimate_question": 42 }

  $ tsonnet ../../samples/objects/toplevel_reference.jsonnet
  { "one": 1, "two": 2 }

  $ tsonnet ../../samples/objects/toplevel_bracket_lookup.jsonnet
  { "answer": 42, "answer_to_the_ultimate_question": 42 }

  $ tsonnet ../../samples/objects/self_field_lookup_chain.jsonnet
  { "answer": { "value": 42 }, "answer_to_the_ultimate_question": 42 }

  $ tsonnet ../../samples/objects/self_field_indexed_access.jsonnet
  { "arr": [ 1, 2, 3 ], "first": 1 }

  $ tsonnet ../../samples/objects/toplevel_field_lookup_chain.jsonnet
  { "answer": { "value": 42 }, "answer_to_the_ultimate_question": 42 }

  $ tsonnet ../../samples/objects/merge.jsonnet
  { "a": 1, "b": 3, "c": 4 }

  $ tsonnet ../../samples/objects/untouched_field.jsonnet
  42


  $ tsonnet ../../samples/objects/untouched_invalid_field.jsonnet
  1
