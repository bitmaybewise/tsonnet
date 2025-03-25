  $ tsonnet ../../samples/comments/comments.jsonnet
  "this is a string"

  $ tsonnet ../../samples/comments/unterminated_block.jsonnet
  ../../samples/comments/unterminated_block.jsonnet:12:1 Unterminated block comment
  
  1 "this is code" /*
  2 This is a block comment
  3 .
  4 .
  5 .
  6 isn't
  7 going
  8 to
  9 end
  10 ?
  11 ?
  12 ?
     ^
  [1]
