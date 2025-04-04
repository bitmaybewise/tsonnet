%{
  [@@@coverage exclude_file]
  open Ast

  let with_pos startpos value : Ast.expr = {
    startpos = startpos;
    value = value
  }
%}

%token <int> INT
%token <float> FLOAT
%token NULL
%token <bool> BOOL
%token <string> STRING
%token LEFT_SQR_BRACKET
%token RIGHT_SQR_BRACKET
%token LEFT_PAREN
%token RIGHT_PAREN
%token COMMA
%token LEFT_CURLY_BRACKET
%token RIGHT_CURLY_BRACKET
%token COLON
%token PLUS MINUS MULTIPLY DIVIDE
%left PLUS MINUS
%left MULTIPLY DIVIDE
%token <string> ID
%token NOT BITWISE_NOT
%left NOT BITWISE_NOT
%token EOF

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | i = INT { with_pos $startpos (Number (Int i)) }
  | f = FLOAT { with_pos $startpos (Number (Float f)) }
  | NULL { with_pos $startpos Null }
  | b = BOOL { with_pos $startpos (Bool b) }
  | s = STRING { with_pos $startpos (String s) }
  | id = ID { with_pos $startpos (Ident id) }
  | LEFT_PAREN; e = expr; RIGHT_PAREN { with_pos $startpos e.value }
  | LEFT_SQR_BRACKET; values = list_fields; RIGHT_SQR_BRACKET { with_pos $startpos (Array values) }
  | LEFT_CURLY_BRACKET; attrs = obj_fields; RIGHT_CURLY_BRACKET { with_pos $startpos (Object attrs) }
  | e1 = expr; PLUS; e2 = expr { with_pos $startpos (BinOp (Add, e1.value, e2.value)) }
  | e1 = expr; MINUS; e2 = expr { with_pos $startpos (BinOp (Subtract, e1.value, e2.value)) }
  | e1 = expr; MULTIPLY; e2 = expr { with_pos $startpos (BinOp (Multiply, e1.value, e2.value)) }
  | e1 = expr; DIVIDE; e2 = expr { with_pos $startpos (BinOp (Divide, e1.value, e2.value)) }
  | PLUS; e = expr; { with_pos $startpos (UnaryOp (Plus, e.value)) }
  | MINUS; e = expr; { with_pos $startpos (UnaryOp (Minus, e.value)) }
  | NOT; e = expr; { with_pos $startpos (UnaryOp (Not, e.value)) }
  | BITWISE_NOT; e = expr; { with_pos $startpos (UnaryOp (BitwiseNot, e.value)) }
  ;

list_value:
  e = expr { e.value };

list_fields:
  vl = separated_list(COMMA, list_value) { vl };

obj_field:
  | k = STRING; COLON; e = expr { (k, e.value) }
  | k = ID; COLON; e = expr { (k, e.value) }
  ;

obj_fields:
    obj = separated_list(COMMA, obj_field) { obj };
