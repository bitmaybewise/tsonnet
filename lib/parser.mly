%{
  [@@@coverage exclude_file]
  open Ast

  let with_pos startpos endpos = {
    startpos = startpos;
    endpos = endpos;
  }
%}

%token <int> INT
%token <float> FLOAT
%token NULL
%token <bool> BOOL
%token <string> STRING
%token LEFT_SQR_BRACKET RIGHT_SQR_BRACKET
%token LEFT_PAREN RIGHT_PAREN
%token COMMA
%token LEFT_CURLY_BRACKET RIGHT_CURLY_BRACKET
%token COLON
%token PLUS MINUS MULTIPLY DIVIDE
%left PLUS MINUS
%left MULTIPLY DIVIDE
%token <string> ID
%token NOT BITWISE_NOT
%left NOT BITWISE_NOT
%token SEMICOLON
%token LOCAL
%token ASSIGN
%token EOF

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  | e = expr_seq; EOF { e }
  ;

expr:
  | e = assignable_expr { e }
  | e = vars { e }
  ;

expr_seq:
  e = expr; SEMICOLON; rest = separated_list(SEMICOLON, expr) { Seq (e :: rest) };

assignable_expr:
  | e = scoped_expr { e }
  | e = literal { e }
  | e1 = assignable_expr; op = bin_op; e2 = assignable_expr { BinOp (with_pos $startpos $endpos, op, e1, e2) }
  | op = unary_op; e = assignable_expr; { UnaryOp (with_pos $startpos $endpos, op, e) }
  ;

scoped_expr:
  | LEFT_PAREN; e = expr; RIGHT_PAREN { e }
  | LEFT_PAREN; e = expr_seq; RIGHT_PAREN { e }
  ;

literal:
  | n = number { Number (with_pos $startpos $endpos, n) }
  | NULL { Null (with_pos $startpos $endpos) }
  | b = BOOL { Bool (with_pos $startpos $endpos, b) }
  | s = STRING { String (with_pos $startpos $endpos, s) }
  | id = ID { Ident (with_pos $startpos $endpos, id) }
  | LEFT_SQR_BRACKET; values = list_fields; RIGHT_SQR_BRACKET { Array (with_pos $startpos $endpos, values) }
  | LEFT_CURLY_BRACKET; attrs = obj_fields; RIGHT_CURLY_BRACKET { Object (with_pos $startpos $endpos, attrs) }
  ;

list_fields:
  vl = separated_list(COMMA, assignable_expr) { vl };

obj_field:
  | k = STRING; COLON; e = assignable_expr { (k, e) }
  | k = ID; COLON; e = assignable_expr { (k, e) }
  ;

obj_fields:
  obj = separated_list(COMMA, obj_field) { obj };

%inline number:
  | i = INT { Int i }
  | f = FLOAT { Float f }
  ;

%inline bin_op:
  | PLUS { Add }
  | MINUS { Subtract }
  | MULTIPLY { Multiply }
  | DIVIDE { Divide }
  ;

%inline unary_op:
  | PLUS { Plus }
  | MINUS { Minus }
  | NOT { Not }
  | BITWISE_NOT { BitwiseNot }
  ;

var:
  varname = ID; ASSIGN; e = assignable_expr { (varname, e) };

vars:
  LOCAL; vars = separated_nonempty_list(COMMA, var) { Local (with_pos $startpos $endpos, vars) };
