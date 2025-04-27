%{
  [@@@coverage exclude_file]
  open Ast

  let with_pos startpos endpos value : Ast.expr = {
    position = {
      startpos = startpos;
      endpos = endpos;
    };
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
%token SEMICOLON
%token LOCAL
%token ASSIGN
%right ASSIGN
%token EOF

%start <Ast.prog> prog

%%

prog:
  | e = expr; EOF { Expr e }
  | e = expr; SEMICOLON; seq = expr_seq; EOF { Sequence (e :: seq) }
  ;

expr:
  | n = number { with_pos $startpos $endpos (Number n) }
  | NULL { with_pos $startpos $endpos Null }
  | b = BOOL { with_pos $startpos $endpos (Bool b) }
  | s = STRING { with_pos $startpos $endpos (String s) }
  | id = ID { with_pos $startpos $endpos (Ident id) }
  | LEFT_PAREN; e = expr; RIGHT_PAREN { with_pos $startpos $endpos e.value }
  | LEFT_SQR_BRACKET; values = list_fields; RIGHT_SQR_BRACKET { with_pos $startpos $endpos (Array values) }
  | LEFT_CURLY_BRACKET; attrs = obj_fields; RIGHT_CURLY_BRACKET { with_pos $startpos $endpos (Object attrs) }
  | e1 = expr; op = bin_op; e2 = expr { with_pos $startpos $endpos (BinOp (op, e1.value, e2.value)) }
  | op = unary_op; e = expr; { with_pos $startpos $endpos (UnaryOp (op, e.value)) }
  ;

expr_seq:
  exprs = separated_list(SEMICOLON, expr) { exprs };

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

number:
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
