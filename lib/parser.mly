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
  | i = INT { with_pos $startpos $endpos (Number (Int i)) }
  | f = FLOAT { with_pos $startpos $endpos (Number (Float f)) }
  | NULL { with_pos $startpos $endpos Null }
  | b = BOOL { with_pos $startpos $endpos (Bool b) }
  | s = STRING { with_pos $startpos $endpos (String s) }
  | id = ID { with_pos $startpos $endpos (Ident id) }
  | LEFT_PAREN; e = expr; RIGHT_PAREN { with_pos $startpos $endpos e.value }
  | LEFT_SQR_BRACKET; values = list_fields; RIGHT_SQR_BRACKET { with_pos $startpos $endpos (Array values) }
  | LEFT_CURLY_BRACKET; attrs = obj_fields; RIGHT_CURLY_BRACKET { with_pos $startpos $endpos (Object attrs) }
  | e1 = expr; PLUS; e2 = expr { with_pos $startpos $endpos (BinOp (Add, e1.value, e2.value)) }
  | e1 = expr; MINUS; e2 = expr { with_pos $startpos $endpos (BinOp (Subtract, e1.value, e2.value)) }
  | e1 = expr; MULTIPLY; e2 = expr { with_pos $startpos $endpos (BinOp (Multiply, e1.value, e2.value)) }
  | e1 = expr; DIVIDE; e2 = expr { with_pos $startpos $endpos (BinOp (Divide, e1.value, e2.value)) }
  | PLUS; e = expr; { with_pos $startpos $endpos (UnaryOp (Plus, e.value)) }
  | MINUS; e = expr; { with_pos $startpos $endpos (UnaryOp (Minus, e.value)) }
  | NOT; e = expr; { with_pos $startpos $endpos (UnaryOp (Not, e.value)) }
  | BITWISE_NOT; e = expr; { with_pos $startpos $endpos (UnaryOp (BitwiseNot, e.value)) }
  | LOCAL; varname = ID; ASSIGN; e = expr; { with_pos $startpos $endpos (Local (varname, e.value)) }
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
