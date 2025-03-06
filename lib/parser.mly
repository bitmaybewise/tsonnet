%{
  [@@@coverage exclude_file]
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
%token NOT
%token EOF

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | i = INT { Number (Int i) }
  | f = FLOAT { Number (Float f) }
  | NULL { Null }
  | b = BOOL { Bool b }
  | s = STRING { String s }
  | id = ID { Ident id }
  | LEFT_PAREN; e = expr; RIGHT_PAREN { e }
  | LEFT_SQR_BRACKET; values = list_fields; RIGHT_SQR_BRACKET { Array values }
  | LEFT_CURLY_BRACKET; attrs = obj_fields; RIGHT_CURLY_BRACKET { Object attrs }
  | e1 = expr; PLUS; e2 = expr { BinOp (Add, e1, e2) }
  | e1 = expr; MINUS; e2 = expr { BinOp (Subtract, e1, e2) }
  | e1 = expr; MULTIPLY; e2 = expr { BinOp (Multiply, e1, e2) }
  | e1 = expr; DIVIDE; e2 = expr { BinOp (Divide, e1, e2) }
  | PLUS; e = expr; { UnaryOp (Plus, e) }
  | MINUS; e = expr; { UnaryOp (Minus, e) }
  | NOT; e = expr; { UnaryOp (Not, e) }
  ;

list_fields:
  vl = separated_list(COMMA, expr) { vl };

obj_field:
  | k = STRING; COLON; v = expr { (k, v) }
  | k = ID; COLON; v = expr { (k, v) }
  ;

obj_fields:
    obj = separated_list(COMMA, obj_field) { obj };
