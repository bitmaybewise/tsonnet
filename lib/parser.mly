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
%token COMMA
%token LEFT_CURLY_BRACKET
%token RIGHT_CURLY_BRACKET
%token COLON
%token ADD SUBTRACT MULTIPLY DIVIDE
%left ADD SUBTRACT
%left MULTIPLY DIVIDE
%token EOF

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | i = INT { Int i }
  | f = FLOAT { Float f }
  | NULL { Null }
  | b = BOOL { Bool b }
  | s = STRING { String s }
  | LEFT_SQR_BRACKET; values = list_fields; RIGHT_SQR_BRACKET { Array values }
  | LEFT_CURLY_BRACKET; attrs = obj_fields; RIGHT_CURLY_BRACKET { Object attrs }
  | e1 = expr; ADD; e2 = expr { BinOp (Add, e1, e2) }
  | e1 = expr; SUBTRACT; e2 = expr { BinOp (Subtract, e1, e2) }
  | e1 = expr; MULTIPLY; e2 = expr { BinOp (Multiply, e1, e2) }
  | e1 = expr; DIVIDE; e2 = expr { BinOp (Divide, e1, e2) }
  ;

list_fields:
  vl = separated_list(COMMA, expr) { vl };

obj_field:
  k = STRING; COLON; v = expr { (k, v) };

obj_fields:
    obj = separated_list(COMMA, obj_field) { obj };
