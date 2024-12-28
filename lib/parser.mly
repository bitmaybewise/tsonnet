%token <int> INT
%token <float> FLOAT
%token NULL
%token <bool> BOOL
%token <string> STRING
%token LEFT_SQR_BRACKET
%token RIGHT_SQR_BRACKET
%token COMMA
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
  ;

list_fields:
  vl = separated_list(COMMA, expr) { vl };
