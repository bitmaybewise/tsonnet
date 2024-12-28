%token <int> INT
%token <float> FLOAT
%token NULL
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
  ;
