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
%token DOT
%token SELF TOP_LEVEL_OBJ
%token PLUS MINUS MULTIPLY DIVIDE MODULO
%left PLUS MINUS
%left MULTIPLY DIVIDE MODULO
%token <string> ID
%token NOT BITWISE_NOT BITWISE_OR BITWISE_AND BITWISE_XOR LOGICAL_AND LOGICAL_OR
%left  NOT BITWISE_NOT BITWISE_OR BITWISE_AND BITWISE_XOR LOGICAL_AND LOGICAL_OR
%token SEMICOLON
%token LOCAL
%token ASSIGN
%token EQUALITY INEQUALITY GREATER GREATER_EQUAL LESS LESS_EQUAL IN
%left EQUALITY INEQUALITY GREATER GREATER_EQUAL LESS LESS_EQUAL IN
%token SHIFT_LEFT SHIFT_RIGHT
%left SHIFT_LEFT SHIFT_RIGHT
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
  | op = unary_op; e = assignable_expr { UnaryOp (with_pos $startpos $endpos, op, e) }
  | e = indexed_expr { e }
  | e = obj_field_access { e }
  | e = funcall { e }
  ;

indexed_expr:
  | varname = ID; LEFT_SQR_BRACKET; e = assignable_expr; RIGHT_SQR_BRACKET { IndexedExpr (with_pos $startpos $endpos, varname, e) }
  ;

scoped_expr:
  | LEFT_PAREN; e = expr; RIGHT_PAREN { e }
  | LEFT_PAREN; e = expr_seq; RIGHT_PAREN { e }
  ;

identifier:
  | id = ID { Ident (with_pos $startpos $endpos, id) }
  ;

literal:
  | n = number { Number (with_pos $startpos $endpos, n) }
  | NULL { Null (with_pos $startpos $endpos) }
  | b = BOOL { Bool (with_pos $startpos $endpos, b) }
  | s = STRING { String (with_pos $startpos $endpos, s) }
  | id = identifier { id }
  | LEFT_SQR_BRACKET; values = array_field_list; RIGHT_SQR_BRACKET { Array (with_pos $startpos $endpos, values) }
  | LEFT_CURLY_BRACKET; attrs = obj_field_list; RIGHT_CURLY_BRACKET { ParsedObject (with_pos $startpos $endpos, attrs) }
  ;

array_field_list:
  | { [] }
  | e = assignable_expr { [e] }
  | e = assignable_expr; COMMA; es = array_field_list { e :: es }
  ;

obj_key:
  | k = STRING { k }
  | k = ID { k }
  ;

obj_field:
  | k = obj_key; COLON; e = assignable_expr { ObjectField (k, e) }
  | e = single_var { ObjectExpr e }
  ;

obj_field_list:
  | { [] }
  | obj_field { [$1] }
  | f = obj_field; COMMA; fields = obj_field_list { f :: fields }
  ;

obj_field_expr:
  | DOT; LEFT_SQR_BRACKET; e = assignable_expr; RIGHT_SQR_BRACKET { e }
  | DOT; id = identifier { id }
  ;

obj_field_chain_item:
  | e = obj_field_expr { e }
  | LEFT_SQR_BRACKET; e = assignable_expr; RIGHT_SQR_BRACKET { e }
  ;

obj_field_chain:
  | { [] }
  | id = obj_field_chain_item; ids = obj_field_chain { id :: ids }
  ;

obj_field_chain_nonempty:
  | id = obj_field_chain_item { [id] }
  | id = obj_field_chain_item; ids = obj_field_chain_nonempty { id :: ids }
  ;

obj_field_access:
  (* For self and $, allow empty chain *)
  | SELF; chain = obj_field_chain { ObjectFieldAccess (with_pos $startpos $endpos, Self, chain) }
  | TOP_LEVEL_OBJ; chain = obj_field_chain { ObjectFieldAccess (with_pos $startpos $endpos, TopLevel, chain) }
  (* For ID-based scope, only match if there's a dot (obj_field_expr) or multiple bracket accesses *)
  | id = ID; field = obj_field_expr; chain = obj_field_chain { ObjectFieldAccess (with_pos $startpos $endpos, ObjVarRef id, field :: chain) }
  | id = ID; LEFT_SQR_BRACKET; e = assignable_expr; RIGHT_SQR_BRACKET; rest = obj_field_chain_nonempty { ObjectFieldAccess (with_pos $startpos $endpos, ObjVarRef id, e :: rest) }
  ;

%inline number:
  | i = INT { Int i }
  | f = FLOAT { Float f }
  ;

%inline bin_op:
     | PLUS { Add }
     | MINUS { Subtract }
     | MULTIPLY { Multiply }
     | DIVIDE { Divide }
     | MODULO { Modulo }
     | EQUALITY { Equality }
     | INEQUALITY { Inequality }
     | GREATER { GreaterThan }
     | GREATER_EQUAL { GreaterThanOrEqual }
     | LESS { LessThan }
     | LESS_EQUAL { LessThanOrEqual }
     | BITWISE_OR { BitwiseOr }
     | BITWISE_AND { BitwiseAnd }
     | BITWISE_XOR { BitwiseXor }
     | LOGICAL_AND { LogicalAnd }
     | LOGICAL_OR { LogicalOr }
     | IN { In }
     | SHIFT_LEFT { ShiftLeft }
     | SHIFT_RIGHT { ShiftRight }
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
  | LOCAL; vars = separated_nonempty_list(COMMA, var) { Local (with_pos $startpos $endpos, vars) }
  | LOCAL; def = fundef { FunctionDef (with_pos $startpos $endpos, def) }
  ;

single_var:
  | LOCAL; var_expr = var { Local (with_pos $startpos $endpos, [var_expr]) }
  ;

fundef_param:
  | name = ID { (name, None) }
  | name = ID; ASSIGN; default = assignable_expr { (name, Some default) }
  ;

fundef:
  | fname = ID;
    LEFT_PAREN; params = separated_nonempty_list(COMMA, fundef_param); RIGHT_PAREN;
    ASSIGN;
    body = fundef_body { (fname, params, body) }
  ;

fundef_body:
  | e = assignable_expr { e }
  | local_bindings = vars; SEMICOLON; body = fundef_body { Seq [local_bindings; body] }
  ;

funcall:
  | fname = ID;
    LEFT_PAREN; params = separated_nonempty_list(COMMA, assignable_expr); RIGHT_PAREN
    { FunctionCall (with_pos $startpos $endpos, fname, params) }
  ;