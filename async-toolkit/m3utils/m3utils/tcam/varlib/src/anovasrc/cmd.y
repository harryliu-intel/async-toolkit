%left T_LOR
%left T_LAND
%left T_EQ T_NEQ
%left T_LT T_LE T_GT T_GE
%left '+' '-'
%left '*' '/' T_DIV T_MOD
%nonassoc uminus_expr

%start start


start:
  expr		expr

expr:
  literal	T_LITERAL
  constant	T_FLOAT
  sum		expr '+' expr
  diff		expr '-' expr
  prod 		expr '*' expr
  quot 		expr '/' expr
  iquot         expr T_DIV expr
  imod          expr T_MOD expr
  paren		'(' expr ')'
  uminus	'-' expr
  func		T_LITERAL '(' expr_list ')'
  array         T_LITERAL '[' expr ']'

  or		expr T_LOR expr
  and		expr T_LAND expr
  equal		expr T_EQ expr
  notequal	expr T_NEQ expr
  lessthan	expr T_LT expr
  lessequal	expr T_LE expr
  greaterthan	expr T_GT expr
  greaterequal	expr T_GE expr

expr_list:
  single	expr
  mult		expr ',' expr_list
