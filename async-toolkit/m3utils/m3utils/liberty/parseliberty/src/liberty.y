%left '+' '-'
%left '*' '/'

%start file

file:
  group             group

group:
  nonempty          head LBRACK statements RBRACK
  empty             head LBRACK RBRACK

statements:
  x                 statement
  cons              statements statement

statement:
  simple_attr       simple_attr
  complex_attr      complex_attr
  define            define
  define_group      define_group
  group             group

simple_attr:
  colonsemi         IDENT ':' attr_val_expr ';'
  colon             IDENT ':' attr_val_expr
  eq                IDENT '=' attr_val_expr ';'

complex_attr:
  headsemi          head ';'
  head              head

head:
  x                 IDENT '(' param_list ')'
  empty             IDENT '(' ')'

param_list:
  x                 attr_val
  commacons         param_list ',' attr_val
  cons              param_list attr_val

define:
  x                 DEFINE '(' s_or_i ',' s_or_i ',' s_or_i ')' ';'

define_group:
  x                 DEFINE_GROUP '(' s_or_i ',' s_or_i ')' ';'

s_or_i:
  string            STRING
  ident             IDENT

attr_val:
  num               NUM
  s_or_i            s_or_i
  colon             s_or_i ':' s_or_i
  true              TTRUE
  false             TFALSE

attr_val_expr:
  string            STRING
  true              TTRUE
  false             TFALSE
  expr              expr

expr:
  plus              expr '+' expr
  minus             expr '-' expr
  times             expr '*' expr
  div               expr '/' expr
  paren             '(' expr ')'
  uminus            '-' expr
  uplus             '+' expr
  uinv              '!' expr
  ucompl            '~' expr
  num               NUM
  ident             IDENT
  
