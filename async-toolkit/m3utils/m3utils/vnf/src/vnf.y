%start cmdList

cmdList:
  empty
  cons		cmdList module

module:
  module        MDL IDENT '(' modparamlist ')' ';' decllist ENDMDL

modparamlist:
  empty
  single        modparam
  cons          modparam ',' modparamlist

modparam:
  ident         IDENT

decllist:
  empty
  cons          decl ';' decllist

instparam:
  instparam         DOT IDENT '(' nodes ')'

nodes:
  empty 
  ident             IDENT
  list              '{' nodelist '}'

nodelist:
  empty
  single            IDENT
  list              IDENT ',' nodelist

instparamlist:
  empty
  single            instparam
  cons              instparam ',' instparamlist

decl:
  instance          IDENT IDENT '(' instparamlist ')' 
  wire              WIRE opt_arrayspec IDENT
  input             INPUT opt_arrayspec IDENT
  output            OUTPUT opt_arrayspec IDENT

opt_arrayspec:
  empty
  range             arrayrange
  
arrayrange:
  range             '[' INT ':' INT ']'

