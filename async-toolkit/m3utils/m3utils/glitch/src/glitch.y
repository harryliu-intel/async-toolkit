%left '|'
%left '&'
%nonassoc '~'

%start cmdList

cmdList:
  empty
  cons		cmdList stmt

stmt:
  gate               gate
  asyncDecl          asyncDecl
  outputDecl         outputDecl

gate:
  gate          GATE name EQ expression

asyncDecl:
  async         ASYNC         name

outputDecl:
  output        OUTPUT        name

expression:
  paren		'(' expression ')'
  and		expression '&' expression
  or		expression '|' expression
  xor		expression '^' expression
  not		'~' expression
  node		node
  
node:
  named		name

name:
  ident         IDENT
