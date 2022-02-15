%left '|'
%left '^'
%left '&'
%nonassoc '!'

%start cmdList

cmdList:
  empty
  cons		cmdList stmt

stmt:
  gate               gate

gate:
  gate          name EQ expression

expression:
  paren		'(' expression ')'
  and		expression '&' expression
  or		expression '|' expression
  xor		expression '^' expression
  not		'!' expression
  node		node
  
node:
  named		name

name:
  ident         IDENT
