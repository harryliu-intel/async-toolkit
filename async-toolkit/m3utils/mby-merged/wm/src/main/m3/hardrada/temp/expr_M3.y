%start Expr

Expr:
	recurse Expr oOR E1
	end E1

E1:
	recurse E1 oAND E2
	end E2

E2:
	recurse NOT E2
	end E3

E3:
	recurse E3 Relop E4
	end E4

E4:
	recurse E4 Addop E5
	end E5

E5:
	recurse E5 Mulop E6
	end E6

E6:
	recurseA '+' E6
	recurseB '-' E6
	end E7

E7:
	recurse E7 Selector
	end E8

E8:
	myid oID
	mynumber oNUMBER
	myCharLiteral oCHARLITERAL
	myTextLiteral oTEXTLITERAL
	myConstr Constructor
	myExpr '(' Expr ')'

Relop:
	eq '='
	neq '#'
	lte oLTEOP
	lt '<'
	gt '>'
	gte oGTEOP
	in oIN

Addop:
	add '+'
	sub '-'
	ampersand '&'

Mulop:
	mult '*'
	div '/'
	worddiv oDIV
	mod oMOD

Selector:
	carat '^'
	myid '.' oID
	wexpr '[' exprlist ']'
	wact '(' actuallist ')'
	blank '(' ')'

exprlist:
	recurse exprlist ',' Expr
	end Expr

actuallist:
	recurse actuallist ',' Actual
	end Actual
