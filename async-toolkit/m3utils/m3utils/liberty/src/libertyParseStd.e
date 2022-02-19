%source liberty.t liberty.y
%import libertyLexStd libertyParse

%module {

}

%interface {

}

%public {
}

expr: { val : LibertyExpr.T }
  plus    { $$ := LibertyExpr.Plus($1, $2) }
  minus   { $$ := LibertyExpr.Minus($1, $2) }
  times   { $$ := LibertyExpr.Times($1, $2) }
  div     { $$ := LibertyExpr.Div($1, $2) }
  paren   { $$ := $1 }
  uminus  { $$ := LibertyExpr.Uminus($1) }
  uplus   { $$ := LibertyExpr.Uplus($1) }
  num     { $$ := LibertyExpr.Num($1) }
  ident   { $$ := LibertyExpr.Ident($1) }