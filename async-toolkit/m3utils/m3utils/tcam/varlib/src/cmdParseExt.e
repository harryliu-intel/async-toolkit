%source cmd.t cmd.y
%import cmdLexExt cmdParse

%module { 
IMPORT Process;
IMPORT RefList;
IMPORT Text, Fmt;
IMPORT AssignmentList, Assignment;
IMPORT VarExpr;
}

%interface { 
IMPORT VarExpr;
IMPORT RefList;
IMPORT AssignmentList, Assignment;
}

%public {
   lst : AssignmentList.T;
}

start:
  ass_list      { self.lst := $1.val }

expr: 		{ val : VarExpr.T }
  literal       { $$ := NEW(VarExpr.Variable, nm := $1) }
  constant	{ $$ := NEW(VarExpr.Literal, val := $1) }
  sum		{ $$ := NEW(VarExpr.Plus, a := $1, b:= $2) }
  diff		{ $$ := NEW(VarExpr.Minus, a := $1, b:= $2) }
  prod		{ $$ := NEW(VarExpr.Times, a := $1, b:= $2) }
  quot		{ $$ := NEW(VarExpr.Divide, a := $1, b:= $2) }
  equal    	{ $$ := NEW(VarExpr.Equal, a := $1, b:= $2) }
  uminus        { $$ := NEW(VarExpr.UMinus, a := $1) }
  paren		{ $$ := $1 }
  func          {
    VAR  a := NEW(REF ARRAY OF VarExpr.T, RefList.Length($2));
         p := $2;
         i := 0;
    BEGIN
      WHILE p # NIL DO
        a[i] := p.head;
        p := p.tail; INC(i)
      END;
      $$ := NEW(VarExpr.Func, nm := $1, args := a)
    END
  }
  qmark         { $$ := NEW(VarExpr.QMark, a := $1, b := $2, c := $3) }


expr_list: 	{ val : RefList.T }
  single     	{ $$ := RefList.List1($1) }
  mult		{ $$ := RefList.Cons($1,$2) }

ass:            { val : Assignment.T }
  simple        { $$ := Assignment.T { tgt := $1, val := $2 } }
  pct           { $1.val.pct := TRUE; $$ := $1 }
  
ass_list: 	{ val : AssignmentList.T }
  single     	{ $$ := AssignmentList.List1($1) }
  mult		{ $$ := AssignmentList.Cons($1,$2) }
