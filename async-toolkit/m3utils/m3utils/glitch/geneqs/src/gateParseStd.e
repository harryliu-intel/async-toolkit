%source gate.t gate.y
%import gateLexStd gateParse

%module {
IMPORT Debug;
IMPORT GateExpr;
IMPORT Gate;
}

%interface {
IMPORT GateExpr;
IMPORT Gate;
}

%public {
  gate : Gate.T;
}

name:  { val : TEXT }
  ident   { Debug.Out("Saw ident " & $1.val);
            $$ := $1 }

expression: { val : GateExpr.T }
  paren { $$ := $1 }
  and   { $$ := GateExpr.And($1, $2) }
  or    { $$ := GateExpr.Or($1, $2) }
  xor    { $$ := GateExpr.Xor($1, $2) }
  not   { $$ := GateExpr.Not($1) }
  node  { WITH x = Gate.GetLiteral($1) DO
            $$ := x
          END
        }

node: { val : TEXT }
  named  { $$ := $1 }

gate: {  }
  gate { self.gate := Gate.New($1, $2) }