%source glitch.t glitch.y
%import glitchLexStd glitchParse

%module {
IMPORT Debug;
IMPORT GlitchExpr;
IMPORT Glitch;
}

%interface {
IMPORT RefList;
IMPORT GlitchExpr;
}

%public {
  lst : RefList.T;
}

name:  { val : TEXT }
  ident   { Debug.Out("Saw ident " & $1.val);
            $$ := $1 }

expression: { val : GlitchExpr.T }
  paren { $$ := $1 }
  and   { $$ := GlitchExpr.And($1, $2) }
  or    { $$ := GlitchExpr.Or($1, $2) }
  xor   { $$ := GlitchExpr.Or(GlitchExpr.And($1, $2),GlitchExpr.Not(GlitchExpr.Or($1,$2))) }
  not   { $$ := GlitchExpr.Not($1) }
  node  { WITH x = Glitch.GetLiteral($1) DO
            $$ := x
          END
        }

node: { val : TEXT }
  named  { $$ := $1 }

outputDecl:
  output { Glitch.Output($1) }
  
asyncDecl:
  async { Glitch.Async($1) }
  
gate:
  gate { Glitch.Gate($1, $2) }  