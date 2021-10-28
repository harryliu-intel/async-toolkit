INTERFACE Glitch;
IMPORT GlitchExpr;

PROCEDURE Output(nm : TEXT);

PROCEDURE Async(nm : TEXT);

PROCEDURE Gate(tgt : TEXT; expr : GlitchExpr.T);

PROCEDURE Literal(nm : TEXT; expr : GlitchExpr.T);

PROCEDURE GetLiteral(nm : TEXT) : GlitchExpr.T;

PROCEDURE RunChecks() : BOOLEAN;
  
CONST Brand = "Glitch";
      
END Glitch.
