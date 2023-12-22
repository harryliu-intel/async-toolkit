INTERFACE SpecialScan;
IMPORT Lex, FloatMode;

PROCEDURE Int(txt: TEXT; defaultBase: [2..16] := 10): INTEGER
  RAISES {Lex.Error, FloatMode.Trap};

PROCEDURE LongReal(txt: TEXT): LONGREAL
  RAISES {Lex.Error, FloatMode.Trap};

PROCEDURE String(txt : TEXT) : TEXT;
  
END SpecialScan.
