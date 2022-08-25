MODULE SpecialScan;
IMPORT Scan;
IMPORT Env;
IMPORT FloatMode, Lex;

VAR
  zero := Env.Get("SCANZERO") # NIL;
  
PROCEDURE Int(txt: TEXT; defaultBase: [2..16] := 10): INTEGER
  RAISES {Lex.Error, FloatMode.Trap} =
  BEGIN
    IF zero THEN
      RETURN 0
    ELSE
      RETURN Scan.Int(txt, defaultBase)
    END
  END Int;

PROCEDURE LongReal(txt: TEXT): LONGREAL
  RAISES {Lex.Error, FloatMode.Trap} =
  BEGIN
    IF zero THEN
      RETURN 0.0d0
    ELSE
      RETURN Scan.LongReal(txt)
    END
  END LongReal;

BEGIN END SpecialScan.
