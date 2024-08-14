MODULE Main;

IMPORT ParseParams;
IMPORT Stdio;
IMPORT IO;
IMPORT Math;
IMPORT Debug;
IMPORT Params;
IMPORT Fmt;

CONST Usage = "exampleprog usage wrong!";

VAR
  pp                          := NEW(ParseParams.T).init(Stdio.stderr);

  vdd, delp, deln : LONGREAL;

BEGIN
  TRY
    IF pp.keywordPresent("-vdd") THEN
      vdd := pp.getNextLongReal()
    END; 
    IF pp.keywordPresent("-delp") THEN
      delp := pp.getNextLongReal()
    END; 
    IF pp.keywordPresent("-deln") THEN
      deln := pp.getNextLongReal()
    END; 
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  WITH dv = vdd  - 1.0d0,
       dp = delp - 2.0d0,
       dn = deln - 3.0d0,

       euclid = dv * dv + dp * dp + dn * dn,
       val = Math.exp(euclid) DO
    IO.Put(Fmt.LongReal(val) & "\n")
  END
END Main.
    
    
