MODULE Main;

IMPORT ParseParams;
IMPORT Stdio;
IMPORT IO;
IMPORT Math;
IMPORT Debug;
IMPORT Params;
IMPORT Fmt;
IMPORT FileWr;
IMPORT Wr;

CONST Usage = "exampleprog usage wrong!";

VAR
  pp                          := NEW(ParseParams.T).init(Stdio.stderr);

  temp, vdd, delp, deln : LONGREAL;

BEGIN
  TRY
    IF pp.keywordPresent("-temp") THEN
      temp := pp.getNextLongReal()
    END; 
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

  WITH dv = vdd  - temp,
       dp = delp - 2.0d0,
       dn = deln - 3.0d0,

       euclid = dv * dv + dp * dp + dn * dn,
       val = Math.exp(euclid) DO
    IO.Put(Fmt.LongReal(val) & "\n");

    WITH wr = FileWr.Open("example.out") DO
      Wr.PutText(wr, Fmt.LongReal(val) & "\n");
      Wr.Close(wr)
    END
      
  END
END Main.
    
    
