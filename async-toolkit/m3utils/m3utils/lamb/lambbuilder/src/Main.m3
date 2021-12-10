MODULE Main;

(* Main module for running spice simulations of LAMBs *)

IMPORT LambMain;
IMPORT ParseParams;
IMPORT Stdio, Text;
IMPORT Debug;
IMPORT Wr, OSError;
IMPORT Params;
FROM Fmt IMPORT F, Int;

CONST TE = Text.Equal;

VAR
  pp        := NEW(ParseParams.T).init(Stdio.stderr);

TYPE DoIter = PROCEDURE(pp : ParseParams.T) RAISES { ParseParams.Error, Wr.Failure, OSError.E };

     Design = { Lamb };

CONST DesignNames = ARRAY Design OF TEXT { "lamb" };

      DoItArray = ARRAY Design OF DoIter { LambMain.DoIt };

VAR
  design := Design.Lamb;
  
BEGIN
  FOR i := 0 TO Params.Count-1 DO
    Debug.Out(F("Params(%s) : %s", Int(i), Params.Get(i)))
  END;
  IF pp.keywordPresent("-design") THEN
    VAR
      dn := pp.getNext();
      f := FALSE;
    BEGIN
      FOR i := FIRST(Design) TO LAST(Design) DO
        IF TE(dn, DesignNames[i]) THEN
          design := i; f := TRUE
        END
      END;
      IF NOT f THEN Debug.Error("Unknown design \"" & dn & "\"") END
    END
  END;

  DoItArray[design](pp)
END Main.
