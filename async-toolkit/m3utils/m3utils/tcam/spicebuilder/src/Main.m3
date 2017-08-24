MODULE Main;
IMPORT TcamAndrewMain;
IMPORT TcamSDG64Main;
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

     Design = { Andrew, SDG64 };

CONST DesignNames = ARRAY Design OF TEXT { "andrew", "sdg64" };

      DoItArray = ARRAY Design OF DoIter { TcamAndrewMain.DoIt, TcamSDG64Main.DoIt };

VAR
  design := Design.Andrew;
  
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
