MODULE Cellophane EXPORTS Main;
IMPORT Text;
IMPORT Debug;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Pathname;
IMPORT RegEx, RegExPatList;
FROM Fmt IMPORT F, Int;
IMPORT Params;
IMPORT AL;
IMPORT OSError;
IMPORT SpiceError;
IMPORT FileRd;
IMPORT SpiceFormat;
IMPORT Rd;
IMPORT SpiceCircuit;
IMPORT Wr;
IMPORT FileWr;
IMPORT Thread;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;
      
VAR doDebug := Debug.DebugThis("Cellophane");

CONST Usage = "";


PROCEDURE WrapOne(wr : Wr.T; tn : TEXT; ckt : SpiceCircuit.T) =
  BEGIN
    TRY
      Wr.PutText(wr, F(".SUBCKT %s%s", pfx, tn));
      FOR i := 0 TO ckt.params.size() - 1 DO
        Wr.PutText(wr, " ");
        Wr.PutText(wr, ckt.params.get(i));
      END;
      Wr.PutText(wr, "\n");

      
      Wr.PutText(wr, "Xprobed ");
      FOR i := 0 TO ckt.params.size() - 1 DO
        Wr.PutText(wr, ckt.params.get(i));
        Wr.PutText(wr, " ");
      END;
      Wr.PutText(wr, tn);
      Wr.PutText(wr, "\n");
      
      FOR i := 0 TO ckt.params.size() - 1 DO
        WITH p = ckt.params.get(i) DO
          Wr.PutText(wr, F(".PROBE v(%s)\n", p))
        END
      END;
      Wr.PutText(wr, ".ENDS\n\n");
    EXCEPT
      Wr.Failure(e) => Debug.Error("WrapOne: I/O error writing output : Wr.Failure : " & AL.Format(e))
    END
  END WrapOne;
  
VAR
  pp                        := NEW(ParseParams.T).init(Stdio.stderr);
  ifn, ofn : Pathname.T;
  pats : RegExPatList.T := NIL;
  spice : SpiceFormat.T;
  pfx   := "WRAP_";
  thisPat : TEXT;
  wr : Wr.T;
BEGIN

  ofn := "-";

  TRY
    IF pp.keywordPresent("-i") THEN
      ifn := pp.getNext()
    END;
    IF pp.keywordPresent("-o") THEN
      ofn := pp.getNext()
    END;
    IF pp.keywordPresent("-pfx") THEN
      pfx := pp.getNext()
    END;
    WHILE pp.keywordPresent("-p") DO
      thisPat := pp.getNext();
      pats := RegExPatList.Cons(RegEx.Compile(thisPat), pats)
    END
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  |
    RegEx.Error => Debug.Error("Illegal regex : " & thisPat)
  END;

  TRY
    WITH spiceRd = FileRd.Open(ifn) DO
      spice := SpiceFormat.ParseSpice(spiceRd, ".", ifn);
      Rd.Close(spiceRd)
    END
  EXCEPT
    OSError.E(e) =>
    Debug.Error(F("Can't open top level file %s : OSError.E : %s",
                  ifn, AL.Format(e)))
  |
    Rd.Failure(e) =>
    Debug.Error(F("I/O error parsing top level file %s : Rd.Failure : %s",
                  ifn, AL.Format(e)))
  |
    SpiceError.E(e) =>
    Debug.Error(F("Parsing input : caught SpiceError.E : %s at line %s of file %s",
                  e.msg, Int(e.lNo), Debug.UnNil(e.fn)))
  END;

  TRY
    IF TE(ofn, "-") THEN
      wr := Stdio.stdout
    ELSE
      wr := FileWr.Open(ofn)
    END
  EXCEPT
    OSError.E(e) =>
    Debug.Error(F("Can't open output file %s : OSError.E : %s",
                  ofn, AL.Format(e)))
  END;
  
  VAR
    iter := spice.subCkts.iterate();
    tn : TEXT;
    ckt : SpiceCircuit.T;
    ok : BOOLEAN;
    p : RegExPatList.T;
  BEGIN
    WHILE iter.next(tn, ckt) DO
      IF pats = NIL THEN
        ok := TRUE
      ELSE
        ok := FALSE;
        p := pats;
        WHILE p # NIL DO
          IF RegEx.Execute(p.head, tn) # -1 THEN
            (* we have a match *)
            ok := TRUE;
            EXIT
          END;
          p := p.tail
        END
      END;

      IF ok THEN
        WrapOne(wr, tn, ckt)
      END
    END
  END;

  TRY
    Wr.Close(wr)
  EXCEPT
    Wr.Failure(e) => Debug.Error("WrapOne: I/O error closing output : Wr.Failure : " & AL.Format(e))
  END  
END Cellophane.
