MODULE Main;

(* 
   Flexible graphing program 
*)

IMPORT ParseParams;
IMPORT Debug;
IMPORT Pathname;
IMPORT Stdio;
FROM Fmt IMPORT F;
IMPORT Params;
IMPORT TextSeq;
IMPORT Thread;
IMPORT OSError;
IMPORT Scheme;
IMPORT FS;
FROM SchemaGraph IMPORT ReadSchema, ReadData, EvalFormulas, DoSweeps;
  
VAR
  pp                        := NEW(ParseParams.T).init(Stdio.stderr);
  schemaFn : Pathname.T     := NIL;
  dataFiles                 := NEW(TextSeq.T).init();
  scmFiles                  := NEW(TextSeq.T).init();
  scm      : Scheme.T;
  targDir                   := ".";
  doLabels                  := TRUE;
BEGIN
  TRY
    IF pp.keywordPresent("-schema") OR pp.keywordPresent("-s") THEN
      schemaFn := pp.getNext()
    ELSE
      RAISE ParseParams.Error
    END;

    IF pp.keywordPresent("-nolabel") THEN
      doLabels := FALSE
    END;

    IF pp.keywordPresent("-dir") THEN
      targDir := pp.getNext();
      TRY FS.CreateDirectory(targDir) EXCEPT ELSE END
    END;

    WHILE pp.keywordPresent("-scminit") OR pp.keywordPresent("-S") DO
      scmFiles.addhi(pp.getNext())
    END;
    
    pp.skipParsed();

    WHILE pp.next < NUMBER(pp.arg^) DO
      dataFiles.addhi(pp.getNext())
    END
    
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & "\n" )
  END;

  WITH scmarr = NEW(REF ARRAY OF Pathname.T, scmFiles.size()) DO
    FOR i := FIRST(scmarr^) TO LAST(scmarr^) DO
      scmarr[i] := scmFiles.get(i)
    END;
    TRY
      scm := NEW(Scheme.T).init(scmarr^)
    EXCEPT
      Scheme.E(x) =>
      Debug.Error(F("EXCEPTION! Scheme.E \"%s\" when initializing interpreter", x))
    END
  END;

  WITH schema = ReadSchema(schemaFn),
       data   = ReadData(schema, dataFiles) DO
    EvalFormulas(scm, schema, data);
    DoSweeps(targDir, schema, data, doLabels)
  END
  
END Main.
