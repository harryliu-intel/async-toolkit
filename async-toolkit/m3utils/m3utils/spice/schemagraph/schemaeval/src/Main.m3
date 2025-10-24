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
IMPORT Scheme, SchemeM3;
IMPORT FS;
FROM SchemaGraph IMPORT ReadSchema, ReadData, EvalFormulas, DoSweeps;
IMPORT Wr;
IMPORT SchemeString;
  
VAR
  pp                        := NEW(ParseParams.T).init(Stdio.stderr);
  schemaFn : Pathname.T;
  dataFiles                 := NEW(TextSeq.T).init();
  scmFiles                  := NEW(TextSeq.T).init();
  scm      : Scheme.T;
  toEval   : TEXT;
  targDir                   := ".";
BEGIN
  TRY
    IF pp.keywordPresent("-schema") OR pp.keywordPresent("-s") THEN
      schemaFn := pp.getNext()
    ELSE
      Debug.Error("? must specify -schema")
    END;

    IF pp.keywordPresent("-dir") THEN
      targDir := pp.getNext();
      TRY FS.CreateDirectory(targDir) EXCEPT ELSE END
    END;

    WHILE pp.keywordPresent("-scminit") OR pp.keywordPresent("-S") OR pp.keywordPresent("-scm") DO
      scmFiles.addhi(pp.getNext())
    END;

    IF pp.keywordPresent("-data") THEN
      dataFiles.addhi(pp.getNext())
    ELSE
      Debug.Error("? must specify -data")
    END;

    IF pp.keywordPresent("-eval") THEN
      toEval := pp.getNext()
    END;

    IF toEval = NIL THEN
      Debug.Error("? must specify -eval")
    END;

    pp.skipParsed();

    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & "\n" )
  END;

  WITH scmarr = NEW(REF ARRAY OF Pathname.T, scmFiles.size()) DO
    FOR i := FIRST(scmarr^) TO LAST(scmarr^) DO
      scmarr[i] := scmFiles.get(i)
    END;
    TRY
      scm := NEW(SchemeM3.T).init(scmarr^)
    EXCEPT
      Scheme.E(x) =>
      Debug.Error(F("EXCEPTION! Scheme.E \"%s\" when initializing interpreter", x))
    END
  END;

  TRY
    WITH schema = ReadSchema(schemaFn),
         data   = ReadData(schema, dataFiles) DO
      EvalFormulas(scm, schema, data);
      Wr.PutText(Stdio.stdout,
                 SchemeString.ToText(scm.loadEvalText(F("(stringify %s)", toEval))));
      Wr.PutChar(Stdio.stdout, '\n')
    END
  EXCEPT
    Scheme.E(x) =>
    Debug.Error("?error in Scheme interpreter : " & x)
  END
  
END Main.
