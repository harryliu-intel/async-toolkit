MODULE Main;

(* look at die cost of chopping Tofino Creek in two *)

IMPORT Math;
IMPORT Fmt; FROM Fmt IMPORT F;
IMPORT Wr, FileWr;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT SchemeM3;
IMPORT SchemeStubs;
IMPORT ReadLine, SchemeReadLine;
IMPORT Debug;
IMPORT Scheme;
IMPORT Pathname;
IMPORT Thread;
IMPORT TextSeq;
IMPORT MySQL;

PROCEDURE GetPaths(extras : TextSeq.T) : REF ARRAY OF Pathname.T = 
  CONST
    fixed = ARRAY OF Pathname.T { "require", "m3" };
  VAR
    res := NEW(REF ARRAY OF Pathname.T, NUMBER(fixed) + extras.size());
  BEGIN
    FOR i := 0 TO NUMBER(fixed) - 1 DO
      res[i] := fixed[i]
    END;
    FOR i := NUMBER(fixed) TO extras.size() + NUMBER(fixed) - 1 DO
      res[i] := extras.remlo()
    END;
    RETURN res
  END GetPaths;
  
VAR
  pp := NEW(ParseParams.T).init(Stdio.stderr);
  doScheme := TRUE;
  extra := NEW(TextSeq.T).init();
BEGIN

  VAR mysql : MySQL.T; BEGIN
    mysql := MySQL.Init(NIL)
  END;

  
  TRY
    doScheme := pp.keywordPresent("-scm");
    pp.skipParsed();
    WITH n = NUMBER(pp.arg^) - pp.next DO
      FOR i := 0 TO n - 1 DO
        extra.addhi(pp.getNext())
      END
    END;
    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  SchemeStubs.RegisterStubs();
  TRY
    WITH scm = NEW(SchemeM3.T).init(GetPaths(extra)^) DO
      SchemeReadLine.MainLoop(NEW(ReadLine.Default).init(), scm)
    END
  EXCEPT
    Scheme.E(err) => Debug.Error("Caught Scheme.E : " & err)
  END
END Main.
